open Lwt.Infix

let log_src = Logs.Src.create "charamel.ssh.wish" ~doc:"Charamel SSH wish server"

module Log = (val Logs.src_log log_src : Logs.LOG)

exception Protocol_error of string
exception Connection_stop of string

type pty = { term : string; rows : int; cols : int }
type address = [ `Tcp of string * int | `Unix of string ]

module Flow = Charamel_net.Ssh_server.Flow

type output_item = Packet of string | Flush of unit Lwt.u | Stop

type connection = {
  flow : Flow.flow;
  remote_addr : address;
  idle_timeout : float option;
  public_key_auth : (user:string -> Awa.Hostkey.pub -> bool) option;
  password_auth : (user:string -> string -> bool) option;
  mutable state : unit Awa.Server.t;
  lock : Lwt_mutex.t;
  outgoing : output_item Lwt_stream.t;
  emit : output_item option -> unit;
  mutable worker_sw : Lwt_switch.t option;
  mutable handler : unit Lwt.t option;
  mutable closed : bool;
  mutable channel_id : int32 option;
  mutable handler_started : bool;
  mutable session : session option;
  endpoint : session -> unit Lwt.t;
  banner : string option;
  banner_handler : (session -> string option) option;
  mutable banner_sent : bool;
}

and session = {
  conn : connection;
  mutable user_name : string;
  mutable authenticated_key : Awa.Hostkey.pub option;
  mutable command_words : string list;
  mutable terminal : pty option;
  mutable allocated_pty : Charamel_os.Pty.t option;
  mutable environment : (string * string) list;
  resize : (int * int) Lwt_stream.t;
  resize_to : (int * int) option -> unit;
  stdin : Lwt_io.input_channel;
  feed : string option -> unit;
  stdout : Lwt_io.output_channel;
  stderr : Lwt_io.output_channel;
  mutable handler_done : bool;
  mutable exit_requested : int option;
  mutable exit_sent : bool;
}

(* The peer's input as an [Lwt_io] channel over a chunk queue. One [feed (Some chunk)]
   makes one more piece readable; [feed None] — an SSH channel EOF — ends the channel,
   which is what a reader sees as end of input. A read waits for at least one byte,
   because a keypress is not a line. *)
let input_channel () =
  let stream, feed = Lwt_stream.create () in
  let pending = ref "" in
  let rec fill () =
    if not (String.equal !pending "") then Lwt.return_unit
    else
      Lwt_stream.get stream >>= function
      | None -> Lwt.return_unit
      | Some chunk ->
          if String.equal chunk "" then fill ()
          else begin
            pending := chunk;
            Lwt.return_unit
          end
  in
  let read buffer offset length =
    fill () >>= fun () ->
    let count = min length (String.length !pending) in
    if count = 0 then Lwt.return 0
    else begin
      Lwt_bytes.blit_from_string !pending 0 buffer offset count;
      pending := String.sub !pending count (String.length !pending - count);
      Lwt.return count
    end
  in
  (Lwt_io.make ~mode:Lwt_io.Input read, feed)

(* Session output as an [Lwt_io] channel over one sink callback. The SSH layer never
   partially accepts output, so every request is answered as fully written. *)
let output_channel send =
  let write buffer offset length =
    let data = Bytes.sub_string (Lwt_bytes.to_bytes buffer) offset length in
    send data >|= fun () -> length
  in
  Lwt_io.make ~mode:Lwt_io.Output write

let unwrap context = function
  | Ok value -> value
  | Error message -> raise (Protocol_error (Fmt.str "%s: %s" context message))

let with_lock connection body = Lwt_mutex.with_lock connection.lock body

let mtime_now () =
  Int64.of_float (Charamel_os.Time.now Charamel_os.Time.lwt *. 1e9) |> Mtime.of_uint64_ns

let remote_name (address : address) =
  match address with
  | `Unix path -> Fmt.str "unix:%s" path
  | `Tcp (host, port) -> Fmt.str "%s:%d" host port

let session_of_connection connection =
  match connection.session with
  | Some session -> session
  | None -> raise (Protocol_error "session is not initialized")

let enqueue connection item = connection.emit (Some item)

let rate_key (address : address) =
  match address with `Unix path -> "unix:" ^ path | `Tcp (host, _) -> "tcp:" ^ host

let recipient_id (state : unit Awa.Server.t) id =
  match Awa.Channel.lookup id state.Awa.Server.channels with
  | Some channel -> Awa.Channel.their_id channel
  | None -> id

let emit_message_locked connection message =
  let state, wire =
    unwrap "encoding SSH message" (Awa.Server.output_msg connection.state message)
  in
  connection.state <- state;
  enqueue connection (Packet wire)

let emit_many_locked connection messages =
  List.iter (emit_message_locked connection) messages

let send_stdout connection data =
  if String.length data = 0 || connection.closed then Lwt.return_unit
  else
    with_lock connection (fun () ->
        match connection.channel_id with
        | None -> raise (Protocol_error "channel output before channel start")
        | Some id ->
            let state, messages =
              unwrap "encoding channel output"
                (Awa.Server.output_channel_data connection.state id data)
            in
            connection.state <- state;
            emit_many_locked connection messages;
            Lwt.return_unit)

let send_stderr connection data =
  if String.length data = 0 || connection.closed then Lwt.return_unit
  else
    with_lock connection (fun () ->
        match connection.channel_id with
        | None -> raise (Protocol_error "channel error output before channel start")
        | Some id ->
            emit_message_locked connection
              (Awa.Ssh.Msg_channel_extended_data
                 (recipient_id connection.state id, 1l, data));
            Lwt.return_unit)

let mark_closed connection =
  if not connection.closed then begin
    connection.closed <- true;
    (match connection.session with Some session -> session.feed None | None -> ());
    enqueue connection Stop
  end

let send_exit_locked session code =
  let connection = session.conn in
  if connection.closed || session.exit_sent then None
  else
    match connection.channel_id with
    | None ->
        session.exit_sent <- true;
        None
    | Some id ->
        let status =
          Awa.Ssh.Msg_channel_request
            ( recipient_id connection.state id,
              false,
              Awa.Ssh.Exit_status (Int32.of_int code) )
        in
        let state, status_wire =
          unwrap "encoding channel exit status"
            (Awa.Server.output_msg connection.state status)
        in
        connection.state <- state;
        enqueue connection (Packet status_wire);
        let state, eof_wires = Awa.Server.eof connection.state id in
        connection.state <- state;
        List.iter (fun wire -> enqueue connection (Packet wire)) eof_wires;
        let state, close_wire = Awa.Server.close connection.state id in
        connection.state <- state;
        (match close_wire with
        | Some wire -> enqueue connection (Packet wire)
        | None -> ());
        let promise, resolver = Lwt.wait () in
        enqueue connection (Flush resolver);
        enqueue connection Stop;
        session.exit_sent <- true;
        Some promise

(* The request is recorded under the connection lock; the flush is awaited outside it, so
   the exit packets reach the wire before the caller releases its own resources. *)
let request_exit session code =
  with_lock session.conn (fun () ->
      if Option.is_none session.exit_requested then session.exit_requested <- Some code;
      Lwt.return
        (if session.handler_done then
           Option.bind session.exit_requested (fun code -> send_exit_locked session code)
         else None))
  >>= function
  | Some promise -> promise
  | None -> Lwt.return_unit

let finish_handler session default_code =
  with_lock session.conn (fun () ->
      session.handler_done <- true;
      Lwt.return_unit)
  >>= fun () -> request_exit session default_code

module Session = struct
  type t = session
  type nonrec pty = pty

  let user t = t.user_name
  let public_key t = t.authenticated_key
  let command t = t.command_words
  let pty t = t.terminal
  let env t = t.environment
  let remote_addr t = t.conn.remote_addr
  let resize_events t = t.resize
  let stdin t = t.stdin
  let stdout t = t.stdout
  let stderr t = t.stderr
  let exit t code = request_exit t code
  let print t text = Lwt_io.write t.stdout text >>= fun () -> Lwt_io.flush t.stdout
  let print_line t text = print t (text ^ "\n")
  let write_string t text = print t text >|= fun () -> String.length text
  let error t text = Lwt_io.write t.stderr text >>= fun () -> Lwt_io.flush t.stderr
  let error_line t text = error t (text ^ "\n")
  let fatal t text = error_line t text >>= fun () -> exit t 1
end

type handler = Session.t -> unit Lwt.t
type middleware = handler -> handler

let shell_words command =
  let words = ref [] in
  let token = Buffer.create (String.length command) in
  let quoted = ref None in
  let escaped = ref false in
  let flush () =
    if Buffer.length token > 0 then begin
      words := Buffer.contents token :: !words;
      Buffer.clear token
    end
  in
  String.iter
    (fun ch ->
      if !escaped then begin
        Buffer.add_char token ch;
        escaped := false
      end
      else
        match (!quoted, ch) with
        | _, '\\' -> escaped := true
        | Some quote, c when c = quote -> quoted := None
        | Some _, c -> Buffer.add_char token c
        | None, (' ' | '\t' | '\r' | '\n' : Char.t) -> flush ()
        | None, (('\'' | '"') as quote) -> quoted := Some quote
        | None, c -> Buffer.add_char token c)
    command;
  if !escaped then Buffer.add_char token '\\';
  flush ();
  List.rev !words

let update_environment session key value =
  let rec replace prefix = function
    | [] -> List.rev_append prefix [ (key, value) ]
    | (existing, _) :: rest when String.equal existing key ->
        List.rev_append prefix ((key, value) :: rest)
    | item :: rest -> replace (item :: prefix) rest
  in
  session.environment <- replace [] session.environment

let int32_dimension value = max 0 (Int32.to_int value)

let size_allocated session rows cols =
  match session.allocated_pty with
  | None -> ()
  | Some pty -> (
      match Charamel_os.Pty.resize pty ~rows ~cols with Ok () | Error _ -> ())

let set_pty session term cols rows =
  let next = { term; rows = int32_dimension rows; cols = int32_dimension cols } in
  session.terminal <- Some next;
  size_allocated session next.rows next.cols

let resize_pty session cols rows =
  match session.terminal with
  | None -> ()
  | Some current ->
      let rows = int32_dimension rows in
      let cols = int32_dimension cols in
      session.terminal <- Some { current with rows; cols };
      session.resize_to (Some (rows, cols));
      size_allocated session rows cols

let start_handler connection session =
  let task =
    Lwt.try_bind
      (fun () -> connection.endpoint session)
      (fun () -> finish_handler session 0)
      (fun exn -> finish_handler session 1 >>= fun () -> Lwt.fail exn)
  in
  connection.handler <- Some task;
  (match connection.worker_sw with
  | Some sw ->
      Lwt_switch.add_hook (Some sw) (fun () ->
          Lwt.cancel task;
          Lwt.return_unit)
  | None -> ());
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> task)
        (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn))

let channel_start connection id words =
  if connection.handler_started then
    raise (Protocol_error "multiple channel start requests")
  else begin
    connection.channel_id <- Some id;
    let session = session_of_connection connection in
    session.command_words <- words;
    connection.handler_started <- true;
    if Option.is_none connection.worker_sw then
      raise (Protocol_error "channel started outside connection switch")
    else start_handler connection session
  end

let authenticate connection username auth =
  let session = session_of_connection connection in
  let accepted, key =
    match auth with
    | Awa.Server.Pubkey details ->
        let public_key = Awa.Server.pubkey_of_pubkeyauth details in
        let signature_ok = Awa.Server.verify_pubkeyauth ~user:username details in
        let callback_ok =
          match connection.public_key_auth with
          | None -> false
          | Some callback -> signature_ok && callback ~user:username public_key
        in
        (callback_ok, Some public_key)
    | Awa.Server.Password password ->
        let callback_ok =
          match connection.password_auth with
          | None -> false
          | Some callback -> callback ~user:username password
        in
        (callback_ok, None)
  in
  let banner =
    match connection.banner_handler with
    | Some handler -> handler session
    | None -> connection.banner
  in
  (match (banner, connection.banner_sent) with
  | Some text, false when String.length text > 0 ->
      emit_message_locked connection (Awa.Ssh.Msg_userauth_banner (text, ""));
      connection.banner_sent <- true
  | _ -> ());
  if accepted then begin
    let state, reply =
      unwrap "accepting user authentication"
        (Awa.Server.accept_userauth connection.state auth ())
    in
    connection.state <- state;
    emit_message_locked connection reply;
    session.user_name <- username;
    session.authenticated_key <- key
  end
  else begin
    let state, reply =
      unwrap "rejecting user authentication"
        (Awa.Server.reject_userauth connection.state auth)
    in
    connection.state <- state;
    emit_message_locked connection reply
  end

let dispatch_event connection = function
  | Awa.Server.Userauth (username, auth) -> authenticate connection username auth
  | Awa.Server.Pty (term, cols, rows, _pixel_width, _pixel_height, _modes) ->
      set_pty (session_of_connection connection) term cols rows
  | Awa.Server.Pty_set (cols, rows, _pixel_width, _pixel_height) ->
      resize_pty (session_of_connection connection) cols rows
  | Awa.Server.Set_env (key, value) ->
      update_environment (session_of_connection connection) key value
  | Awa.Server.Channel_data (id, data) ->
      connection.channel_id <- Some id;
      if String.length data > 0 then (session_of_connection connection).feed (Some data)
  | Awa.Server.Channel_eof id ->
      connection.channel_id <- Some id;
      (session_of_connection connection).feed None
  | Awa.Server.Start_shell id -> channel_start connection id []
  | Awa.Server.Channel_exec (id, command) ->
      channel_start connection id (shell_words command)
  | Awa.Server.Channel_subsystem (id, name) ->
      channel_start connection id [ "subsystem"; name ]
  | Awa.Server.Disconnected _ ->
      mark_closed connection;
      raise (Connection_stop "peer disconnected")

let handle_message connection message =
  let now = mtime_now () in
  let state, replies, event =
    unwrap "processing SSH message" (Awa.Server.input_msg connection.state message now)
  in
  connection.state <- state;
  emit_many_locked connection replies;
  match event with Some event -> dispatch_event connection event | None -> ()

let rec drain_messages connection buffer =
  let state, message, rest =
    unwrap "decoding SSH packet" (Awa.Server.pop_msg2 connection.state buffer)
  in
  connection.state <- state;
  match message with
  | None -> rest
  | Some message ->
      handle_message connection message;
      if String.length rest > 4_194_304 then
        raise (Protocol_error "SSH input buffer is too large");
      drain_messages connection rest

let read_flow connection =
  let read () =
    match connection.idle_timeout with
    | None -> Flow.read connection.flow
    | Some timeout -> Lwt_unix.with_timeout timeout (fun () -> Flow.read connection.flow)
  in
  read () >>= function
  | Ok `Eof -> Lwt.fail End_of_file
  | Error error -> Lwt.fail (Unix.Unix_error (error, "read", ""))
  | Ok (`Data chunk) -> Lwt.return (Cstruct.to_string chunk)

let reader connection =
  let rec loop buffer =
    if connection.closed then Lwt.fail (Connection_stop "connection closed")
    else
      read_flow connection >>= fun data ->
      with_lock connection (fun () ->
          Lwt.return (drain_messages connection (buffer ^ data)))
      >>= loop
  in
  Lwt.catch
    (fun () -> loop "")
    (function
      | End_of_file ->
          mark_closed connection;
          Lwt.fail (Connection_stop "peer closed")
      | (Unix.Unix_error _ | Lwt_unix.Timeout) as exn ->
          mark_closed connection;
          Lwt.fail exn
      | exn -> Lwt.fail exn)

let writer connection =
  let write data =
    Flow.write connection.flow (Cstruct.of_string data) >>= function
    | Ok () -> Lwt.return_unit
    | Error `Closed ->
        mark_closed connection;
        Lwt.fail (Connection_stop "socket closed")
  in
  let rec loop () =
    Lwt_stream.next connection.outgoing >>= function
    | Packet data -> if String.length data = 0 then loop () else write data >>= loop
    | Flush resolver ->
        (match Lwt.wakeup_later resolver () with () -> () | exception _ -> ());
        loop ()
    | Stop -> Lwt.fail (Connection_stop "session complete")
  in
  Lwt.catch loop (fun exn ->
      mark_closed connection;
      match exn with
      | Connection_stop _ | Unix.Unix_error _ | End_of_file | Lwt_stream.Empty ->
          Lwt.fail (Connection_stop "writer stopped")
      | exn -> Lwt.fail exn)

let make_connection ~idle_timeout ~public_key_auth ~password_auth ~flow ~remote_addr
    ~state ~banner ~banner_handler endpoint =
  let outgoing, emit = Lwt_stream.create () in
  let stdin, feed = input_channel () in
  let resize, resize_to = Lwt_stream.create () in
  let connection =
    {
      flow;
      remote_addr;
      idle_timeout;
      public_key_auth;
      password_auth;
      state;
      lock = Lwt_mutex.create ();
      outgoing;
      emit;
      worker_sw = None;
      handler = None;
      closed = false;
      channel_id = None;
      handler_started = false;
      session = None;
      endpoint;
      banner;
      banner_handler;
      banner_sent = false;
    }
  in
  let session =
    {
      conn = connection;
      user_name = "";
      authenticated_key = None;
      command_words = [];
      terminal = None;
      allocated_pty = None;
      environment = [];
      resize;
      resize_to;
      stdin;
      feed;
      stdout = output_channel (send_stdout connection);
      stderr = output_channel (send_stderr connection);
      handler_done = false;
      exit_requested = None;
      exit_sent = false;
    }
  in
  connection.session <- Some session;
  connection

let report_failure remote_addr = function
  | Connection_stop _ -> ()
  | Protocol_error message ->
      Log.warn (fun log ->
          log "SSH protocol failure from %s: %s" (remote_name remote_addr) message)
  | Lwt_unix.Timeout ->
      Log.warn (fun log ->
          log "SSH connection from %s timed out" (remote_name remote_addr))
  | Unix.Unix_error (error, _, _) ->
      Log.info (fun log ->
          log "SSH connection from %s failed: %s" (remote_name remote_addr)
            (Unix.error_message error))
  | End_of_file -> ()
  | exn ->
      Log.warn (fun log ->
          log "SSH connection from %s raised: %s" (remote_name remote_addr)
            (Printexc.to_string exn))

let run_connection ~idle_timeout ~public_key_auth ~password_auth ~max_timeout ~flow
    ~remote_addr ~host_key ~banner ~banner_handler endpoint =
  let state, initial_messages = Awa.Server.make host_key in
  let connection =
    make_connection ~idle_timeout ~public_key_auth ~password_auth ~flow ~remote_addr
      ~state ~banner ~banner_handler endpoint
  in
  let run () =
    Lwt_switch.with_switch (fun worker_sw ->
        connection.worker_sw <- Some worker_sw;
        with_lock connection (fun () ->
            emit_many_locked connection initial_messages;
            Lwt.return_unit)
        >>= fun () -> Lwt.pick [ reader connection; writer connection ])
  in
  let bounded () =
    match max_timeout with
    | None -> run ()
    | Some timeout -> Lwt_unix.with_timeout timeout run
  in
  let report result =
    match result with Ok () -> () | Error (`Exn exn) -> report_failure remote_addr exn
  in
  Lwt.catch
    (fun () -> bounded () >|= fun () -> Ok ())
    (fun exn -> Lwt.return (Error (`Exn exn)))
  >>= fun result ->
  report result;
  match connection.handler with
  | Some task -> Lwt.catch (fun () -> task) (fun _exn -> Lwt.return_unit)
  | None -> Lwt.return_unit

let listen_address (address : address) =
  match address with
  | `Unix path -> (Lwt_unix.PF_UNIX, Lwt_unix.ADDR_UNIX path)
  | `Tcp (host, port) ->
      let domain =
        if String.contains host ':' then Lwt_unix.PF_INET6 else Lwt_unix.PF_INET
      in
      (domain, Lwt_unix.ADDR_INET (Unix.inet_addr_of_string host, port))

let peer_address (sockaddr : Unix.sockaddr) : address =
  match sockaddr with
  | ADDR_UNIX path -> `Unix path
  | ADDR_INET (address, port) -> `Tcp (Unix.string_of_inet_addr address, port)

let valid_timeout name value =
  match classify_float value with
  | FP_nan | FP_infinite -> invalid_arg (Fmt.str "%s must be finite" name)
  | (FP_zero | FP_normal | FP_subnormal) when value <= 0. ->
      invalid_arg (Fmt.str "%s must be positive" name)
  | FP_zero | FP_normal | FP_subnormal -> value

let windows_unsupported = "pseudo-terminals are not supported on Windows"

let pty_size session =
  match Session.pty session with
  | Some { rows; cols; _ } when rows > 0 && cols > 0 -> (rows, cols)
  | _ -> (24, 80)

let exit_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

let wait_exit_code pid = Lwt_unix.waitpid [] pid >|= fun (_, status) -> exit_code status

let command_env session extra =
  Array.of_list
    (Array.to_list (Unix.environment ())
    @ List.map (fun (key, value) -> Fmt.str "%s=%s" key value) (Session.env session)
    @ extra)

let rec copy_pty session pty =
  Charamel_os.Pty.read pty 4096 >>= function
  | Ok "" -> Lwt.return_unit
  | Ok data -> Session.print session data >>= fun () -> copy_pty session pty
  | Error _ -> Lwt.return_unit

let rec write_pty pty text off =
  let length = String.length text - off in
  if length <= 0 then Lwt.return_unit
  else
    Charamel_os.Pty.write pty text off length >>= function
    | Ok 0 | Error _ -> Lwt.return_unit
    | Ok count -> write_pty pty text (off + count)

let rec pump_input session pty =
  Lwt_io.read ~count:4096 (Session.stdin session) >>= fun data ->
  if String.is_empty data then Lwt.return_unit
  else write_pty pty data 0 >>= fun () -> pump_input session pty

let pty_failure = function
  | `Error message -> message
  | `Unsupported -> windows_unsupported

let with_session_pty session body =
  match session.allocated_pty with
  | Some pty -> body pty
  | None -> (
      let rows, cols = pty_size session in
      Charamel_os.Pty.create ~rows ~cols () >>= function
      | Error reason -> Lwt.return (Error (`Failed (pty_failure reason)))
      | Ok pty ->
          session.allocated_pty <- Some pty;
          Lwt.finalize
            (fun () -> body pty)
            (fun () ->
              session.allocated_pty <- None;
              Charamel_os.Pty.terminate pty;
              Charamel_os.Pty.close pty;
              Lwt.return_unit))

let command ?dir ?env session program args =
  let argv = program :: args in
  let child_env = command_env session (Option.value ~default:[] env) in
  with_session_pty session (fun pty ->
      Charamel_os.Pty.exec pty ?cwd:dir ~env:child_env argv >>= function
      | Error reason -> Lwt.return (Error (`Failed (pty_failure reason)))
      | Ok pid ->
          let input = pump_input session pty in
          Lwt.finalize
            (fun () -> copy_pty session pty)
            (fun () ->
              Lwt.cancel input;
              Lwt.return_unit)
          >>= fun () ->
          Lwt.catch
            (fun () -> wait_exit_code pid >|= fun code -> Ok code)
            (fun exn -> Lwt.return (Error (`Failed (Printexc.to_string exn)))))

let session_exec session = function
  | [] -> invalid_arg "exec requires a command"
  | program :: args -> (
      command session program args >>= function
      | Ok code -> Lwt.return code
      | Error (`Failed reason) -> Lwt.fail (Failure reason))

let tea ~env make _next session =
  let size () = pty_size session in
  let environment name =
    match List.assoc_opt name (Session.env session) with
    | Some value -> Some value
    | None when String.equal name "TERM" ->
        Option.map (fun ({ term; _ } : Session.pty) -> term) (Session.pty session)
    | None -> None
  in
  let notifications, notify = Lwt_stream.create () in
  let relay =
    let rec loop () =
      Lwt_stream.get (Session.resize_events session) >>= function
      | None -> Lwt.return_unit
      | Some (_rows, _cols) ->
          notify (Some Fun.id);
          loop ()
    in
    loop ()
  in
  let input = Charamel_os.Console_input.of_channel (Session.stdin session) in
  let output = Session.stdout session in
  let on_resize = Some notifications in
  let is_tty = Option.is_some (Session.pty session) in
  let terminal =
    match session.allocated_pty with
    | None ->
        Charamel_tea.Terminal.custom ~input ~output ~size ~on_resize ~env:environment
          ~is_tty
    | Some _ ->
        Charamel_tea.Terminal.custom_with_exec ~input ~output ~size ~on_resize
          ~env:environment ~is_tty ~exec:(session_exec session)
  in
  let color_profile =
    match session.allocated_pty with
    | Some _ -> Some (Charamel_colorprofile.detect ~is_tty:true ~env:environment)
    | None -> None
  in
  let report_error exn =
    Session.error session (Fmt.str "tea: %s\n" (Printexc.to_string exn))
  in
  Lwt.finalize
    (fun () ->
      Charamel_tea.run ~terminal ?color_profile ~clock:env.Charamel_cli.Env.clock
        (make session)
      >>= function
      | Ok _ -> Session.exit session 0
      | Error `Interrupted -> Session.exit session 130
      | Error (`Exn (exn, _)) -> report_error exn >>= fun () -> Session.exit session 1)
    (fun () ->
      Lwt.cancel relay;
      Lwt.return_unit)

let tea_with_stream ~env make next session =
  let app, stream = make session in
  let stream_app =
    {
      app with
      Charamel_tea.subscriptions =
        (fun model ->
          Charamel_tea.Sub.batch
            [ app.Charamel_tea.subscriptions model; Charamel_tea.Sub.stream stream ]);
    }
  in
  tea ~env (fun _session -> stream_app) next session

let active_term next session =
  match Session.pty session with
  | Some _ -> next session
  | None ->
      Session.print_line session "Requires an active PTY" >>= fun () ->
      Session.exit session 1

let allocate_pty next session =
  let rows, cols = pty_size session in
  Charamel_os.Pty.create ~rows ~cols () >>= function
  | Error reason ->
      Session.fatal session (Fmt.str "cannot allocate a PTY: %s" (pty_failure reason))
  | Ok pty ->
      session.allocated_pty <- Some pty;
      Lwt.finalize
        (fun () -> next session)
        (fun () ->
          session.allocated_pty <- None;
          Charamel_os.Pty.terminate pty;
          Charamel_os.Pty.close pty;
          Lwt.return_unit)

let emulated_pty session = Option.is_none session.allocated_pty

let public_blob public_key =
  match public_key with
  | Awa.Hostkey.Ed25519_pub pub ->
      Charamel_ssh_keygen.wire_ed25519_blob (Mirage_crypto_ec.Ed25519.pub_to_octets pub)
  | Awa.Hostkey.Rsa_pub rsa ->
      Charamel_ssh_keygen.wire_rsa_blob ~e:rsa.Mirage_crypto_pk.Rsa.e
        ~n:rsa.Mirage_crypto_pk.Rsa.n

let admitted_by public_key entries =
  let blob = public_blob public_key in
  List.exists
    (fun (entry : Charamel_ssh_keygen.authorized_entry) -> String.equal entry.blob blob)
    entries

let deny_session session =
  Session.print_line session "Access denied" >>= fun () -> Session.exit session 1

let password_or_key_auth ~authorized next session =
  match Session.public_key session with
  | Some public_key when List.exists (Awa.Hostkey.pub_eq public_key) authorized ->
      next session
  | _ -> deny_session session

let read_authorized_keys path =
  Lwt.catch
    (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.input path (fun channel ->
          Lwt_io.read channel >|= fun text ->
          Ok (Charamel_ssh_keygen.parse_authorized_keys text)))
    (fun exn -> Lwt.return (Error (Printexc.to_string exn)))

let authorized_keys_file ~path next session =
  read_authorized_keys path >>= function
  | Error message ->
      Log.warn (fun log -> log "authorized_keys %s: %s" path message);
      deny_session session
  | Ok entries -> (
      match Session.public_key session with
      | Some public_key when admitted_by public_key entries -> next session
      | Some _ | None -> deny_session session)

let allow_commands allowed next session =
  match Session.command session with
  | [] -> next session
  | program :: _ when List.exists (String.equal program) allowed -> next session
  | program :: _ -> Session.fatal session (Fmt.str "Command is not allowed: %s" program)

let subsystem handlers next session =
  match Session.command session with
  | [ {|subsystem|}; name ] -> (
      match List.assoc_opt name handlers with
      | Some handler -> handler session
      | None -> Session.fatal session (Fmt.str "unknown subsystem: %s" name))
  | _ -> next session

let comment text next session = next session >>= fun () -> Session.print_line session text

let recover next session =
  Lwt.catch
    (fun () -> next session)
    (fun exn ->
      Log.err (fun log ->
          log "panic: %s\n%s" (Printexc.to_string exn) (Printexc.get_backtrace ()));
      Session.exit session 1)

let logging next (session : session) =
  let started = Charamel_os.Time.now Charamel_os.Time.lwt in
  let address = remote_name (Session.remote_addr session) in
  let command () = String.concat " " (Session.command session) in
  let term, rows, cols =
    match Session.pty session with
    | None -> ("-", 0, 0)
    | Some { term; rows; cols } -> (term, rows, cols)
  in
  Log.info (fun log ->
      log "connect user=%S remote=%S public_key=%B command=%S term=%S rows=%d cols=%d"
        (Session.user session) address
        (Option.is_some (Session.public_key session))
        (command ()) term rows cols);
  let disconnect () =
    let duration = Charamel_os.Time.now Charamel_os.Time.lwt -. started in
    Log.info (fun log ->
        log "disconnect user=%S remote=%S duration=%.3fs" (Session.user session) address
          duration)
  in
  Lwt.finalize
    (fun () -> next session)
    (fun () ->
      disconnect ();
      Lwt.return_unit)

let default_rate_entries = 1024

let evict_oldest buckets =
  let target = ref None in
  Hashtbl.iter
    (fun key (_, _, seen) ->
      match !target with
      | Some (_, best) when best <= seen -> ()
      | _ -> target := Some (key, seen))
    buckets;
  match !target with Some (key, _) -> Hashtbl.remove buckets key | None -> ()

let token_bucket ?(max_entries = default_rate_entries) ~per_second ~burst () =
  (match classify_float per_second with
  | FP_nan | FP_infinite -> invalid_arg "per_second must be finite"
  | (FP_zero | FP_normal | FP_subnormal) when per_second <= 0. ->
      invalid_arg "per_second must be positive"
  | FP_zero | FP_normal | FP_subnormal -> ());
  if burst <= 0 then invalid_arg "burst must be positive";
  if max_entries <= 0 then invalid_arg "max_entries must be positive";
  let buckets = Hashtbl.create 16 in
  let seen = ref 0 in
  let allow key =
    let now = Charamel_os.Time.now Charamel_os.Time.lwt in
    let tokens, last =
      match Hashtbl.find_opt buckets key with
      | Some (tokens, last, _) -> (tokens, last)
      | None -> (float_of_int burst, now)
    in
    let replenished =
      min (float_of_int burst) (tokens +. (max 0. (now -. last) *. per_second))
    in
    let allowed = replenished >= 1. in
    let tokens = if allowed then replenished -. 1. else replenished in
    incr seen;
    Hashtbl.replace buckets key (tokens, now, !seen);
    if Hashtbl.length buckets > max_entries then evict_oldest buckets;
    allowed
  in
  (allow, fun () -> Hashtbl.length buckets)

type rate_limiter = Session.t -> bool

let rate_limit_custom limiter next session =
  if limiter session then next session
  else
    Session.print_line session "rate limit exceeded, please try again later" >>= fun () ->
    Session.exit session 1

let rate_limit ?max_entries ~per_second ~burst =
  let allow, _ = token_bucket ?max_entries ~per_second ~burst () in
  rate_limit_custom (fun session -> allow (rate_key (Session.remote_addr session)))

let duration_template : (float -> string, Format.formatter, unit, string) format4 = "%f"

let elapsed_format template =
  try Scanf.format_from_string template duration_template
  with Scanf.Scan_failure _ ->
    invalid_arg "elapsed_with_format needs one float placeholder"

let elapsed_with_format template =
  let format = elapsed_format template in
  fun next session ->
    let started = Charamel_os.Time.now Charamel_os.Time.lwt in
    let report () =
      let duration = max 0. (Charamel_os.Time.now Charamel_os.Time.lwt -. started) in
      Session.print_line session (Fmt.str format duration)
    in
    Lwt.finalize
      (fun () -> next session)
      (fun () -> report () >>= fun () -> Lwt.return_unit)

let elapsed = elapsed_with_format "elapsed time: %.3fs"

let awa_ed25519 seed =
  match Mirage_crypto_ec.Ed25519.priv_of_octets seed with
  | Ok priv -> Awa.Hostkey.Ed25519_priv priv
  | Error _ -> invalid_arg "wish requires a 32-byte Ed25519 host key seed"

let serve ?stop ~host_key ~addr ?idle_timeout ?max_timeout ?banner ?banner_handler
    ?(public_key_auth : (user:string -> Awa.Hostkey.pub -> bool) option)
    ?(password_auth : (user:string -> string -> bool) option) (endpoint : handler) () =
  let idle_timeout = Option.map (valid_timeout "idle_timeout") idle_timeout in
  let max_timeout = Option.map (valid_timeout "max_timeout") max_timeout in
  let awa_host_key =
    match Charamel_ssh_keygen.ed25519_seed host_key with
    | Some seed -> awa_ed25519 seed
    | None -> invalid_arg "wish requires an Ed25519 host key with awa 0.6.1"
  in
  let domain, sockaddr = listen_address addr in
  let listener = Lwt_unix.socket domain Lwt_unix.SOCK_STREAM 0 in
  let connections = ref [] in
  let serve_one fd remote_addr =
    let flow = Flow.create fd in
    Lwt.finalize
      (fun () ->
        run_connection ~idle_timeout ~public_key_auth ~password_auth ~max_timeout ~flow
          ~remote_addr ~host_key:awa_host_key ~banner ~banner_handler endpoint)
      (fun () -> Flow.close flow)
  in
  let rec accept_forever () =
    Lwt.catch
      (fun () ->
        Lwt_unix.accept listener >>= fun (fd, peer) ->
        let connection = serve_one fd (peer_address peer) in
        connections := connection :: !connections;
        Lwt.async (fun () ->
            Lwt.catch
              (fun () -> connection)
              (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn));
        accept_forever ())
      (function
        | Lwt.Canceled | Unix.Unix_error _ -> Lwt.return_unit | exn -> Lwt.fail exn)
  in
  (* Reached twice on an ordinary shutdown — once from the switch hook, once when the accept
     loop ends — so the descriptor is released at most once. *)
  let listener_open = ref true in
  let close_listener () =
    if not !listener_open then Lwt.return_unit
    else begin
      listener_open := false;
      Lwt_unix.close listener
    end
  in
  let stop_all running =
    Lwt.cancel running;
    Lwt_list.iter_p
      (fun connection ->
        Lwt.cancel connection;
        Lwt.catch (fun () -> connection) (fun _exn -> Lwt.return_unit))
      !connections
    >>= fun () -> close_listener ()
  in
  Lwt_unix.setsockopt listener Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.bind listener sockaddr >>= fun () ->
  Lwt_unix.listen listener 128;
  let running = accept_forever () in
  (match stop with
  | None -> ()
  | Some switch -> Lwt_switch.add_hook (Some switch) (fun () -> stop_all running));
  running >>= fun () -> stop_all running
