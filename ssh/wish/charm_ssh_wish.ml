let log_src = Logs.Src.create "charm.ssh.wish" ~doc:"Charm SSH wish server"

module Log = (val Logs.src_log log_src : Logs.LOG)

exception Protocol_error of string
exception Connection_stop of string

type pty = { term : string; rows : int; cols : int }

type input_flow = {
  chunks : string option Eio.Stream.t;
  mutable current : string;
  mutable offset : int;
  mutable closed : bool;
}

type output_flow = { send : string -> unit }
type output_item = Packet of string | Flush of unit Eio.Promise.u | Stop

type connection = {
  socket : Eio.Flow.two_way_ty Eio.Resource.t;
  remote_addr : Eio.Net.Sockaddr.stream;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  idle_timeout : float option;
  public_key_auth : (user:string -> Awa.Hostkey.pub -> bool) option;
  password_auth : (user:string -> string -> bool) option;
  mutable state : unit Awa.Server.t;
  lock : Eio.Mutex.t;
  outgoing : output_item Eio.Stream.t;
  mutable worker_sw : Eio.Switch.t option;
  mutable closed : bool;
  mutable channel_id : int32 option;
  mutable handler_started : bool;
  mutable session : session option;
  endpoint : session -> unit;
  banner : string option;
  mutable banner_sent : bool;
}

and session = {
  conn : connection;
  mutable user_name : string;
  mutable authenticated_key : Awa.Hostkey.pub option;
  mutable command_words : string list;
  mutable terminal : pty option;
  mutable environment : (string * string) list;
  resize : (int * int) Eio.Stream.t;
  input_state : input_flow;
  output_state : output_flow;
  error_state : output_flow;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  mutable handler_done : bool;
  mutable exit_requested : int option;
  mutable exit_sent : bool;
}

module Input_source : Eio.Flow.Pi.SOURCE with type t = input_flow = struct
  type t = input_flow

  let read_methods = []

  let rec await_chunk (t : input_flow) =
    if t.offset < String.length t.current then ()
    else if t.closed then raise End_of_file
    else
      match Eio.Stream.take t.chunks with
      | None ->
          t.closed <- true;
          raise End_of_file
      | Some chunk ->
          if String.length chunk = 0 then await_chunk t
          else begin
            t.current <- chunk;
            t.offset <- 0
          end

  let single_read t destination =
    await_chunk t;
    let available = String.length t.current - t.offset in
    let count = min available (Cstruct.length destination) in
    Cstruct.blit_from_string t.current t.offset destination 0 count;
    t.offset <- t.offset + count;
    count
end

module Output_sink : Eio.Flow.Pi.SINK with type t = output_flow = struct
  type t = output_flow

  let single_write t bufs =
    if bufs = [] then 0
    else begin
      let written = ref 0 in
      List.iter
        (fun buf ->
          let data = Cstruct.to_string buf in
          let length = String.length data in
          if length > 0 then begin
            t.send data;
            written := !written + length
          end)
        bufs;
      !written
    end

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let source_resource state : Eio.Flow.source_ty Eio.Resource.t =
  let ops = Eio.Flow.Pi.source (module Input_source) in
  Eio.Resource.T (state, ops)

let sink_resource state : Eio.Flow.sink_ty Eio.Resource.t =
  let ops = Eio.Flow.Pi.sink (module Output_sink) in
  Eio.Resource.T (state, ops)

let unwrap context = function
  | Ok value -> value
  | Error message -> raise (Protocol_error (Fmt.str "%s: %s" context message))

let mtime_now clock =
  let nanos = Int64.of_float (Eio.Time.now clock *. 1_000_000_000.) in
  Mtime.of_uint64_ns nanos

let remote_name address = Fmt.str "%a" Eio.Net.Sockaddr.pp address

let session_of_connection connection =
  match connection.session with
  | Some session -> session
  | None -> raise (Protocol_error "session is not initialized")

let enqueue connection item = Eio.Stream.add connection.outgoing item

let rate_key = function
  | `Unix path -> "unix:" ^ path
  | `Tcp (ip, _) -> Fmt.str "tcp:%a" Eio.Net.Ipaddr.pp ip

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
  if String.length data > 0 && not connection.closed then
    Eio.Mutex.use_rw ~protect:true connection.lock (fun () ->
        match connection.channel_id with
        | None -> raise (Protocol_error "channel output before channel start")
        | Some id ->
            let state, messages =
              unwrap "encoding channel output"
                (Awa.Server.output_channel_data connection.state id data)
            in
            connection.state <- state;
            emit_many_locked connection messages)

let send_stderr connection data =
  if String.length data > 0 && not connection.closed then
    Eio.Mutex.use_rw ~protect:true connection.lock (fun () ->
        match connection.channel_id with
        | None -> raise (Protocol_error "channel error output before channel start")
        | Some id ->
            emit_message_locked connection
              (Awa.Ssh.Msg_channel_extended_data
                 (recipient_id connection.state id, 1l, data)))

let push_eof (state : input_flow) =
  if not state.closed then begin
    state.closed <- true;
    Eio.Stream.add state.chunks None
  end

let mark_closed connection =
  if not connection.closed then begin
    connection.closed <- true;
    (match connection.session with
    | Some session -> push_eof session.input_state
    | None -> ());
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
        let promise, resolver = Eio.Promise.create () in
        enqueue connection (Flush resolver);
        enqueue connection Stop;
        session.exit_sent <- true;
        Some promise

let request_exit session code =
  Eio.Cancel.protect (fun () ->
      let promise =
        Eio.Mutex.use_rw ~protect:true session.conn.lock (fun () ->
            if Option.is_none session.exit_requested then
              session.exit_requested <- Some code;
            if session.handler_done then
              match session.exit_requested with
              | Some requested -> send_exit_locked session requested
              | None -> None
            else None)
      in
      match promise with Some promise -> Eio.Promise.await promise | None -> ())

let finish_handler session default_code =
  Eio.Mutex.use_rw ~protect:true session.conn.lock (fun () ->
      session.handler_done <- true);
  request_exit session default_code

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
  let stdin t = source_resource t.input_state
  let stdout t = sink_resource t.output_state
  let stderr t = sink_resource t.error_state
  let exit t code = request_exit t code
end

type handler = Session.t -> unit
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
        | None, '\'' | None, '"' -> quoted := Some ch
        | None, c when c = ' ' || c = '\t' || c = '\r' || c = '\n' -> flush ()
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

let set_pty session term cols rows =
  let next = { term; rows = int32_dimension rows; cols = int32_dimension cols } in
  session.terminal <- Some next

let resize_pty session cols rows =
  match session.terminal with
  | None -> ()
  | Some current ->
      let rows = int32_dimension rows in
      let cols = int32_dimension cols in
      session.terminal <- Some { current with rows; cols };
      Eio.Stream.add session.resize (rows, cols)

let channel_start connection id words =
  if connection.handler_started then
    raise (Protocol_error "multiple channel start requests")
  else begin
    connection.channel_id <- Some id;
    let session = session_of_connection connection in
    session.command_words <- words;
    connection.handler_started <- true;
    match connection.worker_sw with
    | None -> raise (Protocol_error "channel started outside connection switch")
    | Some sw ->
        Eio.Fiber.fork ~sw (fun () ->
            try
              connection.endpoint session;
              finish_handler session 0
            with exn ->
              finish_handler session 1;
              raise exn)
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
  (match (connection.banner, connection.banner_sent) with
  | Some banner, false when String.length banner > 0 ->
      emit_message_locked connection (Awa.Ssh.Msg_userauth_banner (banner, ""));
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
      let session = session_of_connection connection in
      if String.length data > 0 then Eio.Stream.add session.input_state.chunks (Some data)
  | Awa.Server.Channel_eof id ->
      connection.channel_id <- Some id;
      push_eof (session_of_connection connection).input_state
  | Awa.Server.Start_shell id -> channel_start connection id []
  | Awa.Server.Channel_exec (id, command) ->
      channel_start connection id (shell_words command)
  | Awa.Server.Channel_subsystem (id, name) ->
      channel_start connection id [ "subsystem"; name ]
  | Awa.Server.Disconnected _ ->
      mark_closed connection;
      raise (Connection_stop "peer disconnected")

let handle_message (connection : connection) message =
  let now = mtime_now connection.clock in
  let state, replies, event =
    unwrap "processing SSH message" (Awa.Server.input_msg connection.state message now)
  in
  connection.state <- state;
  emit_many_locked connection replies;
  (match event with Some event -> dispatch_event connection event | None -> ());
  match Awa.Server.maybe_rekey connection.state now with
  | None -> ()
  | Some (state, message) ->
      connection.state <- state;
      emit_message_locked connection message

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

let read_once connection buffer =
  let chunk = Cstruct.create 16_384 in
  let read () = Eio.Flow.single_read connection.socket chunk in
  let count =
    match connection.idle_timeout with
    | None -> read ()
    | Some timeout -> Eio.Time.with_timeout_exn connection.clock timeout read
  in
  let data = Cstruct.to_string ~len:count chunk in
  buffer ^ data

let reader connection =
  let rec loop buffer =
    if connection.closed then raise (Connection_stop "connection closed")
    else
      let next = read_once connection buffer in
      let rest =
        Eio.Mutex.use_rw ~protect:true connection.lock (fun () ->
            drain_messages connection next)
      in
      loop rest
  in
  try loop "" with
  | End_of_file ->
      mark_closed connection;
      raise (Connection_stop "peer closed")
  | Eio.Io _ as exn ->
      mark_closed connection;
      raise exn

let writer connection =
  let rec loop () =
    match Eio.Stream.take connection.outgoing with
    | Packet data ->
        if String.length data > 0 then
          Eio.Flow.write connection.socket [ Cstruct.of_string data ];
        loop ()
    | Flush resolver ->
        Eio.Promise.resolve resolver ();
        loop ()
    | Stop -> raise (Connection_stop "session complete")
  in
  try loop () with
  | Connection_stop _ as exn ->
      mark_closed connection;
      raise exn
  | End_of_file ->
      mark_closed connection;
      raise (Connection_stop "socket closed")
  | Eio.Io _ as exn ->
      mark_closed connection;
      raise exn

let make_connection ~clock ~idle_timeout ~public_key_auth ~password_auth ~socket
    ~remote_addr ~state ~banner endpoint =
  let outgoing = Eio.Stream.create max_int in
  let connection =
    {
      socket;
      remote_addr;
      idle_timeout;
      public_key_auth;
      password_auth;
      clock;
      state;
      lock = Eio.Mutex.create ();
      outgoing;
      worker_sw = None;
      closed = false;
      channel_id = None;
      handler_started = false;
      session = None;
      endpoint;
      banner;
      banner_sent = false;
    }
  in
  let input_state =
    { chunks = Eio.Stream.create max_int; current = ""; offset = 0; closed = false }
  in
  let session =
    {
      conn = connection;
      user_name = "";
      authenticated_key = None;
      command_words = [];
      terminal = None;
      environment = [];
      resize = Eio.Stream.create max_int;
      input_state;
      output_state = { send = send_stdout connection };
      error_state = { send = send_stderr connection };
      clock;
      handler_done = false;
      exit_requested = None;
      exit_sent = false;
    }
  in
  connection.session <- Some session;
  (connection, session)

let run_connection ~clock ~idle_timeout ~public_key_auth ~password_auth ~max_timeout
    ~socket ~remote_addr ~host_key ~banner endpoint =
  let state, initial_messages = Awa.Server.make host_key in
  let connection, _session =
    make_connection ~clock ~idle_timeout ~public_key_auth ~password_auth ~socket
      ~remote_addr ~state ~banner endpoint
  in
  let run () =
    Eio.Switch.run (fun worker_sw ->
        connection.worker_sw <- Some worker_sw;
        Eio.Fiber.fork ~sw:worker_sw (fun () -> writer connection);
        Eio.Mutex.use_rw ~protect:true connection.lock (fun () ->
            emit_many_locked connection initial_messages);
        Eio.Fiber.fork ~sw:worker_sw (fun () -> reader connection);
        Eio.Fiber.await_cancel ())
  in
  match max_timeout with
  | None -> run ()
  | Some timeout -> Eio.Time.with_timeout_exn clock timeout run

let valid_timeout name value =
  match classify_float value with
  | FP_nan | FP_infinite -> invalid_arg (Fmt.str "%s must be finite" name)
  | (FP_zero | FP_normal | FP_subnormal) when value <= 0. ->
      invalid_arg (Fmt.str "%s must be positive" name)
  | FP_zero | FP_normal | FP_subnormal -> value

let serve ~sw ~net ~(clock : float Eio.Time.clock_ty Eio.Resource.t) ~host_key ~addr
    ?idle_timeout ?max_timeout ?banner
    ?(public_key_auth : (user:string -> Awa.Hostkey.pub -> bool) option)
    ?(password_auth : (user:string -> string -> bool) option) (endpoint : handler) : unit
    =
  let idle_timeout = Option.map (valid_timeout "idle_timeout") idle_timeout in
  let max_timeout = Option.map (valid_timeout "max_timeout") max_timeout in
  let awa_host_key =
    match Charm_ssh_keygen.ed25519_seed host_key with
    | Some seed -> Awa.Keys.of_seed `Ed25519 seed
    | None -> invalid_arg "wish requires an Ed25519 host key with awa 0.6.1"
  in
  let listener = Eio.Net.listen ~reuse_addr:true ~backlog:128 ~sw net addr in
  let on_error exn =
    Log.warn (fun message -> message "SSH connection failed: %a" Eio.Exn.pp exn)
  in
  let connection (socket : [> `Generic ] Eio.Net.stream_socket_ty Eio.Resource.t)
      remote_addr =
    try
      run_connection ~clock ~idle_timeout ~public_key_auth ~password_auth ~max_timeout
        ~socket:(socket :> Eio.Flow.two_way_ty Eio.Resource.t)
        ~remote_addr ~host_key:awa_host_key ~banner endpoint
    with
    | Connection_stop _ -> ()
    | Protocol_error message ->
        Log.warn (fun log ->
            log "SSH protocol failure from %s: %s" (remote_name remote_addr) message)
    | Eio.Time.Timeout ->
        Log.warn (fun log ->
            log "SSH connection from %s timed out" (remote_name remote_addr))
  in
  Eio.Net.run_server listener ~on_error connection

let write_line session text = Eio.Flow.copy_string (text ^ "\n") (Session.stdout session)

let tea ~env make _next session =
  let size () =
    match Session.pty session with
    | Some { rows; cols; _ } when rows > 0 && cols > 0 -> (rows, cols)
    | _ -> (24, 80)
  in
  let environment name =
    match List.assoc_opt name (Session.env session) with
    | Some value -> Some value
    | None when String.equal name "TERM" ->
        Option.map (fun ({ term; _ } : Session.pty) -> term) (Session.pty session)
    | None -> None
  in
  let resize_callbacks = Eio.Stream.create max_int in
  let worker_sw =
    match session.conn.worker_sw with
    | Some sw -> sw
    | None -> invalid_arg "wish.tea called outside an SSH connection"
  in
  Eio.Fiber.fork ~sw:worker_sw (fun () ->
      try
        while not session.conn.closed do
          ignore (Eio.Stream.take (Session.resize_events session));
          Eio.Stream.add resize_callbacks (fun () -> ())
        done
      with Eio.Cancel.Cancelled _ -> ());
  let terminal =
    Charm_tea.Terminal.custom ~input:(Session.stdin session)
      ~output:(Session.stdout session) ~size ~on_resize:(Some resize_callbacks)
      ~env:environment
      ~is_tty:(Option.is_some (Session.pty session))
  in
  match Charm_tea.run ~terminal ~clock:(Eio.Stdenv.clock env) (make session) env with
  | Ok _ -> Session.exit session 0
  | Error `Interrupted -> Session.exit session 130
  | Error `Killed -> Session.exit session 137
  | Error (`Exn (exn, _)) ->
      Eio.Flow.copy_string
        (Fmt.str "tea: %s\n" (Printexc.to_string exn))
        (Session.stderr session);
      Session.exit session 1

let active_term next session =
  match Session.pty session with
  | Some _ -> next session
  | None ->
      write_line session "Requires an active PTY";
      Session.exit session 1

let access_control ~authorized next session =
  match Session.public_key session with
  | Some public_key when List.exists (Awa.Hostkey.pub_eq public_key) authorized ->
      next session
  | _ ->
      write_line session "Access denied";
      Session.exit session 1

let logging next (session : session) =
  let started = Eio.Time.now session.clock in
  let address = remote_name (Session.remote_addr session) in
  let command () = String.concat " " (Session.command session) in
  let pty () =
    match Session.pty session with
    | None -> ("-", 0, 0)
    | Some { term; rows; cols } -> (term, rows, cols)
  in
  let term, rows, cols = pty () in
  Log.info (fun log ->
      log "connect user=%S remote=%S public_key=%B command=%S term=%S rows=%d cols=%d"
        (Session.user session) address
        (Option.is_some (Session.public_key session))
        (command ()) term rows cols);
  Fun.protect
    ~finally:(fun () ->
      let duration = Eio.Time.now session.clock -. started in
      Log.info (fun log ->
          log "disconnect user=%S remote=%S duration=%.3fs" (Session.user session) address
            duration))
    (fun () -> next session)

let rate_limit ~per_second ~burst =
  (match classify_float per_second with
  | FP_nan | FP_infinite -> invalid_arg "per_second must be finite"
  | (FP_zero | FP_normal | FP_subnormal) when per_second <= 0. ->
      invalid_arg "per_second must be positive"
  | FP_zero | FP_normal | FP_subnormal -> ());
  if burst <= 0 then invalid_arg "burst must be positive";
  let buckets = Hashtbl.create 16 in
  let lock = Eio.Mutex.create () in
  let allow (session : session) =
    Eio.Mutex.use_rw ~protect:true lock (fun () ->
        let now = Eio.Time.now session.clock in
        let key = rate_key (Session.remote_addr session) in
        let tokens, last =
          match Hashtbl.find_opt buckets key with
          | Some bucket -> bucket
          | None -> (float_of_int burst, now)
        in
        let replenished =
          min (float_of_int burst) (tokens +. (max 0. (now -. last) *. per_second))
        in
        if replenished >= 1. then begin
          Hashtbl.replace buckets key (replenished -. 1., now);
          true
        end
        else begin
          Hashtbl.replace buckets key (replenished, now);
          false
        end)
  in
  fun next session ->
    if allow session then next session
    else begin
      write_line session "rate limit exceeded, please try again later";
      Session.exit session 1
    end

let elapsed next (session : session) =
  let started = Eio.Time.now session.clock in
  Fun.protect
    ~finally:(fun () ->
      let duration = max 0. (Eio.Time.now session.clock -. started) in
      write_line session (Fmt.str "elapsed time: %.3fs" duration))
    (fun () -> next session)
