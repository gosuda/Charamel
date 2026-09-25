open Lwt.Infix

type script = {
  status : int;
  headers : (string * string) list;
  pieces : string list;
  chunked : bool;
  before : float;
  hold : float option;
  truncate : int option;
}

type t = {
  mutable port : int;
  scripts : script Queue.t;
  mutable request_path : string option;
  mutable request_body : string option;
  mutable request_headers : (string * string) list;
  mutable served : int;
}

let lower = String.lowercase_ascii
let return = Lwt.return
let return_unit = Lwt.return_unit

(* A fixture connection ends whenever the client stops reading early, so input and output
   failures close the script instead of failing the test fiber. *)
let swallow body =
  Lwt.catch body (function
    | End_of_file | Lwt.Canceled | Lwt_unix.Timeout -> return_unit
    | Unix.Unix_error _ -> return_unit
    | exn -> Lwt.fail exn)

let read_line ic =
  Lwt.catch
    (fun () -> Lwt_io.read_line ic >|= fun line -> Some line)
    (function End_of_file -> return None | exn -> Lwt.fail exn)

let rec read_headers ic acc =
  read_line ic >>= function
  | None | Some "" -> return (List.rev acc)
  | Some line -> (
      match String.index_opt line ':' with
      | Some position ->
          let name = lower (String.trim (String.sub line 0 position)) in
          let value =
            String.trim
              (String.sub line (position + 1) (String.length line - position - 1))
          in
          read_headers ic ((name, value) :: acc)
      | None -> read_headers ic acc)

let read_request_body ic count =
  let buffer = Buffer.create count in
  let rec loop remaining =
    if remaining <= 0 then return (Buffer.contents buffer)
    else
      Lwt.catch
        (fun () -> Lwt_io.read ~count:remaining ic)
        (function End_of_file -> return "" | exn -> Lwt.fail exn)
      >>= fun chunk ->
      if String.length chunk = 0 then return (Buffer.contents buffer)
      else (
        Buffer.add_string buffer chunk;
        loop (remaining - String.length chunk))
  in
  loop count

let request_path line =
  match String.index_opt line ' ' with
  | None -> None
  | Some position ->
      let rest = String.sub line (position + 1) (String.length line - position - 1) in
      Some
        (match String.index_opt rest ' ' with
        | Some stop -> String.sub rest 0 stop
        | None -> rest)

let body_length pieces =
  List.fold_left (fun total piece -> total + String.length piece) 0 pieces

let head_lines script =
  let content_type =
    if Option.is_some (List.assoc_opt "content-type" script.headers) then []
    else [ ("content-type", "text/event-stream") ]
  in
  let framing =
    if script.chunked then [ ("transfer-encoding", "chunked") ]
    else [ ("content-length", string_of_int (body_length script.pieces)) ]
  in
  Printf.sprintf "HTTP/1.1 %d %s\r\n" script.status
    (Cohttp.Code.reason_phrase_of_code script.status)
  :: List.map
       (fun (name, value) -> Printf.sprintf "%s: %s\r\n" name value)
       (framing @ content_type @ script.headers)
  @ [ "\r\n" ]

let write_piece oc script piece =
  let payload =
    if script.chunked then Printf.sprintf "%x\r\n%s\r\n" (String.length piece) piece
    else piece
  in
  Lwt_io.write oc payload >>= fun () -> Lwt_io.flush oc

let write_pieces oc script =
  let rec loop pieces =
    match pieces with
    | [] -> return_unit
    | piece :: rest -> (
        write_piece oc script piece >>= fun () ->
        match (script.hold, rest) with
        | Some seconds, [] -> Lwt_unix.sleep seconds >>= fun () -> loop rest
        | _ -> loop rest)
  in
  loop script.pieces

let write_body oc script =
  match script.truncate with
  | Some limit ->
      let body = String.concat "" script.pieces in
      Lwt_io.write oc (String.sub body 0 (min limit (String.length body))) >>= fun () ->
      Lwt_io.flush oc
  | None ->
      write_pieces oc script >>= fun () ->
      if script.chunked then Lwt_io.write oc "0\r\n\r\n" >>= fun () -> Lwt_io.flush oc
      else return_unit

let respond_script oc script =
  Lwt_list.iter_s (fun line -> Lwt_io.write oc line) (head_lines script) >>= fun () ->
  Lwt_io.flush oc >>= fun () -> write_body oc script

let missing =
  {
    status = 500;
    headers = [];
    pieces = [];
    chunked = false;
    before = 0.;
    hold = None;
    truncate = None;
  }

let serve t ic oc =
  read_line ic >>= function
  | None -> return_unit
  | Some request_line ->
      read_headers ic [] >>= fun headers ->
      let content_length =
        match List.assoc_opt "content-length" headers with
        | Some value -> Option.value ~default:0 (int_of_string_opt value)
        | None -> 0
      in
      read_request_body ic content_length >>= fun body ->
      t.request_path <- request_path request_line;
      t.request_body <- Some body;
      t.request_headers <- headers;
      t.served <- t.served + 1;
      let script = Option.value ~default:missing (Queue.take_opt t.scripts) in
      (if Float.compare script.before 0. > 0 then Lwt_unix.sleep script.before
       else return_unit)
      >>= fun () -> swallow (fun () -> respond_script oc script)

(* The two channels share one descriptor, so the second close reports it as gone. *)
let close_channels ic oc =
  Lwt.catch
    (fun () -> Lwt_io.close ic >>= fun () -> Lwt_io.close oc)
    (fun _exn -> return_unit)

let channels fd = (Lwt_io.of_fd ~mode:Lwt_io.Input fd, Lwt_io.of_fd ~mode:Lwt_io.Output fd)

let connection t fd =
  let ic, oc = channels fd in
  Lwt.finalize
    (fun () -> swallow (fun () -> serve t ic oc))
    (fun () -> close_channels ic oc)

let listen switch t =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.bind socket (Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, 0)) >>= fun () ->
  Lwt_unix.listen socket 16;
  t.port <-
    (match Lwt_unix.getsockname socket with
    | Lwt_unix.ADDR_INET (_, selected) -> selected
    | _sockaddr -> 0);
  let connections = ref [] in
  let rec accept_forever () =
    Lwt.catch
      (fun () ->
        Lwt_unix.accept socket >>= fun (fd, _address) ->
        connections := connection t fd :: !connections;
        accept_forever ())
      (fun _exn -> return_unit)
  in
  let acceptor = accept_forever () in
  Lwt.return
    (Lwt_switch.add_hook (Some switch) (fun () ->
         Lwt.cancel acceptor;
         Lwt_list.iter_p
           (fun fiber -> Lwt.catch (fun () -> fiber) (fun _exn -> return_unit))
           (acceptor :: !connections)
         >>= fun () ->
         Lwt.catch (fun () -> Lwt_unix.close socket) (fun _exn -> return_unit)))

let with_server f =
  let t =
    {
      port = 0;
      scripts = Queue.create ();
      request_path = None;
      request_body = None;
      request_headers = [];
      served = 0;
    }
  in
  Lwt_switch.with_switch (fun switch -> listen switch t >>= fun () -> f t)

let uri t path = Uri.of_string (Printf.sprintf "http://127.0.0.1:%d%s" t.port path)

let respond t ?(status = 200) ?(headers = []) ?retry_after ?(before = 0.) ?truncate body =
  let retry =
    match retry_after with
    | Some seconds -> [ ("retry-after", Printf.sprintf "%.3g" seconds) ]
    | None -> []
  in
  Queue.push
    {
      status;
      headers = headers @ retry;
      pieces = [ body ];
      chunked = false;
      before;
      hold = None;
      truncate;
    }
    t.scripts

let respond_chunks t ?(status = 200) ?(headers = []) ?(before = 0.) ?hold chunks =
  Queue.push
    { status; headers; pieces = chunks; chunked = true; before; hold; truncate = None }
    t.scripts

let requests t = t.served
let last_path t = t.request_path
let last_body t = t.request_body
let last_headers t = t.request_headers
