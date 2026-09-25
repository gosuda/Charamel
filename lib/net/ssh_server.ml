open Lwt.Infix

let return = Lwt.return
let src = Logs.Src.create "charamel.net.ssh" ~doc:"Charamel net SSH server"

module Log = (val Logs.src_log src : Logs.LOG)

let io_error = function Unix.Unix_error (error, _, _) -> Some error | _ -> None

let ignore_error body =
  Lwt.catch
    (fun () -> body ())
    (fun exn -> match io_error exn with Some _ -> Lwt.return_unit | _ -> Lwt.fail exn)

type user = Awa_mirage.Auth.user

let make_user = Awa_mirage.Auth.make_user
let lookup_user = Awa_mirage.Auth.lookup_user

module Flow = struct
  type flow = Lwt_unix.file_descr * Bytes.t
  type error = Unix.error
  type write_error = Mirage_flow.write_error

  let pp_error ppf error = Fmt.string ppf (Unix.error_message error)
  let pp_write_error ppf `Closed = Fmt.string ppf "closed"
  let create ?(buffer_size = 4096) fd = (fd, Bytes.create buffer_size)

  let read_failure exn =
    match io_error exn with
    | Some error -> Lwt.return (Error error)
    | None -> Lwt.fail exn

  let copied buffer received =
    let source = Cstruct.of_bytes ~off:0 ~len:received buffer in
    let copy = Cstruct.create received in
    Cstruct.blit source 0 copy 0 received;
    `Data copy

  let read (fd, buffer) =
    Lwt.catch
      (fun () ->
        Lwt_unix.read fd buffer 0 (Bytes.length buffer) >>= function
        | 0 -> Lwt.return (Ok `Eof)
        | received -> Lwt.return (Ok (copied buffer received)))
      read_failure

  let rec write_all fd buffer offset remaining =
    if remaining = 0 then Lwt.return_unit
    else
      Lwt_unix.write fd buffer offset remaining >>= fun written ->
      write_all fd buffer (offset + written) (remaining - written)

  let write_closed error =
    Log.warn (fun m -> m "ssh flow write failed: %a" pp_error error);
    Lwt.return (Error `Closed)

  let write (fd, _) data =
    let buffer = Cstruct.to_bytes data in
    Lwt.catch
      (fun () -> write_all fd buffer 0 (Cstruct.length data) >|= fun () -> Ok ())
      (fun exn ->
        match io_error exn with Some error -> write_closed error | None -> Lwt.fail exn)

  let writev flow data =
    Lwt_list.fold_left_s
      (fun result chunk ->
        match result with
        | Error _ as failure -> Lwt.return failure
        | Ok () -> write flow chunk)
      (Ok ()) data

  let shutdown (fd, _) mode =
    let command =
      match mode with
      | `read -> Unix.SHUTDOWN_RECEIVE
      | `write -> Unix.SHUTDOWN_SEND
      | `read_write -> Unix.SHUTDOWN_ALL
    in
    ignore_error (fun () ->
        Lwt_unix.shutdown fd command;
        Lwt.return_unit)

  let close (fd, _) = ignore_error (fun () -> Lwt_unix.close fd)
end

module Ssh = Awa_mirage.Make (Flow)

type request = Ssh.request
type exec_callback = Ssh.exec_callback
type error = Ssh.error
type write_error = Ssh.write_error

let pp_error = Ssh.pp_error

let serve_connection ?stop ~host_key ~users ~exec flow =
  let state, messages = Awa.Server.make host_key in
  Lwt.catch
    (fun () ->
      Ssh.spawn_server ?stop state users messages flow exec >|= fun _nexus -> Ok ())
    (function
      | Invalid_argument message -> return (Error (`Msg message)) | exn -> Lwt.fail exn)

let serve_fd ?stop ~host_key ~users ~exec fd =
  let flow = Flow.create fd in
  Lwt.finalize
    (fun () -> serve_connection ?stop ~host_key ~users ~exec flow)
    (fun () -> Flow.close flow)

let set_nodelay fd =
  ignore_error (fun () ->
      Lwt_unix.setsockopt fd Lwt_unix.TCP_NODELAY true;
      Lwt.return_unit)

type server = { port : int; stop : unit -> unit Lwt.t }

let listen ?(backlog = 16) ?(address = Unix.inet_addr_loopback) ~port ~host_key ~users
    ~exec () =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.bind socket (Lwt_unix.ADDR_INET (address, port)) >>= fun () ->
  Lwt_unix.listen socket backlog;
  let bound_port =
    match Lwt_unix.getsockname socket with
    | Lwt_unix.ADDR_INET (_, selected) -> selected
    | _addr -> port
  in
  let switch = Lwt_switch.create () in
  let connections = ref [] in
  let report serve =
    serve >>= function
    | Ok () -> Lwt.return_unit
    | Error error ->
        Log.warn (fun m -> m "ssh connection failed: %a" pp_error error);
        Lwt.return_unit
  in
  let serve_one fd =
    set_nodelay fd >>= fun () ->
    Lwt.catch
      (fun () -> report (serve_fd ~stop:switch ~host_key ~users ~exec fd))
      (function
        | Lwt.Canceled -> Lwt.return_unit
        | exn ->
            Log.warn (fun m -> m "ssh connection raised: %s" (Printexc.to_string exn));
            Lwt.return_unit)
  in
  let rec accept_forever () =
    Lwt.catch
      (fun () ->
        Lwt_unix.accept socket >>= fun (fd, _address) ->
        connections := serve_one fd :: !connections;
        accept_forever ())
      (fun exn ->
        match io_error exn with
        | Some error ->
            Log.info (fun m -> m "ssh listener stopped: %a" Flow.pp_error error);
            Lwt.return_unit
        | None ->
            Log.info (fun m -> m "ssh listener stopped: %s" (Printexc.to_string exn));
            Lwt.return_unit)
  in
  let acceptor = accept_forever () in
  let stop () =
    Lwt.cancel acceptor;
    Lwt_switch.turn_off switch >>= fun () ->
    Lwt_list.iter_p (fun connection -> connection) !connections >>= fun () ->
    ignore_error (fun () -> Lwt_unix.close socket)
  in
  Lwt.return { port = bound_port; stop }
