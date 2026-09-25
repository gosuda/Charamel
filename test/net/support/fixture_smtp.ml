open Lwt.Infix

type t = {
  mutable port : int;
  mutable message_body : string option;
  mutable served : int;
}

let return = Lwt.return
let return_unit = Lwt.return_unit
let write oc value = Lwt_io.write oc value >>= fun () -> Lwt_io.flush oc

let read_line ic =
  Lwt.catch
    (fun () -> Lwt_io.read_line ic >|= fun line -> Some line)
    (function End_of_file -> return None | exn -> Lwt.fail exn)

let collect_data ic =
  let output = Buffer.create 1024 in
  let rec loop () =
    read_line ic >>= function
    | None -> return None
    | Some "." -> return (Some (Buffer.contents output))
    | Some line ->
        if Buffer.length output > 0 then Buffer.add_string output "\r\n";
        Buffer.add_string output line;
        loop ()
  in
  loop ()

let session t ic oc =
  let rec respond reply = write oc reply >>= fun () -> loop ()
  and challenge prompt next =
    write oc prompt >>= fun () ->
    read_line ic >>= function None -> return_unit | Some _credential -> next ()
  and loop () =
    read_line ic >>= function
    | None -> return_unit
    | Some line ->
        let command = String.uppercase_ascii line in
        if String.starts_with ~prefix:"EHLO" command then
          respond "250-pop-test\r\n250-AUTH PLAIN LOGIN\r\n250 OK\r\n"
        else if String.starts_with ~prefix:"HELO" command then respond "250 pop-test\r\n"
        else if String.starts_with ~prefix:"AUTH PLAIN" command then
          respond "235 2.7.0 authenticated\r\n"
        else if String.starts_with ~prefix:"AUTH LOGIN" command then
          challenge "334 VXNlcm5hbWU6\r\n" (fun () ->
              challenge "334 UGFzc3dvcmQ6\r\n" (fun () ->
                  respond "235 2.7.0 authenticated\r\n"))
        else if String.starts_with ~prefix:"MAIL FROM:" command then
          respond "250 2.1.0 OK\r\n"
        else if String.starts_with ~prefix:"RCPT TO:" command then
          respond "250 2.1.5 OK\r\n"
        else if String.equal command "DATA" then (
          write oc "354 End data with <CR><LF>.<CR><LF>\r\n" >>= fun () ->
          collect_data ic >>= fun body ->
          t.message_body <- body;
          respond "250 2.0.0 queued\r\n")
        else if String.equal command "QUIT" then write oc "221 2.0.0 bye\r\n"
        else respond "250 2.0.0 OK\r\n"
  in
  write oc "220 pop-test ESMTP\r\n" >>= fun () -> loop ()

let swallow body =
  Lwt.catch body (function
    | End_of_file | Lwt.Canceled | Unix.Unix_error _ -> return_unit
    | exn -> Lwt.fail exn)

let channels fd = (Lwt_io.of_fd ~mode:Lwt_io.Input fd, Lwt_io.of_fd ~mode:Lwt_io.Output fd)

let connection t fd =
  let ic, oc = channels fd in
  Lwt.finalize
    (fun () ->
      t.served <- t.served + 1;
      swallow (fun () -> session t ic oc))
    (fun () ->
      Lwt.catch
        (fun () -> Lwt_io.close ic >>= fun () -> Lwt_io.close oc)
        (function
          | End_of_file | Lwt.Canceled | Unix.Unix_error _ -> return_unit
          | exn -> Lwt.fail exn))

let listen switch t =
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.bind socket (Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, 0)) >>= fun () ->
  Lwt_unix.listen socket 8;
  t.port <-
    (match Lwt_unix.getsockname socket with
    | Lwt_unix.ADDR_INET (_, selected) -> selected
    | _sockaddr -> 0);
  let sessions = ref [] in
  let rec accept_forever () =
    Lwt.catch
      (fun () ->
        Lwt_unix.accept socket >>= fun (fd, _address) ->
        sessions := connection t fd :: !sessions;
        accept_forever ())
      (fun _exn -> return_unit)
  in
  let acceptor = accept_forever () in
  Lwt.return
    (Lwt_switch.add_hook (Some switch) (fun () ->
         Lwt.cancel acceptor;
         Lwt_list.iter_p
           (fun fiber -> Lwt.catch (fun () -> fiber) (fun _exn -> return_unit))
           (acceptor :: !sessions)
         >>= fun () ->
         Lwt.catch (fun () -> Lwt_unix.close socket) (fun _exn -> return_unit)))

let with_server f =
  let t = { port = 0; message_body = None; served = 0 } in
  Lwt_switch.with_switch (fun switch -> listen switch t >>= fun () -> f t)

let port t = t.port
let body t = t.message_body
let sessions t = t.served
