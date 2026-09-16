type t = { mutable port : int; mutable body : string option }

let port fixture = fixture.port
let body fixture = fixture.body
let write flow value = Eio.Flow.copy_string value flow
let command reader = try Some (Eio.Buf_read.line reader) with End_of_file -> None

let data reader fixture =
  let output = Buffer.create 1024 in
  let rec collect () =
    match command reader with
    | None -> ()
    | Some "." -> fixture.body <- Some (Buffer.contents output)
    | Some line ->
        if Buffer.length output > 0 then Buffer.add_string output "\r\n";
        Buffer.add_string output line;
        collect ()
  in
  collect ()

let session fixture flow =
  let reader = Eio.Buf_read.of_flow ~max_size:65_536 flow in
  write flow "220 pop-test ESMTP\r\n";
  let rec loop () =
    match command reader with
    | None -> ()
    | Some line ->
        let upper = String.uppercase_ascii line in
        if String.starts_with ~prefix:"EHLO" upper then (
          write flow "250-pop-test\r\n250-AUTH PLAIN LOGIN\r\n250 OK\r\n";
          loop ())
        else if String.starts_with ~prefix:"HELO" upper then (
          write flow "250 pop-test\r\n";
          loop ())
        else if String.starts_with ~prefix:"AUTH PLAIN" upper then (
          write flow "235 2.7.0 authenticated\r\n";
          loop ())
        else if String.starts_with ~prefix:"AUTH LOGIN" upper then (
          write flow "334 VXNlcm5hbWU6\r\n";
          (match command reader with
          | None -> ()
          | Some _ -> write flow "334 UGFzc3dvcmQ6\r\n");
          (match command reader with
          | None -> ()
          | Some _ -> write flow "235 2.7.0 authenticated\r\n");
          loop ())
        else if String.starts_with ~prefix:"MAIL FROM:" upper then (
          write flow "250 2.1.0 OK\r\n";
          loop ())
        else if String.starts_with ~prefix:"RCPT TO:" upper then (
          write flow "250 2.1.5 OK\r\n";
          loop ())
        else if String.equal upper "DATA" then (
          write flow "354 End data with <CR><LF>.<CR><LF>\r\n";
          data reader fixture;
          write flow "250 2.0.0 queued\r\n";
          loop ())
        else if String.equal upper "QUIT" then write flow "221 2.0.0 bye\r\n"
        else (
          write flow "250 2.0.0 OK\r\n";
          loop ())
  in
  Fun.protect ~finally:(fun () -> Eio.Flow.close flow) loop

let start ~sw ~net () =
  let fixture = { port = 0; body = None } in
  let listener =
    Eio.Net.listen ~reuse_addr:true ~backlog:8 ~sw net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  (match Eio.Net.listening_addr listener with
  | `Tcp (_, selected_port) -> fixture.port <- selected_port
  | `Unix _ -> Alcotest.fail "SMTP fixture did not bind TCP loopback");
  let on_error = function
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _)
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure _), _)
    | End_of_file ->
        ()
    | exn -> raise exn
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw listener ~on_error (fun flow _addr ->
            session fixture flow)
      done;
      `Stop_daemon);
  fixture
