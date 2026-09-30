open Pop_core
open Lwt.Infix

(* The farewell reply is held back so the mock only answers QUIT after every client
   command has been drained; a newline-terminated script's last line therefore starts
   after its second-to-last newline — scanning from the final newline itself would
   find an empty "last line" and send everything eagerly. *)
let split_final replies =
  let length = String.length replies in
  let search_from =
    if length > 0 && replies.[length - 1] = '\n' then length - 2 else length - 1
  in
  if search_from < 0 then (replies, None)
  else
    match String.rindex_from_opt replies search_from '\n' with
    | Some index ->
        let prior = String.sub replies 0 (index + 1) in
        let last = String.sub replies (index + 1) (length - index - 1) in
        if String.starts_with ~prefix:"221" last then (prior, Some last)
        else (replies, None)
    | None ->
        if String.starts_with ~prefix:"221" replies then ("", Some replies)
        else (replies, None)

let swallow_io body =
  Lwt.catch body (function
    | End_of_file | Lwt.Canceled | Unix.Unix_error _ -> Lwt.return_unit
    | exn -> Lwt.fail exn)

let write_all fd data =
  let length = String.length data in
  let raw_fd = Lwt_unix.unix_file_descr fd in
  let raw = Bytes.unsafe_of_string data in
  let rec loop offset =
    if offset >= length then Lwt.return_unit
    else
      match Unix.write raw_fd raw offset (length - offset) with
      | written -> loop (offset + written)
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          Lwt_unix.wait_write fd >>= fun () -> loop offset
  in
  loop 0

let serve ~earlier ~final writes fd =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input ~close:(fun () -> Lwt.return_unit) fd in
  (* Replies go through [write_all] — a raw [Unix.write] loop — rather than an
     [Lwt_io] output channel so they reach the wire eagerly: a buffered channel
     may hold the farewell back past the point where the client stops waiting
     for it. *)
  let send value = write_all fd value in
  Lwt.finalize
    (fun () ->
      send earlier >>= fun () ->
      let released = ref (final = None) in
      let release () =
        released := true;
        match final with Some reply -> send reply | None -> Lwt.return_unit
      in
      (* [read_into] answers as soon as any bytes are available and answers 0 only
         at end-of-file; [Lwt_io.read] would hold the callback until the peer
         closes, so a deferred farewell could never be triggered mid-dialogue. *)
      let piece = Bytes.create 4096 in
      let rec drain () =
        Lwt_io.read_into ic piece 0 (Bytes.length piece) >>= fun count ->
        if count = 0 then Lwt.return_unit
        else (
          Buffer.add_substring writes (Bytes.unsafe_to_string piece) 0 count;
          (if !released then Lwt.return_unit
           else
             let sent = Buffer.contents writes in
             if
               String.starts_with ~prefix:"QUIT\r\n" sent
               || Test_support.contains ~needle:"\r\nQUIT\r\n" ~haystack:sent
             then release ()
             else Lwt.return_unit)
          >>= drain)
      in
      swallow_io drain)
    (fun () -> swallow_io (fun () -> Lwt_io.close ic >>= fun () -> Lwt_unix.close fd))

let run_mock ?(timeout = 30.) replies f =
  let writes = Buffer.create 1024 in
  let earlier, final = split_final replies in
  Lwt_switch.with_switch (fun switch ->
      let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
      Lwt_unix.setsockopt socket Lwt_unix.SO_REUSEADDR true;
      Lwt_unix.bind socket (Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, 0)) >>= fun () ->
      Lwt_unix.listen socket 8;
      let port =
        match Lwt_unix.getsockname socket with
        | Lwt_unix.ADDR_INET (_, selected) -> selected
        | _sockaddr -> 0
      in
      let sessions = ref [] in
      let rec accept_forever () =
        Lwt.catch
          (fun () ->
            Lwt_unix.accept socket >>= fun (fd, _address) ->
            sessions := serve ~earlier ~final writes fd :: !sessions;
            accept_forever ())
          (fun _exn -> Lwt.return_unit)
      in
      let acceptor = accept_forever () in
      Lwt.return
        (Lwt_switch.add_hook (Some switch) (fun () ->
             Lwt.cancel acceptor;
             List.iter Lwt.cancel !sessions;
             Lwt.catch (fun () -> acceptor) (fun _exn -> Lwt.return_unit) >>= fun () ->
             Lwt.catch (fun () -> Lwt_unix.close socket) (fun _exn -> Lwt.return_unit)))
      >>= fun () ->
      Smtp.connect ~host:"127.0.0.1" ~port ~security:Smtp.Plain ~timeout ()
      >>= fun session -> f session writes)

let plain_dialogue () =
  let replies =
    "220 smtp.test ESMTP ready\r\n"
    ^ "250-smtp.test\r\n250-AUTH PLAIN LOGIN\r\n250 8BITMIME\r\n"
    ^ "250 sender accepted\r\n250 recipient accepted\r\n354 send mail\r\n"
    ^ "250 queued\r\n221 closing\r\n"
  in
  run_mock replies (fun session writes ->
      match session with
      | Error error -> Alcotest.failf "connect failed: %a" Smtp.pp_error error
      | Ok session -> (
          Smtp.send ~from:"sender@example.test" ~recipients:[ "recipient@example.test" ]
            ~body:"hello\r\n.second line\r\n" session
          >>= function
          | Error error -> Alcotest.failf "send failed: %a" Smtp.pp_error error
          | Ok () ->
              let sent = Buffer.contents writes in
              Alcotest.(check bool)
                "MAIL FROM" true
                (Test_support.contains ~needle:"MAIL FROM:<sender@example.test>\r\n"
                   ~haystack:sent);
              Alcotest.(check bool)
                "RCPT TO" true
                (Test_support.contains ~needle:"RCPT TO:<recipient@example.test>\r\n"
                   ~haystack:sent);
              Alcotest.(check bool)
                "dot stuffing" true
                (Test_support.contains ~needle:"..second line\r\n" ~haystack:sent);
              Alcotest.(check bool)
                "DATA terminator" true
                (Test_support.contains ~needle:"\r\n.\r\n" ~haystack:sent);
              Alcotest.(check bool)
                "QUIT" true
                (Test_support.contains ~needle:"QUIT\r\n" ~haystack:sent);
              Lwt.return_unit))

let login_fallback () =
  let replies =
    "220 ready\r\n250-smtp.test\r\n250 AUTH PLAIN LOGIN\r\n"
    ^ "535 PLAIN rejected\r\n\
       334 VXNlcm5hbWU6\r\n\
       334 UGFzc3dvcmQ6\r\n\
       235 authenticated\r\n"
    ^ "250 sender\r\n250 recipient\r\n354 data\r\n250 queued\r\n221 bye\r\n"
  in
  run_mock replies (fun session writes ->
      match session with
      | Error error -> Alcotest.failf "connect failed: %a" Smtp.pp_error error
      | Ok session -> (
          Smtp.send ~auth:("user", "pass") ~from:"sender@example.test"
            ~recipients:[ "recipient@example.test" ] ~body:"body" session
          >>= function
          | Error error -> Alcotest.failf "login fallback failed: %a" Smtp.pp_error error
          | Ok () ->
              let sent = Buffer.contents writes in
              Alcotest.(check bool)
                "AUTH PLAIN first" true
                (Test_support.contains ~needle:"AUTH PLAIN " ~haystack:sent);
              Alcotest.(check bool)
                "AUTH LOGIN fallback" true
                (Test_support.contains ~needle:"AUTH LOGIN\r\n" ~haystack:sent);
              Alcotest.(check bool)
                "username" true
                (Test_support.contains ~needle:"dXNlcg==\r\n" ~haystack:sent);
              Alcotest.(check bool)
                "password" true
                (Test_support.contains ~needle:"cGFzcw==\r\n" ~haystack:sent);
              Lwt.return_unit))

let malformed_reply () =
  run_mock "220 ready\r\nnot an SMTP reply\r\n" (fun session _writes ->
      match session with
      | Error (`Bad_reply _) -> Lwt.return_unit
      | Error error -> Alcotest.failf "wrong error: %a" Smtp.pp_error error
      | Ok _ -> Alcotest.fail "malformed reply accepted")

let no_recipients () =
  run_mock "220 ready\r\n250 smtp.test\r\n" (fun session writes ->
      match session with
      | Error error -> Alcotest.failf "connect failed: %a" Smtp.pp_error error
      | Ok session -> (
          Smtp.send ~from:"sender@example.test" ~recipients:[] ~body:"body" session
          >>= function
          | Error `No_recipients ->
              Alcotest.(check string)
                "no command after local rejection" ""
                ( Buffer.contents writes |> fun value ->
                  if Test_support.contains ~needle:"MAIL FROM" ~haystack:value then
                    "MAIL FROM"
                  else "" );
              Lwt.return_unit
          | Error error -> Alcotest.failf "wrong error: %a" Smtp.pp_error error
          | Ok () -> Alcotest.fail "empty recipient list accepted"))

let envelope_injection () =
  run_mock "220 ready\r\n250 smtp.test\r\n" (fun session writes ->
      match session with
      | Error error -> Alcotest.failf "connect failed: %a" Smtp.pp_error error
      | Ok session -> (
          Smtp.send ~from:"sender@example.test\r\nX-Injected: yes"
            ~recipients:[ "recipient@example.test" ] ~body:"body" session
          >>= function
          | Error (`Invalid_address _) ->
              Alcotest.(check bool)
                "no injected command" false
                (Test_support.contains ~needle:"MAIL FROM"
                   ~haystack:(Buffer.contents writes));
              Lwt.return_unit
          | Error error -> Alcotest.failf "wrong error: %a" Smtp.pp_error error
          | Ok () -> Alcotest.fail "envelope injection accepted"))

let timeout () =
  run_mock ~timeout:0.1 "" (fun session _writes ->
      match session with
      | Error `Timeout -> Lwt.return_unit
      | Error error -> Alcotest.failf "wrong timeout error: %a" Smtp.pp_error error
      | Ok _ -> Alcotest.fail "blocked greeting did not time out")

let oversized_reply () =
  let replies = "220 ready\r\n250 " ^ String.make ((64 * 1024) + 64) 'x' ^ "\r\n" in
  run_mock replies (fun session _writes ->
      match session with
      | Error (`Bad_reply _) -> Lwt.return_unit
      | Error error -> Alcotest.failf "wrong error: %a" Smtp.pp_error error
      | Ok _ -> Alcotest.fail "oversized reply line accepted")

let cases =
  [
    Alcotest_lwt.test_case "multiline EHLO and dot stuffing" `Quick (fun _switch () ->
        plain_dialogue ());
    Alcotest_lwt.test_case "AUTH LOGIN fallback" `Quick (fun _switch () ->
        login_fallback ());
    Alcotest_lwt.test_case "malformed reply" `Quick (fun _switch () -> malformed_reply ());
    Alcotest_lwt.test_case "no recipients" `Quick (fun _switch () -> no_recipients ());
    Alcotest_lwt.test_case "envelope injection" `Quick (fun _switch () ->
        envelope_injection ());
    Alcotest_lwt.test_case "finite deadline" `Quick (fun _switch () -> timeout ());
    Alcotest_lwt.test_case "oversized reply line" `Quick (fun _switch () ->
        oversized_reply ());
  ]
