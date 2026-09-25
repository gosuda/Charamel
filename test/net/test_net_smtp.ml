open Lwt.Infix

let connect fixture =
  Lwt_io.open_connection
    (Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, Fixture_smtp.port fixture))

let send oc line = Lwt_io.write_line oc line

let expect ic name expected =
  Lwt_io.read_line ic >>= fun line ->
  Alcotest.(check string) name expected line;
  Lwt.return_unit

let full_session =
  Alcotest_lwt.test_case "runs a full SMTP session" `Quick (fun _switch () ->
      Fixture_smtp.with_server (fun fixture ->
          connect fixture >>= fun (ic, oc) ->
          expect ic "greeting" "220 pop-test ESMTP" >>= fun () ->
          send oc "EHLO client" >>= fun () ->
          expect ic "capability" "250-pop-test" >>= fun () ->
          expect ic "auth" "250-AUTH PLAIN LOGIN" >>= fun () ->
          expect ic "ehlo end" "250 OK" >>= fun () ->
          send oc "AUTH PLAIN AGNoYXJhbWVsAGM=" >>= fun () ->
          expect ic "plain auth" "235 2.7.0 authenticated" >>= fun () ->
          send oc "MAIL FROM:<sender@example.com>" >>= fun () ->
          expect ic "mail from" "250 2.1.0 OK" >>= fun () ->
          send oc "RCPT TO:<recipient@example.com>" >>= fun () ->
          expect ic "rcpt to" "250 2.1.5 OK" >>= fun () ->
          send oc "DATA" >>= fun () ->
          expect ic "data" "354 End data with <CR><LF>.<CR><LF>" >>= fun () ->
          send oc "Subject: greeting" >>= fun () ->
          send oc "" >>= fun () ->
          send oc "hello from the fixture" >>= fun () ->
          send oc "." >>= fun () ->
          expect ic "queued" "250 2.0.0 queued" >>= fun () ->
          send oc "QUIT" >>= fun () ->
          expect ic "quit" "221 2.0.0 bye" >>= fun () ->
          Lwt_io.close ic >>= fun () ->
          Lwt_io.close oc >>= fun () ->
          Alcotest.(check (option string))
            "captured body" (Some "Subject: greeting\r\n\r\nhello from the fixture")
            (Fixture_smtp.body fixture);
          Alcotest.(check int) "sessions" 1 (Fixture_smtp.sessions fixture);
          Lwt.return_unit))

let login_and_unknown_commands =
  Alcotest_lwt.test_case "answers LOGIN challenges, HELO, and unknown commands" `Quick
    (fun _switch () ->
      Fixture_smtp.with_server (fun fixture ->
          connect fixture >>= fun (ic, oc) ->
          expect ic "greeting" "220 pop-test ESMTP" >>= fun () ->
          send oc "HELO client" >>= fun () ->
          expect ic "helo" "250 pop-test" >>= fun () ->
          send oc "AUTH LOGIN" >>= fun () ->
          expect ic "username challenge" "334 VXNlcm5hbWU6" >>= fun () ->
          send oc "Y2hhcmFtZWw=" >>= fun () ->
          expect ic "password challenge" "334 UGFzc3dvcmQ6" >>= fun () ->
          send oc "Yw==" >>= fun () ->
          expect ic "login auth" "235 2.7.0 authenticated" >>= fun () ->
          send oc "VRFY someone" >>= fun () ->
          expect ic "unknown command" "250 2.0.0 OK" >>= fun () ->
          send oc "QUIT" >>= fun () ->
          expect ic "quit" "221 2.0.0 bye" >>= fun () ->
          Lwt_io.close ic >>= fun () -> Lwt_io.close oc))

let cases = [ full_session; login_and_unknown_commands ]
