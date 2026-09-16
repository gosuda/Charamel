open Pop_core

let contains haystack needle =
  let rec loop index =
    if index + String.length needle > String.length haystack then false
    else if String.sub haystack index (String.length needle) = needle then true
    else loop (index + 1)
  in
  needle = "" || loop 0

let run_mock ?(timeout = 30.) replies f =
  Eio_mock.Backend.run_full (fun env ->
      let net = Eio_mock.Net.make "smtp-net" in
      let writes = Buffer.create 1024 in
      let flow =
        Eio_mock.Flow.make
          ~pp:(fun _ppf chunk -> Buffer.add_string writes chunk)
          "smtp-flow"
      in
      Eio_mock.Flow.on_read flow [ `Return replies; `Raise End_of_file ];
      Eio_mock.Net.on_getaddrinfo net
        [ `Return [ (`Tcp (Eio.Net.Ipaddr.V4.loopback, 2525) : Eio.Net.Sockaddr.t) ] ];
      Eio_mock.Net.on_connect net [ `Return flow ];
      Eio.Switch.run (fun sw ->
          let session =
            Smtp.connect ~sw ~clock:env#clock ~net ~host:"smtp.test" ~port:2525
              ~security:Smtp.Plain ~timeout ()
          in
          f env sw session writes))

let plain_dialogue () =
  let replies =
    "220 smtp.test ESMTP ready\r\n"
    ^ "250-smtp.test\r\n250-AUTH PLAIN LOGIN\r\n250 8BITMIME\r\n"
    ^ "250 sender accepted\r\n250 recipient accepted\r\n354 send mail\r\n"
    ^ "250 queued\r\n221 closing\r\n"
  in
  run_mock replies (fun _env _sw session writes ->
      match session with
      | Error error -> Alcotest.failf "connect failed: %a" Smtp.pp_error error
      | Ok session -> (
          match
            Smtp.send ~from:"sender@example.test" ~recipients:[ "recipient@example.test" ]
              ~body:"hello\r\n.second line\r\n" session
          with
          | Error error -> Alcotest.failf "send failed: %a" Smtp.pp_error error
          | Ok () ->
              let sent = Buffer.contents writes in
              Alcotest.(check bool)
                "MAIL FROM" true
                (contains sent "MAIL FROM:<sender@example.test>\r\n");
              Alcotest.(check bool)
                "RCPT TO" true
                (contains sent "RCPT TO:<recipient@example.test>\r\n");
              Alcotest.(check bool)
                "dot stuffing" true
                (contains sent "..second line\r\n");
              Alcotest.(check bool) "DATA terminator" true (contains sent "\r\n.\r\n");
              Alcotest.(check bool) "QUIT" true (contains sent "QUIT\r\n")))

let login_fallback () =
  let replies =
    "220 ready\r\n250-smtp.test\r\n250 AUTH PLAIN LOGIN\r\n"
    ^ "535 PLAIN rejected\r\n\
       334 VXNlcm5hbWU6\r\n\
       334 UGFzc3dvcmQ6\r\n\
       235 authenticated\r\n"
    ^ "250 sender\r\n250 recipient\r\n354 data\r\n250 queued\r\n221 bye\r\n"
  in
  run_mock replies (fun _env _sw session writes ->
      match session with
      | Error error -> Alcotest.failf "connect failed: %a" Smtp.pp_error error
      | Ok session -> (
          match
            Smtp.send ~auth:("user", "pass") ~from:"sender@example.test"
              ~recipients:[ "recipient@example.test" ] ~body:"body" session
          with
          | Error error -> Alcotest.failf "login fallback failed: %a" Smtp.pp_error error
          | Ok () ->
              let sent = Buffer.contents writes in
              Alcotest.(check bool) "AUTH PLAIN first" true (contains sent "AUTH PLAIN ");
              Alcotest.(check bool)
                "AUTH LOGIN fallback" true
                (contains sent "AUTH LOGIN\r\n");
              Alcotest.(check bool) "username" true (contains sent "dXNlcg==\r\n");
              Alcotest.(check bool) "password" true (contains sent "cGFzcw==\r\n")))

let malformed_reply () =
  run_mock "220 ready\r\nnot an SMTP reply\r\n" (fun _env _sw session _writes ->
      match session with
      | Error (`Bad_reply _) -> ()
      | Error error -> Alcotest.failf "wrong error: %a" Smtp.pp_error error
      | Ok _ -> Alcotest.fail "malformed reply accepted")

let no_recipients () =
  run_mock "220 ready\r\n250 smtp.test\r\n" (fun _env _sw session writes ->
      match session with
      | Error error -> Alcotest.failf "connect failed: %a" Smtp.pp_error error
      | Ok session -> (
          match
            Smtp.send ~from:"sender@example.test" ~recipients:[] ~body:"body" session
          with
          | Error `No_recipients ->
              Alcotest.(check string)
                "no command after local rejection" ""
                ( Buffer.contents writes |> fun value ->
                  if contains value "MAIL FROM" then "MAIL FROM" else "" )
          | Error error -> Alcotest.failf "wrong error: %a" Smtp.pp_error error
          | Ok () -> Alcotest.fail "empty recipient list accepted"))

let envelope_injection () =
  run_mock "220 ready\r\n250 smtp.test\r\n" (fun _env _sw session writes ->
      match session with
      | Error error -> Alcotest.failf "connect failed: %a" Smtp.pp_error error
      | Ok session -> (
          match
            Smtp.send ~from:"sender@example.test\r\nX-Injected: yes"
              ~recipients:[ "recipient@example.test" ] ~body:"body" session
          with
          | Error (`Invalid_address _) ->
              Alcotest.(check bool)
                "no injected command" false
                (contains (Buffer.contents writes) "MAIL FROM")
          | Error error -> Alcotest.failf "wrong error: %a" Smtp.pp_error error
          | Ok () -> Alcotest.fail "envelope injection accepted"))

let timeout () =
  Eio_mock.Backend.run_full (fun env ->
      let net = Eio_mock.Net.make "timeout-net" in
      let flow = Eio_mock.Flow.make "timeout-flow" in
      let pending, _resolver = Eio.Promise.create () in
      Eio_mock.Flow.on_read flow [ `Await pending ];
      Eio_mock.Net.on_getaddrinfo net
        [ `Return [ (`Tcp (Eio.Net.Ipaddr.V4.loopback, 2525) : Eio.Net.Sockaddr.t) ] ];
      Eio_mock.Net.on_connect net [ `Return flow ];
      Eio.Switch.run (fun sw ->
          match
            Smtp.connect ~sw ~clock:env#clock ~net ~host:"smtp.test" ~port:2525
              ~security:Smtp.Plain ~timeout:0.1 ()
          with
          | Error `Timeout -> ()
          | Error error -> Alcotest.failf "wrong timeout error: %a" Smtp.pp_error error
          | Ok _ -> Alcotest.fail "blocked greeting did not time out"))

let cases =
  [
    Alcotest.test_case "multiline EHLO and dot stuffing" `Quick plain_dialogue;
    Alcotest.test_case "AUTH LOGIN fallback" `Quick login_fallback;
    Alcotest.test_case "malformed reply" `Quick malformed_reply;
    Alcotest.test_case "no recipients" `Quick no_recipients;
    Alcotest.test_case "envelope injection" `Quick envelope_injection;
    Alcotest.test_case "finite deadline" `Quick timeout;
  ]
