open Pop_core

let contains text needle =
  let text_length = String.length text and needle_length = String.length needle in
  let rec search index =
    if index + needle_length > text_length then false
    else if String.sub text index needle_length = needle then true
    else search (index + 1)
  in
  needle_length = 0 || search 0

let fixed_date () =
  match Ptime.of_date_time ((2026, 9, 16), ((12, 34, 56), 0)) with
  | Some value -> value
  | None -> Alcotest.fail "invalid fixed date"

let address value =
  match Mime.Address.v value with
  | Ok value -> value
  | Error error -> Alcotest.failf "invalid test address: %a" Mime.pp_error error

let test_message () =
  let attachment =
    match Mime.attachment ~name:"note.txt" ~data:"attachment" () with
    | Ok value -> value
    | Error error -> Alcotest.failf "invalid test attachment: %a" Mime.pp_error error
  in
  match
    Mime.message
      ~from:(address "sender@example.com")
      ~to_:[ address "to@example.com" ]
      ~cc:[ address "copy@example.com" ]
      ~bcc:[ address "blind@example.com" ]
      ~subject:"Hello" ~date:(fixed_date ()) ~body_text:"Hello\n"
      ~body_html:"<p>Hello</p>" ~attachments:[ attachment ] ()
  with
  | Ok value -> value
  | Error error -> Alcotest.failf "could not build test message: %a" Mime.pp_error error

let test_mime_and_preview () =
  let message = test_message () in
  let wire = Preview.render message in
  Alcotest.(check bool) "Bcc header omitted" false (contains wire "Bcc:");
  Alcotest.(check bool) "body present" true (contains wire "Hello");
  Alcotest.(check bool) "attachment present" true (contains wire "YXR0YWNobWVudA==");
  Alcotest.(check string) "preview is serialised message" (Mime.serialise message) wire;
  match Mime.envelope message with
  | Error error -> Alcotest.failf "envelope failed: %a" Mime.pp_error error
  | Ok (from, recipients) ->
      Alcotest.(check string) "envelope sender" "sender@example.com" from;
      Alcotest.(check (list string))
        "envelope recipients"
        [ "to@example.com"; "copy@example.com"; "blind@example.com" ]
        recipients

let prepare_with_body env ~unsafe_html body =
  Eio.Switch.run (fun sw ->
      match
        Pop_lib.prepare ~sw ~clock:env#clock ~cwd:env#cwd
          ~stdin:(Eio.Flow.string_source "") ~date:(fixed_date ())
          {
            Pop_lib.empty_options with
            to_ = [ "to@example.com" ];
            from = Some "from@example.com";
            subject = Some "Subject";
            body = Some body;
            unsafe_html;
          }
      with
      | Ok value -> value
      | Error error -> Alcotest.failf "prepare failed: %a" Pop_lib.pp_error error)

let test_markdown_safety env =
  let body = "# Hello\n\n<script>alert(1)</script>" in
  let safe = prepare_with_body env ~unsafe_html:false body in
  let unsafe = prepare_with_body env ~unsafe_html:true body in
  let safe_html = Option.get safe.Pop_lib.message.Mime.body_html in
  let unsafe_html = Option.get unsafe.Pop_lib.message.Mime.body_html in
  Alcotest.(check bool)
    "safe HTML removes raw script" false
    (contains safe_html "<script>");
  Alcotest.(check bool)
    "unsafe HTML preserves raw script" true
    (contains unsafe_html "<script>");
  Alcotest.(check bool)
    "plain rendering has heading" true
    (contains safe.Pop_lib.message.Mime.body_text "Hello")

let test_config_env () =
  let bindings =
    [
      ("POP_SMTP_HOST", "smtp.example.com");
      ("POP_SMTP_PORT", "465");
      ("POP_SMTP_USERNAME", "user");
      ("POP_SMTP_PASSWORD", "password");
      ("POP_SMTP_ENCRYPTION", "ssl");
    ]
  in
  let env name = List.assoc_opt name bindings in
  match Pop_lib.config_of_env ~env with
  | Error error -> Alcotest.failf "configuration failed: %a" Pop_lib.pp_error error
  | Ok config ->
      Alcotest.(check string) "SMTP host" "smtp.example.com" config.Send.host;
      Alcotest.(check int) "SMTP port" 465 config.Send.port;
      Alcotest.(check bool)
        "implicit TLS" true
        (match config.Send.security with Smtp.Tls -> true | _ -> false)

let test_config_rejects_bad_port () =
  let env name =
    if name = "POP_SMTP_HOST" then Some "smtp"
    else if name = "POP_SMTP_PORT" then Some "bad"
    else None
  in
  match Pop_lib.config_of_env ~env with
  | Error (`Input message) ->
      Alcotest.(check bool) "mentions port" true (contains message "POP_SMTP_PORT")
  | Error error -> Alcotest.failf "wrong configuration error: %a" Pop_lib.pp_error error
  | Ok _ -> Alcotest.fail "bad port was accepted"

let test_resend_payload () =
  let payload = Send.resend_payload (test_message ()) in
  Alcotest.(check bool) "payload has from field" true (contains payload "\"from\"");
  Alcotest.(check bool)
    "payload has Bcc recipients" true
    (contains payload "blind@example.com");
  Alcotest.(check bool)
    "payload has base64 attachment" true
    (contains payload "YXR0YWNobWVudA==")

let test_smtp_delivery env =
  Eio.Switch.run (fun sw ->
      let fixture = Fixture_smtp.start ~sw ~net:env#net () in
      let config =
        {
          Send.host = "127.0.0.1";
          port = Fixture_smtp.port fixture;
          username = None;
          password = None;
          security = Smtp.Plain;
        }
      in
      match Send.smtp ~sw ~clock:env#clock ~net:env#net ~config (test_message ()) with
      | Error error -> Alcotest.failf "SMTP delivery failed: %a" Send.pp_error error
      | Ok () -> (
          match Fixture_smtp.body fixture with
          | None -> Alcotest.fail "SMTP fixture received no DATA payload"
          | Some body ->
              Alcotest.(check bool) "DATA has From" true (contains body "From:");
              Alcotest.(check bool) "DATA omits Bcc" false (contains body "Bcc:")))

let suite =
  [
    ("mail core MIME", Test_mime.cases);
    ("mail core SMTP", Test_smtp.cases);
    ("CLI", Test_cli.cases);
    ( "composition",
      [
        Alcotest.test_case "MIME and preview" `Quick test_mime_and_preview;
        Alcotest.test_case "Resend payload" `Quick test_resend_payload;
        Alcotest.test_case "configuration" `Quick test_config_env;
        Alcotest.test_case "configuration validation" `Quick test_config_rejects_bad_port;
      ] );
    ( "markdown",
      [
        Alcotest.test_case "safe and unsafe HTML" `Quick (fun () ->
            Eio_main.run test_markdown_safety);
      ] );
    ( "smtp",
      [
        Alcotest.test_case "plain delivery" `Quick (fun () ->
            Eio_main.run test_smtp_delivery);
      ] );
  ]

let () =
  Mirage_crypto_rng_unix.use_default ();
  Alcotest.run "pop" suite
