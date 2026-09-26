open Pop_core
open Lwt.Infix

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
  Alcotest.(check bool)
    "Bcc header omitted" false
    (Test_support.contains ~needle:"Bcc:" ~haystack:wire);
  Alcotest.(check bool)
    "body present" true
    (Test_support.contains ~needle:"Hello" ~haystack:wire);
  Alcotest.(check bool)
    "attachment present" true
    (Test_support.contains ~needle:"YXR0YWNobWVudA==" ~haystack:wire);
  Alcotest.(check string) "preview is serialised message" (Mime.serialise message) wire;
  match Mime.envelope message with
  | Error error -> Alcotest.failf "envelope failed: %a" Mime.pp_error error
  | Ok (from, recipients) ->
      Alcotest.(check string) "envelope sender" "sender@example.com" from;
      Alcotest.(check (list string))
        "envelope recipients"
        [ "to@example.com"; "copy@example.com"; "blind@example.com" ]
        recipients

let prepare_with_body ~unsafe_html body =
  let stdin = Lwt_io.of_bytes ~mode:Lwt_io.Input (Lwt_bytes.of_string "") in
  Pop_lib.prepare ~cwd:(Sys.getcwd ()) ~stdin ~date:(fixed_date ())
    {
      Pop_lib.empty_options with
      to_ = [ "to@example.com" ];
      from = Some "from@example.com";
      subject = Some "Subject";
      body = Some body;
      unsafe_html;
    }
  >>= function
  | Ok value -> Lwt.return value
  | Error error -> Alcotest.failf "prepare failed: %a" Pop_lib.pp_error error

let test_markdown_safety () =
  let body = "# Hello\n\n<script>alert(1)</script>" in
  prepare_with_body ~unsafe_html:false body >>= fun safe ->
  prepare_with_body ~unsafe_html:true body >>= fun unsafe ->
  let safe_html = Option.get safe.Pop_lib.message.Mime.body_html in
  let unsafe_html = Option.get unsafe.Pop_lib.message.Mime.body_html in
  Alcotest.(check bool)
    "safe HTML removes raw script" false
    (Test_support.contains ~needle:"<script>" ~haystack:safe_html);
  Alcotest.(check bool)
    "unsafe HTML preserves raw script" true
    (Test_support.contains ~needle:"<script>" ~haystack:unsafe_html);
  Alcotest.(check bool)
    "plain rendering has heading" true
    (Test_support.contains ~needle:"Hello" ~haystack:safe.Pop_lib.message.Mime.body_text);
  Lwt.return_unit

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
      Alcotest.(check bool)
        "mentions port" true
        (Test_support.contains ~needle:"POP_SMTP_PORT" ~haystack:message)
  | Error error -> Alcotest.failf "wrong configuration error: %a" Pop_lib.pp_error error
  | Ok _ -> Alcotest.fail "bad port was accepted"

let test_resend_payload () =
  let payload = Send.resend_payload (test_message ()) in
  Alcotest.(check bool)
    "payload has from field" true
    (Test_support.contains ~needle:"\"from\"" ~haystack:payload);
  Alcotest.(check bool)
    "payload has Bcc recipients" true
    (Test_support.contains ~needle:"blind@example.com" ~haystack:payload);
  Alcotest.(check bool)
    "payload has base64 attachment" true
    (Test_support.contains ~needle:"YXR0YWNobWVudA==" ~haystack:payload)

let test_smtp_delivery () =
  Fixture_smtp.with_server (fun fixture ->
      let config =
        {
          Send.host = "127.0.0.1";
          port = Fixture_smtp.port fixture;
          username = None;
          password = None;
          security = Smtp.Plain;
        }
      in
      Send.smtp ~config (test_message ()) >>= fun delivered ->
      match delivered with
      | Error error -> Alcotest.failf "SMTP delivery failed: %a" Send.pp_error error
      | Ok () -> (
          match Fixture_smtp.body fixture with
          | None -> Alcotest.fail "SMTP fixture received no DATA payload"
          | Some body ->
              Alcotest.(check bool)
                "DATA has From" true
                (Test_support.contains ~needle:"From:" ~haystack:body);
              Alcotest.(check bool)
                "DATA omits Bcc" false
                (Test_support.contains ~needle:"Bcc:" ~haystack:body);
              Lwt.return_unit))

let suite =
  [
    ("mail core MIME", Test_mime.cases);
    ("mail core SMTP", Test_smtp.cases);
    ("CLI", Test_cli.cases);
    ( "composition",
      [
        Alcotest_lwt.test_case_sync "MIME and preview" `Quick test_mime_and_preview;
        Alcotest_lwt.test_case_sync "Resend payload" `Quick test_resend_payload;
        Alcotest_lwt.test_case_sync "configuration" `Quick test_config_env;
        Alcotest_lwt.test_case_sync "configuration validation" `Quick
          test_config_rejects_bad_port;
      ] );
    ( "markdown",
      [
        Alcotest_lwt.test_case "safe and unsafe HTML" `Quick (fun _switch () ->
            test_markdown_safety ());
      ] );
    ( "smtp",
      [
        Alcotest_lwt.test_case "plain delivery" `Quick (fun _switch () ->
            test_smtp_delivery ());
      ] );
  ]

let () =
  Mirage_crypto_rng_unix.use_default ();
  Test_support.run_lwt "pop" suite
