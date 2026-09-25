open Pop_core

let header_block rendered =
  let lines = String.split_on_char '\n' rendered in
  let rec take acc = function
    | [] -> String.concat "\n" (List.rev acc)
    | line :: _rest when String.trim line = "" -> String.concat "\n" (List.rev acc)
    | line :: rest -> take (line :: acc) rest
  in
  take [] lines

let address ?display value =
  match Mime.Address.v ?display value with
  | Ok address -> address
  | Error error -> Alcotest.failf "address construction failed: %a" Mime.pp_error error

let date =
  match Ptime.of_date ~tz_offset_s:0 (2026, 9, 16) with
  | Some value -> value
  | None -> Alcotest.fail "invalid fixture date"

let make_message ?(cc = []) ?(bcc = []) ?body_html ?(attachments = []) subject body_text =
  let from = address ~display:"Sender" "sender@example.test" in
  let recipient = address "recipient@example.test" in
  match
    Mime.message ~from ~to_:[ recipient ] ~cc ~bcc ~subject ~date ~body_text ?body_html
      ~attachments ()
  with
  | Ok message -> message
  | Error error -> Alcotest.failf "message construction failed: %a" Mime.pp_error error

let plain_message () =
  let message = make_message "hello" "line one\nline two" in
  let rendered = Mime.serialise message in
  Alcotest.(check bool)
    "CRLF line endings" true
    (Test_support.contains ~needle:"\r\n" ~haystack:rendered);
  Alcotest.(check bool)
    "quoted-printable" true
    (Test_support.contains ~needle:"Content-Transfer-Encoding: quoted-printable\r\n"
       ~haystack:rendered);
  Alcotest.(check bool)
    "no Bcc header" false
    (Test_support.contains ~needle:"Bcc:" ~haystack:rendered);
  Alcotest.(check bool)
    "stable rendering" true
    (String.equal rendered (Mime.serialise message))

let alternative_message () =
  let message = make_message ~body_html:"<p>hello</p>" "hello" "hello" in
  let rendered = Mime.serialise message in
  Alcotest.(check bool)
    "alternative content type" true
    (Test_support.contains ~needle:"multipart/alternative; boundary=\"pop-alt-"
       ~haystack:rendered);
  Alcotest.(check bool)
    "text part" true
    (Test_support.contains ~needle:"text/plain; charset=UTF-8" ~haystack:rendered);
  Alcotest.(check bool)
    "html part" true
    (Test_support.contains ~needle:"text/html; charset=UTF-8" ~haystack:rendered);
  Alcotest.(check bool)
    "alternative top-level composite has no transfer header" false
    (Test_support.contains ~needle:"Content-Transfer-Encoding:"
       ~haystack:(header_block rendered))

let attachment_message () =
  let attachment =
    match Mime.attachment ~name:"hello.txt" ~data:"hello" () with
    | Ok value -> value
    | Error error ->
        Alcotest.failf "attachment construction failed: %a" Mime.pp_error error
  in
  let message = make_message ~attachments:[ attachment ] "attached" "hello" in
  let rendered = Mime.serialise message in
  Alcotest.(check bool)
    "mixed content type" true
    (Test_support.contains ~needle:"multipart/mixed; boundary=\"pop-mixed-"
       ~haystack:rendered);
  Alcotest.(check bool)
    "base64 transfer" true
    (Test_support.contains ~needle:"Content-Transfer-Encoding: base64\r\n"
       ~haystack:rendered);
  Alcotest.(check bool)
    "base64 payload" true
    (Test_support.contains ~needle:"aGVsbG8=\r\n" ~haystack:rendered);
  Alcotest.(check bool)
    "attachment filename" true
    (Test_support.contains ~needle:"filename=\"hello.txt\"" ~haystack:rendered);
  Alcotest.(check bool)
    "mixed top-level composite has no transfer header" false
    (Test_support.contains ~needle:"Content-Transfer-Encoding:"
       ~haystack:(header_block rendered));
  Alcotest.(check bool)
    "no Bcc header" false
    (Test_support.contains ~needle:"Bcc:" ~haystack:rendered)

let encoded_subject () =
  let message = make_message "héllo" "body" in
  let subject = Mime.encoded_subject message in
  Alcotest.(check bool)
    "encoded-word prefix" true
    (Test_support.contains ~needle:"=?UTF-8?B?" ~haystack:subject);
  Alcotest.(check bool)
    "encoded-word terminator" true
    (Test_support.contains ~needle:"?=" ~haystack:subject)

let envelope () =
  let blind = address "blind@example.test" in
  let message = make_message ~bcc:[ blind ] "subject" "body" in
  match Mime.envelope message with
  | Error error -> Alcotest.failf "envelope failed: %a" Mime.pp_error error
  | Ok (from, recipients) ->
      Alcotest.(check string) "reverse path" "sender@example.test" from;
      Alcotest.(check (list string))
        "recipient order"
        [ "recipient@example.test"; "blind@example.test" ]
        recipients

let injection_rejection () =
  (match Mime.Address.v "victim@example.test\r\nBcc: attacker@example.test" with
  | Error (`Header_injection _) -> ()
  | Error error -> Alcotest.failf "wrong address error: %a" Mime.pp_error error
  | Ok _ -> Alcotest.fail "address injection accepted");
  let from = address "sender@example.test" in
  let recipient = address "recipient@example.test" in
  match
    Mime.message ~from ~to_:[ recipient ] ~subject:"ok\r\nBcc: bad" ~date
      ~body_text:"body" ()
  with
  | Error (`Header_injection _) -> ()
  | Error error -> Alcotest.failf "wrong subject error: %a" Mime.pp_error error
  | Ok _ -> Alcotest.fail "subject injection accepted"

let attachment_validation () =
  (match Mime.attachment ~name:"../secret" ~data:"x" () with
  | Error (`Header_injection _) -> ()
  | Error error -> Alcotest.failf "wrong filename error: %a" Mime.pp_error error
  | Ok _ -> Alcotest.fail "path attachment accepted");
  Alcotest.(check string)
    "content type fallback" "application/octet-stream"
    (Mime.guess_content_type "unknown.something")

let cases =
  [
    Alcotest.test_case "plain message" `Quick plain_message;
    Alcotest.test_case "multipart alternative" `Quick alternative_message;
    Alcotest.test_case "base64 attachment" `Quick attachment_message;
    Alcotest.test_case "RFC2047 subject" `Quick encoded_subject;
    Alcotest.test_case "envelope includes Bcc" `Quick envelope;
    Alcotest.test_case "header injection rejection" `Quick injection_rejection;
    Alcotest.test_case "attachment validation" `Quick attachment_validation;
  ]
