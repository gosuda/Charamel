open Pop_core

let contains text needle =
  let text_length = String.length text and needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then false
    else if String.sub text index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let address value =
  match Mime.Address.v value with
  | Ok value -> value
  | Error error -> Alcotest.failf "address failed: %a" Mime.pp_error error

let test_recipient_values () =
  Alcotest.(check (list string))
    "comma-separated values"
    [ "to@example.com"; "copy@example.com"; "\"quoted, name@example.com\"" ]
    (Pop_lib.split_addresses
       [ "to@example.com, copy@example.com"; "\"quoted, name@example.com\"" ])

let test_preview_does_not_add_bcc_header () =
  let date =
    match Ptime.of_date_time ((2026, 9, 16), ((12, 34, 56), 0)) with
    | Some value -> value
    | None -> Alcotest.fail "invalid date"
  in
  let message =
    match
      Mime.message ~from:(address "from@example.com")
        ~to_:[ address "to@example.com" ]
        ~bcc:[ address "blind@example.com" ]
        ~subject:"preview" ~date ~body_text:"body" ~body_html:"<p>body</p>" ()
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "message failed: %a" Mime.pp_error error
  in
  let output = Preview.render message in
  Alcotest.(check bool) "preview has no Bcc header" false (contains output "Bcc:");
  Alcotest.(check bool) "preview includes body" true (contains output "body")

let cases =
  [
    Alcotest.test_case "recipient parsing" `Quick test_recipient_values;
    Alcotest.test_case "preview does not leak Bcc" `Quick
      test_preview_does_not_add_bcc_header;
  ]
