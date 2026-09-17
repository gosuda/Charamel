let base = Table.default_options

let test_header_and_rows () =
  match Table.parse_input base "name,value\na,1\nb,2\n" with
  | Ok (headers, rows) ->
      Alcotest.(check (list string)) "headers" [ "name"; "value" ] headers;
      Alcotest.(check (list (list string))) "rows" [ [ "a"; "1" ]; [ "b"; "2" ] ] rows
  | Error error -> Alcotest.fail (Table.error_message error)

let test_explicit_columns_and_padding () =
  let options = { base with columns = [ "name"; "value" ] } in
  match Table.parse_input options "a\nb,2\n" with
  | Ok (headers, rows) ->
      Alcotest.(check (list string)) "explicit headers" [ "name"; "value" ] headers;
      Alcotest.(check (list (list string)))
        "padded row"
        [ [ "a"; "" ]; [ "b"; "2" ] ]
        rows
  | Error error -> Alcotest.fail (Table.error_message error)

let test_wider_row_rejected () =
  let options = { base with columns = [ "name" ] } in
  match Table.parse_input options "a,b\n" with
  | Ok _ -> Alcotest.fail "wide row accepted"
  | Error `Invalid_columns -> ()
  | Error error -> Alcotest.fail (Table.error_message error)

let test_custom_separator () =
  let options = { base with separator = "\t" } in
  match Table.parse_input options "name\tvalue\na\t1\n" with
  | Ok (headers, rows) ->
      Alcotest.(check (list string)) "tab headers" [ "name"; "value" ] headers;
      Alcotest.(check (list (list string))) "tab rows" [ [ "a"; "1" ] ] rows
  | Error error -> Alcotest.fail (Table.error_message error)

let test_static_render () =
  let headers, rows =
    match Table.parse_input base "name,value\na,1\n" with
    | Ok value -> value
    | Error error -> Alcotest.fail (Table.error_message error)
  in
  let output =
    match Table.render_static base ~headers ~rows with
    | Ok value -> Charamel_ansi.Text.strip value
    | Error error -> Alcotest.fail (Table.error_message error)
  in
  Alcotest.(check bool)
    "contains rendered cells" true
    (String.contains output 'n' && String.contains output 'v'
   && String.contains output 'a' && String.contains output '1')

let test_static_rejects_malformed_padding () =
  let options = { base with padding = "not-padding" } in
  match Table.render_static options ~headers:[ "name" ] ~rows:[ [ "value" ] ] with
  | Error (`Invalid_padding _) -> ()
  | Ok _ -> Alcotest.fail "malformed padding silently rendered"
  | Error error -> Alcotest.fail (Table.error_message error)

let cases =
  [
    Alcotest.test_case "headers and rows" `Quick test_header_and_rows;
    Alcotest.test_case "explicit columns and padding" `Quick
      test_explicit_columns_and_padding;
    Alcotest.test_case "wide row" `Quick test_wider_row_rejected;
    Alcotest.test_case "custom separator" `Quick test_custom_separator;
    Alcotest.test_case "static render" `Quick test_static_render;
    Alcotest.test_case "static malformed padding" `Quick
      test_static_rejects_malformed_padding;
  ]
