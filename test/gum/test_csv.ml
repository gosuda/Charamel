let parse_exn input =
  match Csv.parse input with
  | Ok rows -> rows
  | Error error -> Alcotest.fail (Csv.error_message error)

let test_quotes () =
  let rows =
    parse_exn "name,description\nAlice,\"hello, world\"\nBob,\"say \"\"hi\"\"\"\n"
  in
  Alcotest.(check (list (list string)))
    "quoted fields"
    [ [ "name"; "description" ]; [ "Alice"; "hello, world" ]; [ "Bob"; "say \"hi\"" ] ]
    rows

let test_crlf_and_bom () =
  let rows = parse_exn "\xEF\xBB\xBFa,b\r\n1,2\r\n" in
  Alcotest.(check (list (list string))) "BOM and CRLF" [ [ "a"; "b" ]; [ "1"; "2" ] ] rows

let test_malformed () =
  match Csv.parse "a,\"unterminated\n" with
  | Ok _ -> Alcotest.fail "unterminated quote accepted"
  | Error _ -> ()

let test_lazy_quotes () =
  match Csv.parse ~lazy_quotes:true "a\"b,c\n" with
  | Ok [ [ value; "c" ] ] -> Alcotest.(check string) "lazy quote" "a\"b" value
  | Ok _ -> Alcotest.fail "unexpected lazy quote rows"
  | Error error -> Alcotest.fail (Csv.error_message error)

let test_writer () =
  let row = Csv.write_row ~separator:',' [ "a,b"; "say \"hi\""; " x " ] in
  Alcotest.(check string) "quoted writer" "\"a,b\",\"say \"\"hi\"\"\",\" x \"\n" row

let cases =
  [
    Alcotest_lwt.test_case_sync "quotes" `Quick test_quotes;
    Alcotest_lwt.test_case_sync "BOM and CRLF" `Quick test_crlf_and_bom;
    Alcotest_lwt.test_case_sync "malformed" `Quick test_malformed;
    Alcotest_lwt.test_case_sync "lazy quotes" `Quick test_lazy_quotes;
    Alcotest_lwt.test_case_sync "writer" `Quick test_writer;
  ]
