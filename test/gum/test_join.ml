let vertical_join () =
  match Join.join ~vertical:true [ "one"; "two" ] with
  | Error (`Msg message) -> Alcotest.fail message
  | Ok output -> Alcotest.(check string) "vertical blocks" "one\ntwo" output

let default_join () =
  match Join.join [ "a\nb"; "c" ] with
  | Error (`Msg message) -> Alcotest.fail message
  | Ok output -> Alcotest.(check string) "no direction flag" "ac\nb " output

let aligned_horizontal () =
  match Join.join ~horizontal:true ~align:"bottom" [ "a\nb"; "c" ] with
  | Ok output -> Alcotest.(check string) "bottom aligned" "a \nbc" output
  | Error (`Msg message) -> Alcotest.fail ("bottom align failed: " ^ message)

let invalid_alignment () =
  match Join.join ~align:"diagonal" [ "x" ] with
  | Error (`Msg message) ->
      Alcotest.(check bool) "diagnostic" true (String.length message > 0)
  | Ok _ -> Alcotest.fail "invalid alignment accepted"

let no_text () =
  match Join.join [] with
  | Error (`Msg _) -> ()
  | Ok _ -> Alcotest.fail "empty text list accepted"

let cases =
  [
    Alcotest_lwt.test_case_sync "vertical" `Quick vertical_join;
    Alcotest_lwt.test_case_sync "default direction" `Quick default_join;
    Alcotest_lwt.test_case_sync "horizontal alignment" `Quick aligned_horizontal;
    Alcotest_lwt.test_case_sync "invalid alignment" `Quick invalid_alignment;
    Alcotest_lwt.test_case_sync "no text" `Quick no_text;
  ]
