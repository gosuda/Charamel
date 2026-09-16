module BList = Charm_bubbles.List

let make () =
  let delegate = BList.default_delegate ~title:(fun item -> item) () in
  BList.v ~width:40 ~height:10 ~delegate
    ~filter_value:(fun item -> item)
    [ "foo"; "bar"; "baz" ]

let items () =
  let model = make () in
  Alcotest.(check (list string))
    "all items" [ "foo"; "bar"; "baz" ] (BList.visible_items model);
  let model = BList.set_filter_text "ba" model in
  Alcotest.(check (list string))
    "filtered items" [ "bar"; "baz" ] (BList.visible_items model);
  Alcotest.(check bool) "filtered state" true (BList.is_filtered model)

let selection_and_pages () =
  let model = make () |> BList.select 2 in
  Alcotest.(check (option string))
    "selected item" (Some "baz") (BList.selected_item model);
  let model = BList.cursor_up model in
  Alcotest.(check int) "cursor up" 1 (BList.index model);
  let model = BList.go_to_start model in
  Alcotest.(check int) "start" 0 (BList.index model);
  let model = BList.go_to_end model in
  Alcotest.(check int) "end" 2 (BList.index model)

let filter_states () =
  let model =
    make () |> BList.set_filter_text "ba" |> BList.set_filter_state BList.Unfiltered
  in
  Alcotest.(check (list string))
    "unfiltered state restores all" [ "foo"; "bar"; "baz" ] (BList.visible_items model);
  let model = BList.set_filter_state BList.Filtering model in
  Alcotest.(check bool) "setting filter" true (BList.setting_filter model)

let empty_and_view () =
  let delegate = BList.default_delegate ~title:(fun item -> item) () in
  let model = BList.v ~width:30 ~height:8 ~delegate ~filter_value:(fun item -> item) [] in
  Alcotest.(check (option string)) "empty selection" None (BList.selected_item model);
  let plain = Charm_ansi.Text.strip (BList.view model) in
  Alcotest.(check bool) "empty status" true (String.length plain > 0)

let cases =
  [
    Alcotest.test_case "items and filter" `Quick items;
    Alcotest.test_case "selection and pages" `Quick selection_and_pages;
    Alcotest.test_case "filter states" `Quick filter_states;
    Alcotest.test_case "empty and view" `Quick empty_and_view;
  ]
