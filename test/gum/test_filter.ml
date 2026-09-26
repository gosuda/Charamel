let key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message

let exact_matching () =
  let matches = Filter.exact_matches ~pattern:"ba" [ "Alpha"; "bar"; "beta" ] in
  Alcotest.(check (list string))
    "matched text" [ "bar" ]
    (List.map (fun match_ -> match_.Filter.text) matches);
  Alcotest.(check (list int))
    "matched graphemes" [ 0; 1 ] (List.hd matches).Filter.matched

let fuzzy_script () =
  let options =
    {
      Filter.default_options with
      options = [ "alpha"; "beta" ];
      value = "be";
      show_help = false;
      padding = "0";
    }
  in
  let model, _frame =
    Charamel_tea.Test.run (Filter.app options)
      ~events:[ `Key (key "enter") ]
      ~size:(12, 80)
  in
  Alcotest.(check bool) "submitted" true (Filter.submitted model)

let matching_ranges () =
  Alcotest.(check (list (pair int int)))
    "coalesced ranges"
    [ (1, 3); (5, 6); (10, 10) ]
    (Filter.matched_ranges [ 1; 2; 3; 5; 6; 10 ])

let non_tty_shortcut () =
  let options =
    {
      Filter.default_options with
      options = [ "alpha"; "beta" ];
      value = "bet";
      select_if_one = true;
    }
  in
  match Filter.single_option options with
  | Ok (Some value) -> Alcotest.(check string) "sole match" "beta" value
  | Ok None -> Alcotest.fail "shortcut was not selected"
  | Error message -> Alcotest.fail message

let cases =
  [
    Alcotest_lwt.test_case_sync "exact matching" `Quick exact_matching;
    Alcotest_lwt.test_case_sync "fuzzy script" `Quick fuzzy_script;
    Alcotest_lwt.test_case_sync "matching ranges" `Quick matching_ranges;
    Alcotest_lwt.test_case_sync "non-tty sole match" `Quick non_tty_shortcut;
  ]
