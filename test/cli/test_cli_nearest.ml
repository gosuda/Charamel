let check_option_string name expected actual =
  Alcotest.(check (option string)) name expected actual

let cases =
  [
    Alcotest.test_case "exact candidate" `Quick (fun () ->
        check_option_string "exact" (Some "build")
          (Charamel_cli.nearest_candidate ~candidates:[ "build"; "test" ] "build"));
    Alcotest.test_case "closest candidate" `Quick (fun () ->
        check_option_string "closest" (Some "build")
          (Charamel_cli.nearest_candidate ~candidates:[ "build"; "test" ] "buld"));
    Alcotest.test_case "stable tie" `Quick (fun () ->
        check_option_string "tie" (Some "cat")
          (Charamel_cli.nearest_candidate ~candidates:[ "cat"; "bat" ] "dat"));
    Alcotest.test_case "bounded" `Quick (fun () ->
        check_option_string "bounded" None
          (Charamel_cli.nearest_candidate ~candidates:[ "status" ] "unrelated"));
    Alcotest.test_case "dark hint" `Quick (fun () ->
        Alcotest.(check bool)
          "dark" true
          (Charamel_cli.is_dark ~env:(function "COLORFGBG" -> Some "15;8" | _ -> None)));
    Alcotest.test_case "light hint" `Quick (fun () ->
        Alcotest.(check bool)
          "light" false
          (Charamel_cli.is_dark ~env:(function "COLORFGBG" -> Some "0;231" | _ -> None)));
    Alcotest.test_case "missing hint defaults dark" `Quick (fun () ->
        Alcotest.(check bool) "default" true (Charamel_cli.is_dark ~env:(fun _ -> None)));
  ]
