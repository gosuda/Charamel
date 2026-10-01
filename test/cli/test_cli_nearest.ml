let cases =
  [
    Alcotest_lwt.test_case_sync "dark hint" `Quick (fun () ->
        Alcotest.(check bool)
          "dark" true
          (Charamel_cli.is_dark ~env:(function "COLORFGBG" -> Some "15;8" | _ -> None)));
    Alcotest_lwt.test_case_sync "light hint" `Quick (fun () ->
        Alcotest.(check bool)
          "light" false
          (Charamel_cli.is_dark ~env:(function "COLORFGBG" -> Some "0;231" | _ -> None)));
    Alcotest_lwt.test_case_sync "missing hint defaults dark" `Quick (fun () ->
        Alcotest.(check bool) "default" true (Charamel_cli.is_dark ~env:(fun _ -> None)));
  ]
