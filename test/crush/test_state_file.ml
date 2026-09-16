module State_file = Crush_core.State_file

let with_root f =
  Eio_main.run (fun env ->
      let root = Fmt.str "/tmp/crush-state-%d-%d" (Unix.getpid ()) (Random.bits ()) in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / root);
      Fun.protect
        ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / root))
        (fun () -> f env Eio.Path.(env#fs / root)))

let temp_entries root =
  Eio.Path.read_dir root
  |> List.filter (fun name ->
      String.starts_with ~prefix:"." name && String.contains name 'c')

let check_replace_and_mode () =
  with_root (fun _env root ->
      let target = Eio.Path.(root / "state.json") in
      (match State_file.replace target "first\n" with
      | Error error -> Alcotest.failf "replace failed: %a" State_file.pp_error error
      | Ok () -> ());
      Alcotest.(check string) "complete contents" "first\n" (Eio.Path.load target);
      let stat = Eio.Path.stat ~follow:false target in
      Alcotest.(check int) "private mode" 0o600 (stat.perm land 0o777);
      Alcotest.(check (list string)) "no temporary sibling remains" [] (temp_entries root);
      (match State_file.replace target "second\n" with
      | Error error ->
          Alcotest.failf "second replace failed: %a" State_file.pp_error error
      | Ok () -> ());
      Alcotest.(check string) "replacement contents" "second\n" (Eio.Path.load target);
      Alcotest.(check (list string)) "second replacement cleans up" [] (temp_entries root))

let check_symlink_destination_is_replaced_not_followed () =
  with_root (fun _env root ->
      let original = Eio.Path.(root / "original") in
      let alias = Eio.Path.(root / "alias") in
      Eio.Path.save ~create:(`Exclusive 0o600) original "keep\n";
      Eio.Path.symlink alias ~link_to:"original";
      (match State_file.replace alias "replacement\n" with
      | Error error ->
          Alcotest.failf "symlink replacement failed: %a" State_file.pp_error error
      | Ok () -> ());
      Alcotest.(check string)
        "original target is unchanged" "keep\n" (Eio.Path.load original);
      Alcotest.(check string)
        "symlink path now contains replacement" "replacement\n" (Eio.Path.load alias);
      Alcotest.(check bool)
        "alias is a regular file" true
        (Eio.Path.kind ~follow:false alias = `Regular_file))

let check_missing_parent_is_error () =
  with_root (fun _env root ->
      let target = Eio.Path.(root / "missing" / "state.json") in
      match State_file.replace target "value" with
      | Ok () -> Alcotest.fail "replace unexpectedly created a missing parent"
      | Error (`Io (path, _)) ->
          Alcotest.(check bool)
            "error names destination" true
            (String.ends_with ~suffix:"state.json" path))

let cases =
  [
    Alcotest.test_case "atomic replacement and mode" `Quick check_replace_and_mode;
    Alcotest.test_case "symlink destination" `Quick
      check_symlink_destination_is_replaced_not_followed;
    Alcotest.test_case "missing parent" `Quick check_missing_parent_is_error;
  ]
