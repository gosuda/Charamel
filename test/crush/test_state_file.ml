module State_file = Crush_core.State_file
open Lwt_direct

let temp_entries root =
  Array.to_list (Sys.readdir root)
  |> List.filter (fun name ->
      String.starts_with ~prefix:"." name && String.contains name 'c')

let check_replace_and_mode () =
  Test_tools_test_support.with_scratch (fun root ->
      let target = Filename.concat root "state.json" in
      (match await (State_file.replace target "first\n") with
      | Error error -> Alcotest.failf "replace failed: %a" State_file.pp_error error
      | Ok () -> ());
      Alcotest.(check string)
        "complete contents" "first\n"
        (Test_tools_test_support.load_file target);
      Alcotest.(check int)
        "private mode"
        (if Sys.win32 then 0o666 else 0o600)
        ((Unix.lstat target).Unix.st_perm land 0o777);
      Alcotest.(check (list string)) "no temporary sibling remains" [] (temp_entries root);
      (match await (State_file.replace target "second\n") with
      | Error error ->
          Alcotest.failf "second replace failed: %a" State_file.pp_error error
      | Ok () -> ());
      Alcotest.(check string)
        "replacement contents" "second\n"
        (Test_tools_test_support.load_file target);
      Alcotest.(check (list string)) "second replacement cleans up" [] (temp_entries root))

let check_symlink_destination_is_replaced_not_followed () =
  Test_tools_test_support.with_scratch (fun root ->
      let original = Filename.concat root "original" in
      let alias = Filename.concat root "alias" in
      let channel =
        open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_binary ] 0o600 original
      in
      output_string channel "keep\n";
      close_out channel;
      Unix.symlink "original" alias;
      (match await (State_file.replace alias "replacement\n") with
      | Error error ->
          Alcotest.failf "symlink replacement failed: %a" State_file.pp_error error
      | Ok () -> ());
      Alcotest.(check string)
        "original target is unchanged" "keep\n"
        (Test_tools_test_support.load_file original);
      Alcotest.(check string)
        "symlink path now contains replacement" "replacement\n"
        (Test_tools_test_support.load_file alias);
      Alcotest.(check bool)
        "alias is a regular file" true
        ((Unix.lstat alias).Unix.st_kind = Unix.S_REG))

let check_missing_parent_is_error () =
  Test_tools_test_support.with_scratch (fun root ->
      let target = Filename.concat (Filename.concat root "missing") "state.json" in
      match await (State_file.replace target "value") with
      | Ok () -> Alcotest.fail "replace unexpectedly created a missing parent"
      | Error (`Io (path, _)) ->
          Alcotest.(check bool)
            "error names destination" true
            (String.ends_with ~suffix:"state.json" path))

let cases =
  [
    Test_tools_test_support.case "atomic replacement and mode" `Quick
      check_replace_and_mode;
    Test_tools_test_support.case "symlink destination" `Quick
      check_symlink_destination_is_replaced_not_followed;
    Test_tools_test_support.case "missing parent" `Quick check_missing_parent_is_error;
  ]
