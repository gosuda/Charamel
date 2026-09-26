open Lwt.Syntax
module Store = Skate_core.Store

type test = unit Alcotest_lwt.test_case

let path_env = "/usr/bin:/bin"

let minimal_environment ~data_home =
  let path name = Filename.concat data_home name in
  [|
    "PATH=" ^ path_env;
    "LANG=C";
    "TERM=xterm-256color";
    "HOME=" ^ path "home";
    "XDG_CONFIG_HOME=" ^ path "xdg-config";
    "XDG_DATA_HOME=" ^ data_home;
    "XDG_STATE_HOME=" ^ path "xdg-state";
    "XDG_CACHE_HOME=" ^ path "xdg-cache";
    "TMPDIR=" ^ path "tmp";
  |]

let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/skate/main.exe"

let fail_error error = Alcotest.fail (Fmt.str "store error: %a" Store.pp_error error)
let get_ok = function Ok value -> value | Error error -> fail_error error
let unit_ok = function Ok () -> () | Error error -> fail_error error

let load_file path =
  let ic = open_in_bin path in
  let length = in_channel_length ic in
  let body = really_input_string ic length in
  close_in ic;
  body

let write_corrupt root db body =
  let path = Filename.concat root (db ^ ".json") in
  let out = open_out_bin path in
  output_string out body;
  close_out out

let error_matches name predicate operation =
  let* result = operation () in
  match result with
  | Error error when predicate error -> Lwt.return_unit
  | Error error ->
      Alcotest.fail (Fmt.str "%s: unexpected error %a" name Store.pp_error error)
  | Ok _ -> Alcotest.fail (name ^ ": expected an error")

let roundtrip_text =
  Alcotest_lwt.test_case "text roundtrip" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let* result = Store.set ~root ~db:"default" "greeting" "héllo" in
          unit_ok result;
          let* result = Store.get ~root ~db:"default" "greeting" in
          (match get_ok result with
          | Store.Text value -> Alcotest.(check string) "text" "héllo" value
          | Store.Binary _ -> Alcotest.fail "UTF-8 text was classified as binary");
          Lwt.return_unit))

let roundtrip_binary =
  Alcotest_lwt.test_case "binary roundtrip" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let raw = "\255\000\001\254" in
          let* result = Store.set ~root ~db:"default" "blob" raw in
          unit_ok result;
          let* result = Store.get ~root ~db:"default" "blob" in
          (match get_ok result with
          | Store.Binary value -> Alcotest.(check string) "bytes" raw value
          | Store.Text _ -> Alcotest.fail "invalid UTF-8 was classified as text");
          Lwt.return_unit))

let absent_db_get =
  Alcotest_lwt.test_case "get absent database" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          error_matches "absent get"
            (function `No_such_db "missing" -> true | _ -> false)
            (fun () -> Store.get ~root ~db:"missing" "key")))

let absent_db_delete =
  Alcotest_lwt.test_case "delete absent database" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          error_matches "absent delete"
            (function `No_such_db "missing" -> true | _ -> false)
            (fun () -> Store.delete ~root ~db:"missing" "key")))

let absent_db_list =
  Alcotest_lwt.test_case "list absent database" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let* result = Store.list ~root ~db:"missing" in
          Alcotest.(check (list (pair string string)))
            "empty" []
            (match result with
            | Ok entries ->
                List.map
                  (fun (key, value) ->
                    ( key,
                      match value with Store.Text s -> s | Store.Binary _ -> "binary" ))
                  entries
            | Error error -> fail_error error);
          Lwt.return_unit))

let absent_key_get =
  Alcotest_lwt.test_case "get absent key" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let* result = Store.set ~root ~db:"default" "present" "value" in
          unit_ok result;
          error_matches "absent key get"
            (function `No_such_key "missing" -> true | _ -> false)
            (fun () -> Store.get ~root ~db:"default" "missing")))

let absent_key_delete =
  Alcotest_lwt.test_case "delete absent key" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let* result = Store.set ~root ~db:"default" "present" "value" in
          unit_ok result;
          error_matches "absent key delete"
            (function `No_such_key "missing" -> true | _ -> false)
            (fun () -> Store.delete ~root ~db:"default" "missing")))

let corrupt_file_get =
  Alcotest_lwt.test_case "corrupt file get" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let body = "{ not json" in
          write_corrupt root "broken" body;
          let* () =
            error_matches "corrupt get"
              (function `Corrupt _ -> true | _ -> false)
              (fun () -> Store.get ~root ~db:"broken" "key")
          in
          Alcotest.(check string)
            "file unchanged" body
            (load_file (Filename.concat root "broken.json"));
          Lwt.return_unit))

let corrupt_file_list =
  Alcotest_lwt.test_case "corrupt file list" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let body = "[1, 2, 3]" in
          write_corrupt root "broken" body;
          let* () =
            error_matches "corrupt list"
              (function `Corrupt _ -> true | _ -> false)
              (fun () -> Store.list ~root ~db:"broken")
          in
          Alcotest.(check string)
            "file unchanged" body
            (load_file (Filename.concat root "broken.json"));
          Lwt.return_unit))

let corrupt_file_set =
  Alcotest_lwt.test_case "corrupt file set" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let body = "{ broken" in
          write_corrupt root "broken" body;
          let* () =
            error_matches "corrupt set"
              (function `Corrupt _ -> true | _ -> false)
              (fun () -> Store.set ~root ~db:"broken" "key" "value")
          in
          Alcotest.(check string)
            "file unchanged" body
            (load_file (Filename.concat root "broken.json"));
          Lwt.return_unit))

let corrupt_file_delete =
  Alcotest_lwt.test_case "corrupt file delete" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let body = "{ broken" in
          write_corrupt root "broken" body;
          let* () =
            error_matches "corrupt delete"
              (function `Corrupt _ -> true | _ -> false)
              (fun () -> Store.delete ~root ~db:"broken" "key")
          in
          Alcotest.(check string)
            "file unchanged" body
            (load_file (Filename.concat root "broken.json"));
          Lwt.return_unit))

let invalid_db_slash =
  Alcotest_lwt.test_case "database slash rejected" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          error_matches "slash"
            (function `Invalid_db "a/b" -> true | _ -> false)
            (fun () -> Store.set ~root ~db:"a/b" "key" "value")))

let invalid_db_dotdot =
  Alcotest_lwt.test_case "database dot-dot rejected" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          error_matches "dot-dot"
            (function `Invalid_db "a..b" -> true | _ -> false)
            (fun () -> Store.set ~root ~db:"a..b" "key" "value")))

let invalid_db_empty =
  Alcotest_lwt.test_case "database empty rejected" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          error_matches "empty"
            (function `Invalid_db "" -> true | _ -> false)
            (fun () -> Store.set ~root ~db:"" "key" "value")))

let delete_db =
  Alcotest_lwt.test_case "delete database" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let* result = Store.set ~root ~db:"temporary" "key" "value" in
          unit_ok result;
          let* result = Store.delete_db ~root ~db:"temporary" in
          unit_ok result;
          error_matches "deleted database"
            (function `No_such_db "temporary" -> true | _ -> false)
            (fun () -> Store.get ~root ~db:"temporary" "key")))

let dbs_sorted =
  Alcotest_lwt.test_case "database names sorted after suffix removal" `Quick
    (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let* () =
            Lwt_list.iter_s
              (fun db ->
                let* result = Store.set ~root ~db "key" db in
                unit_ok result;
                Lwt.return_unit)
              [ "a-b"; "z"; "a" ]
          in
          let* result = Store.dbs ~root in
          Alcotest.(check (list string))
            "names" [ "a"; "a-b"; "z" ]
            (match result with Ok names -> names | Error error -> fail_error error);
          Lwt.return_unit))

let dbs_absent_root =
  Alcotest_lwt.test_case "databases absent root" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun parent ->
          let root = Filename.concat parent "absent" in
          let* result = Store.dbs ~root in
          Alcotest.(check (list string))
            "empty" []
            (match result with Ok names -> names | Error error -> fail_error error);
          Lwt.return_unit))

let atomic_0600 =
  Alcotest_lwt.test_case "database mode is 0600" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let* result = Store.set ~root ~db:"private" "key" "value" in
          unit_ok result;
          Alcotest.(check int)
            "mode" 0o600 (Unix.stat (Filename.concat root "private.json")).Unix.st_perm;
          Lwt.return_unit))

let binary_not_utf8 =
  Alcotest_lwt.test_case "binary marker is persisted" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let raw = "\255\000" in
          let* result = Store.set ~root ~db:"binary" "key" raw in
          unit_ok result;
          let body = load_file (Filename.concat root "binary.json") in
          Alcotest.(check bool)
            "binary marker" true
            (Test_support.contains ~needle:"\"b\": true" ~haystack:body);
          let* result = Store.get ~root ~db:"binary" "key" in
          (match get_ok result with
          | Store.Binary value -> Alcotest.(check string) "raw bytes" raw value
          | Store.Text _ -> Alcotest.fail "binary marker was not decoded as binary");
          Lwt.return_unit))

let show_binary_flag =
  Alcotest_lwt.test_case "show binary flag" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let exe = executable () in
          let env = minimal_environment ~data_home:root in
          let raw = "\255\000\001" in
          let* status, _, error =
            Test_support.run_cli ~exe ~env ~stdin:raw [ "set"; "payload"; "-" ]
          in
          Alcotest.(check int) "set status" 0 status;
          Alcotest.(check string) "set stderr" "" error;
          let* status, hidden, error =
            Test_support.run_cli ~exe ~env [ "get"; "payload" ]
          in
          Alcotest.(check int) "hidden status" 0 status;
          Alcotest.(check string) "hidden output" "[binary 3 bytes]\n" hidden;
          Alcotest.(check string) "hidden stderr" "" error;
          let* status, shown, error =
            Test_support.run_cli ~exe ~env [ "get"; "payload"; "--show-binary" ]
          in
          Alcotest.(check int) "shown status" 0 status;
          Alcotest.(check string) "shown output" raw shown;
          Alcotest.(check string) "shown stderr" "" error;
          Lwt.return_unit))

let cli_roundtrip =
  Alcotest_lwt.test_case "CLI roundtrip" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let exe = executable () in
          let env = minimal_environment ~data_home:root in
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "set"; "alpha"; "plain" ]
          in
          Alcotest.(check int) "set status" 0 status;
          Alcotest.(check string) "set output" "" output;
          Alcotest.(check string) "set error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "get"; "alpha" ]
          in
          Alcotest.(check int) "get status" 0 status;
          Alcotest.(check string) "get output" "plain\n" output;
          Alcotest.(check string) "get error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "set"; "scoped@other"; "scoped-value" ]
          in
          Alcotest.(check int) "scoped set status" 0 status;
          Alcotest.(check string) "scoped set output" "" output;
          Alcotest.(check string) "scoped set error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "get"; "scoped@other" ]
          in
          Alcotest.(check int) "scoped get status" 0 status;
          Alcotest.(check string) "scoped get output" "scoped-value\n" output;
          Alcotest.(check string) "scoped get error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "set"; "Foo@BAR"; "case-value" ]
          in
          Alcotest.(check int) "case set status" 0 status;
          Alcotest.(check string) "case set output" "" output;
          Alcotest.(check string) "case set error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "get"; "Foo@BAR" ]
          in
          Alcotest.(check int) "case get status" 0 status;
          Alcotest.(check string) "case get output" "case-value\n" output;
          Alcotest.(check string) "case get error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "get"; "foo@bar" ]
          in
          Alcotest.(check int) "case distinction status" 1 status;
          Alcotest.(check string) "case distinction output" "" output;
          Alcotest.(check bool)
            "case distinction error" true
            (Test_support.contains ~needle:"no such database" ~haystack:error);
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "set"; "foo@"; "default-suffix" ]
          in
          Alcotest.(check int) "default suffix set status" 0 status;
          Alcotest.(check string) "default suffix set output" "" output;
          Alcotest.(check string) "default suffix set error" "" error;
          let* status, output, error = Test_support.run_cli ~exe ~env [ "get"; "foo" ] in
          Alcotest.(check int) "default suffix get status" 0 status;
          Alcotest.(check string) "default suffix get output" "default-suffix\n" output;
          Alcotest.(check string) "default suffix get error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "list"; "@other" ]
          in
          Alcotest.(check int) "list status" 0 status;
          Alcotest.(check string) "list output" "scoped\tscoped-value\n" output;
          Alcotest.(check string) "list error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "list"; "@other"; "--keys-only" ]
          in
          Alcotest.(check int) "keys status" 0 status;
          Alcotest.(check string) "keys output" "scoped\n" output;
          Alcotest.(check string) "keys error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "list"; "@other"; "--values-only" ]
          in
          Alcotest.(check int) "values status" 0 status;
          Alcotest.(check string) "values output" "scoped-value\n" output;
          Alcotest.(check string) "values error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "delete"; "scoped@other" ]
          in
          Alcotest.(check int) "delete status" 0 status;
          Alcotest.(check string) "delete output" "" output;
          Alcotest.(check string) "delete error" "" error;
          let* status, _output, error =
            Test_support.run_cli ~exe ~env [ "delete-db"; "other" ]
          in
          Alcotest.(check int) "delete db status" 0 status;
          Alcotest.(check string) "delete db error" "" error;
          let* status, _output, error =
            Test_support.run_cli ~exe ~env [ "delete-db"; "BAR" ]
          in
          Alcotest.(check int) "case delete db status" 0 status;
          Alcotest.(check string) "case delete db error" "" error;
          let* status, output, error = Test_support.run_cli ~exe ~env [ "dbs" ] in
          Alcotest.(check int) "dbs status" 0 status;
          Alcotest.(check string) "dbs output" "default\n" output;
          Alcotest.(check string) "dbs error" "" error;
          Lwt.return_unit))

let cli_absent_paths =
  Alcotest_lwt.test_case "CLI absent paths" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let exe = executable () in
          let env = minimal_environment ~data_home:root in
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "get"; "missing" ]
          in
          Alcotest.(check int) "get status" 1 status;
          Alcotest.(check string) "get output" "" output;
          Alcotest.(check bool)
            "get error" true
            (Test_support.contains ~needle:"no such database" ~haystack:error);
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "delete"; "missing" ]
          in
          Alcotest.(check int) "delete status" 1 status;
          Alcotest.(check string) "delete output" "" output;
          Alcotest.(check bool)
            "delete error" true
            (Test_support.contains ~needle:"no such database" ~haystack:error);
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "list"; "@missing" ]
          in
          Alcotest.(check int) "list status" 0 status;
          Alcotest.(check string) "list output" "" output;
          Alcotest.(check string) "list error" "" error;
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "delete-db"; "missing" ]
          in
          Alcotest.(check int) "delete db status" 1 status;
          Alcotest.(check string) "delete db output" "" output;
          Alcotest.(check bool)
            "delete db error" true
            (Test_support.contains ~needle:"no such database" ~haystack:error);
          let* status, output, error =
            Test_support.run_cli ~exe ~env [ "list"; "--keys-only"; "--values-only" ]
          in
          Alcotest.(check int) "conflicting flags status" 1 status;
          Alcotest.(check string) "conflicting flags output" "" output;
          Alcotest.(check bool)
            "conflicting flags error" true
            (Test_support.contains ~needle:"cannot be combined" ~haystack:error);
          let* status, output, error = Test_support.run_cli ~exe ~env [ "dbs" ] in
          Alcotest.(check int) "dbs status" 0 status;
          Alcotest.(check string) "dbs output" "" output;
          Alcotest.(check string) "dbs error" "" error;
          Lwt.return_unit))

let cli_unknown_command =
  Alcotest_lwt.test_case "CLI unknown command rejection" `Quick (fun _switch () ->
      Test_support.with_temp_dir (fun root ->
          let exe = executable () in
          let env = minimal_environment ~data_home:root in
          let* status, output, _ = Test_support.run_cli ~exe ~env [ "gett" ] in
          Alcotest.(check int) "status" 2 status;
          Alcotest.(check string) "output" "" output;
          Lwt.return_unit))

let cases =
  ( "store",
    [
      roundtrip_text;
      roundtrip_binary;
      absent_db_get;
      absent_db_delete;
      absent_db_list;
      absent_key_get;
      absent_key_delete;
      corrupt_file_get;
      corrupt_file_list;
      corrupt_file_set;
      corrupt_file_delete;
      invalid_db_slash;
      invalid_db_dotdot;
      invalid_db_empty;
      delete_db;
      dbs_sorted;
      dbs_absent_root;
      atomic_0600;
      binary_not_utf8;
      show_binary_flag;
      cli_roundtrip;
      cli_absent_paths;
      cli_unknown_command;
    ] )

let () = Test_support.run_lwt "skate" [ cases ]
