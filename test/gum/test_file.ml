open Lwt.Syntax

let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/gum/main.exe"

let env_for root =
  let path name = Filename.concat root name in
  [|
    "PATH=/usr/bin:/bin";
    "LANG=C";
    "TERM=xterm-256color";
    "HOME=" ^ path "home";
    "XDG_CONFIG_HOME=" ^ path "xdg-config";
    "XDG_DATA_HOME=" ^ path "xdg-data";
    "XDG_STATE_HOME=" ^ path "xdg-state";
    "XDG_CACHE_HOME=" ^ path "xdg-cache";
  |]

let test_cli_rejects_without_selection_kind () =
  Test_support.with_temp_dir (fun root ->
      let* status, _, diagnostics =
        Test_support.run_cli ~exe:(executable ()) ~env:(env_for root) ~cwd:root
          ~timeout:10.
          [ "file"; "--no-file"; "--no-directory" ]
      in
      Alcotest.(check int) "status" 1 status;
      Alcotest.(check bool)
        "selection diagnostic" true
        (Test_support.contains ~needle:"at least one" ~haystack:diagnostics);
      Lwt.return_unit)

let cases =
  [
    Alcotest_lwt.test_case "CLI selection policy" `Quick (fun _switch () ->
        test_cli_rejects_without_selection_kind ());
  ]
