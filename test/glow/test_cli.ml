open Lwt.Syntax

let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/glow/main.exe"

let env_for root =
  let path name = Filename.concat root name in
  [|
    "PATH=/usr/bin:/bin";
    "LANG=C";
    "TERM=xterm-256color";
    "EDITOR=true";
    "PAGER=cat";
    "HOME=" ^ path "home";
    "XDG_CONFIG_HOME=" ^ path "xdg-config";
    "XDG_DATA_HOME=" ^ path "xdg-data";
    "XDG_STATE_HOME=" ^ path "xdg-state";
    "XDG_CACHE_HOME=" ^ path "xdg-cache";
  |]

let write_markdown root =
  let path = Filename.concat root "README.md" in
  let channel = open_out path in
  output_string channel "# Heading\n\nBody text\n";
  close_out channel;
  path

let stdout_render () =
  Test_support.with_temp_dir (fun root ->
      let path = write_markdown root in
      let* status, output, _ =
        Test_support.run_cli ~exe:(executable ()) ~env:(env_for root) ~cwd:root
          ~timeout:10. [ path ]
      in
      Alcotest.(check int) "status" 0 status;
      Alcotest.(check bool) "heading" true (String.contains output 'H');
      Lwt.return_unit)

let pager_pipe () =
  Test_support.with_temp_dir (fun root ->
      let path = write_markdown root in
      let* status, output, _ =
        Test_support.run_cli ~exe:(executable ()) ~env:(env_for root) ~cwd:root
          ~timeout:10. [ "--pager"; path ]
      in
      Alcotest.(check int) "status" 0 status;
      Alcotest.(check bool) "pager output" true (String.contains output 'H');
      Lwt.return_unit)

let tui_requires_terminal () =
  Test_support.with_temp_dir (fun root ->
      let path = write_markdown root in
      let* status, _, errors =
        Test_support.run_cli ~exe:(executable ()) ~env:(env_for root) ~cwd:root
          ~timeout:10. [ "--tui"; path ]
      in
      Alcotest.(check bool) "non-tty failure" true (status <> 0);
      Alcotest.(check bool) "diagnostic" true (String.length errors > 0);
      Lwt.return_unit)

let config_command () =
  Test_support.with_temp_dir (fun root ->
      let* status, _, _ =
        Test_support.run_cli ~exe:(executable ()) ~env:(env_for root) ~cwd:root
          ~timeout:10. [ "config" ]
      in
      let path = Filename.concat root "xdg-config/glow/config.json" in
      Alcotest.(check int) "status" 0 status;
      Alcotest.(check bool) "created" true (Sys.file_exists path);
      Lwt.return_unit)

let suite =
  ( "cli",
    [
      Alcotest_lwt.test_case "stdout render" `Quick (fun _switch () -> stdout_render ());
      Alcotest_lwt.test_case "pager" `Quick (fun _switch () -> pager_pipe ());
      Alcotest_lwt.test_case "tui non-tty" `Quick (fun _switch () ->
          tui_requires_terminal ());
      Alcotest_lwt.test_case "config" `Quick (fun _switch () -> config_command ());
    ] )
