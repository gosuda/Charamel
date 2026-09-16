let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/glow/main.exe"

let with_root f =
  let path = Filename.temp_file "glow-cli" ".dir" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote path)))
    (fun () -> f path)

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

let run_cli root args =
  Eio_main.run (fun env ->
      let status = ref None in
      let errors = Buffer.create 256 in
      let output =
        Eio.Time.with_timeout_exn env#clock 10. (fun () ->
            Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
              ~env:(env_for root) ~stdin:(Eio.Flow.string_source "")
              ~stderr:(Eio.Flow.buffer_sink errors)
              ~is_success:(fun code ->
                status := Some code;
                true)
              (executable () :: args))
      in
      (Option.value !status ~default:127, output, Buffer.contents errors))

let write_markdown root =
  let path = Filename.concat root "README.md" in
  let channel = open_out path in
  output_string channel "# Heading\n\nBody text\n";
  close_out channel;
  path

let stdout_render () =
  with_root (fun root ->
      let path = write_markdown root in
      let status, output, _ = run_cli root [ path ] in
      Alcotest.(check int) "status" 0 status;
      Alcotest.(check bool) "heading" true (String.contains output 'H'))

let pager_pipe () =
  with_root (fun root ->
      let path = write_markdown root in
      let status, output, _ = run_cli root [ "--pager"; path ] in
      Alcotest.(check int) "status" 0 status;
      Alcotest.(check bool) "pager output" true (String.contains output 'H'))

let tui_requires_terminal () =
  with_root (fun root ->
      let path = write_markdown root in
      let status, _, errors = run_cli root [ "--tui"; path ] in
      Alcotest.(check bool) "non-tty failure" true (status <> 0);
      Alcotest.(check bool) "diagnostic" true (String.length errors > 0))

let config_command () =
  with_root (fun root ->
      let status, _, _ = run_cli root [ "config" ] in
      let path = Filename.concat root "xdg-config/glow/config.json" in
      Alcotest.(check int) "status" 0 status;
      Alcotest.(check bool) "created" true (Sys.file_exists path))

let suite =
  ( "cli",
    [
      Alcotest.test_case "stdout render" `Quick stdout_render;
      Alcotest.test_case "pager" `Quick pager_pipe;
      Alcotest.test_case "tui non-tty" `Quick tui_requires_terminal;
      Alcotest.test_case "config" `Quick config_command;
    ] )
