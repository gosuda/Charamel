let heading = "Charmed Heading"
let markdown = "# " ^ heading ^ "\n\n## Sub\n\nBody paragraph.\n"

let run () =
  Eio_main.run @@ fun env ->
  let root = Goal_fixture.repo_root () in
  let binary = Filename.concat root "_build/default/bin/glow/main.exe" in
  if not (Sys.file_exists binary) then Alcotest.failf "glow binary is missing: %s" binary;
  let scratch = Goal_fixture.fresh_scratch env ~root ~name:"sc-05" in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / scratch))
    (fun () ->
      let markdown_path = Filename.concat scratch "fixture.md" in
      Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(env#fs / markdown_path) markdown;
      let home = Filename.concat scratch "home" in
      let xdg_config = Filename.concat scratch "xdg-config" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / home);
      (* An absent, explicit --config bypasses Glow's upward config search
         and the XDG fallback entirely (bin/glow/config.ml:124-131,
         185-213), so no real state above the scratch tree can leak in. *)
      let config_path = Filename.concat scratch "unused-config.json" in
      let child_env =
        [|
          "PATH=" ^ Goal_fixture.real_path ();
          "HOME=" ^ home;
          "XDG_CONFIG_HOME=" ^ xdg_config;
          "TERM=xterm-256color";
          "CLICOLOR_FORCE=1";
          "COLORTERM=truecolor";
        |]
      in
      let status = ref None in
      let stderr_buf = Buffer.create 256 in
      let stdout =
        Eio.Time.with_timeout_exn env#clock 15. (fun () ->
            Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
              ~cwd:Eio.Path.(env#fs / scratch)
              ~stderr:(Eio.Flow.buffer_sink stderr_buf)
              ~is_success:(fun code ->
                status := Some code;
                true)
              ~env:child_env
              [ binary; "--config"; config_path; "--style"; "dark"; markdown_path ])
      in
      Alcotest.(check int) "glow exits 0" 0 (Option.value !status ~default:127);
      Alcotest.(check string) "no diagnostics on stderr" "" (Buffer.contents stderr_buf);
      Alcotest.(check bool)
        "the output carries real ANSI styling" true
        (not (String.equal (Charm_ansi.Text.strip stdout) stdout));
      Alcotest.(check bool)
        "the H1 uses the dark theme's indexed foreground 228" true
        (Goal_fixture.contains ~needle:"38;5;228" stdout);
      Alcotest.(check bool)
        "the H1 uses the dark theme's indexed background 63" true
        (Goal_fixture.contains ~needle:"48;5;63" stdout);
      Alcotest.(check bool)
        "the h2 inherits the dark theme's heading foreground 39" true
        (Goal_fixture.contains ~needle:"38;5;39" stdout);
      Alcotest.(check bool)
        "the light theme's heading foreground 27 is absent" false
        (Goal_fixture.contains ~needle:"38;5;27" stdout);
      Alcotest.(check bool)
        "the rendered text carries the heading" true
        (Goal_fixture.contains ~needle:heading (Charm_ansi.Text.strip stdout)))
