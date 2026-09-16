let run () =
  Eio_main.run @@ fun env ->
  let root = Goal_fixture.repo_root () in
  let binary = Filename.concat root "_build/default/bin/gum/main.exe" in
  if not (Sys.file_exists binary) then Alcotest.failf "gum binary is missing: %s" binary;
  let scratch = Goal_fixture.fresh_scratch env ~root ~name:"sc-02" in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / scratch))
    (fun () ->
      let home = Filename.concat scratch "home" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / home);
      let child_env =
        [| "PATH=" ^ Goal_fixture.real_path (); "HOME=" ^ home; "TERM=xterm-256color" |]
      in
      let status = ref None in
      let stderr_buf = Buffer.create 256 in
      let stdout =
        Eio.Time.with_timeout_exn env#clock 15. (fun () ->
            Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
              ~cwd:Eio.Path.(env#fs / scratch)
              ~stdin:(Eio.Flow.string_source "b\n")
              ~stderr:(Eio.Flow.buffer_sink stderr_buf)
              ~is_success:(fun code ->
                status := Some code;
                true)
              ~env:child_env [ binary; "choose"; "--select-if-one" ])
      in
      Alcotest.(check int) "gum choose --select-if-one exits 0" 0
        (Option.value !status ~default:127);
      Alcotest.(check string) "no diagnostics on stderr" "" (Buffer.contents stderr_buf);
      Alcotest.(check string) "the sole piped option is printed verbatim" "b\n" stdout)
