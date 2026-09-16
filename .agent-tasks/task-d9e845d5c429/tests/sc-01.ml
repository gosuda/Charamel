let apps =
  [ "gum"; "glow"; "freeze"; "sequin"; "pop"; "skate"; "melt"; "keygen"; "hotdiva2000"; "crush" ]

let run () =
  Eio_main.run @@ fun env ->
  let root = Goal_fixture.repo_root () in
  let build_dir = Goal_fixture.fresh_scratch env ~root ~name:"sc-01-build" in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / build_dir))
    (fun () ->
      let home = Filename.concat build_dir "home" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / home);
      let child_env = [| "PATH=" ^ Goal_fixture.real_path (); "HOME=" ^ home |] in
      let stdout_buf = Buffer.create 4096 in
      let stderr_buf = Buffer.create 4096 in
      let status =
        Eio.Time.with_timeout_exn env#clock 1800. (fun () ->
            Eio.Switch.run @@ fun sw ->
            let child =
              Eio.Process.spawn ~sw env#process_mgr ~cwd:Eio.Path.(env#fs / root) ~env:child_env
                ~stdout:(Eio.Flow.buffer_sink stdout_buf)
                ~stderr:(Eio.Flow.buffer_sink stderr_buf)
                [ "dune"; "build"; "--profile"; "release"; "--build-dir"; build_dir ]
            in
            Eio.Process.await child)
      in
      (match status with
      | `Exited 0 -> ()
      | `Exited code ->
          Alcotest.failf "dune build --profile release exited %d\nstdout:\n%s\nstderr:\n%s" code
            (Buffer.contents stdout_buf) (Buffer.contents stderr_buf)
      | `Signaled signal ->
          Alcotest.failf "dune build --profile release was killed by signal %d" signal);
      List.iter
        (fun app ->
          let binary = Filename.concat build_dir (Fmt.str "default/bin/%s/main.exe" app) in
          Alcotest.(check bool) (app ^ " binary was built in the isolated build directory") true
            (Sys.file_exists binary))
        apps;
      Alcotest.(check bool) "the charm library package installed" true
        (Sys.file_exists (Filename.concat build_dir "default/charm.install"));
      Alcotest.(check bool) "the charm-ssh library package installed" true
        (Sys.file_exists (Filename.concat build_dir "default/charm-ssh.install")))
