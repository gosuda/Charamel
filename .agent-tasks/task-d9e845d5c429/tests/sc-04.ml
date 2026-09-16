let fingerprint_token output =
  match
    List.find_opt (String.starts_with ~prefix:"SHA256:")
      (String.split_on_char ' ' (String.trim output))
  with
  | Some token -> token
  | None -> Alcotest.failf "ssh-keygen printed no SHA256 fingerprint: %S" output

let run () =
  Eio_main.run @@ fun env ->
  let root = Goal_fixture.repo_root () in
  let keygen_binary = Filename.concat root "_build/default/bin/keygen/main.exe" in
  if not (Sys.file_exists keygen_binary) then
    Alcotest.failf "keygen binary is missing: %s" keygen_binary;
  let scratch = Goal_fixture.fresh_scratch env ~root ~name:"sc-04" in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / scratch))
    (fun () ->
      let key_path = Filename.concat scratch "k" in
      let pub_path = key_path ^ ".pub" in
      let key = Eio.Path.(env#fs / key_path) in
      let pub = Eio.Path.(env#fs / pub_path) in
      let home = Filename.concat scratch "home" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / home);
      let child_env = [| "PATH=" ^ Goal_fixture.real_path (); "HOME=" ^ home |] in
      let printed_fingerprint =
        Eio.Time.with_timeout_exn env#clock 30. (fun () ->
            String.trim
              (Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all ~env:child_env
                 [ keygen_binary; "-t"; "ed25519"; "-f"; key_path ]))
      in
      Alcotest.(check bool) "the printed fingerprint has the SHA256 form" true
        (String.length printed_fingerprint > 7
        && String.equal (String.sub printed_fingerprint 0 7) "SHA256:");
      Alcotest.(check bool) "the private key was written" true (Eio.Path.is_file key);
      Alcotest.(check bool) "the public key was written" true (Eio.Path.is_file pub);
      let ssh_output =
        Eio.Time.with_timeout_exn env#clock 30. (fun () ->
            Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all ~env:child_env
              [ "ssh-keygen"; "-l"; "-f"; pub_path ])
      in
      Alcotest.(check string) "ssh-keygen confirms the printed fingerprint" printed_fingerprint
        (fingerprint_token ssh_output);
      Alcotest.(check bool) "ssh-keygen reports the ED25519 key type" true
        (Goal_fixture.contains ~needle:"ED25519" ssh_output);
      Alcotest.(check bool) "the public key carries the ssh-ed25519 format" true
        (String.starts_with ~prefix:"ssh-ed25519 " (Eio.Path.load pub));
      let private_bytes_before = Eio.Path.load key in
      let public_bytes_before = Eio.Path.load pub in
      let private_mode_before = (Eio.Path.stat ~follow:false key).perm in
      let public_mode_before = (Eio.Path.stat ~follow:false pub).perm in
      let status = ref None in
      let stderr_buf = Buffer.create 256 in
      let (_ : string) =
        Eio.Time.with_timeout_exn env#clock 30. (fun () ->
            Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all ~env:child_env
              ~stderr:(Eio.Flow.buffer_sink stderr_buf)
              ~is_success:(fun code ->
                status := Some code;
                true)
              [ keygen_binary; "-t"; "ed25519"; "-f"; key_path ])
      in
      Alcotest.(check bool) "a non-force rerun on the same path is refused" true
        (Option.value !status ~default:0 <> 0);
      Alcotest.(check bool) "the refusal names the existing path" true
        (Goal_fixture.contains ~needle:"already exists" (Buffer.contents stderr_buf));
      Alcotest.(check string) "the private key bytes are unchanged" private_bytes_before
        (Eio.Path.load key);
      Alcotest.(check string) "the public key bytes are unchanged" public_bytes_before
        (Eio.Path.load pub);
      Alcotest.(check int) "the private key mode is unchanged" private_mode_before
        (Eio.Path.stat ~follow:false key).perm;
      Alcotest.(check int) "the public key mode is unchanged" public_mode_before
        (Eio.Path.stat ~follow:false pub).perm)
