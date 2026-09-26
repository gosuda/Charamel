open Lwt.Syntax
module Key = Charamel_ssh_keygen
module Command = Keygen_core.Keygen

let expect_ok label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Command.pp_error error

let expect_error label = function
  | Error error -> error
  | Ok _ -> Alcotest.failf "%s: expected an error" label

let check_error_path label expected = function
  | `Already_exists path -> Alcotest.check Alcotest.string label expected path
  | error ->
      Alcotest.failf "%s: expected an existing-path error, got %a" label Command.pp_error
        error

let load_file path =
  let ic = open_in_bin path in
  let length = in_channel_length ic in
  let body = really_input_string ic length in
  close_in ic;
  body

let save_exclusive path perm body =
  let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] perm in
  let out = Unix.out_channel_of_descr fd in
  output_string out body;
  close_out out

let nonforce_refuses_private () =
  Test_support.with_temp_dir (fun root ->
      let path = Filename.concat root "key" in
      save_exclusive path 0o600 "unchanged";
      let* result =
        Command.generate ~fs_root:root ~path ~algorithm:Key.Ed25519 ~force:false ()
      in
      let error = expect_error "non-force existing private key" result in
      check_error_path "private path" path error;
      Alcotest.check Alcotest.string "private key remains unchanged" "unchanged"
        (load_file path);
      Alcotest.check Alcotest.bool "public key is not written" false
        (Sys.file_exists (path ^ ".pub"));
      Lwt.return_unit)

let nonforce_refuses_public () =
  Test_support.with_temp_dir (fun root ->
      let path = Filename.concat root "key" in
      let public_path = path ^ ".pub" in
      save_exclusive public_path 0o644 "unchanged";
      let* result =
        Command.generate ~fs_root:root ~path ~algorithm:Key.Ed25519 ~force:false ()
      in
      let error = expect_error "non-force existing public key" result in
      check_error_path "public path" public_path error;
      Alcotest.check Alcotest.string "public key remains unchanged" "unchanged"
        (load_file public_path);
      Alcotest.check Alcotest.bool "private key is not written" false
        (Sys.file_exists path);
      Lwt.return_unit)

let force_replaces_pair () =
  Test_support.with_temp_dir (fun root ->
      let path = Filename.concat root "key" in
      let* first_result =
        Command.generate ~fs_root:root ~path ~algorithm:Key.Ed25519 ~comment:"first"
          ~force:false ()
      in
      let first_fingerprint = expect_ok "initial key pair" first_result in
      let* second_result =
        Command.generate ~fs_root:root ~path ~algorithm:Key.Ed25519 ~comment:"second"
          ~force:true ()
      in
      let second_fingerprint = expect_ok "forced key pair" second_result in
      Alcotest.(check bool)
        "force generates a fresh key" true
        (not (String.equal first_fingerprint second_fingerprint));
      let public_path = path ^ ".pub" in
      let private_body = load_file path in
      let key =
        match Key.of_openssh_private private_body with
        | Ok key -> key
        | Error _ -> Alcotest.fail "forced private key is not parseable"
      in
      Alcotest.check Alcotest.string "forced fingerprint matches private key"
        second_fingerprint (Key.fingerprint_sha256 key);
      Alcotest.check Alcotest.string "forced public key matches private key"
        (Key.authorized_key ~comment:"second" key)
        (load_file public_path);
      Alcotest.check Alcotest.int "private mode" 0o600 (Unix.lstat path).Unix.st_perm;
      Alcotest.check Alcotest.int "public mode" 0o644
        (Unix.lstat public_path).Unix.st_perm;
      Lwt.return_unit)

let force_rejects_directory_target () =
  Test_support.with_temp_dir (fun root ->
      let path = Filename.concat root "key" in
      Unix.mkdir path 0o700;
      let* result =
        Command.generate ~fs_root:root ~path ~algorithm:Key.Ed25519 ~force:true ()
      in
      (match expect_error "force existing directory target" result with
      | `Io _ -> ()
      | error ->
          Alcotest.failf "force existing directory target: expected `Io, got %a"
            Command.pp_error error);
      Alcotest.check Alcotest.bool "directory is untouched" true (Sys.is_directory path);
      Lwt.return_unit)

let missing_parent_is_created () =
  Test_support.with_temp_dir (fun root ->
      let nested = Filename.concat root "nested" in
      let deeper = Filename.concat nested "deeper" in
      let path = Filename.concat deeper "key" in
      let* result =
        Command.generate ~fs_root:root ~path ~algorithm:Key.Ecdsa_p256 ~force:false ()
      in
      let fingerprint = expect_ok "key pair with missing parent" result in
      Alcotest.check Alcotest.bool "parent directory exists" true
        (Sys.is_directory deeper);
      Alcotest.check Alcotest.int "first created directory mode" 0o700
        (Unix.lstat nested).Unix.st_perm;
      Alcotest.check Alcotest.int "last created directory mode" 0o700
        (Unix.lstat deeper).Unix.st_perm;
      let key =
        match Key.of_openssh_private (load_file path) with
        | Ok key -> key
        | Error _ -> Alcotest.fail "generated private key is not parseable"
      in
      Alcotest.check Alcotest.string "generated fingerprint" fingerprint
        (Key.fingerprint_sha256 key);
      Alcotest.check Alcotest.int "private mode" 0o600 (Unix.lstat path).Unix.st_perm;
      Alcotest.check Alcotest.int "public mode" 0o644
        (Unix.lstat (path ^ ".pub")).Unix.st_perm;
      Lwt.return_unit)

let default_paths () =
  match Charamel_os.Dirs.home () with
  | Error `No_home -> Alcotest.skip ()
  | Ok home ->
      let algorithms = [ Key.Ed25519; Key.Ecdsa_p256; Key.Ecdsa_p384; Key.Ecdsa_p521 ] in
      List.iter
        (fun algorithm ->
          let path = expect_ok "default path" (Command.default_path algorithm) in
          let expected =
            Filename.concat home
              (Filename.concat ".ssh" ("id_" ^ Key.algorithm_name algorithm))
          in
          Alcotest.check Alcotest.string
            ("default " ^ Key.algorithm_name algorithm)
            expected path)
        algorithms

let find_ssh_keygen () =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
      let candidate directory =
        let name = Filename.concat directory "ssh-keygen" in
        if Sys.file_exists name && not (Sys.is_directory name) then Some name else None
      in
      List.find_map candidate (String.split_on_char ':' path)

let run_ssh_keygen executable arguments =
  let* status, output, stderr = Test_support.run_cli ~exe:executable arguments in
  if status = 0 then Lwt.return output
  else Alcotest.failf "ssh-keygen exited with status %d: %s" status (String.trim stderr)

let fingerprint_token output =
  match
    List.find_opt
      (String.starts_with ~prefix:"SHA256:")
      (String.split_on_char ' ' (String.trim output))
  with
  | Some fingerprint -> fingerprint
  | None -> Alcotest.failf "ssh-keygen printed no fingerprint: %S" output

let generated_key_interop () =
  Test_support.with_temp_dir (fun root ->
      match find_ssh_keygen () with
      | None -> Alcotest.skip ()
      | Some ssh_keygen ->
          let path = Filename.concat root "key" in
          let* result =
            Command.generate ~fs_root:root ~path ~algorithm:Key.Ecdsa_p384 ~force:false ()
          in
          let fingerprint = expect_ok "interop key pair" result in
          let* output = run_ssh_keygen ssh_keygen [ "-l"; "-f"; path ^ ".pub" ] in
          Alcotest.check Alcotest.string "ssh-keygen fingerprint" fingerprint
            (fingerprint_token output);
          let* public_raw = run_ssh_keygen ssh_keygen [ "-y"; "-f"; path ] in
          let public_output = String.trim public_raw in
          let key =
            match Key.of_openssh_private (load_file path) with
            | Ok key -> key
            | Error _ -> Alcotest.fail "interop private key is not parseable"
          in
          let expected_public =
            let fields = String.split_on_char ' ' (Key.authorized_key key) in
            match fields with
            | key_type :: encoded :: _ -> key_type ^ " " ^ encoded
            | _ -> Alcotest.fail "authorized key has too few fields"
          in
          Alcotest.check Alcotest.string "ssh-keygen public key" expected_public
            public_output;
          Lwt.return_unit)

let suites =
  [
    ("paths", [ Alcotest_lwt.test_case_sync "default paths" `Quick default_paths ]);
    ( "non-force",
      [
        Alcotest_lwt.test_case "private collision" `Quick (fun _switch () ->
            nonforce_refuses_private ());
        Alcotest_lwt.test_case "public collision" `Quick (fun _switch () ->
            nonforce_refuses_public ());
      ] );
    ( "force",
      [
        Alcotest_lwt.test_case "replaces both files" `Quick (fun _switch () ->
            force_replaces_pair ());
        Alcotest_lwt.test_case "creates missing parent" `Quick (fun _switch () ->
            missing_parent_is_created ());
        Alcotest_lwt.test_case "rejects directory target" `Quick (fun _switch () ->
            force_rejects_directory_target ());
      ] );
    ( "interop",
      [
        Alcotest_lwt.test_case "ssh-keygen reads generated pair" `Quick (fun _switch () ->
            generated_key_interop ());
      ] );
  ]

let () =
  Mirage_crypto_rng_unix.use_default ();
  Test_support.run_lwt "keygen-cli" suites
