module Key = Charm_ssh_keygen
module Command = Keygen_core.Keygen

let ( / ) = Eio.Path.( / )

let expect_ok label = function
  | Ok value -> value
  | Error error -> Alcotest.failf "%s: %a" label Command.pp_error error

let expect_error label = function
  | Error error -> error
  | Ok _ -> Alcotest.failf "%s: expected an error" label

let fresh_dir env name f =
  let absolute =
    Filename.concat (Filename.get_temp_dir_name ()) ("charm-keygen-cli-" ^ name)
  in
  let root = Eio.Path.(env#fs / absolute) in
  Eio.Path.rmtree ~missing_ok:true root;
  Eio.Path.mkdir ~perm:0o700 root;
  Fun.protect
    (fun () -> f root)
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)

let native path = Eio.Path.native_exn path

let check_error_path label expected = function
  | `Already_exists path -> Alcotest.check Alcotest.string label expected path
  | error ->
      Alcotest.failf "%s: expected an existing-path error, got %a" label Command.pp_error
        error

let nonforce_refuses_private env () =
  fresh_dir env "nonforce-private" (fun root ->
      let private_path = root / "key" in
      let path = native private_path in
      Eio.Path.save ~create:(`Exclusive 0o600) private_path "unchanged";
      let error =
        expect_error "non-force existing private key"
          (Command.generate ~fs:(fst env#fs) ~path ~algorithm:Key.Ed25519 ~force:false ())
      in
      check_error_path "private path" path error;
      Alcotest.check Alcotest.string "private key remains unchanged" "unchanged"
        (Eio.Path.load private_path);
      Alcotest.check Alcotest.bool "public key is not written" false
        (Eio.Path.is_file (root / "key.pub")))

let nonforce_refuses_public env () =
  fresh_dir env "nonforce-public" (fun root ->
      let public_path = root / "key.pub" in
      let path = native (root / "key") in
      Eio.Path.save ~create:(`Exclusive 0o644) public_path "unchanged";
      let error =
        expect_error "non-force existing public key"
          (Command.generate ~fs:(fst env#fs) ~path ~algorithm:Key.Ed25519 ~force:false ())
      in
      check_error_path "public path" (path ^ ".pub") error;
      Alcotest.check Alcotest.string "public key remains unchanged" "unchanged"
        (Eio.Path.load public_path);
      Alcotest.check Alcotest.bool "private key is not written" false
        (Eio.Path.is_file (root / "key")))

let force_replaces_pair env () =
  fresh_dir env "force" (fun root ->
      let path = native (root / "key") in
      let first_fingerprint =
        expect_ok "initial key pair"
          (Command.generate ~fs:(fst env#fs) ~path ~algorithm:Key.Ed25519 ~comment:"first"
             ~force:false ())
      in
      let second_fingerprint =
        expect_ok "forced key pair"
          (Command.generate ~fs:(fst env#fs) ~path ~algorithm:Key.Ed25519
             ~comment:"second" ~force:true ())
      in
      Alcotest.(check bool)
        "force generates a fresh key" true
        (not (String.equal first_fingerprint second_fingerprint));
      let private_path = root / "key" in
      let public_path = root / "key.pub" in
      let private_body = Eio.Path.load private_path in
      let key =
        match Key.of_openssh_private private_body with
        | Ok key -> key
        | Error _ -> Alcotest.fail "forced private key is not parseable"
      in
      Alcotest.check Alcotest.string "forced fingerprint matches private key"
        second_fingerprint (Key.fingerprint_sha256 key);
      Alcotest.check Alcotest.string "forced public key matches private key"
        (Key.authorized_key ~comment:"second" key)
        (Eio.Path.load public_path);
      Alcotest.check Alcotest.int "private mode" 0o600
        (Eio.Path.stat ~follow:false private_path).perm;
      Alcotest.check Alcotest.int "public mode" 0o644
        (Eio.Path.stat ~follow:false public_path).perm)

let missing_parent_is_created env () =
  fresh_dir env "missing-parent" (fun root ->
      let private_path = root / "nested" / "deeper" / "key" in
      let path = native private_path in
      let fingerprint =
        expect_ok "key pair with missing parent"
          (Command.generate ~fs:(fst env#fs) ~path ~algorithm:Key.Ecdsa_p256 ~force:false
             ())
      in
      let parent = root / "nested" / "deeper" in
      Alcotest.check Alcotest.bool "parent directory exists" true
        (Eio.Path.is_directory parent);
      Alcotest.check Alcotest.int "first created directory mode" 0o700
        (Eio.Path.stat ~follow:false (root / "nested")).perm;
      Alcotest.check Alcotest.int "last created directory mode" 0o700
        (Eio.Path.stat ~follow:false parent).perm;
      let key =
        match Key.of_openssh_private (Eio.Path.load private_path) with
        | Ok key -> key
        | Error _ -> Alcotest.fail "generated private key is not parseable"
      in
      Alcotest.check Alcotest.string "generated fingerprint" fingerprint
        (Key.fingerprint_sha256 key);
      Alcotest.check Alcotest.int "private mode" 0o600
        (Eio.Path.stat ~follow:false private_path).perm;
      Alcotest.check Alcotest.int "public mode" 0o644
        (Eio.Path.stat ~follow:false (root / "nested" / "deeper" / "key.pub")).perm)

let default_paths () =
  match Command.home_dir () with
  | Error `No_home -> Alcotest.skip ()
  | Error error -> Alcotest.failf "home lookup: %a" Command.pp_error error
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

let find_ssh_keygen env =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
      let candidate directory =
        let name = Filename.concat directory "ssh-keygen" in
        let name =
          if Filename.is_relative name then Filename.concat (Sys.getcwd ()) name else name
        in
        if Eio.Path.is_file Eio.Path.(env#fs / name) then Some name else None
      in
      List.find_map candidate (String.split_on_char ':' path)

let run_ssh_keygen env executable arguments =
  Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all ~executable
    ("ssh-keygen" :: arguments)

let fingerprint_token output =
  match
    List.find_opt
      (String.starts_with ~prefix:"SHA256:")
      (String.split_on_char ' ' (String.trim output))
  with
  | Some fingerprint -> fingerprint
  | None -> Alcotest.failf "ssh-keygen printed no fingerprint: %S" output

let generated_key_interop env () =
  match find_ssh_keygen env with
  | None -> Alcotest.skip ()
  | Some ssh_keygen ->
      fresh_dir env "interop" (fun root ->
          let private_path = root / "key" in
          let path = native private_path in
          let fingerprint =
            expect_ok "interop key pair"
              (Command.generate ~fs:(fst env#fs) ~path ~algorithm:Key.Ecdsa_p384
                 ~force:false ())
          in
          let output =
            run_ssh_keygen env ssh_keygen [ "-l"; "-f"; native (root / "key.pub") ]
          in
          Alcotest.check Alcotest.string "ssh-keygen fingerprint" fingerprint
            (fingerprint_token output);
          let public_output =
            String.trim
              (run_ssh_keygen env ssh_keygen [ "-y"; "-f"; native private_path ])
          in
          let key =
            match Key.of_openssh_private (Eio.Path.load private_path) with
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
            public_output)

let suites env =
  [
    ("paths", [ Alcotest.test_case "default paths" `Quick default_paths ]);
    ( "non-force",
      [
        Alcotest.test_case "private collision" `Quick (nonforce_refuses_private env);
        Alcotest.test_case "public collision" `Quick (nonforce_refuses_public env);
      ] );
    ( "force",
      [
        Alcotest.test_case "replaces both files" `Quick (force_replaces_pair env);
        Alcotest.test_case "creates missing parent" `Quick (missing_parent_is_created env);
      ] );
    ( "interop",
      [
        Alcotest.test_case "ssh-keygen reads generated pair" `Quick
          (generated_key_interop env);
      ] );
  ]

let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env -> Alcotest.run "keygen-cli" (suites env)
