module Key = Charm_ssh_keygen
module Mnemonic = Melt_core.Mnemonic

let source_root () =
  Option.value (Sys.getenv_opt "DUNE_SOURCEROOT") ~default:(Sys.getcwd ())
  |> Unix.realpath

let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/melt/main.exe"

let path_env = "/usr/bin:/bin"

let fixture_parent env =
  let root =
    Filename.concat
      (Filename.concat (source_root ()) ".outline")
      (Filename.concat "worktree" "sandboxfixtures")
  in
  Eio.Path.(env#fs / root)

let minimal_environment ~root ~home =
  let path name = Filename.concat root name in
  [|
    "PATH=" ^ path_env;
    "LANG=C";
    "TERM=xterm-256color";
    "HOME=" ^ home;
    "XDG_CONFIG_HOME=" ^ path "xdg-config";
    "XDG_DATA_HOME=" ^ path "xdg-data";
    "XDG_STATE_HOME=" ^ path "xdg-state";
    "XDG_CACHE_HOME=" ^ path "xdg-cache";
    "TMPDIR=" ^ path "tmp";
  |]

let run_cli env ~root ~home args =
  let error_buffer = Buffer.create 128 in
  let status = ref None in
  let output =
    Eio.Time.with_timeout_exn env#clock 5. (fun () ->
        Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
          ~env:(minimal_environment ~root ~home)
          ~stdin:(Eio.Flow.string_source "")
          ~stderr:(Eio.Flow.buffer_sink error_buffer)
          ~is_success:(fun code ->
            status := Some code;
            true)
          (executable () :: args))
  in
  (Option.value !status ~default:127, output, Buffer.contents error_buffer)

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec loop index =
    if index + needle_length > haystack_length then false
    else if String.sub haystack index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let with_root env f =
  let parent = fixture_parent env in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent;
  let path =
    Filename.temp_file ~temp_dir:(Eio.Path.native_exn parent) "charm-melt-cli-" ".dir"
  in
  Sys.remove path;
  let root = Eio.Path.(env#fs / path) in
  Eio.Path.mkdir ~perm:0o700 root;
  List.iter
    (fun name -> Eio.Path.mkdir ~perm:0o700 Eio.Path.(root / name))
    [ "home"; "xdg-config"; "xdg-data"; "xdg-state"; "xdg-cache"; "tmp" ];
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
    (fun () -> f path)

let fixed_seed () = String.init 32 (fun index -> Char.chr (0x40 + index))

let key_of_seed seed =
  match Key.of_ed25519_seed seed with
  | Ok key -> key
  | Error `Malformed -> Alcotest.fail "fixed seed was rejected"
  | Error (`Unsupported_type | `Encrypted_key | `Already_exists _ | `Io _) ->
      Alcotest.fail "fixed seed returned the wrong keygen error"

let write_key env path key =
  match Key.write ~fs:(fst env#fs) ~path key with
  | Ok () -> ()
  | Error `Malformed -> Alcotest.fail "key write returned malformed"
  | Error `Unsupported_type -> Alcotest.fail "key write returned unsupported type"
  | Error `Encrypted_key -> Alcotest.fail "key write returned encrypted key"
  | Error (`Already_exists existing) ->
      Alcotest.failf "unexpected existing path %s" existing
  | Error (`Io message) -> Alcotest.failf "key write failed: %s" message

let seed_of_key key =
  match Key.ed25519_seed key with
  | Some seed -> seed
  | None -> Alcotest.fail "expected an Ed25519 key"

let check_backup_restore env root =
  let seed = fixed_seed () in
  let original_path = Filename.concat root "id_ed25519" in
  let restored_path = Filename.concat root "restored" in
  let collision_path = Filename.concat root "collision" in
  let original = key_of_seed seed in
  write_key env original_path original;
  let status, phrase, error = run_cli env ~root ~home:root [ "backup"; original_path ] in
  Alcotest.(check int) "backup status" 0 status;
  Alcotest.(check string) "backup stderr" "" error;
  let phrase = String.trim phrase in
  let words = String.split_on_char ' ' phrase in
  Alcotest.(check int) "24 words" 24 (List.length words);
  let decoded =
    match Mnemonic.decode words with
    | Ok seed -> seed
    | Error error ->
        Alcotest.failf "backup phrase did not decode: %a" Mnemonic.pp_error error
  in
  Alcotest.(check string) "backup seed" seed decoded;
  let status, output, error =
    run_cli env ~root ~home:root
      [ "restore"; "--words"; phrase; "--output"; restored_path ]
  in
  Alcotest.(check int) "restore status" 0 status;
  Alcotest.(check string) "restore stdout" "" output;
  Alcotest.(check string) "restore stderr" "" error;
  let restored_pem = Eio.Path.load Eio.Path.(env#fs / restored_path) in
  let restored =
    match Key.of_openssh_private restored_pem with
    | Ok key -> key
    | Error `Malformed -> Alcotest.fail "restored private key is malformed"
    | Error `Unsupported_type -> Alcotest.fail "restored key has an unsupported type"
    | Error `Encrypted_key -> Alcotest.fail "restored key is encrypted"
    | Error (`Already_exists path) -> Alcotest.failf "unexpected existing path %s" path
    | Error (`Io message) -> Alcotest.failf "restored key could not be parsed: %s" message
  in
  Alcotest.(check string) "restored seed" seed (seed_of_key restored);
  Alcotest.(check string)
    "restored public key" (Key.public_blob original) (Key.public_blob restored);
  Alcotest.(check string)
    "restored fingerprint"
    (Key.fingerprint_sha256 original)
    (Key.fingerprint_sha256 restored);
  Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(env#fs / collision_path) "sentinel";
  let status, _, error =
    run_cli env ~root ~home:root
      [ "restore"; "--words"; phrase; "--output"; collision_path ]
  in
  Alcotest.(check int) "collision status" 1 status;
  Alcotest.(check bool)
    "collision diagnostic" true
    (contains ~needle:"already exists" error);
  Alcotest.(check string)
    "collision file unchanged" "sentinel"
    (Eio.Path.load Eio.Path.(env#fs / collision_path));
  let restored_public = Eio.Path.load Eio.Path.(env#fs / (restored_path ^ ".pub")) in
  Alcotest.(check string)
    "restored public file" (Key.authorized_key restored) (String.trim restored_public)

let backup_restore_roundtrip () =
  Eio_main.run (fun env -> with_root env (check_backup_restore env))

let defaults () =
  Eio_main.run (fun env ->
      with_root env (fun root ->
          let source_home = Filename.concat root "source-home" in
          let restore_home = Filename.concat root "restore-home" in
          let source_ssh = Filename.concat source_home ".ssh" in
          let restore_ssh = Filename.concat restore_home ".ssh" in
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / source_home);
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / restore_home);
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / source_ssh);
          Eio.Path.mkdir ~perm:0o700 Eio.Path.(env#fs / restore_ssh);
          let source_path = Filename.concat source_ssh "id_ed25519" in
          let restore_path = Filename.concat restore_ssh "id_ed25519" in
          let original = key_of_seed (fixed_seed ()) in
          write_key env source_path original;
          let status, phrase, error = run_cli env ~root ~home:source_home [] in
          Alcotest.(check int) "default backup status" 0 status;
          Alcotest.(check string) "default backup stderr" "" error;
          let status, output, error =
            run_cli env ~root ~home:restore_home
              [ "restore"; "--words"; String.trim phrase ]
          in
          Alcotest.(check int) "default restore status" 0 status;
          Alcotest.(check string) "default restore stdout" "" output;
          Alcotest.(check string) "default restore stderr" "" error;
          let restored =
            match
              Key.of_openssh_private (Eio.Path.load Eio.Path.(env#fs / restore_path))
            with
            | Ok key -> key
            | Error _ -> Alcotest.fail "default restore wrote an invalid private key"
          in
          Alcotest.(check string)
            "default restored fingerprint"
            (Key.fingerprint_sha256 original)
            (Key.fingerprint_sha256 restored)))

let invalid_word () =
  Eio_main.run (fun env ->
      with_root env (fun root ->
          let unknown_words =
            String.concat " "
              (List.init 24 (fun index -> if index = 0 then "notaword" else "abandon"))
          in
          let status, output, error =
            run_cli env ~root ~home:root
              [
                "restore";
                "--words";
                unknown_words;
                "--output";
                Filename.concat root "key";
              ]
          in
          Alcotest.(check int) "invalid word status" 1 status;
          Alcotest.(check string) "invalid word stdout" "" output;
          Alcotest.(check bool)
            "invalid word diagnostic" true
            (contains ~needle:"melt: \"notaword\" is not a BIP-39 word" error);
          let source_words =
            match Mnemonic.encode (String.make 32 '\000') with
            | Ok words -> words
            | Error error ->
                Alcotest.failf "test seed did not encode: %a" Mnemonic.pp_error error
          in
          let rec corrupted_words index =
            if index = Array.length Melt_core.Wordlist.words then
              Alcotest.fail "could not construct a bad checksum phrase"
            else
              let candidate = Melt_core.Wordlist.words.(index) :: List.tl source_words in
              match Mnemonic.decode candidate with
              | Error `Bad_checksum -> candidate
              | Error _ | Ok _ -> corrupted_words (index + 1)
          in
          let checksum_words = String.concat " " (corrupted_words 0) in
          let status, output, error =
            run_cli env ~root ~home:root
              [
                "restore";
                "--words";
                checksum_words;
                "--output";
                Filename.concat root "key2";
              ]
          in
          Alcotest.(check int) "checksum status" 1 status;
          Alcotest.(check string) "checksum stdout" "" output;
          Alcotest.(check bool)
            "checksum diagnostic" true
            (contains ~needle:"melt: mnemonic checksum does not match" error)))

let invalid_key () =
  Eio_main.run (fun env ->
      with_root env (fun root ->
          let path = Filename.concat root "not-a-key" in
          Eio.Path.save ~create:(`Exclusive 0o600)
            Eio.Path.(env#fs / path)
            "not an OpenSSH key";
          let status, output, error = run_cli env ~root ~home:root [ "backup"; path ] in
          Alcotest.(check int) "invalid key status" 1 status;
          Alcotest.(check string) "invalid key stdout" "" output;
          Alcotest.(check bool)
            "invalid key diagnostic" true
            (contains ~needle:"melt: could not parse key" error)))

let non_ed25519_key () =
  Eio_main.run (fun env ->
      with_root env (fun root ->
          let path = Filename.concat root "ecdsa" in
          let key = Key.generate Key.Ecdsa_p256 in
          write_key env path key;
          let status, output, error = run_cli env ~root ~home:root [ "backup"; path ] in
          Alcotest.(check int) "non-Ed25519 status" 1 status;
          Alcotest.(check string) "non-Ed25519 stdout" "" output;
          Alcotest.(check bool)
            "non-Ed25519 diagnostic" true
            (contains ~needle:"melt only supports ed25519 keys" error)))

let cases =
  [
    Alcotest.test_case "backup and restore preserve key identity" `Quick
      backup_restore_roundtrip;
    Alcotest.test_case "default paths use ~/.ssh/id_ed25519" `Quick defaults;
    Alcotest.test_case "restore rejects an unknown word" `Quick invalid_word;
    Alcotest.test_case "backup rejects malformed keys" `Quick invalid_key;
    Alcotest.test_case "backup rejects non-Ed25519 keys" `Quick non_ed25519_key;
  ]

let () = Mirage_crypto_rng_unix.use_default ()
