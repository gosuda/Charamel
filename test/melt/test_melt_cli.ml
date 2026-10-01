open Lwt.Syntax
module Key = Charamel_ssh_keygen
module Mnemonic = Melt_core.Mnemonic

let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/melt/main.exe"

let path_env = "/usr/bin:/bin"

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

let fixed_seed () = String.init 32 (fun index -> Char.chr (0x40 + index))

let key_of_seed seed =
  match Key.of_ed25519_seed seed with
  | Ok key -> key
  | Error `Malformed -> Alcotest.fail "fixed seed was rejected"
  | Error (`Unsupported_type | `Encrypted_key | `Already_exists _ | `Io _) ->
      Alcotest.fail "fixed seed returned the wrong keygen error"

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

let write_key root path key =
  let* result = Key.write ~fs_root:root ~path key in
  match result with
  | Ok () -> Lwt.return_unit
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

let check_backup_restore root =
  let seed = fixed_seed () in
  let original_path = Filename.concat root "id_ed25519" in
  let restored_path = Filename.concat root "restored" in
  let collision_path = Filename.concat root "collision" in
  let original = key_of_seed seed in
  let* () = write_key root original_path original in
  let* status, phrase, error =
    Test_support.run_cli ~exe:(executable ())
      ~env:(minimal_environment ~root ~home:root)
      ~timeout:5. [ "backup"; original_path ]
  in
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
  let* status, output, error =
    Test_support.run_cli ~exe:(executable ())
      ~env:(minimal_environment ~root ~home:root)
      ~timeout:5.
      [ "restore"; "--words"; phrase; "--output"; restored_path ]
  in
  Alcotest.(check int) "restore status" 0 status;
  Alcotest.(check string) "restore stdout" "" output;
  Alcotest.(check string) "restore stderr" "" error;
  let restored_pem = load_file restored_path in
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
  save_exclusive collision_path 0o600 "sentinel";
  let* status, _, error =
    Test_support.run_cli ~exe:(executable ())
      ~env:(minimal_environment ~root ~home:root)
      ~timeout:5.
      [ "restore"; "--words"; phrase; "--output"; collision_path ]
  in
  Alcotest.(check int) "collision status" 1 status;
  Alcotest.(check bool)
    "collision diagnostic" true
    (Test_support.contains ~needle:"already exists" ~haystack:error);
  Alcotest.(check string) "collision file unchanged" "sentinel" (load_file collision_path);
  let restored_public = load_file (restored_path ^ ".pub") in
  Alcotest.(check string)
    "restored public file" (Key.authorized_key restored) (String.trim restored_public);
  Lwt.return_unit

let backup_restore_roundtrip () = Test_support.with_temp_dir check_backup_restore

let defaults () =
  Test_support.with_temp_dir (fun root ->
      let source_home = Filename.concat root "source-home" in
      let restore_home = Filename.concat root "restore-home" in
      let source_ssh = Filename.concat source_home ".ssh" in
      let restore_ssh = Filename.concat restore_home ".ssh" in
      Unix.mkdir source_home 0o700;
      Unix.mkdir restore_home 0o700;
      Unix.mkdir source_ssh 0o700;
      Unix.mkdir restore_ssh 0o700;
      let source_path = Filename.concat source_ssh "id_ed25519" in
      let restore_path = Filename.concat restore_ssh "id_ed25519" in
      let original = key_of_seed (fixed_seed ()) in
      let* () = write_key root source_path original in
      let* status, phrase, error =
        Test_support.run_cli ~exe:(executable ())
          ~env:(minimal_environment ~root ~home:source_home)
          ~timeout:5. []
      in
      Alcotest.(check int) "default backup status" 0 status;
      Alcotest.(check string) "default backup stderr" "" error;
      let* status, output, error =
        Test_support.run_cli ~exe:(executable ())
          ~env:(minimal_environment ~root ~home:restore_home)
          ~timeout:5.
          [ "restore"; "--words"; String.trim phrase ]
      in
      Alcotest.(check int) "default restore status" 0 status;
      Alcotest.(check string) "default restore stdout" "" output;
      Alcotest.(check string) "default restore stderr" "" error;
      let restored =
        match Key.of_openssh_private (load_file restore_path) with
        | Ok key -> key
        | Error _ -> Alcotest.fail "default restore wrote an invalid private key"
      in
      Alcotest.(check string)
        "default restored fingerprint"
        (Key.fingerprint_sha256 original)
        (Key.fingerprint_sha256 restored);
      Lwt.return_unit)

let invalid_word () =
  Test_support.with_temp_dir (fun root ->
      let unknown_words =
        String.concat " "
          (List.init 24 (fun index -> if index = 0 then "notaword" else "abandon"))
      in
      let* status, output, error =
        Test_support.run_cli ~exe:(executable ())
          ~env:(minimal_environment ~root ~home:root)
          ~timeout:5.
          [ "restore"; "--words"; unknown_words; "--output"; Filename.concat root "key" ]
      in
      Alcotest.(check int) "invalid word status" 1 status;
      Alcotest.(check string) "invalid word stdout" "" output;
      Alcotest.(check bool)
        "invalid word diagnostic" true
        (Test_support.contains ~needle:"melt: \"notaword\" is not a BIP-39 word"
           ~haystack:error);
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
      let* status, output, error =
        Test_support.run_cli ~exe:(executable ())
          ~env:(minimal_environment ~root ~home:root)
          ~timeout:5.
          [
            "restore"; "--words"; checksum_words; "--output"; Filename.concat root "key2";
          ]
      in
      Alcotest.(check int) "checksum status" 1 status;
      Alcotest.(check string) "checksum stdout" "" output;
      Alcotest.(check bool)
        "checksum diagnostic" true
        (Test_support.contains ~needle:"melt: mnemonic checksum does not match"
           ~haystack:error);
      Lwt.return_unit)

let invalid_key () =
  Test_support.with_temp_dir (fun root ->
      let path = Filename.concat root "not-a-key" in
      save_exclusive path 0o600 "not an OpenSSH key";
      let* status, output, error =
        Test_support.run_cli ~exe:(executable ())
          ~env:(minimal_environment ~root ~home:root)
          ~timeout:5. [ "backup"; path ]
      in
      Alcotest.(check int) "invalid key status" 1 status;
      Alcotest.(check string) "invalid key stdout" "" output;
      Alcotest.(check bool)
        "invalid key diagnostic" true
        (Test_support.contains ~needle:"melt: could not parse key" ~haystack:error);
      Lwt.return_unit)

let non_ed25519_key () =
  Test_support.with_temp_dir (fun root ->
      let path = Filename.concat root "ecdsa" in
      let key = Key.generate Key.Ecdsa_p256 in
      let* () = write_key root path key in
      let* status, output, error =
        Test_support.run_cli ~exe:(executable ())
          ~env:(minimal_environment ~root ~home:root)
          ~timeout:5. [ "backup"; path ]
      in
      Alcotest.(check int) "non-Ed25519 status" 1 status;
      Alcotest.(check string) "non-Ed25519 stdout" "" output;
      Alcotest.(check bool)
        "non-Ed25519 diagnostic" true
        (Test_support.contains ~needle:"melt only supports ed25519 keys" ~haystack:error);
      Lwt.return_unit)

let cases =
  [
    Alcotest_lwt.test_case "backup and restore preserve key identity" `Quick
      (fun _switch () -> backup_restore_roundtrip ());
    Alcotest_lwt.test_case "default paths use ~/.ssh/id_ed25519" `Quick (fun _switch () ->
        defaults ());
    Alcotest_lwt.test_case "restore rejects an unknown word" `Quick (fun _switch () ->
        invalid_word ());
    Alcotest_lwt.test_case "backup rejects malformed keys" `Quick (fun _switch () ->
        invalid_key ());
    Alcotest_lwt.test_case "backup rejects non-Ed25519 keys" `Quick (fun _switch () ->
        non_ed25519_key ());
  ]

let () = Mirage_crypto_rng_unix.use_default ()
