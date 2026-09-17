module K = Charamel_ssh_keygen

let err_show = function
  | `Malformed -> "`Malformed"
  | `Unsupported_type -> "`Unsupported_type"
  | `Encrypted_key -> "`Encrypted_key"
  | `Already_exists p -> "`Already_exists " ^ Filename.quote p
  | `Io m -> "`Io " ^ Filename.quote m

let expect_ok msg r =
  match r with
  | Ok v -> v
  | Error e -> Alcotest.failf "%s: unexpected error %s" msg (err_show e)

let expect_error msg variant r =
  match r with
  | Error e when e = variant -> ()
  | Error e ->
      Alcotest.failf "%s: expected %s, got %s" msg (err_show variant) (err_show e)
  | Ok _ -> Alcotest.failf "%s: expected %s, got Ok" msg (err_show variant)

let contains ~sub s =
  let sub_len = String.length sub in
  let n = String.length s in
  let rec go i =
    i + sub_len <= n && (String.equal (String.sub s i sub_len) sub || go (i + 1))
  in
  go 0

let u32be n = String.init 4 (fun i -> Char.chr ((n lsr ((3 - i) * 8)) land 0xff))
let ssh_string s = u32be (String.length s) ^ s

let read_u32be s off =
  (Char.code s.[off] lsl 24)
  lor (Char.code s.[off + 1] lsl 16)
  lor (Char.code s.[off + 2] lsl 8)
  lor Char.code s.[off + 3]

let string_field s off = String.sub s (off + 4) (read_u32be s off)
let b64_alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let b64_encode data =
  let n = String.length data in
  let buf = Buffer.create ((n + 2) / 3 * 4) in
  let emit v =
    Buffer.add_char buf b64_alphabet.[(v lsr 18) land 63];
    Buffer.add_char buf b64_alphabet.[(v lsr 12) land 63];
    Buffer.add_char buf b64_alphabet.[(v lsr 6) land 63];
    Buffer.add_char buf b64_alphabet.[v land 63]
  in
  let emit_tail v chars =
    Buffer.add_char buf b64_alphabet.[(v lsr 18) land 63];
    Buffer.add_char buf b64_alphabet.[(v lsr 12) land 63];
    if chars = 3 then Buffer.add_char buf b64_alphabet.[(v lsr 6) land 63];
    Buffer.add_char buf '='
  in
  let i = ref 0 in
  while !i + 3 <= n do
    emit
      ((Char.code data.[!i] lsl 16)
      lor (Char.code data.[!i + 1] lsl 8)
      lor Char.code data.[!i + 2]);
    i := !i + 3
  done;
  if n - !i = 1 then begin
    emit_tail (Char.code data.[!i] lsl 16) 2;
    Buffer.add_char buf '='
  end
  else if n - !i = 2 then
    emit_tail ((Char.code data.[!i] lsl 16) lor (Char.code data.[!i + 1] lsl 8)) 3;
  Buffer.contents buf

let pem_of_body body =
  let n = String.length body in
  let buf = Buffer.create (n + 80) in
  Buffer.add_string buf "-----BEGIN OPENSSH PRIVATE KEY-----\n";
  let i = ref 0 in
  while !i < n do
    let len = min 70 (n - !i) in
    Buffer.add_substring buf body !i len;
    Buffer.add_char buf '\n';
    i := !i + len
  done;
  Buffer.add_string buf "-----END OPENSSH PRIVATE KEY-----\n";
  Buffer.contents buf

let container_body ~cipher ~kdf ~kdfopts ~nkeys ~pubs ~priv =
  "openssh-key-v1\000" ^ ssh_string cipher ^ ssh_string kdf ^ ssh_string kdfopts
  ^ u32be nkeys
  ^ String.concat "" (List.map ssh_string pubs)
  ^ ssh_string priv

let container_pem ~cipher ~kdf ~kdfopts ~nkeys ~pubs ~priv =
  pem_of_body (b64_encode (container_body ~cipher ~kdf ~kdfopts ~nkeys ~pubs ~priv))

let pad_to_block b =
  let pad = (8 - (Buffer.length b mod 8)) mod 8 in
  for i = 1 to pad do
    Buffer.add_char b (Char.chr i)
  done

let ed25519_priv_section ~checkint ~pub ~priv_field ~comment =
  let b = Buffer.create 128 in
  Buffer.add_string b (u32be checkint);
  Buffer.add_string b (u32be checkint);
  Buffer.add_string b (ssh_string "ssh-ed25519");
  Buffer.add_string b (ssh_string pub);
  Buffer.add_string b (ssh_string priv_field);
  Buffer.add_string b (ssh_string comment);
  pad_to_block b;
  Buffer.contents b

let ecdsa_priv_section ~curve ~q ~scalar_field ~comment =
  let b = Buffer.create 160 in
  Buffer.add_string b (u32be 0x01020304);
  Buffer.add_string b (u32be 0x01020304);
  Buffer.add_string b (ssh_string ("ecdsa-sha2-" ^ curve));
  Buffer.add_string b (ssh_string curve);
  Buffer.add_string b (ssh_string q);
  Buffer.add_string b (ssh_string scalar_field);
  Buffer.add_string b (ssh_string comment);
  pad_to_block b;
  Buffer.contents b

let fixed_seed = String.init 32 (fun i -> Char.chr (0x40 + i))
let other_pub = String.init 32 (fun i -> Char.chr (0x80 + i))
let fixed_scalar = String.init 32 (fun i -> Char.chr (i + 1))

let ed25519_seed_of k =
  match K.ed25519_seed k with
  | Some s -> s
  | None -> Alcotest.fail "ed25519_seed returned None for an Ed25519 pair"

let ed25519_pub_octets blob = string_field blob (4 + String.length "ssh-ed25519")

let p256_q blob =
  let keytype = "ecdsa-sha2-nistp256" and curve = "nistp256" in
  string_field blob (4 + String.length keytype + 4 + String.length curve)

let keytype_of = function
  | K.Ed25519 -> "ssh-ed25519"
  | K.Ecdsa_p256 -> "ecdsa-sha2-nistp256"
  | K.Ecdsa_p384 -> "ecdsa-sha2-nistp384"
  | K.Ecdsa_p521 -> "ecdsa-sha2-nistp521"

let ( / ) = Eio.Path.( / )
let of_dir dir name = Eio.Path.of_dir dir / name

let repo_root () =
  let rec up dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if String.equal parent dir then
        failwith "test_keygen: dune-project not found above the working directory"
      else up parent
  in
  up (Sys.getcwd ())

let fixture_root env =
  Eio.Path.(Eio.Stdenv.fs env / (repo_root () ^ "/.outline/worktree/keygen-proof"))

let with_case env name f =
  let dir_path = fixture_root env / name in
  Eio.Path.rmtree ~missing_ok:true dir_path;
  Eio.Path.mkdir ~perm:0o700 dir_path;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree dir_path)
    (fun () -> Eio.Path.with_subtree dir_path (fun sub -> f (fst sub) dir_path))

let algorithms =
  [
    ("ed25519", K.Ed25519);
    ("ecdsa-p256", K.Ecdsa_p256);
    ("ecdsa-p384", K.Ecdsa_p384);
    ("ecdsa-p521", K.Ecdsa_p521);
  ]

let roundtrip_case algo () =
  let k = K.generate algo in
  let name = K.algorithm_name algo in
  let fp = K.fingerprint_sha256 k in
  Alcotest.check Alcotest.bool "fingerprint is SHA256: prefixed" true
    (String.starts_with ~prefix:"SHA256:" fp);
  Alcotest.check Alcotest.int "fingerprint is unpadded base64" 50 (String.length fp);
  Alcotest.check Alcotest.bool "authorized_key starts with the SSH key type" true
    (String.starts_with ~prefix:(keytype_of algo) (K.authorized_key k));
  let pem = K.to_openssh_private ~comment:"charamel-test" k in

  let k2 = expect_ok (name ^ " reloads its own container") (K.of_openssh_private pem) in
  Alcotest.check Alcotest.string "fingerprint survives the container round trip" fp
    (K.fingerprint_sha256 k2);
  Alcotest.check Alcotest.string "authorized_key survives the container round trip"
    (K.authorized_key k) (K.authorized_key k2);
  Alcotest.check Alcotest.string "public blob survives the container round trip"
    (K.public_blob k) (K.public_blob k2);
  let algo2 =
    match K.algorithm_of_name name with
    | Some a -> a
    | None -> Alcotest.failf "algorithm_of_name rejects %s" name
  in
  Alcotest.check Alcotest.string "algorithm name round trips" name
    (K.algorithm_name algo2)

let distinct_case () =
  let a = K.generate K.Ed25519 in
  let b = K.generate K.Ed25519 in
  Alcotest.check Alcotest.bool "fresh Ed25519 pairs differ" false
    (String.equal (K.fingerprint_sha256 a) (K.fingerprint_sha256 b));
  match (K.ed25519_seed a, K.ed25519_seed b) with
  | Some sa, Some sb ->
      Alcotest.check Alcotest.int "seed length" 32 (String.length sa);
      Alcotest.check Alcotest.bool "seeds differ" false (String.equal sa sb)
  | _ -> Alcotest.fail "ed25519_seed returned None for an Ed25519 pair"

let seed_roundtrip_case () =
  let k = K.generate K.Ed25519 in
  let seed = ed25519_seed_of k in
  let k2 =
    expect_ok "of_ed25519_seed accepts the extracted seed" (K.of_ed25519_seed seed)
  in
  Alcotest.check Alcotest.string "seed round trip reproduces the fingerprint"
    (K.fingerprint_sha256 k) (K.fingerprint_sha256 k2);
  Alcotest.check Alcotest.string "seed round trip reproduces the public key"
    (K.authorized_key k) (K.authorized_key k2)

let seed_deterministic_case () =
  let a = expect_ok "first of_ed25519_seed" (K.of_ed25519_seed fixed_seed) in
  let b = expect_ok "second of_ed25519_seed" (K.of_ed25519_seed fixed_seed) in
  Alcotest.check Alcotest.string "same seed gives the same fingerprint"
    (K.fingerprint_sha256 a) (K.fingerprint_sha256 b);
  Alcotest.check Alcotest.string "same seed gives the same authorized_key"
    (K.authorized_key a) (K.authorized_key b)

let seed_lengths =
  [
    ("empty seed", "");
    ("a 16-byte seed", String.sub fixed_seed 0 16);
    ("a 31-byte seed", String.sub fixed_seed 0 31);
    ("a 33-byte seed", fixed_seed ^ "\001");
    ("a 64-byte seed", fixed_seed ^ fixed_seed);
  ]

let seed_length_case (label, bytes) () =
  expect_error ("seed of " ^ label) `Malformed (K.of_ed25519_seed bytes)

let parse_case name expected input =
  Alcotest.test_case name `Quick (fun () ->
      expect_error name expected (K.of_openssh_private (input ())))

let ed25519_pem ?(comment = "") k =
  let pub = K.public_blob k in
  let puboct = ed25519_pub_octets pub in
  let priv =
    ed25519_priv_section ~checkint:0x0a0b0c0d ~pub:puboct
      ~priv_field:(ed25519_seed_of k ^ puboct)
      ~comment
  in
  container_pem ~cipher:"none" ~kdf:"none" ~kdfopts:"" ~nkeys:1 ~pubs:[ pub ] ~priv

let drop_end_line pem =
  let marker = "-----END OPENSSH PRIVATE KEY-----\n" in
  if contains ~sub:marker pem then
    String.sub pem 0 (String.length pem - String.length marker)
  else Alcotest.fail "the generated PEM lacks its END line"

let encrypted_input () =
  let k = K.generate K.Ed25519 in
  let pub = K.public_blob k in
  let puboct = ed25519_pub_octets pub in
  let priv =
    ed25519_priv_section ~checkint:0x11223344 ~pub:puboct
      ~priv_field:(ed25519_seed_of k ^ puboct)
      ~comment:""
  in
  container_pem ~cipher:"aes256-ctr" ~kdf:"bcrypt"
    ~kdfopts:"saltsaltsaltsalt\000\000\002\000" ~nkeys:1 ~pubs:[ pub ] ~priv

let nkeys_two_input () =
  let k = K.generate K.Ed25519 in
  let pub = K.public_blob k in
  let puboct = ed25519_pub_octets pub in
  let priv =
    ed25519_priv_section ~checkint:0x02020202 ~pub:puboct
      ~priv_field:(ed25519_seed_of k ^ puboct)
      ~comment:""
  in
  container_pem ~cipher:"none" ~kdf:"none" ~kdfopts:"" ~nkeys:2 ~pubs:[ pub; pub ] ~priv

let none_kdf_options_input () =
  let k = K.generate K.Ed25519 in
  let pub = K.public_blob k in
  let puboct = ed25519_pub_octets pub in
  let priv =
    ed25519_priv_section ~checkint:0x03030303 ~pub:puboct
      ~priv_field:(ed25519_seed_of k ^ puboct)
      ~comment:""
  in
  container_pem ~cipher:"none" ~kdf:"none" ~kdfopts:"\001\002\003\004" ~nkeys:1
    ~pubs:[ pub ] ~priv

let trailing_bytes_input () =
  let k = K.generate K.Ed25519 in
  let pub = K.public_blob k in
  let puboct = ed25519_pub_octets pub in
  let priv =
    ed25519_priv_section ~checkint:0x09090909 ~pub:puboct
      ~priv_field:(ed25519_seed_of k ^ puboct)
      ~comment:""
  in
  let body =
    container_body ~cipher:"none" ~kdf:"none" ~kdfopts:"" ~nkeys:1 ~pubs:[ pub ] ~priv
    ^ "Z"
  in
  pem_of_body (b64_encode body)

let mismatch_input () =
  let k = K.generate K.Ed25519 in
  let pub = K.public_blob k in
  let priv =
    ed25519_priv_section ~checkint:0x0d0e0f10 ~pub:(ed25519_pub_octets pub)
      ~priv_field:(ed25519_seed_of k ^ other_pub)
      ~comment:""
  in
  container_pem ~cipher:"none" ~kdf:"none" ~kdfopts:"" ~nkeys:1 ~pubs:[ pub ] ~priv

let padded_scalar_input () =
  let k = K.generate K.Ecdsa_p256 in
  let pub = K.public_blob k in
  let priv =
    ecdsa_priv_section ~curve:"nistp256" ~q:(p256_q pub)
      ~scalar_field:("\000" ^ fixed_scalar) ~comment:""
  in
  container_pem ~cipher:"none" ~kdf:"none" ~kdfopts:"" ~nkeys:1 ~pubs:[ pub ] ~priv

let handbuilt_loads_case () =
  let k = K.generate K.Ed25519 in
  let k2 =
    expect_ok "hand-built container loads"
      (K.of_openssh_private (ed25519_pem ~comment:"hand-built" k))
  in
  Alcotest.check Alcotest.string "hand-built container yields the same fingerprint"
    (K.fingerprint_sha256 k) (K.fingerprint_sha256 k2)

let openssh_cases =
  [
    parse_case "empty input" `Malformed (fun () -> "");
    parse_case "plain garbage" `Malformed (fun () -> "not a key at all");
    parse_case "armor over a wrong payload" `Malformed (fun () ->
        pem_of_body (b64_encode "hello"));
    parse_case "armor over undecodable base64" `Malformed (fun () ->
        "-----BEGIN OPENSSH PRIVATE KEY-----\n\
         !!!not-base64!!!\n\
         -----END OPENSSH PRIVATE KEY-----\n");
    parse_case "RSA PEM block" `Unsupported_type (fun () ->
        "-----BEGIN RSA PRIVATE KEY-----\n" ^ b64_encode "openssh-key-v1"
        ^ "\n-----END RSA PRIVATE KEY-----\n");
    parse_case "PEM without the END line" `Malformed (fun () ->
        drop_end_line (ed25519_pem (K.generate K.Ed25519)));
    parse_case "encrypted container" `Encrypted_key encrypted_input;
    parse_case "two public keys in the container" `Malformed nkeys_two_input;
    parse_case "kdf options under a none kdf" `Malformed none_kdf_options_input;
    parse_case "trailing bytes after the private section" `Malformed trailing_bytes_input;
    parse_case "seed and public halves disagree" `Malformed mismatch_input;
    parse_case "sign-padded ECDSA scalar" `Malformed padded_scalar_input;
  ]

let tag_typ =
  Alcotest.of_pp (fun fmt -> function
    | `Generated -> Format.pp_print_string fmt "`Generated"
    | `Loaded -> Format.pp_print_string fmt "`Loaded")

let write_ok msg dir path ?comment key =
  match K.write ~fs:dir ~path ?comment key with
  | Ok () -> ()
  | Error e -> Alcotest.failf "%s: %s" msg (err_show e)

let refuse_existing dir path key msg =
  match K.write ~fs:dir ~path key with
  | Error (`Already_exists _) -> ()
  | Error e -> Alcotest.failf "%s: expected `Already_exists, got %s" msg (err_show e)
  | Ok () -> Alcotest.failf "%s: expected `Already_exists, got Ok" msg

let perms_case env () =
  with_case env "perms" (fun dir _abs_dir ->
      let k = K.generate K.Ed25519 in
      write_ok "write the pair" dir "k" ~comment:"perm-test" k;
      Alcotest.check Alcotest.int "private key mode" 0o600
        (Eio.Path.stat ~follow:true (of_dir dir "k")).Eio.File.Stat.perm;
      Alcotest.check Alcotest.int "public key mode" 0o644
        (Eio.Path.stat ~follow:true (of_dir dir "k.pub")).Eio.File.Stat.perm;
      Alcotest.check Alcotest.bool "private key is PEM armored" true
        (String.starts_with ~prefix:"-----BEGIN OPENSSH PRIVATE KEY-----"
           (Eio.Path.load (of_dir dir "k")));
      Alcotest.check Alcotest.string "public key is the authorized_keys line"
        (K.authorized_key ~comment:"perm-test" k)
        (String.trim (Eio.Path.load (of_dir dir "k.pub"))))

let load_or_generate_case env () =
  with_case env "log" (fun dir _abs_dir ->
      let k, tag =
        expect_ok "load_or_generate on an absent path"
          (K.load_or_generate ~fs:dir ~path:"key" K.Ecdsa_p384)
      in
      Alcotest.check tag_typ "an absent path is generated" `Generated tag;
      Alcotest.check Alcotest.int "generated private mode" 0o600
        (Eio.Path.stat ~follow:true (of_dir dir "key")).Eio.File.Stat.perm;
      Alcotest.check Alcotest.int "generated public mode" 0o644
        (Eio.Path.stat ~follow:true (of_dir dir "key.pub")).Eio.File.Stat.perm;
      let k2, tag2 =
        expect_ok "load_or_generate on an existing path"
          (K.load_or_generate ~fs:dir ~path:"key" K.Ecdsa_p384)
      in
      Alcotest.check tag_typ "an existing path is loaded" `Loaded tag2;
      Alcotest.check Alcotest.string "loaded pair matches the generated pair"
        (K.fingerprint_sha256 k) (K.fingerprint_sha256 k2))

let collision_private_case env () =
  with_case env "collision-private" (fun dir _abs_dir ->
      Eio.Path.save ~create:(`Exclusive 0o600) (of_dir dir "k") "stale-private";
      refuse_existing dir "k" (K.generate K.Ed25519) "existing private key";
      Alcotest.check Alcotest.string "existing private key left untouched" "stale-private"
        (Eio.Path.load (of_dir dir "k"));
      Alcotest.check Alcotest.bool "no public key written" false
        (Eio.Path.is_file (of_dir dir "k.pub")))

let collision_public_case env () =
  with_case env "collision-public" (fun dir _abs_dir ->
      Eio.Path.save ~create:(`Exclusive 0o644) (of_dir dir "k.pub") "stale-public";
      refuse_existing dir "k" (K.generate K.Ed25519) "existing public key";
      Alcotest.check Alcotest.string "existing public key left untouched" "stale-public"
        (Eio.Path.load (of_dir dir "k.pub"));
      Alcotest.check Alcotest.bool "no private key written" false
        (Eio.Path.is_file (of_dir dir "k")))

let ssh_keygen_path env =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path -> (
      let dirs = String.split_on_char ':' path in
      let candidate d =
        String.length d > 0 && Eio.Path.is_file (Eio.Stdenv.fs env / (d ^ "/ssh-keygen"))
      in
      match List.find_opt candidate dirs with
      | Some d -> Some (d ^ "/ssh-keygen")
      | None -> None)

let external_run env executable args =
  Eio.Process.parse_out (Eio.Stdenv.process_mgr env) Eio.Buf_read.take_all ~executable
    args

let external_fingerprint_case env algo () =
  match ssh_keygen_path env with
  | None -> Alcotest.skip ()
  | Some ssh ->
      with_case env
        ("external-" ^ K.algorithm_name algo)
        (fun dir abs_dir ->
          let k = K.generate algo in
          write_ok "write the pair for ssh-keygen" dir "k" k;
          let out =
            external_run env ssh
              [ "ssh-keygen"; "-l"; "-f"; Eio.Path.native_exn (abs_dir / "k.pub") ]
          in
          let token =
            match
              List.find_opt
                (String.starts_with ~prefix:"SHA256:")
                (String.split_on_char ' ' out)
            with
            | Some t -> String.trim t
            | None -> Alcotest.failf "ssh-keygen printed no fingerprint: %S" out
          in
          Alcotest.check Alcotest.string "ssh-keygen fingerprint matches"
            (K.fingerprint_sha256 k) token)

let external_pubkey_case env () =
  match ssh_keygen_path env with
  | None -> Alcotest.skip ()
  | Some ssh ->
      with_case env "external-pubkey" (fun dir abs_dir ->
          let k = K.generate K.Ed25519 in
          write_ok "write the pair for ssh-keygen" dir "k" k;
          let out =
            String.trim
              (external_run env ssh
                 [ "ssh-keygen"; "-y"; "-f"; Eio.Path.native_exn (abs_dir / "k") ])
          in
          let expected =
            match String.split_on_char ' ' (K.authorized_key k) with
            | keytype :: b64 :: _ -> keytype ^ " " ^ b64
            | _ -> Alcotest.fail "authorized_key has fewer than two fields"
          in
          Alcotest.check Alcotest.string "ssh-keygen parses the written public key"
            expected out)

let suites env =
  let case name f = Alcotest.test_case name `Quick f in
  [
    ( "roundtrip",
      List.map (fun (name, algo) -> case name (roundtrip_case algo)) algorithms );
    ("generation", [ case "fresh pairs are distinct" distinct_case ]);
    ( "seed",
      [
        case "extracted seed regenerates the pair" seed_roundtrip_case;
        case "a fixed seed is deterministic" seed_deterministic_case;
      ]
      @ List.map
          (fun (label, bytes) ->
            case ("rejects " ^ label) (seed_length_case (label, bytes)))
          seed_lengths );
    ( "openssh",
      openssh_cases @ [ case "a hand-built container loads" handbuilt_loads_case ] );
    ( "filesystem",
      [
        case "write modes and content" (perms_case env);
        case "load_or_generate" (load_or_generate_case env);
        case "refuses an existing private key" (collision_private_case env);
        case "refuses an existing public key" (collision_public_case env);
      ] );
    ( "external",
      List.map
        (fun (name, algo) ->
          case ("ssh-keygen fingerprint for " ^ name) (external_fingerprint_case env algo))
        [
          ("ed25519", K.Ed25519);
          ("ecdsa-p256", K.Ecdsa_p256);
          ("ecdsa-p384", K.Ecdsa_p384);
          ("ecdsa-p521", K.Ecdsa_p521);
        ]
      @ [ case "ssh-keygen parses the public key" (external_pubkey_case env) ] );
  ]

let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let root = fixture_root env in
  Eio.Path.rmtree ~missing_ok:true root;
  Eio.Path.mkdirs ~perm:0o700 root;
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
    (fun () -> Alcotest.run ~and_exit:false "charamel-ssh.keygen" (suites env))
