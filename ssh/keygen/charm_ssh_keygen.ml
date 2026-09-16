let ( let* ) = Result.bind

type algorithm = Ed25519 | Ecdsa_p256 | Ecdsa_p384 | Ecdsa_p521

type error =
  [ `Malformed
  | `Unsupported_type
  | `Encrypted_key
  | `Already_exists of string
  | `Io of string ]

(* [priv] is the 32-byte Ed25519 seed or the curve-width ECDSA scalar.
   [pub_octets] is the Ed25519 public value or the SEC1 point. *)
type t = { algorithm : algorithm; priv : string; pub_octets : string }

let keytype_name = function
  | Ed25519 -> "ssh-ed25519"
  | Ecdsa_p256 -> "ecdsa-sha2-nistp256"
  | Ecdsa_p384 -> "ecdsa-sha2-nistp384"
  | Ecdsa_p521 -> "ecdsa-sha2-nistp521"

let algorithm_name = function
  | Ed25519 -> "ed25519"
  | Ecdsa_p256 -> "ecdsa-p256"
  | Ecdsa_p384 -> "ecdsa-p384"
  | Ecdsa_p521 -> "ecdsa-p521"

let algorithm_of_name = function
  | "ed25519" -> Some Ed25519
  | "ecdsa-p256" -> Some Ecdsa_p256
  | "ecdsa-p384" -> Some Ecdsa_p384
  | "ecdsa-p521" -> Some Ecdsa_p521
  | _ -> None

let ecdsa_curve_name = function
  | Ecdsa_p256 -> "nistp256"
  | Ecdsa_p384 -> "nistp384"
  | Ecdsa_p521 -> "nistp521"
  | Ed25519 -> invalid_arg "ecdsa_curve_name"

let curve_of_name = function
  | "nistp256" -> Some Ecdsa_p256
  | "nistp384" -> Some Ecdsa_p384
  | "nistp521" -> Some Ecdsa_p521
  | _ -> None

let scalar_length = function
  | Ed25519 -> 32
  | Ecdsa_p256 -> Mirage_crypto_ec.P256.Dsa.byte_length
  | Ecdsa_p384 -> Mirage_crypto_ec.P384.Dsa.byte_length
  | Ecdsa_p521 -> Mirage_crypto_ec.P521.Dsa.byte_length

let public_blob t =
  match t.algorithm with
  | Ed25519 -> Openssh_key.wire_pub_ed25519 t.pub_octets
  | Ecdsa_p256 | Ecdsa_p384 | Ecdsa_p521 ->
      Openssh_key.wire_pub_ecdsa (ecdsa_curve_name t.algorithm) t.pub_octets

let fingerprint_sha256 t =
  let digest = Digestif.SHA256.digest_string (public_blob t) in
  "SHA256:" ^ Base64.encode_string ~pad:false (Digestif.SHA256.to_raw_string digest)

let pp fmt t = Fmt.pf fmt "%s %s" (algorithm_name t.algorithm) (fingerprint_sha256 t)

let generate = function
  | Ed25519 ->
      let priv, pub = Mirage_crypto_ec.Ed25519.generate () in
      {
        algorithm = Ed25519;
        priv = Mirage_crypto_ec.Ed25519.priv_to_octets priv;
        pub_octets = Mirage_crypto_ec.Ed25519.pub_to_octets pub;
      }
  | Ecdsa_p256 ->
      let priv, pub = Mirage_crypto_ec.P256.Dsa.generate () in
      {
        algorithm = Ecdsa_p256;
        priv = Mirage_crypto_ec.P256.Dsa.priv_to_octets priv;
        pub_octets = Mirage_crypto_ec.P256.Dsa.pub_to_octets pub;
      }
  | Ecdsa_p384 ->
      let priv, pub = Mirage_crypto_ec.P384.Dsa.generate () in
      {
        algorithm = Ecdsa_p384;
        priv = Mirage_crypto_ec.P384.Dsa.priv_to_octets priv;
        pub_octets = Mirage_crypto_ec.P384.Dsa.pub_to_octets pub;
      }
  | Ecdsa_p521 ->
      let priv, pub = Mirage_crypto_ec.P521.Dsa.generate () in
      {
        algorithm = Ecdsa_p521;
        priv = Mirage_crypto_ec.P521.Dsa.priv_to_octets priv;
        pub_octets = Mirage_crypto_ec.P521.Dsa.pub_to_octets pub;
      }

let of_ed25519_seed seed =
  if String.length seed <> 32 then Error `Malformed
  else
    match Mirage_crypto_ec.Ed25519.priv_of_octets seed with
    | Error _ -> Error `Malformed
    | Ok priv ->
        let pub = Mirage_crypto_ec.Ed25519.pub_of_priv priv in
        Ok
          {
            algorithm = Ed25519;
            priv = seed;
            pub_octets = Mirage_crypto_ec.Ed25519.pub_to_octets pub;
          }

let ed25519_seed t =
  match t.algorithm with
  | Ed25519 -> Some t.priv
  | Ecdsa_p256 | Ecdsa_p384 | Ecdsa_p521 -> None

let priv_fields t =
  let buf = Buffer.create 128 in
  Openssh_key.put_string buf (keytype_name t.algorithm);
  (match t.algorithm with
  | Ed25519 ->
      Openssh_key.put_string buf t.pub_octets;
      Openssh_key.put_string buf (t.priv ^ t.pub_octets)
  | Ecdsa_p256 | Ecdsa_p384 | Ecdsa_p521 ->
      Openssh_key.put_string buf (ecdsa_curve_name t.algorithm);
      Openssh_key.put_string buf t.pub_octets;
      Openssh_key.put_uint_mpint buf t.priv);
  Buffer.contents buf

let ecdsa_pub_of_scalar algorithm scalar =
  let open Mirage_crypto_ec in
  match algorithm with
  | Ed25519 -> Error `Malformed
  | Ecdsa_p256 -> (
      match P256.Dsa.priv_of_octets scalar with
      | Error _ -> Error `Malformed
      | Ok priv -> Ok (P256.Dsa.pub_to_octets (P256.Dsa.pub_of_priv priv)))
  | Ecdsa_p384 -> (
      match P384.Dsa.priv_of_octets scalar with
      | Error _ -> Error `Malformed
      | Ok priv -> Ok (P384.Dsa.pub_to_octets (P384.Dsa.pub_of_priv priv)))
  | Ecdsa_p521 -> (
      match P521.Dsa.priv_of_octets scalar with
      | Error _ -> Error `Malformed
      | Ok priv -> Ok (P521.Dsa.pub_to_octets (P521.Dsa.pub_of_priv priv)))

let of_ed25519_container blob_pub section =
  let* mat, _comment = Openssh_key.parse_private ~expect:"ssh-ed25519" section in
  match mat with
  | Openssh_key.Prv_ecdsa _ -> Error `Malformed
  | Openssh_key.Prv_ed25519 (sec_pub, sk) ->
      let seed = String.sub sk 0 32 in
      let sk_pub = String.sub sk 32 32 in
      if (not (String.equal sec_pub blob_pub)) || not (String.equal sk_pub blob_pub) then
        Error `Malformed
      else
        let* t = of_ed25519_seed seed in
        if String.equal t.pub_octets blob_pub then Ok t else Error `Malformed

let of_ecdsa_container blob_curve blob_point section =
  let* algorithm =
    match curve_of_name blob_curve with
    | Some algorithm -> Ok algorithm
    | None -> Error `Malformed
  in
  let* mat, _comment =
    Openssh_key.parse_private ~expect:(keytype_name algorithm) section
  in
  match mat with
  | Openssh_key.Prv_ed25519 _ -> Error `Malformed
  | Openssh_key.Prv_ecdsa (curve, point, mpint) ->
      if not (String.equal curve (ecdsa_curve_name algorithm)) then Error `Malformed
      else
        let* scalar =
          Openssh_key.scalar_of_mpint ~width:(scalar_length algorithm) mpint
        in
        let* derived = ecdsa_pub_of_scalar algorithm scalar in
        if (not (String.equal derived point)) || not (String.equal derived blob_point)
        then Error `Malformed
        else Ok { algorithm; priv = scalar; pub_octets = derived }

let of_openssh_private pem =
  let* pubblob, section = Openssh_key.read_container pem in
  let* pub = Openssh_key.parse_pub_octets pubblob in
  match pub with
  | `Ed25519 blob_pub -> of_ed25519_container blob_pub section
  | `Ecdsa (blob_curve, blob_point) -> of_ecdsa_container blob_curve blob_point section

let random_checkint () = String.get_int32_be (Mirage_crypto_rng.generate 4) 0

let to_openssh_private ?(comment = "") t =
  let checkint = random_checkint () in
  Openssh_key.pem_of_body
    (Openssh_key.base64_of_wire
       (Openssh_key.encode_container ~checkint (public_blob t) (priv_fields t) comment))

let authorized_key ?(comment = "") t =
  let b64 = Openssh_key.base64_of_wire (public_blob t) in
  if String.equal comment "" then Fmt.str "%s %s" (keytype_name t.algorithm) b64
  else Fmt.str "%s %s %s" (keytype_name t.algorithm) b64 comment

(* The pair write is not atomic across two names. On failure this module
   removes only the files the failing call itself created. *)
let io_of fn =
  try Ok (fn ())
  with Eio.Io (Eio.Fs.E _, _) as exn ->
    Eio.Fiber.check ();
    Error (`Io (Fmt.str "%a" Eio.Exn.pp exn))

let path_of fs path = Eio.Path.(Eio.Path.of_dir fs / path)

let path_exists p =
  match io_of (fun () -> Eio.Path.kind ~follow:false p) with
  | Ok `Not_found -> Ok false
  | Ok _ -> Ok true
  | Error e -> Error e

let write_pair p path_name pub_p pub_name private_body pub_body =
  let created = ref [] in
  let committed = ref false in
  let rec rollback = function
    | [] -> ()
    | q :: rest ->
        Fun.protect
          ~finally:(fun () -> rollback rest)
          (fun () ->
            try Eio.Path.unlink ~missing_ok:true q with Eio.Io (Eio.Fs.E _, _) -> ())
  in
  let save dest name perm body =
    try
      Eio.Path.with_open_out ~create:(`Exclusive perm) dest (fun flow ->
          created := dest :: !created;
          Eio.Flow.copy_string body flow);
      Ok ()
    with
    | Eio.Io (Eio.Fs.E (Already_exists _), _) -> Error (`Already_exists name)
    | Eio.Io (Eio.Fs.E _, _) as exn ->
        Eio.Fiber.check ();
        Error (`Io (Fmt.str "%a" Eio.Exn.pp exn))
  in
  Fun.protect
    ~finally:(fun () ->
      if not !committed then Eio.Cancel.protect (fun () -> rollback !created))
    (fun () ->
      match
        Eio.Cancel.protect (fun () ->
            let* () = save p path_name 0o600 private_body in
            save pub_p pub_name 0o644 pub_body)
      with
      | Ok () as ok ->
          committed := true;
          Eio.Fiber.check ();
          ok
      | Error _ as err -> err)

let write ~fs ~path ?comment t =
  let p = path_of fs path in
  let pub_name = path ^ ".pub" in
  let pub_p = path_of fs pub_name in
  let* priv_exists = path_exists p in
  let* () = if priv_exists then Error (`Already_exists path) else Ok () in
  let* pub_exists = path_exists pub_p in
  let* () = if pub_exists then Error (`Already_exists pub_name) else Ok () in
  write_pair p path pub_p pub_name
    (to_openssh_private ?comment t)
    (authorized_key ?comment t)

let max_key_file_size = 65536

let load_existing ~path p =
  let* contents =
    io_of (fun () ->
        Eio.Path.with_open_in p (fun flow ->
            Eio.Buf_read.parse ~max_size:(max_key_file_size + 1) Eio.Buf_read.take_all
              flow))
  in
  match contents with
  | Error (`Msg _) -> Error (`Io (Fmt.str "%s: exceeds the key file size limit" path))
  | Ok pem ->
      let* t = of_openssh_private pem in
      Ok (t, `Loaded)

let load_or_generate ~fs ~path algorithm =
  let p = path_of fs path in
  match io_of (fun () -> Eio.Path.kind ~follow:false p) with
  | Error e -> Error e
  | Ok `Not_found ->
      let t = generate algorithm in
      let* () = write ~fs ~path t in
      Ok (t, `Generated)
  | Ok _ -> load_existing ~path p
