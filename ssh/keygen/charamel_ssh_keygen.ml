open Lwt.Infix
open Result.Syntax

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

let wire_ed25519_blob octets = Openssh_key.wire_pub_ed25519 octets

let unsigned_bytes value =
  let bits = Z.to_bits value in
  let last = ref (String.length bits - 1) in
  while !last >= 0 && String.get bits !last = '\000' do
    decr last
  done;
  let bytes = Bytes.create (!last + 1) in
  for index = 0 to !last do
    Bytes.set bytes index (String.get bits (!last - index))
  done;
  Bytes.to_string bytes

let wire_rsa_blob ~e ~n =
  let buffer = Buffer.create 64 in
  Openssh_key.put_string buffer {|ssh-rsa|};
  Openssh_key.put_uint_mpint buffer (unsigned_bytes e);
  Openssh_key.put_uint_mpint buffer (unsigned_bytes n);
  Buffer.contents buffer

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

type authorized_entry = {
  options : string option;
  type_name : string;
  blob : string;
  comment : string;
}

let is_keytype_name name =
  String.starts_with ~prefix:{|ssh-|} name
  || String.starts_with ~prefix:{|ecdsa-|} name
  || String.starts_with ~prefix:{|sk-|} name

let line_fields line =
  let fields = Dynarray.create () in
  let token = Buffer.create 32 in
  let quoted = ref false in
  let flush () =
    if Buffer.length token > 0 then begin
      Dynarray.add_last fields (Buffer.contents token);
      Buffer.clear token
    end
  in
  String.iter
    (fun ch ->
      match ch with
      | '"' ->
          Buffer.add_char token ch;
          quoted := not !quoted
      | (' ' | '\t' | '\r') when not !quoted -> flush ()
      | ch -> Buffer.add_char token ch)
    line;
  flush ();
  Dynarray.to_list fields

let blob_type blob =
  if String.length blob < 4 then None
  else
    let len = Int32.to_int (String.get_int32_be blob 0) in
    if len < 0 || 4 + len > String.length blob then None else Some (String.sub blob 4 len)

let entry_of_blob options keytype b64 rest =
  match Base64.decode ~pad:false b64 with
  | Error _ -> None
  | Ok blob -> (
      match blob_type blob with
      | Some inner when String.equal inner keytype ->
          Some { options; type_name = keytype; blob; comment = String.concat {| |} rest }
      | _ -> None)

let entry_of_line line =
  match line_fields line with
  | keytype :: b64 :: rest when is_keytype_name keytype ->
      entry_of_blob None keytype b64 rest
  | options :: keytype :: b64 :: rest when is_keytype_name keytype ->
      entry_of_blob (Some options) keytype b64 rest
  | _ -> None

let parse_authorized_keys text =
  let entry line =
    let line = String.trim line in
    if String.is_empty line || String.starts_with ~prefix:{|#|} line then None
    else entry_of_line line
  in
  List.filter_map entry (String.split_on_char '\n' text)

(* The pair write is not atomic across two names. On failure this module
   removes only the files the failing call itself created. *)
let fs_text = function
  | `Already_exists -> "already exists"
  | `Is_directory -> "is a directory"
  | `Not_found -> "no such file or directory"
  | `Permission_denied -> "permission denied"

let io_text exn =
  match exn with
  | Unix.Unix_error (kind, _, _) -> Unix.error_message kind
  | Charamel_os.Fs.E (error, path) -> Fmt.str "%s: %s" path (fs_text error)
  | exn -> Printexc.to_string exn

let io_of fn =
  Lwt.catch
    (fun () -> fn () >|= fun value -> Ok value)
    (function
      | Lwt.Canceled as exn -> Lwt.fail exn
      | exn -> Lwt.return (Error (`Io (io_text exn))))

let path_of fs_root path =
  if String.equal path "" || String.equal path "." then fs_root
  else if Filename.is_relative path then Filename.concat fs_root path
  else path

(* Only a short-circuit: [save] creates exclusively, so a wrong answer here still ends up
   reported as [Already_exists] or [Io] by the create that follows. *)
let path_exists p = Charamel_os.Fs.stat p >|= function Ok _ -> true | Error _ -> false

let read_capped channel limit =
  let buffer = Buffer.create 8192 in
  let rec go () =
    let remaining = limit - Buffer.length buffer in
    if remaining <= 0 then Lwt.return_unit
    else
      Lwt_io.read ~count:remaining channel >>= fun chunk ->
      if String.is_empty chunk then Lwt.return_unit
      else begin
        Buffer.add_string buffer chunk;
        go ()
      end
  in
  go () >|= fun () -> Buffer.contents buffer

let write_pair p path_name pub_p pub_name private_body pub_body =
  let created = ref [] in
  let committed = ref false in
  let rec rollback = function
    | [] -> Lwt.return_unit
    | q :: rest ->
        Lwt.catch
          (fun () -> Charamel_os.Fs.unlink q >|= fun _ -> ())
          (fun _exn -> Lwt.return_unit)
        >>= fun () -> rollback rest
  in
  let save dest name perm body =
    Lwt.catch
      (fun () ->
        Lwt_unix.openfile dest [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] perm
        >>= fun fd ->
        created := dest :: !created;
        let channel = Lwt_io.of_fd ~mode:Lwt_io.output fd in
        Lwt_io.write channel body >>= fun () ->
        Lwt_io.close channel >|= fun () -> Ok ())
      (function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return (Error (`Already_exists name))
        | Lwt.Canceled as exn -> Lwt.fail exn
        | exn -> Lwt.return (Error (`Io (io_text exn))))
  in
  Lwt.finalize
    (fun () ->
      save p path_name 0o600 private_body >>= function
      | Error _ as err -> Lwt.return err
      | Ok () -> (
          save pub_p pub_name 0o644 pub_body >>= function
          | Error _ as err -> Lwt.return err
          | Ok () ->
              committed := true;
              Lwt.return (Ok ())))
    (fun () -> if !committed then Lwt.return_unit else rollback !created)

let temp_of dest suffix =
  Filename.concat (Filename.dirname dest)
    (Fmt.str ".charamel-keygen-%d-%s" (Unix.getpid ()) suffix)

let remove q =
  Lwt.catch
    (fun () -> Charamel_os.Fs.unlink q >|= fun _ -> ())
    (fun _exn -> Lwt.return_unit)

let write_temp dest suffix perm body =
  let q = temp_of dest suffix in
  io_of (fun () ->
      Lwt_unix.openfile q [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] perm >>= fun fd ->
      let channel = Lwt_io.of_fd ~mode:Lwt_io.output fd in
      Lwt_io.write channel body >>= fun () -> Lwt_io.close channel)
  >>= function
  | Ok () -> Lwt.return (Ok q)
  | Error _ as err -> remove q >>= fun () -> Lwt.return err

let rec drop_targets = function
  | [] -> Lwt.return_unit
  | (_, q) :: rest -> remove q >>= fun () -> drop_targets rest

let rec write_temps acc i = function
  | [] -> Lwt.return (Ok (List.rev acc))
  | (dest, perm, body) :: rest -> (
      write_temp dest (string_of_int i) perm body >>= function
      | Ok q -> write_temps ((dest, q) :: acc) (i + 1) rest
      | Error _ as err -> drop_targets acc >>= fun () -> Lwt.return err)

let rec commit = function
  | [] -> Lwt.return (Ok ())
  | ((dest, q) as target) :: rest -> (
      io_of (fun () -> Charamel_os.Fs.rename_replace ~src:q ~dst:dest) >>= function
      | Ok () -> commit rest
      | Error _ as err -> drop_targets (target :: rest) >>= fun () -> Lwt.return err)

let directory_at (dest, name) =
  Charamel_os.Fs.stat dest >|= function
  | Ok { Unix.st_kind = Unix.S_DIR; _ } -> Some name
  | Ok _ | Error _ -> None

let rejected_directories names =
  Lwt_list.filter_map_s directory_at names >|= function
  | name :: _ -> Error (`Io (Fmt.str "%s: is a directory" name))
  | [] -> Ok ()

let replace_pair p path_name pub_p pub_name private_body pub_body =
  let targets = [ (p, 0o600, private_body); (pub_p, 0o644, pub_body) ] in
  rejected_directories [ (p, path_name); (pub_p, pub_name) ] >>= function
  | Error _ as err -> Lwt.return err
  | Ok () -> (
      write_temps [] 0 targets >>= function
      | Error _ as err -> Lwt.return err
      | Ok staged -> commit staged)

let write ~fs_root ~path ?comment ?(overwrite = false) t =
  let p = path_of fs_root path in
  let pub_name = path ^ ".pub" in
  let pub_p = path_of fs_root pub_name in
  let private_body = to_openssh_private ?comment t in
  let public_body = authorized_key ?comment t in
  if overwrite then replace_pair p path pub_p pub_name private_body public_body
  else
    path_exists p >>= function
    | true -> Lwt.return (Error (`Already_exists path))
    | false -> (
        path_exists pub_p >>= function
        | true -> Lwt.return (Error (`Already_exists pub_name))
        | false -> write_pair p path pub_p pub_name private_body public_body)

let max_key_file_size = 65536

let decode_saved ~path body =
  if String.length body > max_key_file_size then
    Error (`Io (Fmt.str "%s: exceeds the key file size limit" path))
  else Result.map (fun t -> (t, `Loaded)) (of_openssh_private body)

let load_existing ~path p =
  io_of (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.input p (fun channel ->
          read_capped channel (max_key_file_size + 1)))
  >>= fun read -> Lwt.return (Result.bind read (fun body -> decode_saved ~path body))

let load_or_generate ~fs_root ~path algorithm =
  let p = path_of fs_root path in
  path_exists p >>= function
  | true -> load_existing ~path p
  | false ->
      let t = generate algorithm in
      write ~fs_root ~path t >>= fun written ->
      Lwt.return (Result.map (fun () -> (t, `Generated)) written)
