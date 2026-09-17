(* RFC 4251 section 5 defines the string and mpint framing used here.  The
   container layout follows the OpenSSH PROTOCOL.key document. *)

type rd = { buf : string; mutable pos : int }

let rd_create buf = { buf; pos = 0 }
let rd_done t = t.pos = String.length t.buf

let rd_peek_string_len t =
  let len = String.length t.buf in
  if t.pos + 4 > len then None
  else
    let n = String.get_int32_be t.buf t.pos |> Int32.to_int in
    if n < 0 || t.pos + 4 + n > len then None else Some (t.pos + 4, n)

let rd_get_string t =
  match rd_peek_string_len t with
  | None -> None
  | Some (start, n) ->
      t.pos <- start + n;
      Some (String.sub t.buf start n)

let rd_get_uint32 t =
  if t.pos + 4 > String.length t.buf then None
  else
    let v = String.get_int32_be t.buf t.pos in
    t.pos <- t.pos + 4;
    Some v

let put_string buf s =
  Buffer.add_int32_be buf (Int32.of_int (String.length s));
  Buffer.add_string buf s

open Result.Syntax

let str rd = match rd_get_string rd with Some s -> Ok s | None -> Error `Malformed
let u32 rd = match rd_get_uint32 rd with Some v -> Ok v | None -> Error `Malformed

(* RFC 4251 section 5: an unsigned mpint is minimal and carries a leading
   zero byte only when the high bit of the first remaining byte is set. *)
let put_uint_mpint buf s =
  let rec trim i =
    if i + 1 < String.length s && s.[i] = '\000' then trim (i + 1) else i
  in
  let cut = if String.length s = 0 then 0 else trim 0 in
  let s = String.sub s cut (String.length s - cut) in
  if String.length s > 0 && Char.code s.[0] land 0x80 <> 0 then put_string buf ("\000" ^ s)
  else put_string buf s

let scalar_of_mpint ~width s =
  let len = String.length s in
  let minimal () =
    if len = 0 then Error `Malformed
    else if s.[0] <> '\000' then Ok s
    else if len = 1 then Ok ""
    else if Char.code s.[1] land 0x80 <> 0 then Ok (String.sub s 1 (len - 1))
    else Error `Malformed
  in
  let* m = minimal () in
  let m_len = String.length m in
  if m_len = 0 || m_len > width then Error `Malformed
  else Ok (String.make (width - m_len) '\000' ^ m)

let magic = "openssh-key-v1\000"

(* A key container holds at most a few hundred bytes of key material and
   comment; anything this large is not a key. *)
let max_wire = 1 lsl 20

type key_material =
  | Prv_ed25519 of string * string
  | Prv_ecdsa of string * string * string

let wire_pub_ed25519 pub_octets =
  let buf = Buffer.create 64 in
  put_string buf "ssh-ed25519";
  put_string buf pub_octets;
  Buffer.contents buf

let wire_pub_ecdsa curve_name point_octets =
  let buf = Buffer.create 128 in
  put_string buf ("ecdsa-sha2-" ^ curve_name);
  put_string buf curve_name;
  put_string buf point_octets;
  Buffer.contents buf

let parse_pub_octets wire =
  let rd = rd_create wire in
  let* keytype = str rd in
  match keytype with
  | "ssh-ed25519" ->
      let* pub = str rd in
      if String.length pub <> 32 || not (rd_done rd) then Error `Malformed
      else Ok (`Ed25519 pub)
  | ecdsa when String.starts_with ~prefix:"ecdsa-sha2-" ecdsa ->
      let curve = String.sub ecdsa 11 (String.length ecdsa - 11) in
      let* curve' = str rd in
      let* point = str rd in
      if (not (String.equal curve curve')) || not (rd_done rd) then Error `Malformed
      else Ok (`Ecdsa (curve, point))
  | _ -> Error `Unsupported_type

let parse_container wire =
  if String.length wire > max_wire then Error `Malformed
  else if not (String.starts_with ~prefix:magic wire) then Error `Malformed
  else
    let rd = rd_create wire in
    rd.pos <- String.length magic;
    let* ciphername = str rd in
    let* kdfname = str rd in
    let* kdfoptions = str rd in
    let* nkeys = u32 rd in
    let* () =
      if not (String.equal ciphername "none" && String.equal kdfname "none") then
        Error `Encrypted_key
      else if not (Int32.equal nkeys 1l) then Error `Malformed
      else if kdfoptions <> "" then Error `Malformed
      else Ok ()
    in
    let* pubblob = str rd in
    let* section = str rd in
    if not (rd_done rd) then Error `Malformed else Ok (pubblob, section)

let parse_private ~expect section =
  if String.length section land 7 <> 0 then Error `Malformed
  else
    let rd = rd_create section in
    let* c1 = u32 rd in
    let* c2 = u32 rd in
    let* () = if Int32.equal c1 c2 then Ok () else Error `Malformed in
    let* keytype = str rd in
    let* () = if String.equal keytype expect then Ok () else Error `Malformed in
    let* mat =
      if String.equal expect "ssh-ed25519" then
        let* pub = str rd in
        let* sk = str rd in
        if String.length pub <> 32 || String.length sk <> 64 then Error `Malformed
        else Ok (Prv_ed25519 (pub, sk))
      else if String.starts_with ~prefix:"ecdsa-sha2-" expect then
        let* curve = str rd in
        let* point = str rd in
        let* scalar = str rd in
        Ok (Prv_ecdsa (curve, point, scalar))
      else Error `Malformed
    in
    let* comment = str rd in
    let remaining = String.length section - rd.pos in
    let rec padding_ok i =
      i >= remaining || (Char.code section.[rd.pos + i] = i + 1 && padding_ok (i + 1))
    in
    if remaining >= 8 || not (padding_ok 0) then Error `Malformed else Ok (mat, comment)

let encode_priv_section ~checkint keyfields comment =
  let body = Buffer.create 128 in
  Buffer.add_int32_be body checkint;
  Buffer.add_int32_be body checkint;
  Buffer.add_string body keyfields;
  put_string body comment;
  let pad = (8 - (Buffer.length body land 7)) land 7 in
  Buffer.add_string body (String.init pad (fun i -> Char.chr (i + 1)));
  Buffer.contents body

let encode_container ~checkint pubblob keyfields comment =
  let section = encode_priv_section ~checkint keyfields comment in
  let buf = Buffer.create (64 + String.length pubblob + String.length section) in
  Buffer.add_string buf magic;
  put_string buf "none";
  put_string buf "none";
  put_string buf "";
  Buffer.add_int32_be buf 1l;
  put_string buf pubblob;
  put_string buf section;
  Buffer.contents buf

let base64_of_wire wire = Base64.encode_string wire

let wire_of_base64 b64 =
  match Base64.decode ~pad:true b64 with
  | Ok wire -> Ok wire
  | Error (`Msg _) -> Error `Malformed

let pem_of_body b64 =
  let n = String.length b64 in
  let buf = Buffer.create (n + 128) in
  Buffer.add_string buf "-----BEGIN OPENSSH PRIVATE KEY-----\n";
  let rec wrap off =
    if off >= n then ()
    else begin
      let len = min 70 (n - off) in
      Buffer.add_substring buf b64 off len;
      Buffer.add_char buf '\n';
      wrap (off + len)
    end
  in
  wrap 0;
  Buffer.add_string buf "-----END OPENSSH PRIVATE KEY-----\n";
  Buffer.contents buf

let pem_begin = "-----BEGIN OPENSSH PRIVATE KEY-----"
let pem_end = "-----END OPENSSH PRIVATE KEY-----"

let is_marker l =
  String.starts_with ~prefix:"-----" l && String.ends_with ~suffix:"-----" l

let strip_pem s =
  let lines =
    String.split_on_char '\n' s |> List.map String.trim |> List.filter (fun l -> l <> "")
  in
  match lines with
  | [] -> Error `Malformed
  | first :: rest ->
      if String.equal first pem_begin then
        let rec finish acc = function
          | [] -> Error `Malformed
          | l :: rest ->
              if String.equal l pem_end then
                match rest with
                | [] -> Ok (String.concat "" (List.rev acc))
                | _ -> Error `Malformed
              else if is_marker l then Error `Malformed
              else finish (l :: acc) rest
        in
        finish [] rest
      else if is_marker first then Error `Unsupported_type
      else Error `Malformed

let read_container pem =
  let* b64 = strip_pem pem in
  let* wire = wire_of_base64 b64 in
  parse_container wire
