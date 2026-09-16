(** The OpenSSH private key container and SSH public key blobs.

    The container is the unencrypted "openssh-key-v1" private key format described in the
    OpenSSH PROTOCOL.key document, wrapped in an OPENSSH PRIVATE KEY PEM envelope. Public
    key blobs are the SSH wire form used in authorized_keys lines. Decoding never raises.
    Malformed framing is [`Malformed], a key type outside Ed25519 and the NIST curves is
    [`Unsupported_type], and an encrypted container is [`Encrypted_key]. *)

val put_string : Buffer.t -> string -> unit
(** [put_string buf s] appends [s] to [buf] as an RFC 4251 string. *)

val put_uint_mpint : Buffer.t -> string -> unit
(** [put_uint_mpint buf s] appends [s] to [buf] as an unsigned RFC 4251 mpint. [s] is
    reduced to its minimal form first. *)

val scalar_of_mpint : width:int -> string -> (string, [> `Malformed ]) result
(** [scalar_of_mpint ~width s] is the unsigned scalar encoded by the mpint [s], left
    padded with zero bytes to [width]. The mpint must be minimal and encode a non-zero
    scalar no wider than [width] bytes. Anything else is [`Malformed]. *)

(** The type for the key fields of a decoded private section. *)
type key_material =
  | Prv_ed25519 of string * string
      (** The 32-byte public value and the 64-byte [seed || public] field. *)
  | Prv_ecdsa of string * string * string
      (** The curve name, the SEC1 point, and the scalar mpint. *)

val parse_private :
  expect:string -> string -> (key_material * string, [> `Malformed ]) result
(** [parse_private ~expect section] is the key fields and comment decoded from the
    plaintext private [section] of the container. [expect] is the key type string of the
    public blob. The section length must be a multiple of eight, the two checkints must be
    equal, the framed key type must be [expect], the Ed25519 fields must be 32 and 64
    bytes, and the trailing padding must count 1, 2, ... to an eight-byte boundary. Key
    fields are decoded before the comment and padding. *)

val wire_pub_ed25519 : string -> string
(** [wire_pub_ed25519 pub] is the public key blob for the 32-byte Ed25519 public value
    [pub]. *)

val wire_pub_ecdsa : string -> string -> string
(** [wire_pub_ecdsa curve point] is the public key blob for the curve named [curve] (such
    as [nistp256]) and its SEC1 point [point]. *)

val parse_pub_octets :
  string ->
  ( [ `Ed25519 of string | `Ecdsa of string * string ],
    [> `Malformed | `Unsupported_type ] )
  result
(** [parse_pub_octets wire] is the key type, Ed25519 public value, or ECDSA curve name and
    point decoded from the public key blob [wire]. *)

val read_container :
  string -> (string * string, [> `Malformed | `Unsupported_type | `Encrypted_key ]) result
(** [read_container pem] is the public key blob and plaintext private section decoded from
    the PEM document [pem]. The document must hold a single OPENSSH PRIVATE KEY block.
    Trailing content is [`Malformed]. The ciphername and kdfname must be [none] with empty
    kdf options and exactly one key, and the decoded container must end with the private
    section. *)

val encode_container : checkint:int32 -> string -> string -> string -> string
(** [encode_container ~checkint pubblob keyfields comment] is the raw container body
    encoding one key with cipher [none], kdf [none], public blob [pubblob], key fields
    [keyfields], comment [comment], and checkint [checkint] written twice. *)

val base64_of_wire : string -> string
(** [base64_of_wire wire] is the padded base64 encoding of [wire]. *)

val pem_of_body : string -> string
(** [pem_of_body b64] is [b64] wrapped at 70 columns between the OPENSSH PRIVATE KEY
    markers. *)
