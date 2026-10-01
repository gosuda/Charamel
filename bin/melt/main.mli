(** Melt backup and restore command entry point.

    The command stores no container bytes in the mnemonic. Backup extracts the 32-byte
    Ed25519 seed from an OpenSSH private key and encodes it as the 24-word BIP-39 phrase
    provided by {!Melt_core.Mnemonic}. Restore decodes that phrase and writes a fresh
    unencrypted OpenSSH key pair without replacing an existing private or public file. *)

type error =
  [ `No_home
  | `Read_key of string * string
  | `Parse_key of Charamel_ssh_keygen.error
  | `Unsupported_key
  | `Mnemonic of Melt_core.Mnemonic.error
  | `Write_key of Charamel_ssh_keygen.error ]
(** [error] is a typed failure raised by a backup or restore operation. *)

val pp_error : error Fmt.t
(** [pp_error ppf error] renders the user-facing diagnostic for [error]. *)

val backup : fs_root:string -> path:string -> (string list, error) result Lwt.t
(** [backup ~fs_root ~path] reads the unencrypted OpenSSH private key at [path] and
    returns its 24-word Ed25519 seed phrase. [path] may be relative to [fs_root] or start
    with [~]. A non-Ed25519 key is rejected, and no file is written. *)

val restore :
  fs_root:string -> words:string -> output:string -> (unit, error) result Lwt.t
(** [restore ~fs_root ~words ~output] decodes the whitespace-separated 24-word phrase
    [words], reconstructs its Ed25519 key pair, and writes it at [output] and
    [output ^ ".pub"]. [output] may be relative to [fs_root] or start with [~]. Existing
    files are never replaced. *)
