(** OpenSSH key pair generation for the keygen command.

    The command resolves user paths, creates missing parent directories, and delegates
    writes to the SSH keygen library. *)

type error =
  [ `No_home | `Invalid_path of string | `Already_exists of string | `Io of string ]
(** The type for failures reported by key pair generation. *)

val pp_error : error Fmt.t
(** [pp_error ppf error] renders [error] as a user-facing diagnostic. *)

val default_path : Charamel_ssh_keygen.algorithm -> (string, error) result
(** [default_path algorithm] is the default private-key path for [algorithm] under
    [$HOME/.ssh]. *)

val generate :
  fs_root:string ->
  path:string ->
  algorithm:Charamel_ssh_keygen.algorithm ->
  ?comment:string ->
  force:bool ->
  unit ->
  (string, error) result Lwt.t
(** [generate ~fs_root ~path ~algorithm ?comment ~force ()] is the result containing the
    SHA-256 fingerprint of the generated public key. It creates a key pair and writes it
    through {!Charamel_ssh_keygen.write}.

    [comment] defaults to the empty string. [path] may be absolute, relative, or begin
    with [~/]. Missing parent directories are created with mode [0700]. When [force] is
    false, an existing private or public path, including a directory, is rejected without
    replacement. When [force] is true, an existing regular file or symbolic link at either
    name is replaced with a private file mode [0600] and a public file mode [0644]; a
    directory at either name is still rejected. *)
