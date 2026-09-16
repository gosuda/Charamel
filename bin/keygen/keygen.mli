(** OpenSSH key pair generation for the keygen command.

    The command resolves user paths, creates missing parent directories, and delegates
    non-forced writes to the SSH keygen library. *)

type error =
  [ `No_home
  | `Invalid_path of string
  | `Already_exists of string
  | `Target_is_directory of string
  | `Io of string ]
(** The type for failures reported by key pair generation. *)

val pp_error : error Fmt.t
(** [pp_error ppf error] renders [error] as a user-facing diagnostic. *)

val home_dir : unit -> (string, error) result
(** [home_dir ()] is the absolute user home directory from [$HOME].

    The result is [`No_home] when [$HOME] is unset, empty, or relative. *)

val resolve_path : string -> (string, error) result
(** [resolve_path path] is [path] with a leading [~] expanded and a relative path resolved
    against the current directory. An empty path is rejected. *)

val default_path : Charm_ssh_keygen.algorithm -> (string, error) result
(** [default_path algorithm] is the default private-key path for [algorithm] under
    [$HOME/.ssh]. *)

val generate :
  fs:_ Eio.Fs.dir ->
  path:string ->
  algorithm:Charm_ssh_keygen.algorithm ->
  ?comment:string ->
  force:bool ->
  unit ->
  (string, error) result
(** [generate ~fs ~path ~algorithm ?comment ~force ()] is the result containing the
    SHA-256 fingerprint of the generated public key. It creates a key pair.

    [comment] defaults to the empty string. [path] may be absolute, relative, or begin
    with [~/]. Missing parent directories are created with mode [0700]. When [force] is
    false, existing private or public paths are rejected without replacement. When [force]
    is true, existing regular files and symbolic links are replaced with a private file
    mode [0600] and a public file mode [0644]. Directories at either target are rejected.
*)
