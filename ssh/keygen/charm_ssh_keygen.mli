(** SSH key pair creation, loading, and serialization.

    Key material comes from Mirage_crypto_ec for Ed25519 and the NIST P-256, P-384, and
    P-521 curves. Private keys are serialized to the unencrypted OpenSSH container and
    public keys to the SSH wire blob. The SHA-256 fingerprint is the form `ssh-keygen -l`
    prints. Callers using randomness must initialize the RNG once per process with
    [Mirage_crypto_rng_unix.use_default ()] before calling [generate] or writing a freshly
    generated pair. *)

type algorithm = Ed25519 | Ecdsa_p256 | Ecdsa_p384 | Ecdsa_p521

type error =
  [ `Malformed
  | `Unsupported_type
  | `Encrypted_key
  | `Already_exists of string
  | `Io of string ]

type t
(** A key pair. Pretty-printing reports only the algorithm and SHA-256 fingerprint. The
    private material is never printed. *)

val algorithm_name : algorithm -> string
(** [algorithm_name a] is the CLI spelling of [a], for example [ed25519]. *)

val algorithm_of_name : string -> algorithm option
(** [algorithm_of_name s] is the algorithm whose name is [s]. *)

val pp : t Fmt.t
(** [pp] renders the algorithm name and SHA-256 fingerprint of the key. *)

val public_blob : t -> string
(** [public_blob t] is the raw SSH public key wire blob. *)

val fingerprint_sha256 : t -> string
(** [fingerprint_sha256 t] is the [SHA256:<base64>] fingerprint of the public key blob,
    matching what `ssh-keygen -l` prints. *)

val of_ed25519_seed : string -> (t, error) result
(** [of_ed25519_seed seed] is the Ed25519 key pair whose 32-byte private seed is [seed]. A
    seed of any other length is [`Malformed]. *)

val ed25519_seed : t -> string option
(** [ed25519_seed t] is [t]'s 32-byte private seed when [t] is an Ed25519 pair, and [None]
    for the ECDSA algorithms. *)

val generate : algorithm -> t
(** [generate algorithm] creates a fresh key pair of [algorithm].

    @raise Invalid_argument if the RNG has not been initialized. *)

val of_openssh_private : string -> (t, error) result
(** [of_openssh_private pem] is the key pair encoded by the unencrypted OPENSSH PRIVATE
    KEY document [pem]. The public key blob, the public fields of the private section, and
    the public key derived from the private scalar must all agree. An encrypted container
    is [`Encrypted_key], another PEM block type is [`Unsupported_type], and broken or
    inconsistent material is [`Malformed]. *)

val to_openssh_private : ?comment:string -> t -> string
(** [to_openssh_private ?comment t] is the PEM document for [t]'s private key. The output
    is unencrypted. [comment] defaults to [""]. *)

val authorized_key : ?comment:string -> t -> string
(** [authorized_key ?comment t] is an authorized_keys line. It is the key type, the base64
    public blob, then the comment. [comment] defaults to [""]. *)

val write : fs:_ Eio.Fs.dir -> path:string -> ?comment:string -> t -> (unit, error) result
(** [write ~fs ~path ?comment t] writes the private key to [path] with mode 0600 and the
    public key to [path ^ ".pub"] with mode 0644. Both files are created exclusively.
    [comment] defaults to [""]. When either path already exists, including as a dangling
    or regular symlink, the call writes neither file and is [`Already_exists] naming the
    existing path. On any other failure after a file was created, only files this call
    created are removed, so a failed call leaves no private key behind. The pair is not
    atomic across both names. Other filesystem failures are [`Io]. *)

val load_or_generate :
  fs:_ Eio.Fs.dir ->
  path:string ->
  algorithm ->
  (t * [ `Loaded | `Generated ], error) result
(** [load_or_generate ~fs ~path algorithm] loads the pair from [path] when the file
    exists, and otherwise generates a fresh pair of [algorithm] and writes it. The result
    reports which happened. An existing file is never replaced. A file larger than 65536
    bytes is [`Io]. An unparseable file returns its parse error. *)
