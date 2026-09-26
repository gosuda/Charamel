(** Email input, rendering and MIME composition for [pop].

    The module gathers command-line values, bounded input streams and Markdown renderings
    into one validated {!Mime.message}. It never delivers a message. *)

type options = {
  to_ : string list;
  cc : string list;
  bcc : string list;
  from : string option;
  subject : string option;
  body : string option;
  body_file : string option;
  attachments : string list;
  signature : string option;
  unsafe_html : bool;
}
(** Raw command values before missing fields are filled by the form. *)

type prepared = { message : Mime.message; wire : string }
(** A validated message and its deterministic RFC 5322 representation. *)

type error =
  [ `Missing of string
  | `Input of string
  | `Address of Mime.error
  | `Message of Mime.error
  | `Markdown of string ]
(** An input or composition failure suitable for a command diagnostic. *)

val read_file : cwd:string -> string -> (string, error) result Lwt.t
(** [read_file ~cwd path] reads [path] as bounded bytes, resolved against [cwd] when
    relative. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf error] renders [error] without exposing credentials. *)

val empty_options : options
(** [empty_options] contains no recipients, fields or attachments and does not permit
    unsafe HTML. *)

val split_addresses : string list -> string list
(** [split_addresses values] splits comma-separated mailbox values, preserving order and
    quoted commas, and removes surrounding whitespace and empty entries. *)

val stdin_is_tty : Lwt_io.input_channel -> bool
(** [stdin_is_tty source] is [true] when the process's own standard input is a terminal.
    [source] is accepted for call-site symmetry with {!read_file}'s channel-shaped
    arguments; the check is always against the real process, since the runtime no longer
    threads a substitutable environment. *)

val config_of_env : env:(string -> string option) -> (Send.smtp_config, error) result
(** [config_of_env ~env] reads the SMTP settings from [env]. The default port is [587] and
    the default security is [Starttls]. *)

val prepare :
  cwd:string ->
  stdin:Lwt_io.input_channel ->
  ?date:Ptime.t ->
  options ->
  (prepared, error) result Lwt.t
(** [prepare ~cwd ~stdin ?date options] reads the body and attachments, renders Markdown
    to plain text and safe HTML, and validates all addresses. [date] defaults to the
    current wall-clock time. Files and stdin are bounded to ten mebibytes. *)
