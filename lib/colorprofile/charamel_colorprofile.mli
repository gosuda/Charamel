(** Terminal colour profiles and colour-aware output writers.

    A profile describes the output capabilities selected from terminal environment values.
    A writer preserves terminal control sequences while reducing SGR colours to the
    selected profile. *)

type t =
  | No_tty
  | Ascii
  | Ansi
  | Ansi256
  | True_color
      (** The type for terminal colour profiles. The constructors are ordered from least
          to greatest output capability. *)

type profile = t
(** The type alias for values used as writer profiles. *)

val detect : is_tty:bool -> env:(string -> string option) -> t
(** [detect ~is_tty ~env] is the colour profile selected for an output stream. [is_tty]
    reports whether the stream is attached to a terminal. [env] is a lookup for
    environment variables. [TTY_FORCE] can force terminal detection. [NO_COLOR] disables
    colours on a terminal. [CLICOLOR_FORCE] forces at least sixteen colours, and
    [CLICOLOR] enables at least sixteen colours on a terminal. *)

val convert : t -> Charamel_ansi.Color.t -> Charamel_ansi.Color.t
(** [convert profile color] is [color] reduced to what [profile] can display. [No_tty] and
    [Ascii] return [Default]. [Ansi] and [Ansi256] return a palette colour. [True_color]
    returns [color] unchanged. *)

module Writer : sig
  type nonrec t
  (** The type for colour-aware output writers. *)

  val create : profile:profile -> Lwt_io.output_channel -> t
  (** [create ~profile sink] is a writer that sends transformed output to [sink]. *)

  val write : t -> string -> unit Lwt.t
  (** [write writer text] sends [text] to [writer] and flushes it, so a terminal sees the
      frame without waiting for another write. Incomplete escape sequences remain pending
      until a later call completes them. [Ascii] and [No_tty] remove all SGR sequences.
      Non-SGR sequences and UTF-8 text are preserved. *)
end
