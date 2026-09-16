(** Terminal I/O boundary consumed by the runtime.

    [t] is the private seam between the program runtime and a terminal transport, either
    the local process TTY or a caller-supplied pair of flows such as an SSH channel.
    Constructing a value performs no I/O beyond probing whether the underlying descriptors
    are terminals; every mutation of terminal state happens inside [enter] and [leave].
    Neither constructor installs a signal handler or spawns a fiber. *)

type t = private {
  input : Eio.Flow.source_ty Eio.Resource.t;
      (** The byte source read for keys, mouse events and pastes. *)
  output : Eio.Flow.sink_ty Eio.Resource.t;  (** The byte sink the renderer writes to. *)
  size : unit -> int * int;
      (** [size ()] is the current [(rows, columns)] of the terminal. *)
  on_resize : (unit -> unit) Eio.Stream.t option;
      (** A stream of resize notifications for a transport that cannot rely on SIGWINCH,
          or [None] when the runtime must poll [size] itself. *)
  env : string -> string option;
      (** [env name] is the value of environment variable [name]. *)
  is_tty : bool;
      (** [true] when both [input] and [output] are attached to a real terminal device. *)
  enter : unit -> unit;
      (** [enter ()] switches the transport into raw, unbuffered mode. *)
  leave : unit -> unit;
      (** [leave ()] restores the transport to the state it had before the first
          [enter ()]. It is idempotent. *)
}
(** The type for a terminal transport. *)

val local : ?output:[ `Stdout | `Stderr ] -> Eio_unix.Stdenv.base -> t
(** [local ?output env] is the terminal transport for the calling process. It reads from
    standard input and writes to [output]. [output] defaults to [`Stdout]. [is_tty] is
    [true] only when both standard input and [output] are terminal devices. [size] reads
    the window size of [output] and falls back to the [COLUMNS] and [LINES] environment
    variables, and then to 80 columns by 24 rows, when either is missing, not an integer
    or not positive. [enter] and [leave] save and restore the raw-mode attributes of
    standard input and do nothing when [is_tty] is [false]. [env] reads the process
    environment. [on_resize] is [None]; the caller owns its own SIGWINCH handling. *)

val custom :
  input:_ Eio.Flow.source ->
  output:_ Eio.Flow.sink ->
  size:(unit -> int * int) ->
  on_resize:(unit -> unit) Eio.Stream.t option ->
  env:(string -> string option) ->
  is_tty:bool ->
  t
(** [custom ~input ~output ~size ~on_resize ~env ~is_tty] is a terminal transport over
    caller-supplied flows, such as an SSH channel. [enter] and [leave] do nothing. A
    transport built this way owns no local terminal device for this process to put in raw
    mode; the caller is responsible for any raw-mode negotiation on its side of the
    connection. *)
