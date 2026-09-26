(** Terminal I/O boundary consumed by the runtime.

    [t] is the private seam between the program runtime and a terminal transport, either
    the local process TTY or a caller-supplied pair of channels such as an SSH channel.
    Constructing a value performs no I/O beyond probing whether the underlying descriptors
    are terminals; every mutation of terminal state happens inside [enter] and [leave].
    Neither constructor installs a signal handler or spawns a fiber. *)

type t = private {
  input : Charamel_os.Console_input.console_input;
      (** The source read for keys, mouse events and pastes. *)
  output : Lwt_io.output_channel;  (** The sink the renderer writes to. *)
  size : unit -> int * int;
      (** [size ()] is the current [(rows, columns)] of the terminal. *)
  on_resize : (unit -> unit) Lwt_stream.t option;
      (** A stream of resize notifications for a transport that cannot rely on SIGWINCH,
          or [None] when the runtime must poll [size] itself. *)
  env : string -> string option;
      (** [env name] is the value of environment variable [name]. *)
  is_tty : bool;
      (** [true] when both the input and the output are attached to a real terminal
          device. *)
  enter : unit -> unit;
      (** [enter ()] switches the transport into raw, unbuffered mode. *)
  leave : unit -> unit;
      (** [leave ()] restores the transport to the state it had before the first
          [enter ()]. It is idempotent. *)
}
(** The type for a terminal transport. *)

val local : ?output:[ `Stdout | `Stderr ] -> unit -> t
(** [local ?output ()] is the terminal transport for the calling process. It reads from
    standard input — the Windows console record queue there, the byte stream elsewhere —
    and writes to [output], which defaults to [`Stdout]. [is_tty] is [true] only when
    standard input and [output] are terminal devices. [size] reads the window size of
    [output] itself, through {!Charamel_os.Tty.size_of_output}, and falls back to the
    [COLUMNS] and [LINES] environment variables, and then to 80 columns by 24 rows, when
    either is missing, not an integer, not positive, or when [output] is not a terminal —
    so [~output:`Stderr] keeps painting at the real size while stdout carries data.
    [enter] and [leave] save and restore the raw-mode attributes of standard input through
    {!Charamel_os.Tty.enter_raw} and do nothing when [is_tty] is [false]. [env] reads the
    process environment. [on_resize] is [None]; the runtime watches SIGWINCH itself, which
    is why a transport over another process's terminal must supply [on_resize] of its own.
*)

val custom :
  input:Charamel_os.Console_input.console_input ->
  output:Lwt_io.output_channel ->
  size:(unit -> int * int) ->
  on_resize:(unit -> unit) Lwt_stream.t option ->
  env:(string -> string option) ->
  is_tty:bool ->
  t
(** [custom ~input ~output ~size ~on_resize ~env ~is_tty] is a terminal transport over
    caller-supplied channels, such as an SSH channel. Input is decoded from [input] and
    frames are written to [output]. [size ()] is the current [(rows, columns)] and is
    consulted at startup and after each notification read from [on_resize]. [on_resize] is
    [None] when the transport has no resize notifications of its own. [env name] is the
    value of environment variable [name] as seen by the transport's terminal, and drives
    color profile detection. [is_tty] states whether the far end is a terminal device. No
    raw mode is negotiated on a custom transport, and {!Charamel_tea.Cmd.exec} runs a
    child on the local process's own descriptors, so a command that execs on a remote
    transport inherits this process's terminal, not the far end's. The caller owns that on
    its side of the connection. *)
