(** Commands: one-off work the runtime performs outside [update].

    Building a command runs nothing. [t] is a pure description that the program's runtime
    interprets; [Charamel_tea] re-exports it abstract. Constructors are [private]. Every
    value of [t] is built through the functions below, and any module may still
    pattern-match one. *)

type 'msg t = private
  | None_
  | Batch of 'msg t list
  | Seq of 'msg t list
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Msg of 'msg
  | Perform of (unit -> 'msg)
  | Await of 'msg Lwt.t
  | After of float * (unit -> 'msg)
  | Quit
  | Interrupt
  | Suspend
  | Exec of { argv : string list; on_exit : int -> 'msg }
  | Print of string
  | Set_clipboard of { selection : [ `System | `Primary ]; content : string }
  | Read_clipboard of [ `System | `Primary ]
  | Raw of string
  | Query of
      [ `Background
      | `Foreground
      | `Cursor_color
      | `Terminal_version
      | `Kitty_flags
      | `Cursor_position
      | `Capability of string ]
  | Window_size
      (** The type for a command.

          [Batch] runs its members concurrently; each delivers its messages as it
          completes. [Seq] runs its members in order, starting the next only after the
          previous has delivered. [Map] transforms every message a command produces and
          leaves every other command's effect unchanged. *)

val none : 'msg t
(** [none] does nothing. *)

val batch : 'msg t list -> 'msg t
(** [batch cmds] runs every command in [cmds] concurrently. *)

val seq : 'msg t list -> 'msg t
(** [seq cmds] runs every command in [cmds] in order, one at a time. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f cmd] is [cmd] with every message it produces passed through [f]. Control
    effects of [cmd], such as quitting, interrupting, suspending, executing a process,
    printing, setting the clipboard, reading the clipboard, writing raw bytes, or querying
    the terminal, are unaffected by [f]. *)

val msg : 'msg -> 'msg t
(** [msg m] delivers [m] on the next loop iteration. *)

val perform : (unit -> 'msg) -> 'msg t
(** [perform thunk] runs [thunk] in its own task once the command is dispatched and
    delivers its result. An exception raised by [thunk] terminates the program with
    [`Exn]. [thunk] is not called when the command is built. *)

val await : 'msg Lwt.t -> 'msg t
(** [await promise] delivers the value [promise] resolves to, and fails the program with
    its error if it fails. The run loop keeps rendering while [promise] is pending.
    Cancelling the command does not cancel [promise]. *)

val after : float -> (unit -> 'msg) -> 'msg t
(** [after seconds thunk] runs [thunk] once, [seconds] after the command is dispatched,
    and delivers its result. [thunk] is not called when the command is built. *)

val quit : 'msg t
(** [quit] stops the run loop and exits normally. *)

val interrupt : 'msg t
(** [interrupt] stops the run loop and reports [`Interrupted]. *)

val suspend : 'msg t
(** [suspend] restores the terminal and suspends the process.

    It is a documented no-op, delivering nothing, when the platform has no job control
    ({!Charamel_os.Tty.supports_suspend} is [false], as on Windows) or when the transport
    is not the local terminal: a program driven over an SSH channel cannot suspend the
    process serving it. *)

val exec : argv:string list -> (int -> 'msg) -> 'msg t
(** [exec ~argv on_exit] releases the terminal, runs the process named by [argv] attached
    to the tty, restores the terminal, and delivers [on_exit code] with the process's exit
    code. A transport built by {!Charamel_tea.Terminal.custom_with_exec} runs the child
    through its [exec] instead, and the exit code it returns is the one delivered. *)

val print : string -> 'msg t
(** [print s] writes [s] above the view in inline mode. In alt-screen mode [s] is queued
    and written when the program exits. *)

val set_clipboard : ?selection:[ `System | `Primary ] -> string -> 'msg t
(** [set_clipboard ~selection s] sets the terminal's clipboard ([`System], the default) or
    primary selection ([`Primary]) to [s] through OSC 52, as
    [ESC \] 52 ; c ; <base64 of s>] or [ESC \] 52 ; p ; <base64 of s>], terminated by
    [BEL]. Empty [s] clears it. No reply is expected. *)

val read_clipboard : [ `System | `Primary ] -> 'msg t
(** [read_clipboard selection] asks the terminal for its clipboard ([`System]) or primary
    selection ([`Primary]) with the OSC 52 query [ESC \] 52 ; c ; ?] or
    [ESC \] 52 ; p ; ?], terminated by [BEL]. The run never waits for the answer; a reply
    arrives as {!constructor:Event.Clipboard} through {!val:Sub.terminal}, and a terminal
    that disallows clipboard reads sends nothing. *)

val raw : string -> 'msg t
(** [raw bytes] writes [bytes] verbatim to the terminal, with no framing, escaping or
    normalization added.

    This is the escape hatch for control sequences the library does not model, such as the
    device-attribute query of a terminal-identification example. The renderer tracks none
    of the state such bytes change: nothing is recorded about the terminal modes or the
    cursor position the bytes set, so the next frame is diffed against what the renderer
    last applied rather than against what [raw] left behind, and only a subsequent
    {!val:Charamel_tea.Cmd.window_size}, resize, or full repaint resynchronizes it. *)

val query :
  [ `Background
  | `Foreground
  | `Cursor_color
  | `Terminal_version
  | `Kitty_flags
  | `Cursor_position
  | `Capability of string ] ->
  'msg t
(** [query kind] asks the terminal for [kind]. The bytes sent are:

    {[
      Background        ESC ] 11 ; ? BEL
      Foreground        ESC ] 10 ; ? BEL
      Cursor_color      ESC ] 12 ; ? BEL
      Terminal_version  ESC [ > q
      Kitty_flags       ESC [ ? u
      Cursor_position   ESC [ 6 n
      Capability name   ESC P + q <hex name> ST
    ]}

    The run never waits for the answer. If the terminal replies, the reply arrives as the
    matching {!constructor:Event.t} constructor through {!val:Sub.terminal}; a terminal
    that does not support the query sends nothing. *)

val window_size : 'msg t
(** [window_size] re-delivers the current terminal size as a resize event. *)
