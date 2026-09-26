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
  | Set_clipboard of string
  | Query of
      [ `Background
      | `Foreground
      | `Cursor_color
      | `Terminal_version
      | `Kitty_flags
      | `Cursor_position ]
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
    printing, setting the clipboard, or querying the terminal, are unaffected by [f]. *)

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
(** [suspend] restores the terminal and suspends the process. *)

val exec : argv:string list -> (int -> 'msg) -> 'msg t
(** [exec ~argv on_exit] releases the terminal, runs the process named by [argv] attached
    to the tty, restores the terminal, and delivers [on_exit code] with the process's exit
    code. *)

val print : string -> 'msg t
(** [print s] writes [s] above the view in inline mode. In alt-screen mode [s] is queued
    and written when the program exits. *)

val set_clipboard : string -> 'msg t
(** [set_clipboard s] sets the terminal clipboard to [s]. *)

val query :
  [ `Background
  | `Foreground
  | `Cursor_color
  | `Terminal_version
  | `Kitty_flags
  | `Cursor_position ] ->
  'msg t
(** [query kind] asks the terminal for [kind]. The reply arrives as an [Event.t] delivered
    through {!val:Sub.terminal}. *)

val window_size : 'msg t
(** [window_size] re-delivers the current terminal size as a resize event. *)
