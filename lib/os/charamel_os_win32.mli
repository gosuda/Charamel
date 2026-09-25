(** Raw Win32 console bindings through ctypes-foreign.

    Every value here is a thin, non-blocking-by-construction wrapper over one kernel32
    entry point; no Charamel policy lives at this level. The library is enabled only when
    [%{os_type}] is [Win32], so none of this code exists in a POSIX build, and POSIX
    programs never link libffi for it.

    Handles come from [GetStdHandle] and from [CreateFileW] on [CONIN$]/[CONOUT$]; a
    [Unix.file_descr] is never converted into a handle and a handle is never converted
    into a [Unix.file_descr], because the Windows runtime represents a descriptor as an
    opaque custom block. Both kinds may refer to the same console.

    Failure style: each binding reports [false]/[None]/[[]] on kernel32 failure rather
    than raising, because callers either poll (the resize watcher, the input reader) or
    hold a saved state they must restore even when a later call fails. *)

type handle
(** A [HANDLE] to a console screen buffer or input buffer. *)

type mode = int
(** A [DWORD] console mode bit mask, as accepted by [GetConsoleMode] and [SetConsoleMode].
*)

(** One decoded [INPUT_RECORD]. Fields carry the raw Win32 values: [virtual_key] is a
    [VK_*] code, [wide_char] one UTF-16 code unit (0 when the event carries no character),
    [control_key_state] the [LEFT_CTRL_PRESSED] and related bits, and [repeat] the
    [wRepeatCount] of a key event. *)
type event =
  | Key_event of {
      down : bool;
      repeat : int;
      virtual_key : int;
      scan_code : int;
      wide_char : int;
      control_key_state : int;
    }
  | Buffer_resize of { rows : int; cols : int }
  | Focus_event of { focused : bool }
  | Other_event

val std_input : unit -> handle
(** [std_input ()] is [GetStdHandle(STD_INPUT_HANDLE)], or the [CONIN$] handle when the
    standard handle is not a console. *)

val std_output : unit -> handle
(** [std_output ()] is [GetStdHandle(STD_OUTPUT_HANDLE)], or the [CONOUT$] handle when the
    standard handle is not a console. *)

val get_console_mode : handle -> mode option
(** [get_console_mode h] is the current mode of [h], or [None] when [h] is not a console
    handle ([GetConsoleMode] fails). *)

val set_console_mode : handle -> mode -> bool
(** [set_console_mode h m] applies [m] and reports whether the console accepted it. *)

val console_screen_size : handle -> (int * int) option
(** [console_screen_size h] is [(rows, cols)] of the visible window of the screen buffer
    referenced by [h], or [None] on failure. *)

val get_console_cp : unit -> int
(** [get_console_cp ()] is the current input code page. *)

val get_console_output_cp : unit -> int
(** [get_console_output_cp ()] is the current output code page. *)

val set_console_cp : int -> bool
(** [set_console_cp cp] sets the input code page. *)

val set_console_output_cp : int -> bool
(** [set_console_output_cp cp] sets the output code page. *)

val count_input_events : handle -> int
(** [count_input_events h] is the number of pending [INPUT_RECORD]s, [0] on failure. *)

val read_console_input : handle -> max_events:int -> event list
(** [read_console_input h ~max_events] consumes and returns up to [max_events] pending
    records in arrival order, or [[]] when none are queued or the read fails. *)

val flush_console_input : handle -> unit
(** [flush_console_input h] discards queued input records. *)
