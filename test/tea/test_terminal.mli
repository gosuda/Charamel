(** Terminal transport boundary tests.

    The cases cover custom-flow ownership, local environment fallbacks, and raw-mode
    restoration on a real pseudoterminal. *)

val buffer_output : Buffer.t -> Lwt_io.output_channel
(** [buffer_output buffer] is an output channel that appends every byte to [buffer]. *)

val run_pty_child : unit -> 'a
(** [run_pty_child ()] is the second entry point of this executable: the process the
    pseudoterminal case starts runs one Tea program on its own terminal, reports the
    termios it observed, and exits. *)

val cases : unit Alcotest.test_case list
(** [cases] is the set of terminal transport boundary tests. *)
