(** SC-02 goal harness case.

    [run ()] pipes [b\n] into the real [gum choose --select-if-one] binary and requires
    the exact output [b\n] on stdout, nothing on stderr, and exit code 0. It fails on any
    other status or bytes. *)

val run : unit -> unit
