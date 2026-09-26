(** An editor that asks before it throws away unsaved work.

    Upstream: [.references/bubbletea/examples/prevent-quit/main.go]. Typing marks the
    buffer as changed. [ctrl+s] clears that mark and says [Changes saved!]. [esc] and
    [ctrl+c] request the quit; with unsaved changes the request is refused and the frame
    asks [You have unsaved changes. Quit without saving?], where [y] or another quit key
    exits and any other key returns to the editor. Without changes the same keys print
    [Very important. Thank you.] and exit. Upstream refuses the quit through a program
    filter; this port applies the same check at the head of [update] so the scripted smoke
    runs the same path. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
