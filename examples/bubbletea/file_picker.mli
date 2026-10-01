(** A file picker restricted to source and text files.

    Upstream: [.references/bubbletea/examples/file-picker/main.go]. [j] and [k] move the
    cursor, [g] and [G] jump to the first and last entry, [h] goes back a directory and
    [enter] opens or picks the highlighted entry. Only [.mod], [.sum], [.go], [.txt] and
    [.md] files are selectable and directories are not, so a rejected pick reports
    ["<path> is not valid."] for two seconds. [q] and [ctrl+c] quit and the program prints
    the chosen path afterwards, so [main] returns through [Smoke.run]. [main] lists
    [$HOME], as upstream does; the smoke lists a temporary fixture directory it creates
    and removes, because a host directory would make the frames depend on the machine. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
