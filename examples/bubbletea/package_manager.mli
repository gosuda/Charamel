(** A fake package installer with a spinner and a progress bar.

    Upstream: [.references/bubbletea/examples/package-manager/main.go] and
    [.references/bubbletea/examples/package-manager/packages.go]. The program installs
    twenty nine packages in turn, prints one checked line per package above the view, and
    quits by itself after the last one. [q], [esc] and [ctrl+c] quit early. Three
    divergences: the upstream shuffle and random version suffixes become source order and
    index-derived suffixes so the smoke is deterministic; the random [0..500ms] install
    delay is a fixed [250ms]; and the mid-run smoke scripts end with [q] because a pending
    install command otherwise runs to the next package before the frame is recorded. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
