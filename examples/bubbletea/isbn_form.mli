(** A two-field form that validates an ISBN-13 and a book title.

    Upstream: [.references/bubbletea/examples/isbn-form/main.go]. The ISBN field is
    focused first. [up] and [down] move focus between the two fields, [enter] quits once
    both values pass their validators, and [esc] or [ctrl+c] quits at any time. The view
    shows [Valid ISBN] or [Valid title] under a field whose value passes, the validator
    message when it does not, and [Find ->] when both pass. The four colors are the
    CharmTone hex values of [Tang], [Anchovy], [Guac] and [Cherry]. The port drops the
    upstream [errMsg] branch, which nothing in the example ever sends, and the cursor
    blink comes from [Textinput.subscriptions] rather than from [Init]. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
