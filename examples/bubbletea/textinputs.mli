(** Three text inputs and a submit button, cycled with tab.

    Upstream: [.references/bubbletea/examples/textinputs/main.go]. [tab], [enter] and
    [down] move focus forward; [shift+tab] and [up] move it back. Focus wraps from the
    button to the first input. [enter] on the button quits; [esc] and [ctrl+c] quit at any
    time. [ctrl+r] cycles the cursor mode between blink, static and hide. The third input
    masks its value with bullets. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
