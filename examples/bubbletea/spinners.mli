(** Nine spinner styles, switched with the arrow keys.

    Upstream: [.references/bubbletea/examples/spinners/main.go]. The view shows one
    spinner and the word [Spinning...]. [h] and [left] select the previous spinner and [l]
    and [right] the next one; the list wraps at both ends. Each switch rebuilds the
    spinner, so its first frame shows again. [q], [esc] and [ctrl+c] quit. The port ticks
    the spinner through [Spinner.subscriptions] because this component exposes no [Tick]
    command. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
