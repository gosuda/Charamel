(** A text input with completions fetched from the Charm repos.

    Upstream: [.references/bubbletea/examples/autocomplete/main.go]. The program fetches
    the repo names at startup and installs them as completions. The input shows the
    matching completion inline while you type. [tab] accepts the selected completion.
    [ctrl+n] and [ctrl+p] move through the matches. The help line lists those keys only
    while two matches or more exist. [enter], [esc] and [ctrl+c] quit. Upstream batches a
    cursor blink command into [init]. The port focuses the input instead, because the port
    uses the real terminal cursor. Upstream drops fetch errors and keeps the waiting view.
    The port keeps that behavior. Upstream parses the JSON with [encoding/json]. [yojson]
    is not a dependency of this library, so the port collects the ["name"] fields with a
    small scanner. Upstream offsets the terminal cursor by the header height. The port
    passes the text input cursor unchanged. The smoke stubs the fetch, so it never touches
    the network. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
