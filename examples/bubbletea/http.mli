(** One HTTP request whose status the program prints when it arrives.

    Upstream: [.references/bubbletea/examples/http/main.go]. [GET https://charm.sh/] runs
    as the program's only command; the reply status replaces "Checking…" and quits. [q],
    [escape] and [ctrl+c] quit early. A transport failure is shown on the same line and
    the program keeps running, as upstream does.

    The request is a function passed to the application, so the smoke resolves a canned
    status and never touches the network. Upstream prints Go's [http.StatusText]; this
    port prints [Cohttp.Code.string_of_status], the same reason phrase from the HTTP
    registry. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
