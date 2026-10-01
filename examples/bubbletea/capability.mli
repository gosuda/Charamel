(** A prompt that queries the terminal for one named capability.

    Upstream: [.references/bubbletea/examples/capability/main.go]. Type a capability name,
    such as [TN] or [RGB], and press [enter] to send an XTGETTCAP request. The input
    resets after the request. The terminal's answer is printed above the view as
    ["Got capability: <value>"]. [ctrl+c] and [esc] quit. A negative answer prints an
    empty value, because the runtime's capability event carries only the value. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
