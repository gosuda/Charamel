(** A frame drawn under a forced true-colour profile.

    Upstream: [.references/bubbletea/examples/colorprofile/main.go]. The run overrides the
    detected colour profile with true colour, so the foreground of [Howdy!] stays at
    [#6b50ff] on a terminal that would otherwise reduce it. Any key press quits. The port
    asks for the [RGB] and [Tc] capabilities as upstream does but ignores the answers,
    because this runtime reports a profile change as a capability reply rather than as a
    colour-profile message. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
