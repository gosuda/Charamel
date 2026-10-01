(** A progress bar that animates to each new percentage.

    Upstream: [.references/bubbletea/examples/progress-animated/main.go]. The target
    percentage grows by 25 points each second and the bar eases toward it. The program
    quits on any key press, or by itself once the bar reaches 100 percent. [incr_percent]
    updates the model only, where the Go method also returns a command, because the
    animation frames arrive through [Progress.subscriptions] instead. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
