(** A twelve-page item list driven by the paginator bubble.

    Upstream: [.references/bubbletea/examples/paginator/main.go]. The view shows ten of
    one hundred items. [h], [left] and [page_up] go to the previous page. [l], [right] and
    [page_down] go to the next page. [q], [escape] and [ctrl+c] quit. The runtime reports
    the terminal background through [Sub.terminal], so the colored dots are rebuilt from
    the [Background_color] event instead of the Go [BackgroundColorMsg]. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
