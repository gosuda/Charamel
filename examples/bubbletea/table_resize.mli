(** A pokemon table that follows the terminal size.

    Upstream: [.references/bubbletea/examples/table-resize/main.go]. The table is built by
    the lipgloss table builder with thick borders, and every resize rebuilds it at the
    full terminal width and height, so narrow terminals truncate cells. [q] and [ctrl+c]
    quit; [enter] does nothing, as upstream. Two divergences: the port of
    [Charamel_lipgloss.Table] takes no border style, so the gray border color of upstream
    is not applied, and the table is rebuilt on resize because that interface is immutable
    where the Go builder is mutable. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
