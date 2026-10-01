(** A scrollable table of the hundred largest cities.

    Upstream: [.references/bubbletea/examples/table/main.go]. The table has four columns,
    a height of seven lines and a width of forty-two cells, and it starts focused. [↑/k]
    and [↓/j] move one row. [b/pgup] and [f/pgdn] move one page. [u] and [d] move half a
    page. [g/home] and [G/end] jump to the first and last row. [enter] prints
    ["Let's go to <City>!"] above the view. [esc] toggles focus, and a blurred table
    ignores the movement keys. [q] and [ctrl+c] quit. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
