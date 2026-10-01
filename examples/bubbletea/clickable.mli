(** Dialog windows on a composited canvas that the mouse spawns, moves and closes.

    Upstream: [.references/bubbletea/examples/clickable/main.go] and
    [.references/bubbletea/examples/clickable/words.go]. A click on the background spawns
    a window at the cursor. The window names a food drawn from two cycling word lists. A
    drag on a window moves it and raises it above the other windows. Hovering a window
    recolors its border, and hovering its button recolors the button. A click on the Run
    Away button closes that window. [q], [escape] and [ctrl+c] quit. A background-color
    report selects the word color, as upstream does.

    Divergences from upstream follow. The hit test runs in [update] against
    {!Charamel_lipgloss.Compositor.hit}, because the runtime view carries no mouse
    callback for the hit to travel through. Painting walks the same layer tree one cell at
    a time, because a {!Charamel_lipgloss.Canvas} draw repaints the rest of the row and
    would erase the windows behind it. The button layer carries the [z] of its parent
    window, because layers are ordered by [z] alone. The word lists cycle in source order,
    because upstream shuffles them once with a randomly seeded generator. The word cursor
    and the window identifiers are model state, because upstream keeps the lists in
    package globals and draws ksuid values. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
