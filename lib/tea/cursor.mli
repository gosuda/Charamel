(** The on-screen text cursor an application requests.

    [t] is the cursor a view wants the terminal to show: a position within the view's own
    content grid, a shape, and whether it blinks. *)

type shape = Block | Underline | Bar  (** The type for the cursor's rendered shape. *)

type t = { row : int; col : int; shape : shape; blink : bool }
(** The type for a requested cursor. [row] and [col] are zero-based coordinates within the
    view's own content grid. In the alternate screen that grid fills the terminal, and
    inline it is the view's own lines, independent of where those lines sit on the
    terminal. *)
