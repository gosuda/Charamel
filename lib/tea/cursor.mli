(** The on-screen text cursor an application requests.

    [t] is the cursor a view wants the terminal to show: a position within the view's own
    content grid, a shape, whether it blinks, and an optional color. *)

type shape = Block | Underline | Bar  (** The type for the cursor's rendered shape. *)

type t = {
  row : int;
  col : int;
  shape : shape;  (** The requested cursor shape. *)
  blink : bool;  (** Whether the requested cursor blinks. *)
  color : Charamel_ansi.Color.t option;
      (** The color the terminal's cursor is set to while this cursor shows, or [None] to
          leave the terminal's current cursor color alone. Only {!Charamel_ansi.Color.Rgb}
          colors are applied, as OSC 12; other color spaces have no portable OSC 12
          spelling and are silently not sent, like the view's default foreground and
          background. *)
}
(** The type for a requested cursor. [row] and [col] are zero-based coordinates within the
    view's own content grid. In the alternate screen that grid fills the terminal, and
    inline it is the view's own lines, independent of where those lines sit on the
    terminal. *)

val v : ?shape:shape -> ?blink:bool -> ?color:Charamel_ansi.Color.t -> int -> int -> t
(** [v row col] is a blinking block cursor at [row], [col] showing the terminal's own
    cursor color. [shape] defaults to [Block], [blink] to [true], and [color] to [None];
    [~color:c] requests [c]. The coordinates are the final positional arguments, as in
    {!View.v}, so any of the three channels may be left off. *)
