(** Border glyphs for the thirteen frame positions. *)

type t = {
  top : string;
  bottom : string;
  left : string;
  right : string;
  top_left : string;
  top_right : string;
  bottom_left : string;
  bottom_right : string;
  middle_left : string;
  middle_right : string;
  middle : string;
  middle_top : string;
  middle_bottom : string;
}

val normal : t
val rounded : t
val block : t
val outer_half_block : t
val inner_half_block : t
val thick : t
val double : t
val hidden : t
val markdown : t
val ascii : t
val none : t
val top_size : t -> int
val right_size : t -> int
val bottom_size : t -> int

val left_size : t -> int
(** [left_size t] is the widest cell width among the three glyphs on the left edge. Its
    sibling size functions measure their own edge the same way. *)
