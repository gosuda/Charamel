(** Multi-group form layouts.

    Hidden groups are removed before columns or grid pages are calculated. *)

type t = [ `Default | `Stack | `Columns of int | `Grid of int * int ]
type item = { index : int; header : string; content : string; footer : string }

val group_width : t -> width:int -> int
(** [group_width layout ~width] is the content width allocated to one group. *)

val view : t -> width:int -> selected:int -> item list -> string
(** [view layout ~width ~selected groups] assembles the selected page. *)
