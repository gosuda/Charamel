(** Group state and field navigation helpers.

    A group owns field instances and a cursor. Form supplies field contexts and remains
    the owner of inter-group movement. *)

type t = {
  title : string;
  description : string;
  hide : (Results.t -> bool) option;
  show_help : bool;
  show_errors : bool;
  fields : Field_impl.t array;
  selected : int;
  active : bool;
  width : int;
  height : int;
  y_offset : int;
}

val v :
  ?title:string ->
  ?description:string ->
  ?hide:(Results.t -> bool) ->
  ?show_help:bool ->
  ?show_errors:bool ->
  Field_impl.Field.t list ->
  t
(** [v fields] creates a group. Empty groups are always hidden. *)

val is_hidden : results:Results.t -> t -> bool
val selected : t -> int
val set_selected : int -> t -> t
val active : t -> bool
val set_active : bool -> t -> t
val width : t -> int
val height : t -> int
val set_size : width:int -> height:int -> t -> t
val y_offset : t -> int
val set_y_offset : int -> t -> t
val field_count : t -> int
val field : int -> t -> Field_impl.t
val set_field : int -> Field_impl.t -> t -> t
val focused_field : t -> Field_impl.t option

val visible_indices : skip:(int -> bool) -> t -> int list
(** [visible_indices ~skip g] lists field indices that can receive focus. *)

val first_index : skip:(int -> bool) -> t -> int option
val last_index : skip:(int -> bool) -> t -> int option
val next_index : skip:(int -> bool) -> t -> int -> int option
val previous_index : skip:(int -> bool) -> t -> int -> int option

val errors : t -> string list
(** [errors g] returns stored field errors in field order. *)

val content_lines : separator:string -> (int * string) list -> string
(** [content_lines ~separator views] joins field views in order. *)
