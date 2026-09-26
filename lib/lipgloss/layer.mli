(** A positioned piece of content inside a composition.

    A layer holds rendered text and where it sits: [x] and [y] place it relative to its
    parent, [z] orders it against its siblings and descendants for drawing and hit
    testing, and [id] names it so a hit can be traced back to it. Geometry is never
    stored: a layer's size is the size of its content, measured escape-aware. *)

type t
(** The type for layers. *)

val v : ?id:string -> ?x:int -> ?y:int -> ?z:int -> string -> t
(** [v ?id ?x ?y ?z content] is a layer holding [content]. The identifier defaults to the
    empty string, which marks a layer that cannot be hit or found, and the coordinates
    default to [0]. *)

val of_content : string -> t
(** [of_content content] is {!val:v} with no identifier and no offset. *)

val add : t -> t list -> t
(** [add layer children] appends [children] to [layer]'s existing children. *)

val children : t -> t list
(** [children layer] is the child layers, in the order they were added. *)

val content : t -> string
(** [content layer] is the text the layer paints. *)

val id : t -> string
(** [id layer] is the layer's identifier, possibly empty. *)

val x : t -> int
(** [x layer] is the horizontal offset from the parent. *)

val y : t -> int
(** [y layer] is the vertical offset from the parent. *)

val z : t -> int
(** [z layer] is the stacking order relative to its siblings. *)

val find : t -> string -> t option
(** [find layer id] is the first layer of [layer] and its descendants with that
    identifier, depth first. [None] when [id] is empty or no layer carries it. *)

val max_z : t -> int
(** [max_z layer] is the greatest [z] of [layer] and its descendants. *)
