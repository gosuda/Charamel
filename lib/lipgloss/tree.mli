(** Immutable trees rendered with branch and indentation glyphs. *)

type t
type enumerator = depth:int -> index:int -> last:bool -> string
type indenter = depth:int -> last:bool -> string

val leaf : string -> t
(** [leaf text] is a node with no children. *)

val node : ?value:string -> t list -> t
(** [node ?value children] is a node. The default value is the empty string. *)

val render :
  ?enumerator:enumerator ->
  ?indenter:indenter ->
  ?style:(depth:int -> string -> Style.t) ->
  t ->
  string
(** [render ?enumerator ?indenter ?style tree] formats [tree]. The root value has depth
    [0]; each child receives its zero-based index and [last] flag. Branch glyphs are not
    styled, while node values are passed to [style]. Empty root values are omitted.
    Hidden/filtering/offset and nested list features from upstream are outside this API.
*)
