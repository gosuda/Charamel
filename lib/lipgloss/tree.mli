(** Immutable trees rendered with branch and indentation glyphs.

    A tree is a root value with children, and children are trees, so any node can carry
    its own rendering settings. Rendering walks the visible children of each node: a
    hidden node contributes nothing, and the last visible sibling is the one that gets the
    closing branch glyph. *)

type t
(** The type for trees. *)

type enumerator = depth:int -> index:int -> last:bool -> string
(** The type for branch glyphs. [depth] is the depth of the node being enumerated, [index]
    its position among the visible siblings, and [last] whether that position is final. *)

type indenter = depth:int -> index:int -> last:bool -> string
(** The type for indentation glyphs, applied below a node to continue its branch. *)

type style_func = depth:int -> index:int -> value:string -> Style.t
(** The type for per-node styles. *)

type t_style = {
  root : Style.t;
  item : style_func;
  enumerator : style_func;
  indenter : style_func;
}
(** The type for the four style roles: the root value, item values, branch glyphs and
    indentation glyphs. *)

val leaf : ?hidden:bool -> string -> t
(** [leaf ?hidden value] is a node with no children. [hidden] defaults to [false]. *)

val node :
  ?value:string -> ?hidden:bool -> ?offset:int * int -> ?children:t list -> unit -> t
(** [node ?value ?hidden ?offset ?children ()] is a node. The value defaults to the empty
    string, which contributes no line of its own. [offset = (start, end)] windows the
    children to the half-open range [start, end]: the pair is swapped when inverted and
    each bound is clamped to the child count. *)

val root : string -> t -> t
(** [root value tree] replaces [tree]'s value, keeping its children and settings. *)

val style : t -> t_style -> t
(** [style tree roles] sets the tree's style roles. A node with its own styles renders its
    subtree with them; a node without them keeps the styles in force above it. *)

val width : int -> t -> t
(** [width cells tree] pads every line of [tree]'s subtree to [cells] columns, root line
    included. *)

val hidden : t -> bool
(** [hidden tree] is [true] when the node contributes nothing to a render. *)

val hide : bool -> t -> t
(** [hide flag tree] sets the node's hidden flag. *)

val children : t -> t list
(** [children tree] is the node's children, before any offset or hidden filtering. *)

val value : t -> string
(** [value tree] is the node's value. *)

val enumerate : [ `Default | `Rounded | `Custom of enumerator ] -> t -> t
(** [enumerate kind tree] sets the branch glyphs: [`Default] is [├──] and [└──],
    [`Rounded] closes with [╰──], and [`Custom f] uses [f]. *)

val indent : [ `Default | `Custom of indenter ] -> t -> t
(** [indent kind tree] sets the indentation glyphs. *)

val render : ?enumerator:enumerator -> ?indenter:indenter -> t -> string
(** [render ?enumerator ?indenter tree] formats [tree]. The root value has depth [0]; each
    child receives its depth, its index among the visible siblings, and its [last] flag.
    [enumerator] and [indenter] override the root node's settings for the whole render.
    Hidden nodes and their subtrees contribute nothing, and a node with no value
    contributes no line of its own. *)
