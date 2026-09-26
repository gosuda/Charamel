(** Lists with selectable markers, nesting, and per-item styles.

    A list is a tree in disguise: its items are the children of an unnamed root, so
    nesting, marker alignment and per-item styling all follow the rules in
    {!charamel.lipgloss.Tree}. Sibling markers are right-aligned to the widest marker of
    the level, and a nested list is indented one level below its parent's marker. *)

type t
(** The type for lists. *)

type item =
  | Text of string
  | Nested of t  (** The type for list items: text, or a list nested one level deeper. *)

type marker =
  [ `Bullet
  | `Dash
  | `Asterisk
  | `Arabic
  | `Alphabet
  | `Roman
  | `Custom of index:int -> string ]
(** The type for markers. [`Custom f] uses [f ~index] for the marker text of each item. *)

val v :
  ?marker:marker ->
  ?indent:(index:int -> string) ->
  ?item_style:(index:int -> Style.t) ->
  ?marker_style:(index:int -> Style.t) ->
  ?indent_style:(index:int -> Style.t) ->
  item list ->
  t
(** [v ?marker ?indent ?item_style ?marker_style ?indent_style items] is a list. The
    marker defaults to [`Bullet], the indent to two spaces, and every style to
    {!val:Style.empty}. Marker text ends in its own spacing; [indent] is the gap an item's
    children are written behind, and [index] is zero-based within the level. *)

val item : item -> t -> t
(** [item new_item list] appends [new_item]. *)

val offset : int -> int -> t -> t
(** [offset start end list] windows the items to the half-open range [start, end],
    swapping an inverted pair and clamping both bounds to the item count. *)

val hide : bool -> t -> t
(** [hide flag list] hides the list, which removes it and its items from a parent render.
*)

val marker_of_string : string -> marker option
(** [marker_of_string name] is the marker named [bullet], [dash], [asterisk], [arabic],
    [alphabet] or [roman], lowercase. [None] for any other name. *)

val render : t -> string
(** [render list] is one line per item, marker first, with multiline items aligned under
    their own text and nested lists indented one level deeper. *)
