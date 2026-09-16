(** Flat lists with selectable enumerator styles. *)

type enumerator = [ `Bullet | `Dash | `Asterisk | `Arabic | `Alphabet | `Roman ]
type t

val v : ?enumerator:enumerator -> string list -> t
(** [v ?enumerator items] constructs a flat list. The default enumerator is [`Bullet].
    Alphabetic markers are uppercase and roll over after [Z]; Roman markers are
    right-aligned to the widest marker. Nested lists, transforms, and per-item styles are
    outside this API. *)

val render : t -> string
(** [render list] renders one item per line. *)
