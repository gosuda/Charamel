(** Repeated-character fills.

    A fill is a grapheme pattern cycled to an exact cell width. A grapheme that does not
    fit the remaining cells is replaced by spaces, and a fill that still falls short is
    topped up with spaces, so the result always measures [n] cells. *)

val fill : ?pattern:string -> int -> string
(** [fill ?pattern n] is [n] cells of [pattern] cycled by grapheme. [pattern] defaults to
    a single space; an empty [pattern] is treated as one. Widths of [0] or less give the
    empty string. *)
