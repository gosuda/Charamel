(** Terminal display widths.

    [Width] measures text in terminal cells. A string is segmented into extended grapheme
    clusters and each cluster is measured by its base scalar. Emoji presentation,
    variation selectors, regional indicator pairs and halfwidth voiced sound marks are
    measured by their own rules. Malformed UTF-8 input is replaced by U+FFFD before
    measuring. *)

val string_width : string -> int
(** [string_width s] is the number of terminal cells [s] occupies when rendered on a line.
    Control characters occupy no cell. [s] is measured as given, so escape sequences are
    not recognised and the printable bytes of a sequence count as text. See
    [Charamel_ansi.Text] for escape-aware measurement. *)

val grapheme_width : string -> int
(** [grapheme_width s] is the number of terminal cells occupied by the first grapheme
    cluster of [s], or [0] when [s] is empty. A string holding more than one cluster
    measures only its first cluster. *)

val graphemes : string -> string list
(** [graphemes s] is the grapheme clusters of [s] in order. For valid UTF-8 input the
    concatenation of the clusters is [s]. *)
