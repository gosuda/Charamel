(** Fuzzy matching for item lists.

    Scores use scalar positions and the matching constants from [sahilm/fuzzy]. The
    returned [matched] positions are converted once to unique extended-grapheme indices,
    which callers can pass directly to [Charamel_lipgloss.Layout.style_runes]. *)

type match_ = { index : int; matched : int list; score : int }
(** A candidate's source index, grapheme positions selected in it, and score. *)

val find : pattern:string -> string list -> match_ list
(** [find ~pattern candidates] returns candidates containing every scalar in [pattern],
    sorted by descending score with stable ordering for ties. An empty pattern returns the
    empty list. *)

val find_unsorted : pattern:string -> string list -> match_ list
(** [find_unsorted ~pattern candidates] computes the same matches in input order without
    ranking them. An empty pattern returns the empty list. *)
