(** Unicode helpers for editable text components.

    The functions preserve grapheme-aware editing semantics. *)

val cluster_width : string -> int
(** [cluster_width cluster] is the terminal width of the grapheme [cluster]. *)

val take : int -> 'a list -> 'a list
(** [take n xs] is the first [n] elements of [xs], or all of [xs] when it is shorter. *)

val drop : int -> 'a list -> 'a list
(** [drop n xs] is [xs] without its first [n] elements, or [[]] when it is shorter. *)

val lower : string -> string
(** [lower text] is [text] converted to Unicode lowercase. *)

val upper : string -> string
(** [upper text] is [text] converted to Unicode uppercase. *)

val sanitize :
  replace_tabs:string ->
  replace_newlines:string ->
  normalize_crlf:bool ->
  string ->
  string
(** [sanitize ~replace_tabs ~replace_newlines ~normalize_crlf text] is [text] with invalid
    UTF-8 bytes and control characters removed. Tabs are replaced by [replace_tabs].
    Newline and carriage-return characters are replaced by [replace_newlines].
    [normalize_crlf] suppresses the carriage return in a carriage-return and newline pair
    when it is [true]. *)

val whitespace_cluster : string -> bool
(** [whitespace_cluster cluster] is [true] if [cluster] starts with a Unicode whitespace
    character. *)

val is_cjk_cluster : string -> bool
(** [is_cjk_cluster cluster] is [true] when the first scalar of [cluster] is written in
    the Han, Hangul, Hiragana, or Katakana script. *)

val word_class : string -> [ `Space | `Cjk | `Other ]
(** [word_class cluster] is the editing word class of [cluster]: [`Space] for whitespace,
    [`Cjk] for a CJK cluster, and [`Other] otherwise. A word is a maximal run of clusters
    of one class, so word motion stops between [`Cjk] and [`Other]. *)
