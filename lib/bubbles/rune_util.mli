(** Unicode helpers for editable text components.

    The functions preserve grapheme-aware editing semantics. *)

val clamp : int -> int -> int -> int
(** [clamp n lo hi] is [n] limited to the inclusive interval [[lo], [hi]]. *)

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
  ?replace_tabs:string ->
  ?replace_newlines:string ->
  ?normalize_crlf:bool ->
  string ->
  string
(** [sanitize ?replace_tabs ?replace_newlines ?normalize_crlf text] is [text] with invalid
    UTF-8 bytes and control characters removed. Tabs are replaced by [replace_tabs].
    Newline and carriage-return characters are replaced by [replace_newlines].
    [replace_tabs] defaults to four spaces. [replace_newlines] defaults to a newline.
    [normalize_crlf] defaults to [false] and suppresses the carriage return in a
    carriage-return and newline pair when it is [true]. *)

val whitespace_cluster : string -> bool
(** [whitespace_cluster cluster] is [true] if [cluster] starts with a Unicode whitespace
    character. *)
