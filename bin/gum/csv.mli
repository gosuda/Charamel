(** RFC 4180 CSV parsing and encoding used by [gum table]. *)

type fields_per_record = { expected : int; actual : int; row : int }
(** Field-count details reported by [`Fields_per_record]. *)

type error =
  [ `Invalid_data of string
  | `Invalid_separator
  | `Fields_per_record of fields_per_record ]
(** Errors reported by [parse]. *)

val parse :
  ?separator:char ->
  ?lazy_quotes:bool ->
  ?fields_per_record:int ->
  string ->
  (string list list, error) result
(** [parse ?separator ?lazy_quotes ?fields_per_record input] parses [input]. UTF-8 BOM
    bytes are ignored at the beginning. Quoted fields may contain separators, CRLF and LF;
    a quote inside a quoted field is doubled. When [lazy_quotes] is true, otherwise
    malformed quotes are treated literally. [fields_per_record] defaults to [0], which
    infers the width from the first record and requires subsequent records to have that
    width; a negative value accepts variable widths. *)

val write_row : separator:char -> string list -> string
(** [write_row ~separator fields] encodes one RFC 4180 record followed by LF. Fields
    containing the separator, quotes, CR, LF, or leading/trailing spaces are quoted and
    embedded quotes are doubled. *)

val error_message : error -> string
(** [error_message e] formats [e] for a command diagnostic. *)
