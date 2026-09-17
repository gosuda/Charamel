(** Small JSON codec helpers.

    [Jsonx] centralizes Jsont byte codecs and safe object-member queries. It keeps JSON
    [null] distinct from an absent member: [member] returns [None] only when the object
    has no such member (or is not an object). *)

val json_of_string : string -> (Jsont.json, string) result
(** [json_of_string text] parses [text] as one JSON value. *)

val string_of_json : ?minify:bool -> Jsont.json -> string
(** [string_of_json ?minify json] encodes [json] without a trailing newline. [minify]
    defaults to [true]. *)

val display_string : Jsont.json -> string
(** [display_string json] is the minified encoding of [json], or ["<invalid-json>"] when
    [json] cannot be encoded. *)

val decode : 'a Jsont.t -> string -> ('a, string) result
(** [decode codec text] parses and decodes [text] with [codec]. *)

val encode : ?minify:bool -> 'a Jsont.t -> 'a -> string
(** [encode ?minify codec value] encodes [value] with [codec]. Encoding errors are
    reported as [Invalid_argument]. *)

val member : string -> Jsont.json -> Jsont.json option
(** [member name json] returns the named member when [json] is an object. *)

val string_member : string -> Jsont.json -> string option
(** [string_member name json] returns a string member, or [None] for an absent member,
    JSON [null], or a member of another type. *)

val int_member : string -> Jsont.json -> int option
(** [int_member name json] returns an integer-valued number member. *)

val bool_member : string -> Jsont.json -> bool option
(** [bool_member name json] returns a boolean member. *)
