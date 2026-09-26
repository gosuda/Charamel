(** Language lexer specifications.

    A specification describes the words, delimiters, regular expressions, and aliases
    consumed by {!Charamel_highlight.Scanner}. *)

type kind =
  | Keyword
  | Type
  | Builtin
  | Constant
  | String
  | Number
  | Comment
  | Operator
  | Punct
  | Ident
  | Attribute
  | Text  (** The token classes emitted by a lexer. *)

type t = {
  names : string list;
  keywords : string list;
  types : string list;
  builtins : string list;
  constants : string list;
  line_comment : string list;
  block_comment : (string * string) list;
  strings : (string * string * bool) list;
  raw_strings : (string * string) list;
  number : Re.t;
  ident : Re.t;
  operators : string list;
  attribute : Re.t option;
  case_sensitive : bool;
}
(** A complete lexer specification. *)

val make_spec :
  names:string list ->
  ?keywords:string list ->
  ?types:string list ->
  ?builtins:string list ->
  ?constants:string list ->
  ?line_comment:string list ->
  ?block_comment:(string * string) list ->
  ?strings:(string * string * bool) list ->
  ?raw_strings:(string * string) list ->
  ?operators:string list ->
  ?attribute:Re.t option ->
  ?case_sensitive:bool ->
  number:Re.t ->
  ident:Re.t ->
  unit ->
  t
(** [make_spec ~names ~number ~ident ()] builds a lexer specification. Optional lists
    default to empty, [attribute] defaults to [None], and [case_sensitive] defaults to
    [true]. *)

val not_chars : string -> Re.t
(** [not_chars chars] matches one byte not present in [chars]. *)

val hex_literal : ?suffix:Re.t -> unit -> Re.t
(** [hex_literal ~suffix ()] matches a hexadecimal integer literal: the [0x] prefix and at
    least one hex digit, optionally followed by the text that [suffix] matches. Without
    [suffix] the literal ends after its digits. *)

val decimal : Re.t
(** [decimal] matches a decimal integer or floating-point literal with separators. *)

val decimal_signed : Re.t
(** [decimal_signed] also accepts an optional leading sign. *)

val float_number : Re.t
(** [float_number] matches decimal integers and floating-point literals. *)

val integer_number : Re.t
(** [integer_number] matches digits and underscore separators. *)

val c_number : Re.t
(** [c_number] matches C-family integer and decimal literals. *)

val python_number : Re.t
(** [python_number] matches Python integer, floating-point, and imaginary literals. *)

val rust_number : Re.t
(** [rust_number] matches Rust integer and floating-point literals with suffixes. *)

val json_number : Re.t
(** [json_number] matches the JSON number grammar. *)

val css_number : Re.t
(** [css_number] matches CSS colors, dimensions, and numeric literals. *)

val ident_tail : Re.t
(** [ident_tail] matches an ASCII identifier continuation. *)

val identifier : Re.t
(** [identifier] matches an ASCII identifier. *)

val identifier_dash : Re.t
(** [identifier_dash] also permits hyphens in an identifier. *)

val identifier_dollar : Re.t
(** [identifier_dollar] also permits dollar signs in an identifier. *)
