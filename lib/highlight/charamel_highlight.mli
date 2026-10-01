(** Data-driven syntax highlighting.

    The bundled lexers are byte-preserving and use regular expressions only for token
    starts. Delimiters are matched by the scanner so unterminated input remains lossless.
*)

type spec = Spec.t = {
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
(** A complete language lexer specification. *)

type kind = Spec.kind =
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
  | Text  (** The token classes emitted by {!tokenize}. *)

val tokenize : spec -> string -> (kind * string) list
(** [tokenize spec source] returns a lossless, longest-match token stream. Each [spec] is
    compiled at most once per domain and reused while it is alive; the memo table is
    domain-local and weakly keyed, so compiled state never crosses domains and never
    outlives its specification. *)

val find : string -> spec option
(** [find name] resolves a language alias, extension, or filename extension. Matching is
    ASCII case-insensitive and ignores a leading dot. *)

val languages : spec list
(** [languages] is the ordered list of all 26 bundled language specifications. *)

module Theme : module type of Theme

val render : ?theme:Theme.t -> spec -> string -> string
(** [render ?theme spec source] applies token styles to [source]. The default is the dark
    Charm palette; an empty or unknown style leaves token bytes unchanged. Tabs become
    four spaces and CRLF endings become LF, so every emitted run holds cells that a
    terminal or a grid layout can measure. No other source byte is rewritten. *)
