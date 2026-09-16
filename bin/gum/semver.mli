(** Semantic-version parsing and constraint matching.

    The parser follows Semantic Versioning 2.0.0. Constraint syntax is the useful
    Masterminds subset used by [gum version]: comparators, wildcards, tilde and caret
    ranges, hyphen ranges, conjunction, and disjunction. *)

type t = {
  major : int;
  minor : int;
  patch : int;
  prerelease : string list;
  build : string list;
}
(** A parsed semantic version. Build metadata does not affect ordering. *)

type version = t
(** [version] is an alias retained for callers that prefer the domain name. *)

type constraint_
(** A parsed semantic-version constraint. *)

val parse : string -> (t, [ `Msg of string ]) result
(** [parse text] parses one Semantic Versioning 2.0.0 value. A leading [v] is accepted. *)

val compare : t -> t -> int
(** [compare a b] orders versions according to Semantic Versioning 2.0.0. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] have equal precedence. *)

val parse_constraint : string -> (constraint_, [ `Msg of string ]) result
(** [parse_constraint text] parses a Masterminds-style constraint expression. *)

val satisfies : constraint_ -> t -> bool
(** [satisfies constraint version] tests whether [version] satisfies the constraint. *)

val check : constraint_ -> t -> bool
(** [check] is an alias for {!satisfies}. *)

val to_string : t -> string
(** [to_string version] formats [version] canonically. *)
