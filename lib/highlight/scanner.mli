(** Lossless lexical scanning for {!Charm_highlight.Spec.t} values. *)

val tokenize : Spec.t -> string -> (Spec.kind * string) list
(** [tokenize spec source] returns the longest-match token stream for [source]. Token
    texts are byte-exact slices whose concatenation is [source]. Delimiters are allowed to
    remain unterminated and then consume the remainder of the input.

    The scanner is prepared on first use and memoized: a given [spec] is compiled at most
    once per domain and its record is reused on later calls while the [spec] is alive. The
    memo table is domain-local, so no compiled value (and no [Re.re] inside it) is ever
    shared across domains, and it is keyed weakly on physical identity, so an entry is
    collected together with its specification instead of being retained. *)
