(** Random slug-style name generation over {!Words.modifiers} and {!Words.nouns}.

    A name of [tokens] words is [tokens - 1] independently-drawn modifiers followed by one
    noun, lowercased and joined with [separator] (upstream's own transform, generalized
    from a hardcoded ["-"] to a caller-chosen separator. Join with a space, lowercase,
    replace every space with [separator]).

    Word selection draws bytes through the caller-supplied [random] function and rejects
    the biased tail of each draw so every entry of a word list is equally likely; it never
    reduces a raw draw with plain [mod]. Entropy is an explicit input, never a global.
    [random n] MUST return exactly [n] freshly-drawn random bytes, e.g.
    [Mirage_crypto_rng.generate]. This module has no RNG dependency of its own; the caller
    owns seeding. *)

val generate : random:(int -> string) -> separator:string -> tokens:int -> unit -> string
(** [generate ~random ~separator ~tokens ()] draws one name of [tokens] words joined by
    [separator].

    @raise Invalid_argument if [tokens < 1]. Validation happens before [random] is called.
*)

val generate_many :
  random:(int -> string) ->
  count:int ->
  separator:string ->
  tokens:int ->
  unit ->
  string list
(** [generate_many ~random ~count ~separator ~tokens ()] draws [count] independent names
    via {!generate}, returned in draw order. [count = 0] is a reasonable input and returns
    [[]] without calling [random].

    @raise Invalid_argument
      if [count < 0] or [tokens < 1]. Validation happens before [random] is called. *)
