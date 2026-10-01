(** Escape-aware text algorithms.

    A string holds raw terminal bytes: printable text mixed with escape sequences. Every
    function in this module separates the two, so a transformation cannot corrupt a
    sequence and a sequence cannot make the result occupy the wrong number of cells.
    Sequences are preserved byte for byte, line breaks never fall inside a sequence or a
    grapheme cluster, and cell measurements are grapheme based.

    The recognized sequence families are the control sequences of {!Charamel_ansi.Parser}:
    an [ESC] or C1 introducer followed by the bytes its final byte, and the string
    sequences [DCS], [SOS], [OSC], [PM] and [APC] whose payload ends at [BEL] (OSC only),
    [ST], [CAN] or [SUB]. Bytes that begin no complete sequence are kept as written.
    [strip] removes whole sequences and keeps control characters, the wrapping and
    truncation functions keep every sequence in place.

    [width] is the escape-aware counterpart of {!Width.string_width}, which measures
    escape bytes as text. *)

val width : string -> int
(** [width s] is the number of cells [s] occupies when rendered on one line. Escape
    sequences occupy no cell. Line feeds and other control characters occupy no cell
    either, so [width] of a multi-line string is the sum of its line widths. *)

val strip : string -> string
(** [strip s] is [s] without its escape sequences. Control characters [0x00]-[0x1f] and
    [0x7f]-[0x9f] that are not part of a sequence are kept. Bytes inside a string sequence
    payload are removed together with the sequence. *)

val truncate : ?tail:string -> width:int -> string -> string
(** [truncate ~width s] is [s] cut to at most [width] cells, counting only text cells.
    [tail] is appended inside the sequence state that was open when the cut happened. The
    result is never wider than [width] cells of text and carries every sequence that
    follows the cut point, including reset sequences. Defaults: [tail] is empty. When [s]
    fits [width] the result is [s]. A budget that stays negative after subtracting the
    tail width produces the empty string. *)

val truncate_left : ?prefix:string -> width:int -> string -> string
(** [truncate_left ~width s] is [s] without its first [width] cells of text. [prefix] is
    inserted inside the sequence state that was open at the cut, so a color opened before
    the cut colors the prefix. Defaults: [prefix] is empty. [width] of [0] or less is the
    identity. Trailing sequences of [s] are preserved. *)

val cut : left:int -> right:int -> string -> string
(** [cut ~left ~right s] is the substring of [s] from text cell [left] inclusive to text
    cell [right] exclusive, with the sequences that surround and separate the two bounds
    kept. [right] of [0] or less than [left] yields the empty string. [left] of [0] or
    less truncates from the right only. *)

val hardwrap : ?preserve_space:bool -> width:int -> string -> string
(** [hardwrap ~width s] breaks [s] into lines of at most [width] cells, breaking at
    whatever cell the limit falls on. A cluster that does not fit on the current line
    starts a new line, and a cluster wider than [width] occupies its own line without
    leaving an empty line behind. Spaces at the start of a wrapped line are dropped unless
    [preserve_space] holds. [width] below [1] is the identity. Defaults: [preserve_space]
    is [false]. *)

val wordwrap : ?breakpoints:string -> width:int -> string -> string
(** [wordwrap ~width s] breaks [s] into lines of at most [width] cells, breaking at spaces
    so no word is split. Non-breaking space never ends a word. A character of
    [breakpoints] ends a word and stays attached to the line that holds the text before
    it. Words wider than [width] are never split. [width] below [1] is the identity.
    Defaults: [breakpoints] is empty. *)

val wrap : ?breakpoints:string -> width:int -> string -> string
(** [wrap ~width s] is [wordwrap] followed by a hard break of every word that is still
    wider than [width] cells. Defaults: [breakpoints] is empty. *)

val pad_right : width:int -> string -> string
(** [pad_right ~width s] is [s] followed by spaces up to [width] cells of total text
    width. The padding is appended to the end of [s], so it sits inside a sequence that is
    still open there. [s] wider than [width] is returned unchanged. *)
