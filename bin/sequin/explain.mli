(** Human-readable descriptions of ANSI terminal sequences.

    [explain] runs raw bytes through {!Charm_ansi.Parser} and renders each completed
    action as one line. A kind label, the sequence reconstructed from its decoded fields,
    and a plain-English description separated from the fields by two spaces. Consecutive
    printable text is folded into a single [Print] line. A sequence this module does not
    recognise still prints its kind and reconstructed fields, with the description
    ["Unknown"] rather than a guessed meaning. *)

val explain : string -> string
(** [explain s] is one line per completed action in [s], each terminated by ['\n'].
    Trailing partial input is finalised as if the stream had ended, following
    {!Charm_ansi.Parser.flush}: a lone pending ESC explains as [Execute ESC] and any other
    unterminated sequence is dropped, matching the parser's own contract. *)
