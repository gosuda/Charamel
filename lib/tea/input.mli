(** Incremental terminal input decoder.

    [t] buffers raw bytes read from a terminal and turns them into {!Event.t} values:
    keys, mouse reports, paste content, focus changes, and query replies. It is a private
    byte-level port of ultraviolet's [decoder.go] and [terminal_reader.go] (bubbletea v2's
    input layer), not built on {!Charamel_ansi.Parser}: every branch here needs the exact
    raw bytes of a sequence to preserve them in {!Event.Unknown}, to re-decode an
    ESC-prefixed tail for the alt-key merge, and to reparse a URxvt ["$"]-terminated
    sequence as a ["~"]-terminated one, none of which a general streaming ANSI parser
    exposes.

    Terminfo-sourced key tables and legacy encoding flags are not implemented: every
    sequence the upstream default (non-terminfo) legacy key table recognizes is also
    recognized directly by the parser branches here, so the table is redundant and
    omitted; terminfo itself is out of scope for this repository (no terminfo dependency
    in the approved set).

    Query replies decode alongside the key input: a DCS [ESC P > | <text> ST] XTVERSION
    reply becomes {!Event.Terminal_version} carrying [<text>], and a DCS
    [ESC P 1 + r <hex> = <hex> ST] or [ESC P 0 + r <hex> ST] XTGETTCAP reply becomes
    {!Event.Capability}; any other DCS or OSC report stays {!Event.Unknown}.

    A text key whose grapheme cluster can still grow is held back until the next scalar
    settles it, so a decomposed Hangul syllable block arriving one byte at a time reaches
    the program as one key (see {!pending_cluster}).

    Byte budget: the internal pending buffer and the in-progress bracketed-paste buffer
    are each capped at 64 KiB. A pending buffer that would grow past the cap is
    immediately decoded as if the ESC timeout had fired (see {!flush}), which always
    drains it; a paste that would grow past the cap is delivered early as an
    {!Event.Paste} segment and accumulation continues in a fresh segment. No byte fed to
    {!feed} is ever silently discarded. A byte that cannot be decoded as part of a
    recognized sequence is preserved verbatim in {!Event.Unknown}. A held text cluster is
    bounded by the same cap and delivered whole when it is flushed. *)

type t

val create : unit -> t
(** [create ()] is a fresh decoder with no buffered bytes and no paste in progress. *)

val feed : t -> string -> Event.t list
(** [feed t s] appends [s] to [t]'s pending buffer and returns every event that can be
    produced without ambiguity: complete sequences, and printable text whose grapheme
    cluster is settled. A trailing partial escape sequence, a bare ESC that could still
    become an alt-modified key or the start of a longer sequence, a partial UTF-8 scalar,
    the scalars of a text key whose cluster can still continue, or an in-progress
    bracketed paste is held back in [t] for a later {!feed} or {!flush} call to resolve.

    The runtime is expected to call [feed] with every chunk read from the terminal, and to
    arm a timer whenever a call leaves a bare ESC (see {!pending_escape}) or an unsettled
    text cluster (see {!pending_cluster}) pending. When the timer fires before more bytes
    arrive, it calls {!flush}. This mirrors ultraviolet's escape-sequence disambiguation
    timeout, [DefaultEscTimeout] (50ms). *)

val flush : t -> Event.t list
(** [flush t] decodes every byte and held cluster currently in [t] as if no more input
    will ever arrive: a bare buffered ESC resolves to the [Escape] key, an incomplete or
    unrecognized sequence resolves to {!Event.Unknown} carrying its raw bytes, a held text
    cluster resolves to the one key its scalars spell, and [t]'s pending buffer is empty
    afterwards. An in-progress bracketed paste is not force-closed by [flush]: it keeps
    accumulating across timeouts until its closing sequence arrives or it is segmented by
    the paste byte cap. *)

val pending_escape : t -> bool
(** [pending_escape t] is [true] exactly when [t]'s pending buffer holds a single, bare
    ESC byte (0x1b) and nothing else: the state a lone Escape keypress leaves behind while
    {!feed} waits to see whether more bytes turn it into an alt-modified key or the start
    of a longer escape sequence. It is [false] when nothing is pending, and [false] when
    more (or different) bytes are pending, including a partial UTF-8 scalar prefix, an
    unsettled text cluster, or a bracketed paste. Those states also wait for {!flush};
    this predicate does not distinguish them from each other. *)

val pending_cluster : t -> bool
(** [pending_cluster t] is [true] when [t] is holding back the decoded scalars of a text
    key whose grapheme cluster can still continue: a Hangul jamo sequence still open at
    its leading consonant or its vowel, or awaiting a trailing consonant, a zero-width
    joiner awaiting the scalar it joins, or a lone regional indicator awaiting its pair.
    The runtime arms the same disambiguation timer as for {!pending_escape}, so a held
    cluster is delivered — never dropped — once input pauses, however incomplete it stays.
*)
