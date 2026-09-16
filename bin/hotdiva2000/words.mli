(** Curated hotdiva2000 word lists.

    Transcribed verbatim from the upstream
    {{:https://github.com/charmbracelet/hotdiva2000} hotdiva2000} project (MIT licensed;
    see [.references/LICENSES.txt] and [NOTICE]): every entry of upstream's
    [modifiers.txt] and [nouns.txt] is kept, in upstream's sorted, deduplicated order and
    exact spelling (Title Case, digits and internal hyphenation as embedded upstream, e.g.
    ["180 BPM"], ["3D Renderer"]). Neither list is filtered: entries containing digits or
    unusual punctuation are real upstream data (upstream's own README example output,
    ["180-bpm-lawyer"], is built from the modifier ["180 BPM"]). [Name] lowercases and
    re-separates entries at generation time; this module holds raw data only.

    upstream's [prefix.txt] and [suffix.txt] are not represented here: the ported
    [Name.generate] token-chain design (zero or more modifiers followed by one noun) has
    no optional prefix/suffix slot, unlike upstream's PrefixThreshold/SuffixThreshold
    model. *)

val modifiers : string array
(** [modifiers] is the complete upstream modifier ("adjective") pool, 915 entries.
    Immutable: never write into this array. *)

val nouns : string array
(** [nouns] is the complete upstream noun pool, 1107 entries. Immutable: never write into
    this array. *)
