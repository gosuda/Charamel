(** The BIP-39 English wordlist.

    Transcribed verbatim from bitcoin/bips, file [bip-0039/english.txt] (fetched
    2026-09-15). BIP-39's own Copyright section states "This BIP falls under the MIT
    License", naming Marek Palatinus, Pavol Rusnak, Aaron Voisine, and Sean Bowe as
    authors. The 2048-word, one-word-per-line transcription is pinned by the CRC32
    checksum [c1dbd296], which is also the checksum the MIT-licensed
    github.com/tyler-smith/go-bip39 (Copyright (c) 2014-2018 Tyler Smith and contributors)
    verifies its own copy of the same upstream file against, cross- confirming the bytes
    here. The list is sorted and holds no duplicates, per the BIP-39 spec's "sorted
    wordlists" requirement. *)

val words : string array
(** [words] is the 2048-word English BIP-39 list. [words.(i)] is the word whose 11-bit
    mnemonic index is [i]. *)

val find_index : string -> int option
(** [find_index w] is [Some i] when [words.(i) = w], and [None] when [w] is not a member
    of {!words}. *)
