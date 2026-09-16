(** BIP-39 mnemonic encoding for Ed25519 seeds.

    [encode] and [decode] are the standard BIP-39 procedure (entropy, an appended SHA-256
    checksum, 11-bit big-endian word indices) specialized to Ed25519's 32-byte private
    seed, so the 24-word mnemonic round-trips the exact seed bytes
    {!Charm_ssh_keygen.generate} produces and {!Charm_ssh_keygen.of_ed25519_seed}
    consumes. No OpenSSH container, comment, or fingerprint is involved. Words are looked
    up in {!Wordlist.words}; failures are a closed variant so a malformed seed, a
    corrupted mnemonic, and a foreign (non-BIP-39) word list are distinguishable. *)

type error =
  [ `Wrong_seed_length of int
  | `Wrong_word_count of int
  | `Unknown_word of string
  | `Bad_checksum ]

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] renders [e] for CLI and log output. *)

val encode : string -> (string list, error) result
(** [encode seed] is the 24-word BIP-39 mnemonic for the 32-byte Ed25519 private seed
    [seed]. [seed] is the entropy; the checksum is the first 8 bits of its SHA-256 digest;
    every 11 bits of [entropy ^ checksum], most significant first, indexes
    {!Wordlist.words}. A [seed] whose length is not 32 is
    [`Wrong_seed_length (String.length seed)]. *)

val decode : string list -> (string, error) result
(** [decode words] is the inverse of {!encode}: the 32-byte seed recovered from a 24-word
    BIP-39 mnemonic. A [words] list whose length is not 24 is
    [`Wrong_word_count (List.length words)]. A word absent from {!Wordlist.words} is
    [`Unknown_word w], reported for the first such word encountered in list order. A
    mnemonic whose trailing 8 bits do not equal the first 8 bits of the SHA-256 digest of
    its leading 32 bytes is [`Bad_checksum]. *)
