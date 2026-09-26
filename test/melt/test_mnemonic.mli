(** Alcotest cases for {!module:Mnemonic} and its {!module:Wordlist} data. *)

val cases : unit Alcotest_lwt.test_case list
(** [cases] holds the official BIP-39 test vectors, a round-trip check over seeds the
    vectors do not cover, the wordlist shape invariants, and the seed-length, word-count,
    unknown-word, and checksum rejection cases. *)
