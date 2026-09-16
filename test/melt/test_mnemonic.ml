open Melt_core

(* Test vectors are the 256-bit-entropy rows of the standard BIP-39 test vector set
   (github.com/trezor/python-mnemonic, vectors.json, MIT), which are also the rows
   melt.go's own upstream dependency (github.com/tyler-smith/go-bip39) is tested
   against. Only entropy and mnemonic are used here: the PBKDF2-derived wallet seed
   and the extended key in that file are outside {!Mnemonic}'s contract, which
   operates directly on the 32-byte entropy (the Ed25519 private seed). *)

let seed_of_hex hex =
  let n = String.length hex / 2 in
  String.init n (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (i * 2) 2)))

let words_of_string s = String.split_on_char ' ' s

let vectors =
  [
    ( "0000000000000000000000000000000000000000000000000000000000000000",
      "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon \
       abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon \
       abandon abandon abandon art" );
    ( "7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f",
      "legal winner thank year wave sausage worth useful legal winner thank year wave \
       sausage worth useful legal winner thank year wave sausage worth title" );
    ( "8080808080808080808080808080808080808080808080808080808080808080",
      "letter advice cage absurd amount doctor acoustic avoid letter advice cage absurd \
       amount doctor acoustic avoid letter advice cage absurd amount doctor acoustic \
       bless" );
    ( "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff",
      "zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo \
       zoo zoo zoo vote" );
    ( "68a79eaca2324873eacc50cb9c6eca8cc68ea5d936f98787c60c7ebc74e6ce7c",
      "hamster diagram private dutch cause delay private meat slide toddler razor book \
       happy fancy gospel tennis maple dilemma loan word shrug inflict delay length" );
    ( "9f6a2878b2520799a44ef18bc7df394e7061a224d2c33cd015b157d746869863",
      "panda eyebrow bullet gorilla call smoke muffin taste mesh discover soft ostrich \
       alcohol speed nation flash devote level hobby quick inner drive ghost inside" );
    ( "066dca1a2bb7e8a1db2832148ce9933eea0f3ac9548d793112d9a95c9407efad",
      "all hour make first leader extend hole alien behind guard gospel lava path output \
       census museum junior mass reopen famous sing advance salt reform" );
    ( "f585c11aec520db57dd353c69554b21a89b20fb0650966fa0a9d6f74fd989d8f",
      "void come effort suffer camp survey warrior heavy shoot primary clutch crush open \
       amazing screen patrol group space point ten exist slush involve unfold" );
  ]

let words = Alcotest.list Alcotest.string
let seed = Alcotest.string
let error = Alcotest.of_pp Mnemonic.pp_error
let encode_result = Alcotest.result words error
let decode_result = Alcotest.result seed error

let official_vectors =
  Alcotest.test_case "official BIP-39 vectors" `Quick (fun () ->
      List.iter
        (fun (entropy_hex, mnemonic) ->
          let seed_bytes = seed_of_hex entropy_hex in
          let expected_words = words_of_string mnemonic in
          Alcotest.check encode_result entropy_hex (Ok expected_words)
            (Mnemonic.encode seed_bytes);
          Alcotest.check decode_result mnemonic (Ok seed_bytes)
            (Mnemonic.decode expected_words))
        vectors)

let roundtrip_arbitrary_seeds =
  Alcotest.test_case "round-trips seeds outside the vector set" `Quick (fun () ->
      let seeds =
        [
          String.init 32 (fun i -> Char.chr i);
          String.init 32 (fun i -> Char.chr (255 - i));
          String.init 32 (fun i -> Char.chr (i * 37 land 0xff));
        ]
      in
      List.iter
        (fun s ->
          match Mnemonic.encode s with
          | Error _ -> Alcotest.fail "encode rejected a 32-byte seed"
          | Ok mnemonic -> (
              Alcotest.(check int) "24 words" 24 (List.length mnemonic);
              match Mnemonic.decode mnemonic with
              | Error _ -> Alcotest.fail "decode rejected its own encoding"
              | Ok back -> Alcotest.check seed "round-trip" s back))
        seeds)

let wordlist_shape =
  Alcotest.test_case "wordlist is the full sorted 2048-word BIP-39 list" `Quick (fun () ->
      Alcotest.(check int) "2048 words" 2048 (Array.length Wordlist.words);
      Array.iteri
        (fun i w -> Alcotest.(check (option int)) w (Some i) (Wordlist.find_index w))
        Wordlist.words;
      Array.iteri
        (fun i w ->
          if i > 0 then
            Alcotest.(check bool)
              (Fmt.str "%s < %s" Wordlist.words.(i - 1) w)
              true
              (String.compare Wordlist.words.(i - 1) w < 0))
        Wordlist.words;
      Alcotest.(check (option int)) "unknown word" None (Wordlist.find_index "notaword"))

let rejects_wrong_seed_length =
  Alcotest.test_case "rejects a seed whose length is not 32" `Quick (fun () ->
      Alcotest.check encode_result "empty"
        (Error (`Wrong_seed_length 0))
        (Mnemonic.encode "");
      Alcotest.check encode_result "31 bytes"
        (Error (`Wrong_seed_length 31))
        (Mnemonic.encode (String.make 31 'a'));
      Alcotest.check encode_result "33 bytes"
        (Error (`Wrong_seed_length 33))
        (Mnemonic.encode (String.make 33 'a')))

let rejects_wrong_word_count =
  Alcotest.test_case "rejects a mnemonic whose length is not 24" `Quick (fun () ->
      let full_words = words_of_string (snd (List.hd vectors)) in
      Alcotest.check decode_result "empty"
        (Error (`Wrong_word_count 0))
        (Mnemonic.decode []);
      Alcotest.check decode_result "23 words"
        (Error (`Wrong_word_count 23))
        (Mnemonic.decode (List.filteri (fun i _ -> i < 23) full_words));
      Alcotest.check decode_result "25 words"
        (Error (`Wrong_word_count 25))
        (Mnemonic.decode (full_words @ [ "abandon" ])))

let rejects_unknown_word =
  Alcotest.test_case "rejects a mnemonic containing a non-BIP-39 word" `Quick (fun () ->
      let full_words = words_of_string (snd (List.hd vectors)) in
      let corrupted =
        List.mapi (fun i w -> if i = 3 then "zzznotaword" else w) full_words
      in
      Alcotest.check decode_result "unknown word"
        (Error (`Unknown_word "zzznotaword"))
        (Mnemonic.decode corrupted))

let rejects_bad_checksum =
  Alcotest.test_case "rejects a mnemonic with a broken checksum" `Quick (fun () ->
      let full_words = words_of_string (snd (List.hd vectors)) in
      let corrupted =
        List.mapi
          (fun i w -> if i = List.length full_words - 1 then "ability" else w)
          full_words
      in
      Alcotest.check decode_result "bad checksum" (Error `Bad_checksum)
        (Mnemonic.decode corrupted))

let cases =
  [
    official_vectors;
    roundtrip_arbitrary_seeds;
    wordlist_shape;
    rejects_wrong_seed_length;
    rejects_wrong_word_count;
    rejects_unknown_word;
    rejects_bad_checksum;
  ]
