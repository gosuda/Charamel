let indices matches =
  Stdlib.List.map
    (fun (m : Charm_bubbles.Fuzzy.match_) -> m.Charm_bubbles.Fuzzy.index)
    matches

let matched (matches : Charm_bubbles.Fuzzy.match_ list) =
  match matches with [] -> [] | first :: _ -> first.Charm_bubbles.Fuzzy.matched

let scores matches =
  Stdlib.List.map
    (fun (m : Charm_bubbles.Fuzzy.match_) -> m.Charm_bubbles.Fuzzy.score)
    matches

let basic_vectors () =
  let matches = Charm_bubbles.Fuzzy.find ~pattern:"ba" [ "foo"; "bar"; "baz" ] in
  Alcotest.(check (list int)) "stable equal-score order" [ 1; 2 ] (indices matches);
  Alcotest.(check (list int)) "bar match positions" [ 0; 1 ] (matched matches);
  Alcotest.(check (list int)) "exact score" [ 14; 14 ] (scores matches);
  Alcotest.(check (list int))
    "unmatched pattern is dropped" []
    (indices (Charm_bubbles.Fuzzy.find ~pattern:"xyz" [ "foo"; "bar" ]))

let unsorted_preserves_input_order () =
  let candidates = [ "zzbar"; "bar" ] in
  let sorted = Charm_bubbles.Fuzzy.find ~pattern:"ba" candidates in
  let unsorted = Charm_bubbles.Fuzzy.find_unsorted ~pattern:"ba" candidates in
  Alcotest.(check (list int)) "sorted ranking" [ 1; 0 ] (indices sorted);
  Alcotest.(check (list int)) "unsorted source order" [ 0; 1 ] (indices unsorted)

let upstream_scoring_vectors () =
  let mnr = Charm_bubbles.Fuzzy.find ~pattern:"mnr" [ "moduleNameResolver.ts" ] in
  Alcotest.(check (list int)) "camel-case positions" [ 0; 6; 10 ] (matched mnr);
  Alcotest.(check (list int)) "camel-case score" [ 32 ] (scores mnr);
  let aaa = Charm_bubbles.Fuzzy.find ~pattern:"aaa" [ "aaa"; "bbb" ] in
  Alcotest.(check (list int)) "adjacency positions" [ 0; 1; 2 ] (matched aaa);
  Alcotest.(check (list int)) "adjacency score" [ 30 ] (scores aaa);
  let tk = Charm_bubbles.Fuzzy.find ~pattern:"tk" [ "The Black Knight" ] in
  Alcotest.(check (list int)) "exhaustive match chooses later k" [ 0; 10 ] (matched tk);
  Alcotest.(check (list int)) "exhaustive score" [ 16 ] (scores tk);
  let separator = Charm_bubbles.Fuzzy.find ~pattern:"abcx" [ "abc\\x" ] in
  Alcotest.(check (list int)) "separator positions" [ 0; 1; 2; 4 ] (matched separator);
  Alcotest.(check (list int)) "separator score" [ 49 ] (scores separator);
  let accented = Charm_bubbles.Fuzzy.find ~pattern:"mmt" [ "mémeTemps" ] in
  Alcotest.(check (list int)) "scalar Unicode positions" [ 0; 2; 4 ] (matched accented);
  Alcotest.(check (list int)) "scalar Unicode score" [ 24 ] (scores accented)

let empty_and_nul () =
  Alcotest.(check (list int))
    "empty query" []
    (indices (Charm_bubbles.Fuzzy.find ~pattern:"" [ "cat" ]));
  let matches = Charm_bubbles.Fuzzy.find ~pattern:"ab" [ "a\x00b" ] in
  Alcotest.(check (list int)) "match continues after NUL" [ 0; 2 ] (matched matches);
  Alcotest.(check (list int)) "NUL contributes to scalar penalty" [ 9 ] (scores matches)

let unicode_decomposed_and_emoji () =
  let decomposed = "e\xCC\x81" in
  let accent_matches = Charm_bubbles.Fuzzy.find ~pattern:"e" [ decomposed ] in
  Alcotest.(check (list int))
    "scalar score on decomposed accent" [ 9 ] (scores accent_matches);
  Alcotest.(check (list int))
    "accent maps scalar to grapheme" [ 0 ] (matched accent_matches);
  let emoji = "\xF0\x9F\x91\x8D\xF0\x9F\x8F\xBD" in
  let emoji_matches = Charm_bubbles.Fuzzy.find ~pattern:emoji [ emoji ] in
  Alcotest.(check (list int))
    "emoji scalars map to one grapheme" [ 0 ] (matched emoji_matches);
  Alcotest.(check (list int)) "emoji scalar score" [ 15 ] (scores emoji_matches)

let cases =
  [
    Alcotest.test_case "basic vectors" `Quick basic_vectors;
    Alcotest.test_case "unsorted order" `Quick unsorted_preserves_input_order;
    Alcotest.test_case "upstream scoring" `Quick upstream_scoring_vectors;
    Alcotest.test_case "empty and NUL" `Quick empty_and_nul;
    Alcotest.test_case "decomposed accents and emoji" `Quick unicode_decomposed_and_emoji;
  ]
