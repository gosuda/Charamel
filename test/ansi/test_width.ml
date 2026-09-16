module Width = Charm_ansi.Width

(* Width vectors restated as codepoints from the grapheme column of
   .references/x/ansi/wcwidth_test.go and the width column of
   .references/x/ansi/width_test.go (raw text rows only). The halfwidth
   mark and lone control rows isolate the base-scalar and halfwidth
   voiced-mark rules that upstream only exercises joined to a base. *)

let width_vectors =
  [
    ("empty string", "", 0);
    ("ascii word", "hello", 5);
    ("ascii letter", "a", 1);
    ("precomposed acute", "\u{00E9}", 1);
    ("combining grave", "a\u{0300}", 1);
    ("combining acute", "e\u{0301}", 1);
    ("cjk ideograph", "\u{4E16}", 2);
    ("precomposed hangul", "\u{D55C}", 2);
    ("decomposed hangul", "\u{1112}\u{1161}\u{11AB}", 2);
    ("emoji waving hand", "\u{1F44B}", 2);
    ("emoji bubbles", "\u{1FAE7}", 2);
    ("emoji rainbow", "\u{1F308}", 2);
    ("variation selector 16", "\u{26A0}\u{FE0F}", 2);
    ("variation selector 15", "\u{2639}\u{FE0E}", 1);
    ("keycap sequence", "1\u{FE0F}\u{20E3}", 2);
    ("zwj pair", "\u{1F468}\u{200D}\u{1F4BB}", 2);
    ("zwj family", "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F466}", 2);
    ("zwj flag", "\u{1F3F3}\u{FE0F}\u{200D}\u{1F308}", 2);
    ("skin tone modifier", "\u{1F44D}\u{1F3FD}", 2);
    ("regional indicator pair us", "\u{1F1FA}\u{1F1F8}", 2);
    ("regional indicator pair sa", "\u{1F1F8}\u{1F1E6}", 2);
    ("regional indicator pair de", "\u{1F1E9}\u{1F1EA}", 2);
    ("halfwidth voiced mark", "\u{FF76}\u{FF9E}", 1);
    ("halfwidth mark after ascii", "(\u{FF9F}", 1);
    ("leading halfwidth mark", "\u{FF9E}", 0);
    ("c1 control before ascii", "\u{009B}x", 1);
    ("lone c1 control", "\u{009B}", 0);
    ("newline splits clusters", "hello\nworld", 10);
    ("tab splits clusters", "hello\tworld", 10);
    ("devanagari conjunct", "\u{0938}\u{094D}\u{0924}\u{0947}", 1);
    ("devanagari kssa", "\u{0915}\u{094D}\u{0937}", 1);
    ("curly quotes", "Claire\u{2019}s Boutique", 17);
  ]

let first_cluster_vectors =
  [
    ("empty string is zero", "", 0);
    ("first of ascii word", "hello", 1);
    ("combining cluster", "a\u{0300}", 1);
    ("wide cluster", "\u{4E16}", 2);
    ("devanagari cluster", "\u{0915}\u{094D}\u{0937}", 1);
    ("skin tone cluster", "\u{1F44D}\u{1F3FD}", 2);
    ("keycap cluster", "1\u{FE0F}\u{20E3}", 2);
    ("text presentation cluster", "\u{2639}\u{FE0E}", 1);
    ("regional indicator cluster", "\u{1F1FA}\u{1F1F8}", 2);
    ("halfwidth mark cluster", "\u{FF9E}", 0);
    ("first cluster of a pair", "ab", 1);
    ("first cluster of several", "a\u{0300}\u{4E16}", 1);
  ]

let cluster_vectors =
  [
    ("empty string is empty", "", []);
    ("ascii letters", "hello", [ "h"; "e"; "l"; "l"; "o" ]);
    ("combining joins base", "a\u{0300}", [ "a\u{0300}" ]);
    ( "zwj family is one cluster",
      "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F466}",
      [ "\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F466}" ] );
    ("halfwidth mark joins base", "\u{FF76}\u{FF9E}", [ "\u{FF76}\u{FF9E}" ]);
    ("regional indicators pair", "\u{1F1FA}\u{1F1F8}", [ "\u{1F1FA}\u{1F1F8}" ]);
  ]

let width_case (name, input, expected) =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check Alcotest.int name expected (Width.string_width input))

let first_cluster_case (name, input, expected) =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check Alcotest.int name expected (Width.grapheme_width input))

let cluster_case (name, input, expected) =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check (Alcotest.list Alcotest.string) name expected (Width.graphemes input))

let concatenation =
  Alcotest.test_case "clusters concatenate to the input" `Quick (fun () ->
      List.iter
        (fun (name, input, _) ->
          Alcotest.check Alcotest.string name input
            (String.concat "" (Width.graphemes input)))
        width_vectors)

let cases : unit Alcotest.test_case list =
  List.map width_case width_vectors
  @ List.map first_cluster_case first_cluster_vectors
  @ List.map cluster_case cluster_vectors
  @ [ concatenation ]
