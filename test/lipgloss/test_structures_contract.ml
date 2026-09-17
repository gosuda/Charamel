(* These cases encode the approved Step 6 target contract. They cover the
   width-aware table, tree callback, and flat-list behavior that consumers observe. *)

open Charamel_lipgloss

let check_render name expected actual = Alcotest.(check string) name expected actual

let table_case name expected table =
  Alcotest.test_case name `Quick (fun () ->
      check_render name expected (Table.render table))

let test_table_width_expansion () =
  let table =
    Table.v ~width:10 ~headers:[ "A"; "B" ]
      ~rows:[ [ "x"; "y" ] ]
      ~border:Border.normal ()
  in
  let expected = "┌────┬───┐\n│A   │B  │\n├────┼───┤\n│x   │y  │\n└────┴───┘" in
  check_render "width expansion" expected (Table.render table);
  Alcotest.(check int) "expanded width" 10 (Layout.width (Table.render table))

let test_table_smart_shrink () =
  let table =
    Table.v ~width:16
      ~headers:[ "Name"; "Age of Person"; "Location" ]
      ~rows:
        [
          [ "Kini"; "40"; "New York" ];
          [ "Eli"; "30"; "London" ];
          [ "Iris"; "20"; "Paris" ];
        ]
      ~border:Border.normal ()
  in
  let expected =
    "┌────┬──┬──────┐\n\
     │Name│A…│Locat…│\n\
     ├────┼──┼──────┤\n\
     │Kini│40│New Y…│\n\
     │Eli │30│London│\n\
     │Iris│20│Paris │\n\
     └────┴──┴──────┘"
  in
  check_render "median shrink" expected (Table.render table)

let test_table_even_median () =
  let table =
    Table.v ~width:14
      ~rows:[ [ "abcdefghij"; "123456789" ]; [ ""; "" ] ]
      ~border:Border.normal ()
  in
  let expected = "┌──────┬─────┐\n│abcde…│1234…│\n│      │     │\n└──────┴─────┘" in
  check_render "even median uses both middle widths" expected (Table.render table)

let test_table_unicode_width () =
  let decomposed = "\x65\xcc\x81" in
  let table = Table.v ~rows:[ [ "界"; decomposed ] ] ~border:Border.normal () in
  let expected = "┌──┬─┐\n│界│" ^ decomposed ^ "│\n└──┴─┘" in
  check_render "grapheme widths" expected (Table.render table)

let test_table_minimum_width_floor () =
  let table =
    Table.v ~width:9 ~rows:[ [ "x"; "longword"; "z" ] ] ~border:Border.normal ()
  in
  let expected = "┌─┬───┬─┐\n│x│lo…│z│\n└─┴───┴─┘" in
  check_render "minimum column floor" expected (Table.render table)

let test_table_offset_overflow_height () =
  let table =
    Table.v ~offset:1 ~height:4
      ~rows:[ [ "a" ]; [ "b" ]; [ "c" ]; [ "d" ] ]
      ~border:Border.normal ()
  in
  let expected = "┌─┐\n│b│\n│…│\n└─┘" in
  check_render "row offset and overflow" expected (Table.render table)

let test_table_multiline_wrap () =
  let table =
    Table.v ~wrap:true ~headers:[ "Key"; "Value" ]
      ~rows:[ [ "x"; "y\nz" ] ]
      ~border:Border.normal ()
  in
  let expected =
    "┌───┬─────┐\n│Key│Value│\n├───┼─────┤\n│x  │y    │\n│   │z    │\n└───┴─────┘"
  in
  check_render "multiline wrapped cell" expected (Table.render table)

let test_table_word_wrap () =
  let table =
    Table.v ~width:4 ~wrap:true ~headers:[ "H" ] ~rows:[ [ "ab cd" ] ]
      ~border:Border.normal ()
  in
  let expected = "┌──┐\n│H │\n├──┤\n│ab│\n│cd│\n└──┘" in
  check_render "constrained word wrap" expected (Table.render table)

let test_table_crlf_no_wrap () =
  let rows =
    [
      [ "a0"; "b0"; "c0"; "d0" ];
      [ "a1"; "b1.0\r\nb1.1\r\nb1.2\r\nb1.3"; "c1"; "d1" ];
      [ "a2"; "b2"; "c2"; "d2" ];
    ]
  in
  let table = Table.v ~rows ~border:Border.normal () in
  let expected =
    "┌──┬────┬──┬──┐\n\
     │a0│b0  │c0│d0│\n\
     │a1│b1.0│c1│d1│\n\
     │  │b1.1│  │  │\n\
     │  │b1.2│  │  │\n\
     │  │b1.3│  │  │\n\
     │a2│b2  │c2│d2│\n\
     └──┴────┴──┴──┘"
  in
  check_render "CRLF lines expand no-wrap rows" expected (Table.render table)

let test_table_rows_and_headers () =
  let table =
    Table.v ~headers:[ "H" ]
      ~rows:[ [ "a"; "bb"; "ccc" ]; [ "d" ] ]
      ~border:Border.normal ()
  in
  let expected =
    "┌─┬──┬───┐\n│H│  │   │\n├─┼──┼───┤\n│a│bb│ccc│\n│d│  │   │\n└─┴──┴───┘"
  in
  check_render "uneven rows and headers" expected (Table.render table)

let test_table_empty () =
  let table = Table.v ~border:Border.normal () in
  check_render "empty table" "" (Table.render table)

let test_table_no_headers () =
  let table = Table.v ~rows:[ [ "1"; "2" ] ] ~border:Border.normal () in
  let expected = "┌─┬─┐\n│1│2│\n└─┴─┘" in
  check_render "rows without headers" expected (Table.render table)

let test_table_styled_cells () =
  let style ~row:_ ~col =
    if col = 0 then Style.bold true Style.empty else Style.italic true Style.empty
  in
  let table =
    Table.v ~style ~headers:[ "A"; "B" ] ~rows:[ [ "1"; "2" ] ] ~border:Border.normal ()
  in
  let expected =
    "┌─┬─┐\n\
     │\027[1mA\027[m│\027[3mB\027[m│\n\
     ├─┼─┤\n\
     │\027[1m1\027[m│\027[3m2\027[m│\n\
     └─┴─┘"
  in
  check_render "cell styles" expected (Table.render table)

let test_table_header_style_callback () =
  let style ~row ~col:_ =
    if row = -1 then Style.bold true Style.empty else Style.italic true Style.empty
  in
  let table =
    Table.v ~style ~headers:[ "H" ] ~rows:[ [ "x" ] ] ~border:Border.normal ()
  in
  let expected = "┌─┐\n│\027[1mH\027[m│\n├─┤\n│\027[3mx\027[m│\n└─┘" in
  check_render "header style receives -1 row" expected (Table.render table)

let test_table_style_geometry () =
  let style ~row:_ ~col:_ = Style.padding (Sides.xy ~x:1 ~y:0) Style.empty in
  let table =
    Table.v ~style ~headers:[ "A"; "B" ] ~rows:[ [ "1"; "2" ] ] ~border:Border.normal ()
  in
  let expected = "┌───┬───┐\n│ A │ B │\n├───┼───┤\n│ 1 │ 2 │\n└───┴───┘" in
  check_render "cell frame geometry" expected (Table.render table)

let border_table border = Table.v ~headers:[ "A"; "B" ] ~rows:[ [ "1"; "2" ] ] ~border ()

let test_table_distinct_border_fields () =
  let border : Border.t =
    {
      Border.top = "t";
      bottom = "b";
      left = "l";
      right = "r";
      top_left = "A";
      top_right = "C";
      bottom_left = "D";
      bottom_right = "E";
      middle_left = "L";
      middle_right = "R";
      middle = "M";
      middle_top = "U";
      middle_bottom = "V";
    }
  in
  let expected = "AtUtC\nlAlBr\nLtMtR\nl1l2r\nDbVbE" in
  check_render "distinct border fields" expected (Table.render (border_table border))

let border_cases =
  [
    ("rounded", Border.rounded, "╭─┬─╮\n│A│B│\n├─┼─┤\n│1│2│\n╰─┴─╯");
    ("block", Border.block, "█████\n█A█B█\n█████\n█1█2█\n█████");
    ("outer_half_block", Border.outer_half_block, "▛▀▀▜\n▌A▌B▐\n▀▀\n▌1▌2▐\n▙▄▄▟");
    ("inner_half_block", Border.inner_half_block, "▗▄▄▖\n▐A▐B▌\n▄▄\n▐1▐2▌\n▝▀▀▘");
    ("thick", Border.thick, "┏━┳━┓\n┃A┃B┃\n┣━╋━┫\n┃1┃2┃\n┗━┻━┛");
    ("double", Border.double, "╔═╦═╗\n║A║B║\n╠═╬═╣\n║1║2║\n╚═╩═╝");
    ("hidden", Border.hidden, "     \n A B \n     \n 1 2 \n     ");
    ("markdown", Border.markdown, "|-|-|\n|A|B|\n|-|-|\n|1|2|\n|-|-|");
    ("ascii", Border.ascii, "+-+-+\n|A|B|\n+-+-+\n|1|2|\n+-+-+");
  ]

let test_table_borders () =
  Stdlib.List.iter
    (fun (name, border, expected) ->
      check_render ("border " ^ name) expected (Table.render (border_table border)))
    border_cases

let test_tree_default_shape () =
  let tree =
    Tree.node ~value:"root"
      [
        Tree.leaf "one"; Tree.node ~value:"branch" [ Tree.leaf "deep" ]; Tree.leaf "last";
      ]
  in
  let expected = "root\n├── one\n├── branch\n│   └── deep\n└── last" in
  check_render "nested branches" expected (Tree.render tree)

let test_tree_custom_enumerator_and_indenter () =
  let enumerator ~depth ~index ~last =
    "<" ^ string_of_int depth ^ ":" ^ string_of_int index ^ ":"
    ^ (if last then "L" else "M")
    ^ "> "
  in
  let indenter ~depth ~last =
    "[" ^ string_of_int depth ^ ":" ^ (if last then "L" else "M") ^ "] "
  in
  let tree =
    Tree.node ~value:"R" [ Tree.leaf "A"; Tree.node ~value:"N" [ Tree.leaf "C" ] ]
  in
  let expected = "R\n<1:0:M> A\n<1:1:L> N\n[1:L] <2:0:L> C" in
  check_render "custom branch functions" expected (Tree.render ~enumerator ~indenter tree)

let test_tree_styles_by_depth () =
  let style ~depth _ =
    if depth = 0 then Style.bold true Style.empty else Style.italic true Style.empty
  in
  let tree = Tree.node ~value:"root" [ Tree.leaf "leaf" ] in
  let expected = "\027[1mroot\027[m\n└── \027[3mleaf\027[m" in
  check_render "root and child styles" expected (Tree.render ~style tree)

let test_tree_multiline_values () =
  let tree =
    Tree.node ~value:"Root\nLine" [ Tree.node ~value:"Child\nTail" [ Tree.leaf "Grand" ] ]
  in
  let expected = "Root\nLine\n└── Child\n    Tail\n    └── Grand" in
  check_render "multiline nodes" expected (Tree.render tree)

let test_tree_custom_marker_width () =
  let enumerator ~depth:_ ~index:_ ~last:_ = "-> " in
  let tree = Tree.node ~value:"root" [ Tree.leaf "first\nsecond" ] in
  let expected = "root\n-> first\n   second" in
  check_render "multiline child follows marker width" expected
    (Tree.render ~enumerator tree)

let test_tree_marker_alignment () =
  let enumerator ~depth:_ ~index ~last:_ =
    match index with 0 -> "I " | 1 -> "II " | 2 -> "III " | _ -> "IV "
  in
  let tree =
    Tree.node ~value:"root"
      [ Tree.leaf "one"; Tree.leaf "two"; Tree.leaf "three"; Tree.leaf "four" ]
  in
  let expected = "root\n  I one\n II two\nIII three\n IV four" in
  check_render "sibling marker alignment" expected (Tree.render ~enumerator tree)

let test_tree_empty_root () =
  let tree = Tree.node [ Tree.leaf "child" ] in
  check_render "empty root is omitted" "└── child" (Tree.render tree)

let list_case name enumerator expected =
  Alcotest.test_case name `Quick (fun () ->
      check_render name expected
        (List.render (List.v ~enumerator [ "Foo"; "Bar"; "Baz" ])))

let test_list_multiline_item () =
  let items = [ "first\nsecond\nthird"; "last" ] in
  let expected = "• first\n  second\n  third\n• last" in
  check_render "multiline list item" expected (List.render (List.v items))

let test_list_alphabet_boundaries () =
  let items = Stdlib.List.init 703 (fun _ -> "") in
  let lines =
    String.split_on_char '\n' (List.render (List.v ~enumerator:`Alphabet items))
  in
  let line index = Stdlib.List.nth lines index in
  Alcotest.(check string) "Z" "Z. " (line 25);
  Alcotest.(check string) "AA" "AA. " (line 26);
  Alcotest.(check string) "AZ" "AZ. " (line 51);
  Alcotest.(check string) "BA" "BA. " (line 52);
  Alcotest.(check string) "ZZ" "ZZ. " (line 701);
  Alcotest.(check string) "AAA" "AAA. " (line 702)

let test_list_alphabet_rollover () =
  let items = Stdlib.List.init 28 (fun _ -> "item") in
  let expected =
    "A. item\n\
     B. item\n\
     C. item\n\
     D. item\n\
     E. item\n\
     F. item\n\
     G. item\n\
     H. item\n\
     I. item\n\
     J. item\n\
     K. item\n\
     L. item\n\
     M. item\n\
     N. item\n\
     O. item\n\
     P. item\n\
     Q. item\n\
     R. item\n\
     S. item\n\
     T. item\n\
     U. item\n\
     V. item\n\
     W. item\n\
     X. item\n\
     Y. item\n\
     Z. item\n\
     AA. item\n\
     AB. item"
  in
  check_render "alphabet rollover" expected
    (List.render (List.v ~enumerator:`Alphabet items))

let cases : unit Alcotest.test_case list =
  [
    table_case "table rows and headers"
      "┌────┬─────┐\n│Name│Value│\n├────┼─────┤\n│A   │1    │\n│B   │22   │\n└────┴─────┘"
      (Table.v ~headers:[ "Name"; "Value" ]
         ~rows:[ [ "A"; "1" ]; [ "B"; "22" ] ]
         ~border:Border.normal ());
    Alcotest.test_case "table width expansion" `Quick test_table_width_expansion;
    Alcotest.test_case "table smart shrink" `Quick test_table_smart_shrink;
    Alcotest.test_case "table constrained word wrap" `Quick test_table_word_wrap;
    Alcotest.test_case "table empty" `Quick test_table_empty;
    Alcotest.test_case "table without headers" `Quick test_table_no_headers;
    Alcotest.test_case "table even median" `Quick test_table_even_median;
    Alcotest.test_case "table unicode widths" `Quick test_table_unicode_width;
    Alcotest.test_case "table distinct border fields" `Quick
      test_table_distinct_border_fields;
    Alcotest.test_case "table minimum width floor" `Quick test_table_minimum_width_floor;
    Alcotest.test_case "table offset overflow and height" `Quick
      test_table_offset_overflow_height;
    Alcotest.test_case "table multiline wrap" `Quick test_table_multiline_wrap;
    Alcotest.test_case "table CRLF without wrap" `Quick test_table_crlf_no_wrap;
    Alcotest.test_case "table uneven rows and headers" `Quick test_table_rows_and_headers;
    Alcotest.test_case "table styled cells" `Quick test_table_styled_cells;
    Alcotest.test_case "table header style callback" `Quick
      test_table_header_style_callback;
    Alcotest.test_case "table border families" `Quick test_table_borders;
    Alcotest.test_case "table style geometry" `Quick test_table_style_geometry;
    Alcotest.test_case "tree nested branches" `Quick test_tree_default_shape;
    Alcotest.test_case "tree custom enumerator and indenter" `Quick
      test_tree_custom_enumerator_and_indenter;
    Alcotest.test_case "tree root and child styles" `Quick test_tree_styles_by_depth;
    Alcotest.test_case "tree multiline values" `Quick test_tree_multiline_values;
    Alcotest.test_case "tree custom marker width" `Quick test_tree_custom_marker_width;
    Alcotest.test_case "tree sibling marker alignment" `Quick test_tree_marker_alignment;
    Alcotest.test_case "tree empty root" `Quick test_tree_empty_root;
    list_case "list bullets" `Bullet "• Foo\n• Bar\n• Baz";
    list_case "list dashes" `Dash "- Foo\n- Bar\n- Baz";
    list_case "list asterisks" `Asterisk "* Foo\n* Bar\n* Baz";
    list_case "list arabic" `Arabic "1. Foo\n2. Bar\n3. Baz";
    list_case "list alphabet" `Alphabet "A. Foo\nB. Bar\nC. Baz";
    list_case "list roman" `Roman "  I. Foo\n II. Bar\nIII. Baz";
    Alcotest.test_case "list alphabet rollover" `Quick test_list_alphabet_rollover;
    Alcotest.test_case "list multiline item" `Quick test_list_multiline_item;
    Alcotest.test_case "list alphabet boundaries" `Quick test_list_alphabet_boundaries;
  ]
