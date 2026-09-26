open Charamel_lipgloss

let check_string name expected actual = Alcotest.(check string) name expected actual
let check_int name expected actual = Alcotest.(check int) name expected actual

let test_position () =
  Alcotest.(check bool) "low clamp" true (Position.to_float (Position.v (-1.0)) = 0.0);
  Alcotest.(check bool) "high clamp" true (Position.to_float (Position.v 2.0) = 1.0);
  Alcotest.(check bool) "center" true (Position.to_float Position.center = 0.5)

let test_border_sets () =
  let b = Border.rounded in
  check_string "rounded corner" "╭" b.top_left;
  check_string "normal edge" "─" Border.normal.top;
  check_string "half block has no middle" "" Border.outer_half_block.Border.middle;
  check_int "wide border edge" 1 (Border.left_size b)

let test_style_geometry () =
  check_string "width" "x    " (Style.render (Style.width 5 Style.empty) "x");
  check_string "height" "x\n \n " (Style.render (Style.height 3 Style.empty) "x");
  check_string "padding" "   \n x \n   "
    (Style.render (Style.padding (Sides.all 1) Style.empty) "x");
  check_string "inline newline" "xy" (Style.render (Style.inline true Style.empty) "x\ny");
  check_string "tabs" "    x" (Style.render Style.empty "\tx");
  check_string "rounded border" "╭─╮\n│x│\n╰─╯"
    (Style.render (Style.border Border.rounded Style.empty) "x")

let test_style_attributes () =
  let red = match Color.rgb 255 0 0 with Some c -> c | None -> assert false in
  let rendered = Style.render (Style.foreground red Style.empty) "x" in
  check_string "full fidelity foreground" "\027[38;2;255;0;0mx\027[m" rendered;
  check_string "underline includes spaces" "\027[4mx\027[m\027[4m \027[m\027[4my\027[m"
    (Style.render (Style.underline true Style.empty) "x y")

let test_inheritance () =
  let parent =
    Style.background
      (Option.get (Color.rgb 1 2 3))
      (Style.padding (Sides.all 4) Style.empty)
  in
  let child = Style.foreground (Option.get (Color.rgb 9 8 7)) Style.empty in
  let merged = Style.inherit_ ~parent child in
  check_string "inherited background" "\027[38;2;9;8;7;48;2;1;2;3mx\027[m"
    (Style.render merged "x")

let test_layout () =
  check_string "join horizontal" "ab" (Layout.join_horizontal [ "a"; "b" ]);
  check_string "join vertical centered" "a\nb"
    (Layout.join_vertical ~pos:Position.center [ "a"; "b" ]);
  check_string "place right" "  x"
    (Layout.place_horizontal ~width:3 ~pos:Position.right "x");
  check_string "odd centered gap" " x  "
    (Layout.place_horizontal ~width:4 ~pos:Position.center "x");
  check_int "multiline width" 3 (Layout.width "x\nyyy");
  check_int "height" 2 (Layout.height "x\ny")

let test_table () =
  let table =
    Table.v ~headers:[ "Name"; "Value" ] ~rows:[ [ "a"; "1" ]; [ "b"; "2" ] ] ()
  in
  check_string "table geometry"
    "┌────┬─────┐\n│Name│Value│\n├────┼─────┤\n│a   │1    │\n│b   │2    │\n└────┴─────┘"
    (Table.render table)

let test_tree_and_list () =
  check_string "list" "• one\n• two" (List.render (List.v [ "one"; "two" ]));
  let tree = Tree.node ~value:"root" [ Tree.leaf "leaf" ] in
  check_string "tree" "root\n└── leaf" (Tree.render tree)

let () =
  Alcotest.run "lipgloss"
    [
      ("position", [ Alcotest.test_case "clamp and constants" `Quick test_position ]);
      ("borders", [ Alcotest.test_case "named border sets" `Quick test_border_sets ]);
      ( "style",
        [
          Alcotest.test_case "geometry" `Quick test_style_geometry;
          Alcotest.test_case "attributes" `Quick test_style_attributes;
          Alcotest.test_case "inheritance" `Quick test_inheritance;
        ] );
      ("layout", [ Alcotest.test_case "joins and placement" `Quick test_layout ]);
      ("table", [ Alcotest.test_case "table render" `Quick test_table ]);
      ("tree-list", [ Alcotest.test_case "tree and list" `Quick test_tree_and_list ]);
      ("style-contract", Test_style_contract.cases);
      ("color-util", Test_color_util.cases);
      ("blending", Test_blending.cases);
      ("surface-contract", Test_surface_contract.cases);
      ("layout-contract", Test_layout_contract.cases);
      ("structures-contract", Test_structures_contract.cases);
    ]
