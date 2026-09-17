(* Observable contract cases for Charamel_lipgloss.Layout.  The geometry cases are
   derived from .references/lipgloss/join_test.go, position.go, and size.go;
   styling cases cover the range and rune contracts in ranges.go and runes.go.
   These tests assert rendered geometry and escape-aware text, not implementation
   details. *)

open Charamel_lipgloss

let check_string name expected actual = Alcotest.(check string) name expected actual
let check_int name expected actual = Alcotest.(check int) name expected actual
let blank n = String.make n ' '
let lines xs = String.concat "\n" xs

(* Size and measurement. *)

let test_width_uses_widest_line () = check_int "widest line" 3 (Layout.width "abc\nde\nf")

let test_width_ignores_ansi_and_uses_grapheme_cells () =
  check_int "ANSI is zero width" 2 (Layout.width "\x1b[1mab\x1b[m\n好");
  check_int "combining cluster is one cell" 1 (Layout.width "e\xCC\x81");
  check_int "escape-only text is empty" 0 (Layout.width "\x1b[31m\x1b[m")

let test_height_counts_lines_and_trailing_empty_line () =
  check_int "empty string has one line" 1 (Layout.height "");
  check_int "newline adds a line" 2 (Layout.height "a\n");
  check_int "two lines" 2 (Layout.height "a\nb")

let test_size_combines_width_and_height () =
  let w, h = Layout.size "hello\nworld" in
  check_int "size width" 5 w;
  check_int "size height" 2 h

(* Horizontal joining. *)

let test_join_horizontal_empty_and_single () =
  check_string "empty" "" (Layout.join_horizontal []);
  check_string "single passthrough" "x" (Layout.join_horizontal [ "x" ])

let test_join_horizontal_top_and_bottom () =
  check_string "top" "AB\n B\n B\n B" (Layout.join_horizontal [ "A"; "B\nB\nB\nB" ]);
  check_string "bottom" " B\n B\n B\nAB"
    (Layout.join_horizontal ~pos:Position.bottom [ "A"; "B\nB\nB\nB" ])

let test_join_horizontal_fractional_split () =
  (* For three missing lines and pos .25, one line is before the block and two
     are after it: split = round (3 * .25) = 1. *)
  check_string "fractional split" " B\nAB\n B\n B"
    (Layout.join_horizontal ~pos:(Position.v 0.25) [ "A"; "B\nB\nB\nB" ])

let test_join_horizontal_pads_each_block_to_its_width () =
  check_string "per-block width" "a X\nbb " (Layout.join_horizontal [ "a\nbb"; "X" ])

let test_join_horizontal_uses_ansi_and_grapheme_width () =
  check_string "ANSI remains in output" "\x1b[1mhi\x1b[mX"
    (Layout.join_horizontal [ "\x1b[1mhi\x1b[m"; "X" ]);
  check_string "wide cell padding" "好 \nabc" (Layout.join_vertical [ "好"; "abc" ])

(* Vertical joining. *)

let test_join_vertical_empty_and_single () =
  check_string "empty" "" (Layout.join_vertical []);
  check_string "single passthrough" "x" (Layout.join_vertical [ "x" ])

let test_join_vertical_left_and_right () =
  check_string "left" "A   \nBBBB" (Layout.join_vertical [ "A"; "BBBB" ]);
  check_string "right" "   A\nBBBB"
    (Layout.join_vertical ~pos:Position.right [ "A"; "BBBB" ])

let test_join_vertical_fractional_and_odd_center () =
  check_string "quarter" " A  \nBBBB"
    (Layout.join_vertical ~pos:(Position.v 0.25) [ "A"; "BBBB" ]);
  (* A three-cell gap at the center rounds to two cells on the left and one
     on the right.  This is deliberately opposite the horizontal-placement
     split, and catches the odd-column rule. *)
  check_string "odd center" "  A \nBBBB"
    (Layout.join_vertical ~pos:Position.center [ "A"; "BBBB" ])

(* Horizontal and vertical placement. *)

let test_place_horizontal_edges_and_center () =
  check_string "left" "abc       "
    (Layout.place_horizontal ~width:10 ~pos:Position.left "abc");
  check_string "right" "       abc"
    (Layout.place_horizontal ~width:10 ~pos:Position.right "abc");
  (* gap = 7; round (7 * .5) = 4 on the right, so the odd column is right. *)
  check_string "center odd" "   abc    "
    (Layout.place_horizontal ~width:10 ~pos:Position.center "abc")

let test_place_horizontal_fractional_and_per_line_gap () =
  check_string "quarter" "     abc  "
    (Layout.place_horizontal ~width:10 ~pos:(Position.v 0.25) "abc");
  check_string "per-line gap" "  a  \n abc "
    (Layout.place_horizontal ~width:5 ~pos:Position.center "a\nabc")

let test_place_horizontal_is_noop_when_too_narrow () =
  check_string "no-op" "abcdefghij"
    (Layout.place_horizontal ~width:5 ~pos:Position.left "abcdefghij");
  check_string "negative width no-op" "x"
    (Layout.place_horizontal ~width:(-1) ~pos:Position.right "x")

let test_place_positions_are_clamped () =
  check_string "below zero becomes left" "x   "
    (Layout.place_horizontal ~width:4 ~pos:(Position.v (-1.0)) "x");
  check_string "above one becomes right" "   x"
    (Layout.place_horizontal ~width:4 ~pos:(Position.v 2.0) "x")

let test_place_horizontal_preserves_ansi () =
  check_string "ANSI preserved" "\x1b[1mbold\x1b[m      "
    (Layout.place_horizontal ~width:10 ~pos:Position.left "\x1b[1mbold\x1b[m")

let test_place_vertical_edges_and_center () =
  let empty = blank 2 in
  check_string "top"
    (lines [ "ab"; empty; empty; empty; empty ])
    (Layout.place_vertical ~height:5 ~pos:Position.top "ab");
  check_string "bottom"
    (lines [ empty; empty; empty; empty; "ab" ])
    (Layout.place_vertical ~height:5 ~pos:Position.bottom "ab");
  check_string "center"
    (lines [ empty; empty; "ab"; empty; empty ])
    (Layout.place_vertical ~height:5 ~pos:Position.center "ab")

let test_place_vertical_odd_center_and_widest_fill () =
  let empty = blank 2 in
  check_string "odd center"
    (lines [ empty; empty; "ab"; empty; empty; empty ])
    (Layout.place_vertical ~height:6 ~pos:Position.center "ab");
  check_string "widest filler"
    (lines [ "a"; "bbb"; blank 3; blank 3 ])
    (Layout.place_vertical ~height:4 ~pos:Position.top "a\nbbb")

let test_place_vertical_is_noop_when_too_short () =
  check_string "no-op" "a\nb\nc"
    (Layout.place_vertical ~height:2 ~pos:Position.top "a\nb\nc")

let test_place_composes_horizontal_then_vertical () =
  let row = "    ab    " in
  let empty = blank 10 in
  check_string "two-dimensional center"
    (lines [ empty; empty; row; empty; empty ])
    (Layout.place ~h:Position.center ~v:Position.center ~width:10 ~height:5 "ab")

(* Whitespace patterns and styles. *)

let test_whitespace_pattern_cycles_graphemes_and_partial_fills () =
  check_string "pattern" "ab·····"
    (Layout.place_horizontal ~whitespace:("·", Style.empty) ~width:7 ~pos:Position.left
       "ab");
  (* A two-cell glyph fits once in a three-cell gap; the remaining partial
     cell is filled with an ordinary space rather than a split glyph. *)
  check_string "partial wide glyph" "a界 "
    (Layout.place_horizontal ~whitespace:("界", Style.empty) ~width:4 ~pos:Position.left
       "a");
  check_string "empty pattern defaults to spaces" "a   "
    (Layout.place_horizontal ~whitespace:("", Style.empty) ~width:4 ~pos:Position.left "a")

let test_whitespace_style_covers_complete_fill () =
  let bold = Style.bold true Style.empty in
  check_string "styled fill" "a\x1b[1m···\x1b[m"
    (Layout.place_horizontal ~whitespace:("·", bold) ~width:4 ~pos:Position.left "a")

(* Range styling.  Ranges are half-open terminal-cell intervals. *)

let test_style_ranges_empty_and_zero_length () =
  check_string "empty ranges" "hello world" (Layout.style_ranges [] "hello world");
  let bold = Style.bold true Style.empty in
  check_string "zero-length range" "abc" (Layout.style_ranges [ (2, 2, bold) ] "abc")

let test_style_ranges_single_and_multiple () =
  let bold = Style.bold true Style.empty in
  let italic = Style.italic true Style.empty in
  check_string "single middle" "hello \x1b[1mworld\x1b[m"
    (Layout.style_ranges [ (6, 11, bold) ] "hello world");
  check_string "multiple" "\x1b[1mhello\x1b[m \x1b[3mworld\x1b[m"
    (Layout.style_ranges [ (0, 5, bold); (6, 11, italic) ] "hello world");
  check_string "adjacent" "\x1b[1mhello\x1b[m\x1b[3m world\x1b[m"
    (Layout.style_ranges [ (0, 5, bold); (5, 11, italic) ] "hello world")

let test_style_ranges_preserves_ansi_outside_range () =
  let bold = Style.bold true Style.empty in
  let input = "hello \x1b[32mworld\x1b[m" in
  check_string "ANSI after range" "\x1b[1mhello\x1b[m \x1b[32mworld\x1b[m"
    (Layout.style_ranges [ (0, 5, bold) ] input);
  (* A no-op style must not strip a sequence at all. *)
  let styled = "\x1b[31mred\x1b[m" in
  check_string "identity keeps ANSI" styled
    (Layout.style_ranges [ (0, 3, Style.empty) ] styled)

let test_style_ranges_clamps_wide_grapheme_boundaries () =
  let bold = Style.bold true Style.empty in
  let italic = Style.italic true Style.empty in
  (* Cells: Hello=0..5, space=5..6, 你=6..8, 好=8..10, space=10..11,
     and 世界=11..15.  Starting at cell 7 therefore expands to the whole
     你 cluster, yielding the upstream range fixture's 你好 selection. *)
  check_string "wide graphemes" "\x1b[1mHello\x1b[m \x1b[3m你好\x1b[m \x1b[1m世界\x1b[m"
    (Layout.style_ranges [ (0, 5, bold); (7, 10, italic); (11, 50, bold) ] "Hello 你好 世界")

(* Grapheme-indexed styling. *)

let test_style_runes_selected_and_unselected_runs () =
  let reverse = Style.reverse true Style.empty in
  check_string "first" "\x1b[7mh\x1b[mello"
    (Layout.style_runes reverse Style.empty "hello" ~indices:[ 0 ]);
  check_string "scattered" "h\x1b[7me\x1b[ml\x1b[7ml\x1b[mo"
    (Layout.style_runes reverse Style.empty "hello" ~indices:[ 1; 3 ]);
  check_string "adjacent wide clusters" "你\x1b[7m好\x1b[m"
    (Layout.style_runes reverse Style.empty "你好" ~indices:[ 1 ])

let test_style_runes_never_splits_combining_cluster () =
  let reverse = Style.reverse true Style.empty in
  check_string "combining cluster" "\x1b[7me\xCC\x81\x1b[mx"
    (Layout.style_runes reverse Style.empty "e\xCC\x81x" ~indices:[ 0 ])

let test_style_runes_ignores_out_of_bounds_and_keeps_ansi () =
  let reverse = Style.reverse true Style.empty in
  check_string "out of bounds" "\x1b[7ma\x1b[mbc"
    (Layout.style_runes reverse Style.empty "abc" ~indices:[ 0; 99 ]);
  let input = "\x1b[31mhello\x1b[m" in
  check_string "identity ANSI" input
    (Layout.style_runes Style.empty Style.empty input ~indices:[ 1 ])

let cases : unit Alcotest.test_case list =
  [
    ("width: widest line", `Quick, test_width_uses_widest_line);
    ("width: ANSI and graphemes", `Quick, test_width_ignores_ansi_and_uses_grapheme_cells);
    ("height", `Quick, test_height_counts_lines_and_trailing_empty_line);
    ("size", `Quick, test_size_combines_width_and_height);
    ("join horizontal: empty/single", `Quick, test_join_horizontal_empty_and_single);
    ("join horizontal: edges", `Quick, test_join_horizontal_top_and_bottom);
    ("join horizontal: fractional", `Quick, test_join_horizontal_fractional_split);
    ("join horizontal: widths", `Quick, test_join_horizontal_pads_each_block_to_its_width);
    ( "join horizontal: ANSI/graphemes",
      `Quick,
      test_join_horizontal_uses_ansi_and_grapheme_width );
    ("join vertical: empty/single", `Quick, test_join_vertical_empty_and_single);
    ("join vertical: edges", `Quick, test_join_vertical_left_and_right);
    ("join vertical: fractional/odd", `Quick, test_join_vertical_fractional_and_odd_center);
    ("place horizontal: edges/center", `Quick, test_place_horizontal_edges_and_center);
    ( "place horizontal: fractional/per-line",
      `Quick,
      test_place_horizontal_fractional_and_per_line_gap );
    ("place horizontal: no-op", `Quick, test_place_horizontal_is_noop_when_too_narrow);
    ("place horizontal: clamped positions", `Quick, test_place_positions_are_clamped);
    ("place horizontal: ANSI", `Quick, test_place_horizontal_preserves_ansi);
    ("place vertical: edges/center", `Quick, test_place_vertical_edges_and_center);
    ("place vertical: odd/widest", `Quick, test_place_vertical_odd_center_and_widest_fill);
    ("place vertical: no-op", `Quick, test_place_vertical_is_noop_when_too_short);
    ("place: two-dimensional", `Quick, test_place_composes_horizontal_then_vertical);
    ( "whitespace: pattern/partial",
      `Quick,
      test_whitespace_pattern_cycles_graphemes_and_partial_fills );
    ("whitespace: style", `Quick, test_whitespace_style_covers_complete_fill);
    ("style ranges: empty/zero", `Quick, test_style_ranges_empty_and_zero_length);
    ("style ranges: single/multiple", `Quick, test_style_ranges_single_and_multiple);
    ("style ranges: ANSI", `Quick, test_style_ranges_preserves_ansi_outside_range);
    ( "style ranges: wide graphemes",
      `Quick,
      test_style_ranges_clamps_wide_grapheme_boundaries );
    ("style runes: runs", `Quick, test_style_runes_selected_and_unselected_runs);
    ("style runes: combining", `Quick, test_style_runes_never_splits_combining_cluster);
    ( "style runes: boundaries/ANSI",
      `Quick,
      test_style_runes_ignores_out_of_bounds_and_keeps_ansi );
  ]
