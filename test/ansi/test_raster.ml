module Raster = Charamel_ansi.Raster
module Style = Charamel_ansi.Style
module Link = Charamel_ansi.Link

let check_string name expected actual = Alcotest.(check string) name expected actual
let check_int name expected actual = Alcotest.(check int) name expected actual
let check_bool name expected actual = Alcotest.(check bool) name expected actual
let bold = Style.of_sgr ~params:[ [ Some 1 ] ] Style.default
let cell_text grid ~x ~y = grid.(y).(x).Raster.text
let cell_width grid ~x ~y = grid.(y).(x).Raster.width
let cell_cont grid ~x ~y = grid.(y).(x).Raster.cont
let cell_style grid ~x ~y = grid.(y).(x).Raster.style

let test_wide_cluster () =
  let grid = Raster.of_string ~width:4 ~rows:1 "界x" in
  check_string "glyph cell" "界" (cell_text grid ~x:0 ~y:0);
  check_int "glyph width" 2 (cell_width grid ~x:0 ~y:0);
  check_bool "continuation flag" true (cell_cont grid ~x:1 ~y:0);
  check_string "continuation text" "" (cell_text grid ~x:1 ~y:0);
  check_string "next cell" "x" (cell_text grid ~x:2 ~y:0);
  check_bool "blank tail" true (Raster.is_blank grid.(0).(3))

let test_style_spans_the_run () =
  let grid = Raster.of_string ~width:4 ~rows:1 "\027[1mab\027[m" in
  check_bool "bold on the first cell" true (Style.equal bold (cell_style grid ~x:0 ~y:0));
  check_bool "bold on the second cell" true (Style.equal bold (cell_style grid ~x:1 ~y:0));
  check_bool "reset after the run" true
    (Style.equal Style.default (cell_style grid ~x:2 ~y:0))

let test_zero_width_joins_its_cluster () =
  let grid = Raster.of_string ~width:3 ~rows:1 "e\u{0301}x" in
  check_string "combining mark held by its cluster" "e\u{0301}" (cell_text grid ~x:0 ~y:0);
  check_int "cluster still one cell" 1 (cell_width grid ~x:0 ~y:0);
  check_string "following cluster" "x" (cell_text grid ~x:1 ~y:0)

let test_margin_wrap () =
  let grid = Raster.layout ~width:3 ~max_rows:2 "abcd" in
  check_int "rows produced" 2 (Array.length grid);
  check_string "first row tail" "c" (cell_text grid ~x:2 ~y:0);
  check_string "second row head" "d" (cell_text grid ~x:0 ~y:1)

let test_too_wide_occupies_nothing () =
  let grid = Raster.layout ~width:1 ~max_rows:1 "界x" in
  check_string "dropped glyph leaves no cell" "x" (cell_text grid ~x:0 ~y:0);
  check_int "one row" 1 (Array.length grid)

let test_unknown_sequences_drop () =
  let grid = Raster.layout ~width:4 ~max_rows:1 "\027[2Aab" in
  check_string "cursor movement drops" "a" (cell_text grid ~x:0 ~y:0);
  check_string "text follows at the head" "b" (cell_text grid ~x:1 ~y:0)

let test_pad_rows () =
  let grid = Raster.layout ~width:3 ~max_rows:3 "ab\ncd\nef" in
  let cut = Raster.pad_rows grid ~rows:2 ~width:3 in
  check_int "cut to two rows" 2 (Array.length cut);
  let padded = Raster.pad_rows grid ~rows:4 ~width:3 in
  check_int "padded to four rows" 4 (Array.length padded);
  check_bool "appended row is blank" true
    (Raster.is_blank padded.(3).(0) && Raster.is_blank padded.(3).(2))

let test_last_nonblank () =
  check_int "empty row" (-1) (Raster.last_nonblank (Array.make 3 Raster.blank));
  let grid = Raster.of_string ~width:4 ~rows:1 "ab" in
  check_int "occupied row" 1 (Raster.last_nonblank grid.(0))

let test_to_string_trims () =
  let grid = Raster.of_string ~width:5 ~rows:2 "abc\nx" in
  check_string "trailing cells trimmed" "abc\nx" (Raster.to_string ~trim:true grid);
  check_string "untrimmed keeps the padding" "abc  \nx    "
    (Raster.to_string ~trim:false grid)

let test_to_string_paints_the_pen () =
  let grid = Raster.of_string ~width:2 ~rows:1 "\027[1mab" in
  check_string "style set and reset" "\027[1mab\027[m" (Raster.to_string grid);
  let linked = Raster.of_string ~width:2 ~rows:1 "\027]8;;https://example.test\007go" in
  check_bool "link recorded" true
    (match linked.(0).(0).Raster.link with
    | Some (l : Link.t) -> String.equal l.Link.url "https://example.test"
    | None -> false);
  check_string "link closed by the renderer"
    "\027]8;;https://example.test\007go\027]8;;\007" (Raster.to_string linked)

let cases =
  [
    Alcotest.test_case "wide cluster and continuation" `Quick test_wide_cluster;
    Alcotest.test_case "style spans the run" `Quick test_style_spans_the_run;
    Alcotest.test_case "zero width joins its cluster" `Quick
      test_zero_width_joins_its_cluster;
    Alcotest.test_case "row margin wrap" `Quick test_margin_wrap;
    Alcotest.test_case "too wide occupies nothing" `Quick test_too_wide_occupies_nothing;
    Alcotest.test_case "unknown sequences drop" `Quick test_unknown_sequences_drop;
    Alcotest.test_case "pad rows cuts and pads" `Quick test_pad_rows;
    Alcotest.test_case "last non-blank cell" `Quick test_last_nonblank;
    Alcotest.test_case "to_string trims" `Quick test_to_string_trims;
    Alcotest.test_case "to_string paints the pen" `Quick test_to_string_paints_the_pen;
  ]
