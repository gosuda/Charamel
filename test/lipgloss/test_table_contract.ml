open Charamel_lipgloss
module Data = Table.Data

let check_render name expected table =
  Alcotest.(check string) name expected (Table.render table)

let check_int name expected actual = Alcotest.(check int) name expected actual
let check_string name expected actual = Alcotest.(check string) name expected actual
let check_bool name expected actual = Alcotest.(check bool) name expected actual

let has needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i = i + n <= h && (String.sub haystack i n = needle || loop (i + 1)) in
  loop 0

let red = Option.get (Color.rgb 255 0 0)

let two_by_two =
  Table.v ~headers:[ "A"; "B" ] ~rows:[ [ "1"; "2" ] ] ~border:Border.normal ()

let test_border_column_toggle () =
  check_render "no column separators" "┌──┐\n│AB│\n├──┤\n│12│\n└──┘"
    (Table.v ~border_column:false ~headers:[ "A"; "B" ] ~rows:[ [ "1"; "2" ] ] ());
  check_render "markdown shape" "│AB│\n├──┤\n│12│"
    (Table.v ~border_top:false ~border_bottom:false ~border_column:false
       ~headers:[ "A"; "B" ]
       ~rows:[ [ "1"; "2" ] ]
       ());
  check_render "no edges" "──\nAB\n──\n12\n──"
    (Table.v ~border_left:false ~border_right:false ~border_column:false
       ~headers:[ "A"; "B" ]
       ~rows:[ [ "1"; "2" ] ]
       ())

let test_border_row_toggle () =
  check_render "row separators" "┌─┐\n│a│\n├─┤\n│b│\n└─┘"
    (Table.v ~border_row:true ~rows:[ [ "a" ]; [ "b" ] ] ());
  check_render "no separators by default" "┌─┐\n│a│\n│b│\n└─┘"
    (Table.v ~rows:[ [ "a" ]; [ "b" ] ] ())

let test_data_source () =
  let d = Data.rows [ [ "a"; "b" ]; [ "c"; "d" ] ] in
  check_string "cell" "c" (Data.at d ~row:1 ~col:0);
  check_string "row out of range" "" (Data.at d ~row:5 ~col:0);
  check_string "column out of range" "" (Data.at d ~row:0 ~col:9);
  check_int "row count" 2 (Data.row_count d);
  check_int "columns" 2 (Data.columns d);
  check_int "appended" 3 (Data.row_count (Data.append d [ "e" ]));
  check_bool "filtered matrix" true
    (Stdlib.List.equal
       (Stdlib.List.equal String.equal)
       [ [ "c"; "d" ] ]
       (Data.matrix (Data.filter d (fun i -> i = 1))));
  check_render "filtered data renders one row" "┌─┐\n│b│\n└─┘"
    (Table.v ~data:(Data.filter (Data.rows [ [ "a" ]; [ "b" ] ]) (fun i -> i = 1)) ())

let test_rows_shorthand_matches_data () =
  check_render "same render" (Table.render two_by_two)
    (Table.v ~headers:[ "A"; "B" ] ~data:(Data.rows [ [ "1"; "2" ] ]) ())

let test_fit_content () =
  let padded = Style.padding (Sides.xy ~x:1 ~y:0) Style.empty in
  let style ~row:_ ~col:_ = padded in
  let fitted = Table.v ~headers:[ "A" ] ~rows:[ [ "1" ] ] ~style ~fit_content:true () in
  check_int "content width" 5 (Layout.width (Table.render fitted));
  check_int "width is a maximum" 5
    (Layout.width
       (Table.render
          (Table.v ~width:20 ~headers:[ "A" ] ~rows:[ [ "1" ] ] ~style ~fit_content:true
             ())));
  check_int "without fit content the width is a target" 20
    (Layout.width
       (Table.render (Table.v ~width:20 ~headers:[ "A" ] ~rows:[ [ "1" ] ] ~style ())))

let test_window_accessors () =
  let windowed =
    Table.v ~offset:1 ~height:4 ~rows:[ [ "a" ]; [ "b" ]; [ "c" ]; [ "d" ] ] ()
  in
  check_int "y offset" 1 (Table.y_offset windowed);
  check_int "height" 4 (Table.height windowed);
  check_int "first visible" 1 (Table.first_visible_row windowed);
  check_int "last visible" 1 (Table.last_visible_row windowed);
  check_int "visible rows" 1 (Table.visible_rows windowed);
  check_render "windowed render" "┌─┐\n│b│\n│…│\n└─┘" windowed;
  let fits = Table.v ~rows:[ [ "a" ]; [ "b" ] ] () in
  check_int "unconstrained height" 0 (Table.height fits);
  check_int "everything fits reports -1" (-1) (Table.last_visible_row fits);
  check_int "all rows visible" 2 (Table.visible_rows fits);
  check_int "data survives the window" 4 (Data.row_count (Table.data windowed));
  let past = Table.v ~offset:9 ~rows:[ [ "a" ] ] () in
  check_int "offset past the end" 1 (Table.first_visible_row past);
  check_int "nothing visible" 0 (Table.visible_rows past);
  check_render "frame only" "┌─┐\n└─┘" past

let test_base_and_border_styles () =
  let paint glyph = "\027[38;2;255;0;0m" ^ glyph ^ "\027[m" in
  let base =
    Table.v ~rows:[ [ "a" ] ] ~base_style:(Style.foreground red Style.empty) ()
  in
  check_render "base style covers border and cell"
    (paint "┌" ^ paint "─" ^ paint "┐" ^ "\n" ^ paint "│" ^ paint "a" ^ paint "│" ^ "\n"
   ^ paint "└" ^ paint "─" ^ paint "┘")
    base;
  let bordered =
    Table.v ~rows:[ [ "a" ] ] ~border_style:(Style.foreground red Style.empty) ()
  in
  check_bool "border styled alone" true
    (has (paint "┌" ^ paint "─") (Table.render bordered));
  check_bool "cell stays plain" true (has (paint "│" ^ "a") (Table.render bordered))

let cases =
  [
    Alcotest.test_case "border column toggle" `Quick test_border_column_toggle;
    Alcotest.test_case "border row toggle" `Quick test_border_row_toggle;
    Alcotest.test_case "data source" `Quick test_data_source;
    Alcotest.test_case "rows shorthand" `Quick test_rows_shorthand_matches_data;
    Alcotest.test_case "fit content" `Quick test_fit_content;
    Alcotest.test_case "window accessors" `Quick test_window_accessors;
    Alcotest.test_case "base and border styles" `Quick test_base_and_border_styles;
  ]
