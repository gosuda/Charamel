module Viewport = Charamel_bubbles.Viewport

let check_equal expected actual = Alcotest.(check (list string)) "lines" expected actual

let defaults () =
  let viewport = Viewport.v ~width:10 ~height:3 () in
  Alcotest.(check int) "width" 10 (Viewport.width viewport);
  Alcotest.(check int) "height" 3 (Viewport.height viewport);
  Alcotest.(check int) "empty total" 1 (Viewport.total_line_count viewport);
  check_equal [] (Viewport.visible_lines viewport)

let scroll_bounds () =
  let viewport =
    Viewport.v ~width:5 ~height:2 () |> Viewport.set_content "abcdef\nghij\nklmnop"
  in
  Alcotest.(check int) "line count" 3 (Viewport.total_line_count viewport);
  Alcotest.(check (list string))
    "initial lines" [ "abcde"; "ghij" ]
    (Viewport.visible_lines viewport);
  let viewport = Viewport.scroll_right 100 viewport in
  Alcotest.(check int) "right clamp" 1 (Viewport.x_offset viewport);
  let viewport = Viewport.goto_bottom viewport in
  Alcotest.(check bool) "bottom" true (Viewport.at_bottom viewport);
  let viewport = Viewport.goto_top viewport in
  Alcotest.(check bool) "top" true (Viewport.at_top viewport);
  Alcotest.(check int) "offset reset" 0 (Viewport.y_offset viewport);
  ignore viewport

let soft_wrap_and_gutter () =
  let gutter ({ index; soft; total_lines = _ } : Viewport.gutter_context) =
    if soft then "  | " else " " ^ string_of_int (index + 1) ^ " | "
  in
  let viewport =
    Viewport.v ~width:8 ~height:3 ~soft_wrap:true ~left_gutter:gutter ()
    |> Viewport.set_content "abcdef\nxy"
  in
  Alcotest.(check int) "wrapped lines" 3 (Viewport.total_line_count viewport);
  Alcotest.(check (list string))
    "wrapped output"
    [ " 1 | abc"; "  | def"; " 2 | xy" ]
    (Viewport.visible_lines viewport)

let highlights () =
  let style =
    Charamel_lipgloss.Style.background (Charamel_ansi.Color.Indexed 1)
      Charamel_lipgloss.Style.empty
  in
  let viewport =
    Viewport.v ~width:20 ~height:2 ~highlight_style:style ~selected_highlight_style:style
      ()
    |> Viewport.set_content "hello\nworld"
    |> Viewport.set_highlights [ (1, 4) ]
  in
  let first = List.hd (Viewport.visible_lines viewport) in
  Alcotest.(check bool) "highlight emits style" true (String.contains first '\027');
  let viewport = Viewport.goto_bottom viewport in
  let viewport = Viewport.set_highlights [ (0, 1); (6, 7) ] viewport in
  Alcotest.(check int) "nearest highlight keeps offset" 0 (Viewport.y_offset viewport);
  let viewport = Viewport.set_height 1 viewport |> Viewport.set_y_offset 1 in
  let viewport = Viewport.set_highlights [ (0, 1); (6, 7) ] viewport in
  Alcotest.(check int) "nearest highlight at offset" 1 (Viewport.y_offset viewport)

let key_navigation () =
  let viewport = Viewport.v ~width:10 ~height:2 () in
  let key = Charamel_tea.Key.v Charamel_tea.Key.Down in
  match Viewport.key viewport key with
  | Some Viewport.Down -> ()
  | _ -> Alcotest.fail "down key not bound"

let grapheme_highlights () =
  let style =
    Charamel_lipgloss.Style.background (Charamel_ansi.Color.Indexed 1)
      Charamel_lipgloss.Style.empty
  in
  let render = Charamel_lipgloss.Style.render style in
  let make content ranges =
    Viewport.v ~width:20 ~height:2 ~highlight_style:style ~selected_highlight_style:style
      ()
    |> Viewport.set_content content
    |> Viewport.set_highlights ranges
  in
  (* (1,2) is the cluster [你], occupying cells 1..3; byte indexing would cut the
     three-byte sequence apart. *)
  let first = List.hd (Viewport.visible_lines (make "a你好b" [ (1, 2) ])) in
  Alcotest.(check string) "wide cluster marked" ("a" ^ render "你" ^ "好b") first;
  let second = List.nth (Viewport.visible_lines (make "a你好b" [ (2, 3) ])) 0 in
  Alcotest.(check string) "second cluster marked" ("a你" ^ render "好" ^ "b") second;
  (* A range crossing the newline marks the tail of line 0 and the head of
     line 1: clusters [b, \n, c]. *)
  check_equal
    [ "a" ^ render "b"; render "c" ^ "d" ]
    (Viewport.visible_lines (make "ab\ncd" [ (1, 4) ]));
  (* Reversed bounds and out-of-range indices clamp instead of raising. *)
  check_equal
    [ "a" ^ render "b"; render "c" ^ "d" ]
    (Viewport.visible_lines (make "ab\ncd" [ (4, 1) ]))

let byte_offset_helper () =
  let open Alcotest in
  let of_bytes content ranges =
    Viewport.grapheme_ranges_of_byte_ranges
      (Viewport.v () |> Viewport.set_content content)
      ranges
  in
  check (list (pair int int)) "exact cluster" [ (1, 2) ] (of_bytes "a你好b" [ (1, 4) ]);
  check
    (list (pair int int))
    "partial clusters widen"
    [ (1, 3) ]
    (of_bytes "a你好b" [ (3, 6) ]);
  check
    (list (pair int int))
    "ascii passthrough"
    [ (0, 5) ]
    (of_bytes "hello world" [ (0, 5) ]);
  (* Offsets are in the CRLF-normalized coordinate space: content ["a\r\nb"] is
     stored as ["a\nb"], so bytes 1..2 are the newline cluster. *)
  check
    (list (pair int int))
    "crlf normalized offsets"
    [ (1, 2) ]
    (of_bytes "a\r\nb" [ (1, 2) ]);
  (* End to end: byte-offset search results feed [set_highlights] unchanged. *)
  let style =
    Charamel_lipgloss.Style.background (Charamel_ansi.Color.Indexed 1)
      Charamel_lipgloss.Style.empty
  in
  let ranges = of_bytes "a你好b" [ (4, 7) ] in
  let viewport =
    Viewport.v ~width:20 ~height:2 ~highlight_style:style ~selected_highlight_style:style
      ()
    |> Viewport.set_content "a你好b"
    |> Viewport.set_highlights ranges
  in
  Alcotest.(check string)
    "search highlight marked"
    ("a你" ^ Charamel_lipgloss.Style.render style "好" ^ "b")
    (List.hd (Viewport.visible_lines viewport))

let samples n =
  let rand = Random.State.make [| 20260926 |] in
  List.init n (fun _ -> QCheck2.Gen.generate1 ~rand Test_support.cjk_gen)

let widest_cluster s =
  List.fold_left
    (fun n cluster -> max n (Charamel_ansi.Width.grapheme_width cluster))
    0
    (Charamel_ansi.Width.graphemes s)

let row_fits_box text width =
  let viewport =
    Viewport.v ~width ~height:200 ~soft_wrap:true () |> Viewport.set_content text
  in
  let bound = max width (widest_cluster text) in
  List.iter
    (fun row ->
      Alcotest.(check bool)
        (Format.sprintf "row %S exceeds width %d" row width)
        true
        (Charamel_ansi.Text.width row <= bound))
    (Viewport.visible_lines viewport)

let visible_rows_fit_the_box () =
  List.iter
    (fun text -> List.iter (row_fits_box text) [ 2; 3; 5; 8; 13; 21 ])
    (samples 40)

let ensure_visible_follows_wrapped_rows () =
  let viewport =
    Viewport.v ~width:5 ~height:1 ~soft_wrap:true ()
    |> Viewport.set_content "abcdefghij\nk"
  in
  Alcotest.(check int)
    "the content wraps to three rows" 3
    (Viewport.total_line_count viewport);
  let viewport = Viewport.ensure_visible ~line:1 ~colstart:0 ~colend:1 viewport in
  Alcotest.(check int)
    "the offset counts wrapped rows, not logical lines" 2 (Viewport.y_offset viewport);
  Alcotest.(check (list string))
    "the second logical line is on screen" [ "k" ]
    (Viewport.visible_lines viewport)

let ensure_visible_accounts_for_the_gutter () =
  let viewport =
    Viewport.v ~width:8 ~height:1 ~soft_wrap:true ~left_gutter:(fun _ -> ">> ") ()
    |> Viewport.set_content "abcdefghij"
  in
  Alcotest.(check int)
    "the gutter narrows the wrap" 2
    (Viewport.total_line_count viewport);
  let viewport = Viewport.ensure_visible ~line:0 ~colstart:0 ~colend:1 viewport in
  Alcotest.(check int)
    "the first wrapped row needs no scroll" 0 (Viewport.y_offset viewport);
  let scrolled = Viewport.scroll_down 1 viewport in
  Alcotest.(check (list string))
    "the second wrapped row renders" [ ">> fghij" ]
    (Viewport.visible_lines scrolled)

let cases =
  [
    Alcotest_lwt.test_case_sync "defaults" `Quick defaults;
    Alcotest_lwt.test_case_sync "scroll bounds" `Quick scroll_bounds;
    Alcotest_lwt.test_case_sync "soft wrap and gutter" `Quick soft_wrap_and_gutter;
    Alcotest_lwt.test_case_sync "highlights" `Quick highlights;
    Alcotest_lwt.test_case_sync "key navigation" `Quick key_navigation;
    Alcotest_lwt.test_case_sync "grapheme highlights" `Quick grapheme_highlights;
    Alcotest_lwt.test_case_sync "byte offset helper" `Quick byte_offset_helper;
    Alcotest_lwt.test_case_sync "visible rows fit the box" `Quick visible_rows_fit_the_box;
    Alcotest_lwt.test_case_sync "ensure visible follows wrapped rows" `Quick
      ensure_visible_follows_wrapped_rows;
    Alcotest_lwt.test_case_sync "ensure visible accounts for the gutter" `Quick
      ensure_visible_accounts_for_the_gutter;
  ]
