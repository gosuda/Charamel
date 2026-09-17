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

let cases =
  [
    Alcotest.test_case "defaults" `Quick defaults;
    Alcotest.test_case "scroll bounds" `Quick scroll_bounds;
    Alcotest.test_case "soft wrap and gutter" `Quick soft_wrap_and_gutter;
    Alcotest.test_case "highlights" `Quick highlights;
    Alcotest.test_case "key navigation" `Quick key_navigation;
  ]
