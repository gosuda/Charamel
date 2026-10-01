open Charamel_lipgloss

let check_string name expected actual = Alcotest.(check string) name expected actual
let check_int name expected actual = Alcotest.(check int) name expected actual
let check_bool name expected actual = Alcotest.(check bool) name expected actual
let blank = Charamel_ansi.Raster.blank
let equal_cell = Charamel_ansi.Raster.equal
let cell_text cell = cell.Charamel_ansi.Raster.text

let hit_id hit =
  match (hit : Compositor.hit option) with Some { id; _ } -> id | None -> ""

let test_compositor_golden () =
  let layers = [ Layer.v ~x:2 ~y:1 ~id:"a" "ab"; Layer.v ~id:"b" ~z:1 ~x:2 ~y:1 "CD" ] in
  let c = Compositor.v layers in
  check_string "stacked render" "\n  CD" (Compositor.render c);
  check_string "topmost layer answers" "b" (hit_id (Compositor.hit c ~x:3 ~y:1));
  check_string "origin holds no layer" "" (hit_id (Compositor.hit c ~x:0 ~y:0));
  match Compositor.bounds c with
  | None -> Alcotest.fail "bounds expected"
  | Some b ->
      check_int "left" 0 b.x0;
      check_int "top" 0 b.y0;
      check_int "right" 4 b.x1;
      check_int "bottom" 2 b.y1

let test_compositor_layers () =
  let c = Compositor.v [ Layer.v "a" ] in
  let grown = Compositor.add c [ Layer.v ~id:"b" ~x:1 "b" ] in
  check_string "added layer hits" "b" (hit_id (Compositor.hit grown ~x:1 ~y:0));
  check_string "found by identifier" "b"
    (Layer.id (Option.get (Compositor.find grown "b")));
  check_bool "unknown identifier" true (Compositor.find grown "zz" = None);
  check_string "layers render together" "ab" (Compositor.render grown)

let test_layer_tree () =
  let nested = Layer.v ~id:"a" ~x:2 ~y:1 "ab" in
  let tree =
    Layer.add (Layer.v ~id:"r" "root") [ nested; Layer.v ~id:"c" ~z:3 "child" ]
  in
  check_string "descendant found" "ab" (Layer.content (Option.get (Layer.find tree "a")));
  check_bool "empty identifier never found" true (Layer.find (Layer.v "x") "" = None);
  check_int "children appended" 2 (Stdlib.List.length (Layer.children tree));
  check_int "deepest z wins" 3 (Layer.max_z tree);
  check_int "horizontal offset kept" 2 (Layer.x nested);
  check_int "vertical offset kept" 1 (Layer.y nested);
  check_string "of_content takes no identifier" "" (Layer.id (Layer.of_content "z"))

let test_canvas_cells () =
  let c = Canvas.create ~width:3 ~height:2 in
  check_bool "fresh cell is blank" true (equal_cell blank (Canvas.cell_at c ~x:0 ~y:0));
  check_bool "outside is blank" true (equal_cell blank (Canvas.cell_at c ~x:9 ~y:9));
  let cell = { blank with Charamel_ansi.Raster.text = "x" } in
  let painted = Canvas.set_cell c ~x:1 ~y:1 cell in
  check_string "cell written" "x" (cell_text (Canvas.cell_at painted ~x:1 ~y:1));
  check_string "source untouched" " " (cell_text (Canvas.cell_at c ~x:1 ~y:1));
  check_string "render trims" "\n x" (Canvas.render painted);
  check_string "out of range write ignored" "\n"
    (Canvas.render (Canvas.set_cell c ~x:9 ~y:0 cell))

let test_canvas_draw () =
  let empty = Canvas.create ~width:6 ~height:1 in
  let once = Canvas.draw empty ~x:0 ~y:0 "ab" in
  let drawn = Canvas.draw once ~x:1 ~y:0 "cd" in
  check_string "later draw overwrites" "acd" (Canvas.render drawn);
  check_string "clear empties the canvas" "" (Canvas.render (Canvas.clear drawn));
  let narrow = Canvas.create ~width:3 ~height:1 in
  let styled = Canvas.draw narrow ~x:0 ~y:0 "\027[1mb" in
  check_string "style survives the canvas" "\027[1mb\027[m" (Canvas.render styled);
  let tall = Canvas.create ~width:3 ~height:2 in
  let wrapped = Canvas.draw tall ~x:1 ~y:0 "abcd" in
  check_string "draw wraps inside the region" " ab\n cd" (Canvas.render wrapped);
  let band = Canvas.create ~width:6 ~height:1 in
  let filled = Canvas.draw band ~x:0 ~y:0 "AAAAAA" in
  let over = Canvas.draw filled ~x:2 ~y:0 "ab" in
  check_string "draw paints only its own cells" "AAabAA" (Canvas.render over);
  let split = Canvas.draw (Canvas.draw filled ~x:0 ~y:0 "ab") ~x:4 ~y:0 "cd" in
  check_string "side by side layers keep the fill" "abAAcd" (Canvas.render split);
  let pair = Canvas.create ~width:2 ~height:1 in
  check_string "empty text draws nothing" ""
    (Canvas.render (Canvas.draw pair ~x:0 ~y:0 ""))

let test_canvas_resize () =
  let empty = Canvas.create ~width:4 ~height:2 in
  let first = Canvas.draw empty ~x:0 ~y:0 "abcd" in
  let filled = Canvas.draw first ~x:0 ~y:1 "efgh" in
  let grown = Canvas.resize filled ~width:6 ~height:3 in
  check_int "width grown" 6 (Canvas.width grown);
  check_int "height grown" 3 (Canvas.height grown);
  check_string "content kept at the corner" "abcd\nefgh\n" (Canvas.render grown);
  let cut = Canvas.resize filled ~width:2 ~height:1 in
  check_string "content cut" "ab" (Canvas.render cut)

let cases =
  [
    Alcotest.test_case "compositor golden" `Quick test_compositor_golden;
    Alcotest.test_case "compositor layers" `Quick test_compositor_layers;
    Alcotest.test_case "layer tree" `Quick test_layer_tree;
    Alcotest.test_case "canvas cells" `Quick test_canvas_cells;
    Alcotest.test_case "canvas draw" `Quick test_canvas_draw;
    Alcotest.test_case "canvas resize" `Quick test_canvas_resize;
  ]
