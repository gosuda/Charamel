open Charamel_lipgloss

let color_eq = Alcotest.testable Color.pp Color.equal
let colors = Alcotest.(list color_eq)
let check_colors name expected actual = Alcotest.check colors name expected actual
let check_string name expected actual = Alcotest.(check string) name expected actual
let rgb r g b = Color.Rgb (r, g, b)
let red = rgb 255 0 0
let green = rgb 0 255 0
let blue = rgb 0 0 255
let yellow = rgb 255 255 0
let black = rgb 0 0 0
let white = rgb 255 255 255

let test_blend1d_vectors () =
  check_colors "black to white in five steps"
    [ black; rgb 59 59 59; rgb 119 119 119; rgb 185 185 185; white ]
    (Blending.blend1d ~steps:5 [ black; white ]);
  check_colors "red to blue in ten steps"
    [
      red;
      rgb 246 0 45;
      rgb 235 0 73;
      rgb 223 0 99;
      rgb 210 0 124;
      rgb 193 0 149;
      rgb 173 0 175;
      rgb 147 0 201;
      rgb 109 0 228;
      blue;
    ]
    (Blending.blend1d ~steps:10 [ red; blue ]);
  check_colors "three colors in four steps" [ red; green; green; blue ]
    (Blending.blend1d ~steps:4 [ red; green; blue ]);
  check_colors "four colors in six steps"
    [ red; yellow; yellow; green; green; blue ]
    (Blending.blend1d ~steps:6 [ red; yellow; green; blue ])

let test_blend1d_edges () =
  check_colors "negative steps" [] (Blending.blend1d ~steps:(-3) [ red; blue ]);
  check_colors "zero steps" [] (Blending.blend1d ~steps:0 []);
  check_colors "zero steps with a stop" [] (Blending.blend1d ~steps:0 [ red ]);
  check_colors "steps at most stops keeps leading stops" [ red; green ]
    (Blending.blend1d ~steps:2 [ red; green; blue; yellow; black ]);
  check_colors "one step takes the first stop" [ red ]
    (Blending.blend1d ~steps:1 [ red; blue ]);
  check_colors "one stop repeats" [ red; red; red ] (Blending.blend1d ~steps:3 [ red ]);
  check_colors "default stops are dropped"
    (Blending.blend1d ~steps:4 [ red; blue ])
    (Blending.blend1d ~steps:4 [ red; Color.Default; blue ]);
  check_colors "all default stops leave nothing" []
    (Blending.blend1d ~steps:3 [ Color.Default ])

let test_blend2d () =
  check_colors "row-major diagonal samples"
    [ red; rgb 202 0 137; rgb 202 0 137 ]
    (Blending.blend2d ~width:3 ~height:1 ~angle:0. [ red; blue ]);
  check_colors "empty stops" [] (Blending.blend2d ~width:2 ~height:2 ~angle:0. []);
  check_colors "single stop fills the grid" [ red; red; red; red ]
    (Blending.blend2d ~width:2 ~height:2 ~angle:45. [ red ]);
  check_colors "a zero dimension counts as one" [ red; red ]
    (Blending.blend2d ~width:0 ~height:2 ~angle:0. [ red; blue ]);
  check_colors "angle reduces modulo 360"
    (Blending.blend2d ~width:3 ~height:1 ~angle:0. [ red; blue ])
    (Blending.blend2d ~width:3 ~height:1 ~angle:720. [ red; blue ]);
  check_colors "a half turn mirrors the gradient"
    [ rgb 202 0 137; rgb 202 0 137; red ]
    (Blending.blend2d ~width:3 ~height:1 ~angle:180. [ red; blue ])

let cell ?(bg = Color.Default) color glyph =
  Charamel_ansi.Style.to_sgr { Charamel_ansi.Style.default with fg = color; bg } ^ glyph

let reset = "\027[m"

let frame ?bg g =
  let top = cell ?bg g.(0) "┌" ^ cell ?bg g.(1) "─" ^ cell ?bg g.(2) "┐" ^ reset in
  let body = cell ?bg g.(11) "│" ^ reset ^ "x" ^ cell ?bg g.(5) "│" ^ reset in
  let bottom = cell ?bg g.(10) "└" ^ cell ?bg g.(9) "─" ^ cell ?bg g.(8) "┘" ^ reset in
  top ^ "\n" ^ body ^ "\n" ^ bottom

let rotate a offset =
  let n = Array.length a in
  let step = (-offset mod n) + n in
  Array.init n (fun i -> a.((i + step) mod n))

let test_border_blend () =
  let gradient = Array.of_list (Blending.blend1d ~steps:12 [ red; blue ]) in
  let style = Style.border Border.normal Style.empty in
  check_string "no blend keeps the plain border" "┌─┐\n│x│\n└─┘" (Style.render style "x");
  let blended = Style.border_foreground_blend [ red; blue ] style in
  check_string "one gradient color per border glyph" (frame gradient)
    (Style.render blended "x");
  check_string "the offset rotates the gradient"
    (frame (rotate gradient 1))
    (Style.render (Style.border_foreground_blend_offset 1 blended) "x");
  check_string "an empty gradient keeps the plain border" "┌─┐\n│x│\n└─┘"
    (Style.render (Style.border_foreground_blend [] style) "x");
  check_string "the edge background styles blended glyphs" (frame ~bg:black gradient)
    (Style.render (Style.border_background (Sides_color.all black) blended) "x");
  check_string "unsetting the gradient restores the plain border" "┌─┐\n│x│\n└─┘"
    (Style.render (Style.unset_border_foreground_blend blended) "x")

let cases =
  [
    Alcotest.test_case "blend1d upstream vectors" `Quick test_blend1d_vectors;
    Alcotest.test_case "blend1d edge cases" `Quick test_blend1d_edges;
    Alcotest.test_case "blend2d" `Quick test_blend2d;
    Alcotest.test_case "border foreground blend" `Quick test_border_blend;
  ]
