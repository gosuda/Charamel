open Charamel_lipgloss

let color_eq = Alcotest.testable Color.pp Color.equal
let check_color name expected actual = Alcotest.(check color_eq) name expected actual
let check_bool name expected actual = Alcotest.(check bool) name expected actual

let rgba =
  Alcotest.testable
    (fun fmt { Color_util.r; g; b; alpha } ->
      Fmt.pf fmt "%d %d %d %a" r g b Fmt.float alpha)
    ( = )

let red = Option.get (Color.rgb 255 0 0)
let blue = Option.get (Color.rgb 0 0 255)
let white = Option.get (Color.rgb 255 255 255)
let black = Option.get (Color.rgb 0 0 0)

let test_rgba_clamps () =
  Alcotest.(check rgba)
    "components and alpha clamp"
    { Color_util.r = 255; g = 0; b = 0; alpha = 1.0 }
    (Color_util.rgba ~alpha:2.0 ~r:300 ~g:(-5) ~b:0 ());
  check_color "to_color drops alpha" red
    (Color_util.to_color (Color_util.rgba ~r:255 ~g:0 ~b:0 ()));
  Alcotest.(check rgba)
    "default color resolves to black"
    { Color_util.r = 0; g = 0; b = 0; alpha = 0.5 }
    (Color_util.alpha ~scale:0.5 Color.Default);
  Alcotest.(check rgba)
    "indexed color resolves through the palette"
    { Color_util.r = 0; g = 0; b = 255; alpha = 1.0 }
    (Color_util.alpha (Color.Indexed 21))

let test_darken_lighten () =
  check_color "darken white by half truncates"
    (Option.get (Color.rgb 127 127 127))
    (Color_util.darken white 0.5);
  check_color "lighten black by half"
    (Option.get (Color.rgb 127 127 127))
    (Color_util.lighten black 0.5);
  check_color "darken fully to black" black
    (Color_util.darken (Option.get (Color.rgb 10 10 10)) 1.0);
  check_color "lighten fully to white" white (Color_util.lighten red 1.0);
  check_color "percent clamps below zero" red (Color_util.darken red (-1.0));
  check_color "percent clamps above one" black (Color_util.darken white 2.0);
  check_color "darken of the default color is black" black
    (Color_util.darken Color.Default 0.5)

let test_complementary () =
  check_color "red complements to cyan"
    (Option.get (Color.rgb 0 255 255))
    (Color_util.complementary red);
  check_color "cyan complements back to red" red
    (Color_util.complementary (Option.get (Color.rgb 0 255 255)));
  check_color "grey keeps its hue of zero"
    (Option.get (Color.rgb 128 128 128))
    (Color_util.complementary (Option.get (Color.rgb 128 128 128)))

let test_is_dark () =
  check_bool "pure blue is light" false (Color_util.is_dark blue);
  check_bool "half blue is dark" true
    (Color_util.is_dark (Option.get (Color.rgb 0 0 128)));
  check_bool "the default color is dark" true (Color_util.is_dark Color.Default);
  check_bool "white is light" false (Color_util.is_dark white);
  check_bool "index 8 grey is light under HSL" false (Color_util.is_dark (Color.Basic 8));
  check_bool "the BT.601 rule calls index 8 dark" true (Color.is_dark (Color.Basic 8));
  check_bool "index 7 grey is light" false (Color_util.is_dark (Color.Basic 7))

let test_complete () =
  let triple = (Color.Basic 1, Color.Indexed 124, red) in
  check_color "ansi profile picks the ansi slot" (Color.Basic 1)
    (Color_util.complete Charamel_colorprofile.Ansi triple);
  check_color "ansi256 profile picks the palette slot" (Color.Indexed 124)
    (Color_util.complete Charamel_colorprofile.Ansi256 triple);
  check_color "truecolor profile picks the exact slot" red
    (Color_util.complete Charamel_colorprofile.True_color triple);
  check_color "no tty yields no color" Color.Default
    (Color_util.complete Charamel_colorprofile.No_tty triple);
  check_color "ascii yields no color" Color.Default
    (Color_util.complete Charamel_colorprofile.Ascii triple);
  check_color "adaptive dark truecolor" red
    (Color_util.complete_adaptive Charamel_colorprofile.True_color ~dark:true
       ~light:(Color.Basic 1, Color.Indexed 124, blue)
       ~night:triple);
  check_color "adaptive light ansi256" (Color.Indexed 7)
    (Color_util.complete_adaptive Charamel_colorprofile.Ansi256 ~dark:false
       ~light:(Color.Basic 1, Color.Indexed 7, blue)
       ~night:triple)

let cases =
  [
    Alcotest.test_case "rgba construction and conversion" `Quick test_rgba_clamps;
    Alcotest.test_case "darken and lighten" `Quick test_darken_lighten;
    Alcotest.test_case "complementary" `Quick test_complementary;
    Alcotest.test_case "is_dark" `Quick test_is_dark;
    Alcotest.test_case "complete and complete_adaptive" `Quick test_complete;
  ]
