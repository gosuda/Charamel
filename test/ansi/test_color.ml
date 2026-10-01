let color : Charamel_ansi.Color.t Alcotest.testable =
  Alcotest.of_pp Charamel_ansi.Color.pp

let renders name expected actual = Alcotest.check color name expected actual

let checked_constructors =
  Alcotest.test_case "checked constructors" `Quick (fun () ->
      Alcotest.check
        Alcotest.(option color)
        "basic 0" (Some (Charamel_ansi.Color.Basic 0)) (Charamel_ansi.Color.basic 0);
      Alcotest.check
        Alcotest.(option color)
        "basic 15" (Some (Charamel_ansi.Color.Basic 15))
        (Charamel_ansi.Color.basic 15);
      Alcotest.check
        Alcotest.(option color)
        "basic 16 rejects" None
        (Charamel_ansi.Color.basic 16);
      Alcotest.check
        Alcotest.(option color)
        "basic -1 rejects" None
        (Charamel_ansi.Color.basic (-1));
      Alcotest.check
        Alcotest.(option color)
        "indexed 255" (Some (Charamel_ansi.Color.Indexed 255))
        (Charamel_ansi.Color.indexed 255);
      Alcotest.check
        Alcotest.(option color)
        "indexed 256 rejects" None
        (Charamel_ansi.Color.indexed 256);
      Alcotest.check
        Alcotest.(option color)
        "rgb in range"
        (Some (Charamel_ansi.Color.Rgb (0, 255, 3)))
        (Charamel_ansi.Color.rgb 0 255 3);
      Alcotest.check
        Alcotest.(option color)
        "rgb 256 rejects" None
        (Charamel_ansi.Color.rgb 256 0 0);
      Alcotest.check
        Alcotest.(option color)
        "rgb -1 rejects" None
        (Charamel_ansi.Color.rgb 0 (-1) 0))

let of_hex_ =
  Alcotest.test_case "of_hex" `Quick (fun () ->
      let hex input expected =
        Alcotest.check
          Alcotest.(option color)
          input expected
          (Charamel_ansi.Color.of_hex input)
      in
      hex "#ff8537" (Some (Charamel_ansi.Color.Rgb (255, 133, 55)));
      hex "#fff" (Some (Charamel_ansi.Color.Rgb (255, 255, 255)));
      hex "#abc" (Some (Charamel_ansi.Color.Rgb (170, 187, 204)));
      hex "#ABC" (Some (Charamel_ansi.Color.Rgb (170, 187, 204)));
      hex "#FFFFFF" (Some (Charamel_ansi.Color.Rgb (255, 255, 255)));
      hex "#000" (Some (Charamel_ansi.Color.Rgb (0, 0, 0)));
      hex "ff8537" None;
      hex "#" None;
      hex "#ff" None;
      hex "#ffff" None;
      hex "#ff853" None;
      hex "#ff8537f" None;
      hex "#gg8537" None;
      hex "" None)

let of_hex_or_ =
  Alcotest.test_case "of_hex_or" `Quick (fun () ->
      renders "valid hex ignores default"
        (Charamel_ansi.Color.Rgb (255, 133, 55))
        (Charamel_ansi.Color.of_hex_or ~default:Charamel_ansi.Color.Default "#ff8537");
      renders "invalid hex falls back to the implicit default" Charamel_ansi.Color.Default
        (Charamel_ansi.Color.of_hex_or "not-a-color");
      renders "invalid hex falls back to the given default"
        (Charamel_ansi.Color.Indexed 245)
        (Charamel_ansi.Color.of_hex_or ~default:(Charamel_ansi.Color.Indexed 245)
           "not-a-color"))

let of_string_ =
  Alcotest.test_case "of_string hex or decimal index" `Quick (fun () ->
      let parses input expected =
        Alcotest.check
          Alcotest.(option color)
          input expected
          (Charamel_ansi.Color.of_string input)
      in
      parses "1" (Some (Charamel_ansi.Color.Basic 1));
      parses "+1" (Some (Charamel_ansi.Color.Basic 1));
      parses "15" (Some (Charamel_ansi.Color.Basic 15));
      parses "21" (Some (Charamel_ansi.Color.Indexed 21));
      parses "255" (Some (Charamel_ansi.Color.Indexed 255));
      parses "-1" (Some (Charamel_ansi.Color.Basic 1));
      parses "#ff0000" (Some (Charamel_ansi.Color.Rgb (255, 0, 0)));
      parses "#f00" (Some (Charamel_ansi.Color.Rgb (255, 0, 0)));
      parses "16711680" (Some (Charamel_ansi.Color.Rgb (255, 0, 0)));
      parses "99999999" (Some (Charamel_ansi.Color.Rgb (245, 224, 255)));
      parses "zz" None;
      parses "" None;
      parses "-" None;
      parses "0x21" None;
      parses "1_000" None;
      parses " 1" None;
      parses "#gg8537" None;
      parses "99999999999999999999999" None)

(* The expected 256-color values restate the upstream TestHexTo256 rows as
   8-bit components; the upstream fractional inputs round to these bytes. *)
let to_ansi256_ =
  Alcotest.test_case "to_ansi256 upstream vectors" `Quick (fun () ->
      let vector name r g b n =
        renders name (Charamel_ansi.Color.Indexed n)
          (Charamel_ansi.Color.to_ansi256 (Charamel_ansi.Color.Rgb (r, g, b)))
      in
      vector "white" 255 255 255 231;
      vector "offwhite" 238 238 238 255;
      vector "slightly brighter than offwhite" 242 242 242 255;
      vector "red" 255 0 0 196;
      vector "silver foil" 175 175 175 145;
      vector "silver chalice" 178 178 178 249;
      vector "slightly closer to silver foil" 176 176 176 145;
      vector "slightly closer to silver chalice" 177 177 177 249;
      vector "gray" 128 128 128 244;
      vector "orange" 255 133 85 209;
      renders "indexed identity" (Charamel_ansi.Color.Indexed 209)
        (Charamel_ansi.Color.to_ansi256 (Charamel_ansi.Color.Indexed 209));
      renders "default identity" Charamel_ansi.Color.Default
        (Charamel_ansi.Color.to_ansi256 Charamel_ansi.Color.Default);
      renders "basic dark red to cube" (Charamel_ansi.Color.Indexed 88)
        (Charamel_ansi.Color.to_ansi256 (Charamel_ansi.Color.Basic 1)))

(* The 256 expected values below are transcribed from the ansi256To16 table
   of .references/x/ansi/color.go (charmbracelet/x/ansi, MIT). *)
let ansi256_to16_reference =
  [|
    0;
    1;
    2;
    3;
    4;
    5;
    6;
    7;
    8;
    9;
    10;
    11;
    12;
    13;
    14;
    15;
    0;
    4;
    4;
    4;
    12;
    12;
    2;
    6;
    4;
    4;
    12;
    12;
    2;
    2;
    6;
    4;
    12;
    12;
    2;
    2;
    2;
    6;
    12;
    12;
    10;
    10;
    10;
    10;
    14;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    1;
    5;
    4;
    4;
    12;
    12;
    3;
    8;
    4;
    4;
    12;
    12;
    2;
    2;
    6;
    4;
    12;
    12;
    2;
    2;
    2;
    6;
    12;
    12;
    10;
    10;
    10;
    10;
    14;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    1;
    1;
    5;
    4;
    12;
    12;
    1;
    1;
    5;
    4;
    12;
    12;
    3;
    3;
    8;
    4;
    12;
    12;
    2;
    2;
    2;
    6;
    12;
    12;
    10;
    10;
    10;
    10;
    14;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    1;
    1;
    1;
    5;
    12;
    12;
    1;
    1;
    1;
    5;
    12;
    12;
    1;
    1;
    1;
    5;
    12;
    12;
    3;
    3;
    3;
    7;
    12;
    12;
    10;
    10;
    10;
    10;
    14;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    9;
    9;
    9;
    9;
    13;
    12;
    9;
    9;
    9;
    9;
    13;
    12;
    9;
    9;
    9;
    9;
    13;
    12;
    9;
    9;
    9;
    9;
    13;
    12;
    11;
    11;
    11;
    11;
    7;
    12;
    10;
    10;
    10;
    10;
    10;
    14;
    9;
    9;
    9;
    9;
    9;
    13;
    9;
    9;
    9;
    9;
    9;
    13;
    9;
    9;
    9;
    9;
    9;
    13;
    9;
    9;
    9;
    9;
    9;
    13;
    9;
    9;
    9;
    9;
    9;
    13;
    11;
    11;
    11;
    11;
    11;
    15;
    0;
    0;
    0;
    0;
    0;
    0;
    8;
    8;
    8;
    8;
    8;
    8;
    7;
    7;
    7;
    7;
    7;
    7;
    15;
    15;
    15;
    15;
    15;
    15;
  |]

let to_ansi16_table =
  Alcotest.test_case "to_ansi16 full table" `Quick (fun () ->
      Array.iteri
        (fun i expected ->
          renders
            (Fmt.str "indexed %d to basic %d" i expected)
            (Charamel_ansi.Color.Basic expected)
            (Charamel_ansi.Color.to_ansi16 (Charamel_ansi.Color.Indexed i)))
        ansi256_to16_reference)

let to_ansi16_ =
  Alcotest.test_case "to_ansi16" `Quick (fun () ->
      let vector name n b =
        renders name (Charamel_ansi.Color.Basic b)
          (Charamel_ansi.Color.to_ansi16 (Charamel_ansi.Color.Indexed n))
      in
      vector "black" 0 0;
      vector "bright white" 15 15;
      vector "cube black" 16 0;
      vector "cube red" 196 9;
      vector "orange indexed" 209 9;
      vector "cube white" 231 15;
      vector "gray" 244 7;
      vector "top gray" 255 15;
      renders "basic identity" (Charamel_ansi.Color.Basic 4)
        (Charamel_ansi.Color.to_ansi16 (Charamel_ansi.Color.Basic 4));
      renders "default identity" Charamel_ansi.Color.Default
        (Charamel_ansi.Color.to_ansi16 Charamel_ansi.Color.Default);
      renders "rgb red round trip" (Charamel_ansi.Color.Basic 9)
        (Charamel_ansi.Color.to_ansi16
           (Charamel_ansi.Color.to_ansi256 (Charamel_ansi.Color.Rgb (255, 0, 0))));
      Alcotest.check
        Alcotest.(option color)
        "#ff8537 chain" (Some (Charamel_ansi.Color.Basic 9))
        (let open Charamel_ansi.Color in
         let c = of_hex "#ff8537" in
         match c with Some c -> Some (to_ansi16 (to_ansi256 c)) | None -> None))

let pp_ =
  Alcotest.test_case "pp" `Quick (fun () ->
      let show c = Format.asprintf "%a" Charamel_ansi.Color.pp c in
      Alcotest.check Alcotest.string "default" "default"
        (show Charamel_ansi.Color.Default);
      Alcotest.check Alcotest.string "basic" "basic 9"
        (show (Charamel_ansi.Color.Basic 9));
      Alcotest.check Alcotest.string "indexed" "indexed 209"
        (show (Charamel_ansi.Color.Indexed 209));
      Alcotest.check Alcotest.string "rgb" "rgb 255 133 85"
        (show (Charamel_ansi.Color.Rgb (255, 133, 85))))

let equal_ =
  Alcotest.test_case "equal" `Quick (fun () ->
      let eq name expected a b =
        Alcotest.check Alcotest.bool name expected (Charamel_ansi.Color.equal a b)
      in
      eq "same rgb" true
        (Charamel_ansi.Color.Rgb (1, 2, 3))
        (Charamel_ansi.Color.Rgb (1, 2, 3));
      eq "kinds differ" false (Charamel_ansi.Color.Basic 1)
        (Charamel_ansi.Color.Indexed 1);
      eq "default is not black" false Charamel_ansi.Color.Default
        (Charamel_ansi.Color.Basic 0);
      eq "different rgb" false
        (Charamel_ansi.Color.Rgb (1, 2, 3))
        (Charamel_ansi.Color.Rgb (1, 2, 4)))

let rgb = Alcotest.(option (triple int int int))

let to_rgb_ =
  Alcotest.test_case "to_rgb palette resolution" `Quick (fun () ->
      let rgb' name expected c =
        Alcotest.check rgb name expected (Charamel_ansi.Color.to_rgb c)
      in
      rgb' "default" None Charamel_ansi.Color.Default;
      rgb' "basic silver" (Some (192, 192, 192)) (Charamel_ansi.Color.Basic 7);
      rgb' "basic grey" (Some (128, 128, 128)) (Charamel_ansi.Color.Basic 8);
      rgb' "basic above range clamps"
        (Some (255, 255, 255))
        (Charamel_ansi.Color.Basic 99);
      rgb' "indexed first sixteen share the ansi palette"
        (Some (192, 192, 192))
        (Charamel_ansi.Color.Indexed 7);
      rgb' "indexed cube first" (Some (0, 0, 0)) (Charamel_ansi.Color.Indexed 16);
      rgb' "indexed cube last" (Some (255, 255, 255)) (Charamel_ansi.Color.Indexed 231);
      rgb' "indexed cube orange" (Some (255, 135, 95)) (Charamel_ansi.Color.Indexed 209);
      rgb' "indexed grey ramp first" (Some (8, 8, 8)) (Charamel_ansi.Color.Indexed 232);
      rgb' "indexed grey ramp last"
        (Some (238, 238, 238))
        (Charamel_ansi.Color.Indexed 255);
      rgb' "indexed below range clamps"
        (Some (0, 0, 0))
        (Charamel_ansi.Color.Indexed (-2));
      rgb' "indexed above range clamps"
        (Some (238, 238, 238))
        (Charamel_ansi.Color.Indexed 300);
      rgb' "rgb identity" (Some (255, 133, 85)) (Charamel_ansi.Color.Rgb (255, 133, 85));
      rgb' "rgb components clamp"
        (Some (255, 0, 12))
        (Charamel_ansi.Color.Rgb (300, -5, 12)))

let is_dark_ =
  Alcotest.test_case "is_dark palette boundary" `Quick (fun () ->
      let dark name expected c =
        Alcotest.check Alcotest.bool name expected (Charamel_ansi.Color.is_dark c)
      in
      dark "default is dark" true Charamel_ansi.Color.Default;
      dark "indexed 8 is dark" true (Charamel_ansi.Color.Indexed 8);
      dark "indexed 7 is light" false (Charamel_ansi.Color.Indexed 7);
      dark "basic 0 is dark" true (Charamel_ansi.Color.Basic 0);
      dark "basic 15 is light" false (Charamel_ansi.Color.Basic 15);
      dark "grey 128 is dark" true (Charamel_ansi.Color.Rgb (128, 128, 128));
      dark "grey 129 is light" false (Charamel_ansi.Color.Rgb (129, 129, 129));
      dark "pure red is dark" true (Charamel_ansi.Color.Rgb (255, 0, 0));
      dark "pure green is light" false (Charamel_ansi.Color.Rgb (0, 255, 0));
      dark "grey ramp 244 is dark" true (Charamel_ansi.Color.Indexed 244);
      dark "grey ramp 245 is light" false (Charamel_ansi.Color.Indexed 245);
      dark "below range clamps dark" true (Charamel_ansi.Color.Indexed (-200));
      dark "above range clamps light" false (Charamel_ansi.Color.Indexed 300))

let cases =
  [
    checked_constructors;
    of_hex_;
    of_hex_or_;
    of_string_;
    to_ansi256_;
    to_ansi16_table;
    to_ansi16_;
    to_rgb_;
    is_dark_;
    pp_;
    equal_;
  ]
