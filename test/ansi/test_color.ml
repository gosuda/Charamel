let color : Charm_ansi.Color.t Alcotest.testable = Alcotest.of_pp Charm_ansi.Color.pp
let renders name expected actual = Alcotest.check color name expected actual

let checked_constructors =
  Alcotest.test_case "checked constructors" `Quick (fun () ->
      Alcotest.check
        Alcotest.(option color)
        "basic 0" (Some (Charm_ansi.Color.Basic 0)) (Charm_ansi.Color.basic 0);
      Alcotest.check
        Alcotest.(option color)
        "basic 15" (Some (Charm_ansi.Color.Basic 15)) (Charm_ansi.Color.basic 15);
      Alcotest.check
        Alcotest.(option color)
        "basic 16 rejects" None (Charm_ansi.Color.basic 16);
      Alcotest.check
        Alcotest.(option color)
        "basic -1 rejects" None (Charm_ansi.Color.basic (-1));
      Alcotest.check
        Alcotest.(option color)
        "indexed 255" (Some (Charm_ansi.Color.Indexed 255))
        (Charm_ansi.Color.indexed 255);
      Alcotest.check
        Alcotest.(option color)
        "indexed 256 rejects" None
        (Charm_ansi.Color.indexed 256);
      Alcotest.check
        Alcotest.(option color)
        "rgb in range"
        (Some (Charm_ansi.Color.Rgb (0, 255, 3)))
        (Charm_ansi.Color.rgb 0 255 3);
      Alcotest.check
        Alcotest.(option color)
        "rgb 256 rejects" None
        (Charm_ansi.Color.rgb 256 0 0);
      Alcotest.check
        Alcotest.(option color)
        "rgb -1 rejects" None
        (Charm_ansi.Color.rgb 0 (-1) 0))

let of_hex_ =
  Alcotest.test_case "of_hex" `Quick (fun () ->
      let hex input expected =
        Alcotest.check
          Alcotest.(option color)
          input expected
          (Charm_ansi.Color.of_hex input)
      in
      hex "#ff8537" (Some (Charm_ansi.Color.Rgb (255, 133, 55)));
      hex "#fff" (Some (Charm_ansi.Color.Rgb (255, 255, 255)));
      hex "#abc" (Some (Charm_ansi.Color.Rgb (170, 187, 204)));
      hex "#ABC" (Some (Charm_ansi.Color.Rgb (170, 187, 204)));
      hex "#FFFFFF" (Some (Charm_ansi.Color.Rgb (255, 255, 255)));
      hex "#000" (Some (Charm_ansi.Color.Rgb (0, 0, 0)));
      hex "ff8537" None;
      hex "#" None;
      hex "#ff" None;
      hex "#ffff" None;
      hex "#ff853" None;
      hex "#ff8537f" None;
      hex "#gg8537" None;
      hex "" None)

(* The expected 256-color values restate the upstream TestHexTo256 rows as
   8-bit components; the upstream fractional inputs round to these bytes. *)
let to_ansi256_ =
  Alcotest.test_case "to_ansi256 upstream vectors" `Quick (fun () ->
      let vector name r g b n =
        renders name (Charm_ansi.Color.Indexed n)
          (Charm_ansi.Color.to_ansi256 (Charm_ansi.Color.Rgb (r, g, b)))
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
      renders "indexed identity" (Charm_ansi.Color.Indexed 209)
        (Charm_ansi.Color.to_ansi256 (Charm_ansi.Color.Indexed 209));
      renders "default identity" Charm_ansi.Color.Default
        (Charm_ansi.Color.to_ansi256 Charm_ansi.Color.Default);
      renders "basic dark red to cube" (Charm_ansi.Color.Indexed 88)
        (Charm_ansi.Color.to_ansi256 (Charm_ansi.Color.Basic 1)))

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
            (Charm_ansi.Color.Basic expected)
            (Charm_ansi.Color.to_ansi16 (Charm_ansi.Color.Indexed i)))
        ansi256_to16_reference)

let to_ansi16_ =
  Alcotest.test_case "to_ansi16" `Quick (fun () ->
      let vector name n b =
        renders name (Charm_ansi.Color.Basic b)
          (Charm_ansi.Color.to_ansi16 (Charm_ansi.Color.Indexed n))
      in
      vector "black" 0 0;
      vector "bright white" 15 15;
      vector "cube black" 16 0;
      vector "cube red" 196 9;
      vector "orange indexed" 209 9;
      vector "cube white" 231 15;
      vector "gray" 244 7;
      vector "top gray" 255 15;
      renders "basic identity" (Charm_ansi.Color.Basic 4)
        (Charm_ansi.Color.to_ansi16 (Charm_ansi.Color.Basic 4));
      renders "default identity" Charm_ansi.Color.Default
        (Charm_ansi.Color.to_ansi16 Charm_ansi.Color.Default);
      renders "rgb red round trip" (Charm_ansi.Color.Basic 9)
        (Charm_ansi.Color.to_ansi16
           (Charm_ansi.Color.to_ansi256 (Charm_ansi.Color.Rgb (255, 0, 0))));
      Alcotest.check
        Alcotest.(option color)
        "#ff8537 chain" (Some (Charm_ansi.Color.Basic 9))
        (let open Charm_ansi.Color in
         let c = of_hex "#ff8537" in
         match c with Some c -> Some (to_ansi16 (to_ansi256 c)) | None -> None))

let pp_ =
  Alcotest.test_case "pp" `Quick (fun () ->
      let show c = Format.asprintf "%a" Charm_ansi.Color.pp c in
      Alcotest.check Alcotest.string "default" "default" (show Charm_ansi.Color.Default);
      Alcotest.check Alcotest.string "basic" "basic 9" (show (Charm_ansi.Color.Basic 9));
      Alcotest.check Alcotest.string "indexed" "indexed 209"
        (show (Charm_ansi.Color.Indexed 209));
      Alcotest.check Alcotest.string "rgb" "rgb 255 133 85"
        (show (Charm_ansi.Color.Rgb (255, 133, 85))))

let equal_ =
  Alcotest.test_case "equal" `Quick (fun () ->
      let eq name expected a b =
        Alcotest.check Alcotest.bool name expected (Charm_ansi.Color.equal a b)
      in
      eq "same rgb" true (Charm_ansi.Color.Rgb (1, 2, 3)) (Charm_ansi.Color.Rgb (1, 2, 3));
      eq "kinds differ" false (Charm_ansi.Color.Basic 1) (Charm_ansi.Color.Indexed 1);
      eq "default is not black" false Charm_ansi.Color.Default (Charm_ansi.Color.Basic 0);
      eq "different rgb" false
        (Charm_ansi.Color.Rgb (1, 2, 3))
        (Charm_ansi.Color.Rgb (1, 2, 4)))

let rgb = Alcotest.(option (triple int int int))

let to_rgb_ =
  Alcotest.test_case "to_rgb palette resolution" `Quick (fun () ->
      let rgb' name expected c =
        Alcotest.check rgb name expected (Charm_ansi.Color.to_rgb c)
      in
      rgb' "default" None Charm_ansi.Color.Default;
      rgb' "basic silver" (Some (192, 192, 192)) (Charm_ansi.Color.Basic 7);
      rgb' "basic grey" (Some (128, 128, 128)) (Charm_ansi.Color.Basic 8);
      rgb' "basic above range clamps" (Some (255, 255, 255)) (Charm_ansi.Color.Basic 99);
      rgb' "indexed first sixteen share the ansi palette"
        (Some (192, 192, 192))
        (Charm_ansi.Color.Indexed 7);
      rgb' "indexed cube first" (Some (0, 0, 0)) (Charm_ansi.Color.Indexed 16);
      rgb' "indexed cube last" (Some (255, 255, 255)) (Charm_ansi.Color.Indexed 231);
      rgb' "indexed cube orange" (Some (255, 135, 95)) (Charm_ansi.Color.Indexed 209);
      rgb' "indexed grey ramp first" (Some (8, 8, 8)) (Charm_ansi.Color.Indexed 232);
      rgb' "indexed grey ramp last" (Some (238, 238, 238)) (Charm_ansi.Color.Indexed 255);
      rgb' "indexed below range clamps" (Some (0, 0, 0)) (Charm_ansi.Color.Indexed (-2));
      rgb' "indexed above range clamps"
        (Some (238, 238, 238))
        (Charm_ansi.Color.Indexed 300);
      rgb' "rgb identity" (Some (255, 133, 85)) (Charm_ansi.Color.Rgb (255, 133, 85));
      rgb' "rgb components clamp" (Some (255, 0, 12)) (Charm_ansi.Color.Rgb (300, -5, 12)))

let cases =
  [
    checked_constructors;
    of_hex_;
    to_ansi256_;
    to_ansi16_table;
    to_ansi16_;
    to_rgb_;
    pp_;
    equal_;
  ]
