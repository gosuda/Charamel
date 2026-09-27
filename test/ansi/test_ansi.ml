let () =
  Alcotest.run "ANSI"
    [
      ("width", Test_width.cases);
      ("parser", Test_parser.cases);
      ("color", Test_color.cases);
      ("style", Test_style.cases);
      ("sequences", Test_seq.cases);
      ("text", Test_text.cases);
      ("text properties", Test_text_properties.cases);
      ("raster", Test_raster.cases);
    ]
