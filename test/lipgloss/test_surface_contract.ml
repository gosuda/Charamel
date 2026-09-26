open Charamel_lipgloss

let check_string name expected actual = Alcotest.(check string) name expected actual
let check_int name expected actual = Alcotest.(check int) name expected actual

let check_int_opt name expected actual =
  Alcotest.(check (option int)) name expected actual

let check_bool name expected actual = Alcotest.(check bool) name expected actual

let has needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i =
    if i + n > h then false
    else if String.sub haystack i n = needle then true
    else loop (i + 1)
  in
  loop 0

let red = Option.get (Color.rgb 255 0 0)
let bold_x = Style.render (Style.bold true Style.empty) "x"

let buffer_sink output =
  Lwt_io.make ~mode:Lwt_io.Output (fun bytes offset length ->
      Buffer.add_subbytes output (Lwt_bytes.to_bytes bytes) offset length;
      Lwt.return length)

let captured profile text = Lwt_main.run (Print.sprint ~profile text)

let test_print () =
  check_string "no tty strips the sequence" "x"
    (captured Charamel_colorprofile.No_tty bold_x);
  check_string "ascii strips the sequence" "x"
    (captured Charamel_colorprofile.Ascii bold_x);
  check_string "truecolor keeps the sequence" "\027[1mx\027[m"
    (captured Charamel_colorprofile.True_color bold_x);
  let colored =
    Style.render (Style.foreground (Option.get (Color.rgb 0 0 128)) Style.empty) "x"
  in
  let downsampled = captured Charamel_colorprofile.Ansi256 colored in
  check_bool "ansi256 rewrites the color as a palette entry" true
    (has "38;5;" downsampled);
  check_bool "ansi256 drops the truecolor form" false (has "38;2;" downsampled);
  let buffer = Buffer.create 16 in
  Lwt_main.run
    (Print.println ~profile:Charamel_colorprofile.No_tty ~sink:(buffer_sink buffer) "y");
  check_string "println writes the line" "y\n" (Buffer.contents buffer);
  check_string "sprintln appends a newline" "y\n"
    (Lwt_main.run (Print.sprintln ~profile:Charamel_colorprofile.No_tty "y"))

let test_wrap () =
  check_string "a styled line break resets and reapplies"
    "\027[31mab\027[m\n\027[31mcd\027[m"
    (Layout.wrap ~width:3 "\027[31mab cd\027[m");
  check_string "text that fits is unchanged" "x" (Layout.wrap ~width:80 "x");
  check_string "a width of one wraps nothing" "abc" (Layout.wrap ~width:1 "abc");
  let open_link =
    Charamel_ansi.Link.osc8 (Some { Charamel_ansi.Link.url = "https://a"; params = [] })
  in
  let close_link = Charamel_ansi.Link.osc8 None in
  check_string "an open link is closed and reopened"
    (open_link ^ "abc" ^ close_link ^ "\n" ^ open_link ^ "d" ^ close_link)
    (Layout.wrap ~width:3 (open_link ^ "abcd"))

let test_measurement () =
  let framed =
    Style.padding (Sides.xy ~x:1 ~y:0)
      (Style.border Border.normal (Style.margin (Sides.xy ~x:2 ~y:0) Style.empty))
  in
  check_int "frame width sums padding, margins and both edges" 8
    (Style.get_horizontal_frame_size framed);
  check_int "frame height sums the vertical sides" 2
    (Style.get_vertical_frame_size framed);
  check_int "an empty frame is empty" 0 (fst (Style.get_frame_size Style.empty));
  check_int "a hidden edge contributes nothing" 0
    (Style.get_border_top_size
       (Style.border_top false (Style.border Border.rounded Style.empty)));
  check_int "a shown edge is one cell wide" 1
    (Style.get_border_left_size (Style.border Border.normal Style.empty));
  check_int "no border has no size" 0 (Style.get_border_right_size Style.empty);
  check_bool "get_align mirrors the horizontal axis" true
    (Position.to_float
       (Option.get (Style.get_align (Style.align_horizontal Position.center Style.empty)))
    = 0.5);
  check_int "no tab conversion is minus one" (-1) Style.no_tab_conversion;
  check_string "nbsp is the non-breaking space" "\u{00A0}" Style.nbsp;
  check_string "a kept tab survives rendering" "\t"
    (Style.render (Style.tab_width Style.no_tab_conversion Style.empty) "\t");
  check_string "a zero width deletes tabs" ""
    (Style.render (Style.tab_width 0 Style.empty) "\t")

let accent = "e\u{0301}x"

let styled text ~indices =
  Layout.style_runes (Style.foreground red Style.empty) Style.empty text ~indices

let accented text ~basis ~indices =
  Layout.style_runes ~basis (Style.foreground red Style.empty) Style.empty text ~indices

let test_rune_basis () =
  let cluster = Style.render (Style.foreground red Style.empty) "e\u{0301}" in
  let plain_x = Style.render (Style.foreground red Style.empty) "x" in
  check_bool "cluster index zero styles the whole cluster" true
    (Test_support.contains ~needle:cluster ~haystack:(styled accent ~indices:[ 0 ]));
  check_bool "cluster index one styles the next cluster" true
    (Test_support.contains ~needle:plain_x ~haystack:(styled accent ~indices:[ 1 ]));
  check_bool "a scalar index inside a cluster styles that cluster" true
    (Test_support.contains ~needle:cluster
       ~haystack:(accented accent ~basis:`Scalar ~indices:[ 1 ]));
  check_bool "a scalar index past the cluster styles the next one" true
    (Test_support.contains ~needle:plain_x
       ~haystack:(accented accent ~basis:`Scalar ~indices:[ 2 ]))

let test_per_side () =
  let one_side = Style.padding_side `Top 1 (Style.padding_side `Left 1 Style.empty) in
  check_string "two independent padding sides" "  \n x" (Style.render one_side "x");
  check_string "unsetting one side leaves the other" " \nx"
    (Style.render (Style.unset_padding_side `Left one_side) "x");
  check_int_opt "the top side still reads back" (Some 1)
    (Style.get_padding_side `Top (Style.unset_padding_side `Left one_side));
  check_string "padding fills with the padding character" "...\n.x.\n..."
    (Style.render (Style.padding_char "." (Style.padding (Sides.all 1) Style.empty)) "x");
  check_string "margins fill with the margin character" "...\n.x.\n..."
    (Style.render (Style.margin_char "." (Style.margin (Sides.all 1) Style.empty)) "x");
  check_int "horizontal padding adds both sides" 4
    (Style.get_horizontal_padding (Style.padding (Sides.xy ~x:2 ~y:0) Style.empty));
  check_int "vertical padding ignores the horizontal axis" 0
    (Style.get_vertical_padding (Style.padding (Sides.xy ~x:2 ~y:0) Style.empty));
  check_int_opt "an unset side reads back as none" None
    (Style.get_margin_side `Right Style.empty);
  check_string "an empty padding character restores spaces" "   \n x \n   "
    (Style.render
       (Style.padding_char ""
          (Style.padding_char "." (Style.padding (Sides.all 1) Style.empty)))
       "x");
  Alcotest.check_raises "a multi-grapheme fill is rejected"
    (Invalid_argument "Style.padding_char: one grapheme is required") (fun () ->
      ignore (Style.padding_char "ab" Style.empty))

let test_underline_reexport () =
  check_string "the aggregator supplies underline kinds" "\027[4:2mx\027[m"
    (Style.render (Style.underline_style Underline.Double Style.empty) "x")

let cases =
  [
    Alcotest.test_case "print helpers" `Quick test_print;
    Alcotest.test_case "public wrap" `Quick test_wrap;
    Alcotest.test_case "measurement getters" `Quick test_measurement;
    Alcotest.test_case "per-side padding and margin" `Quick test_per_side;
    Alcotest.test_case "style_runes index basis" `Quick test_rune_basis;
    Alcotest.test_case "underline re-export" `Quick test_underline_reexport;
  ]
