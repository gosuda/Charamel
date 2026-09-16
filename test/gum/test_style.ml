let side_equal (expected : Charm_lipgloss.Sides.t) (actual : Charm_lipgloss.Sides.t) =
  Alcotest.(check int) "top" expected.top actual.top;
  Alcotest.(check int) "right" expected.right actual.right;
  Alcotest.(check int) "bottom" expected.bottom actual.bottom;
  Alcotest.(check int) "left" expected.left actual.left

let trims_each_line () =
  Alcotest.(check string)
    "per-line trim" "one\ntwo\nthree"
    (Style_cmd.trim_lines "  one  \n\ttwo\t\n three ")

let keeps_line_structure () =
  Alcotest.(check string)
    "empty lines survive" "a\n\nb"
    (Style_cmd.trim_lines " a \n \n b ")

let renders_complete_style () =
  let style =
    Gum_style.defaults ~foreground:"196" ~background:"22" ~border:"rounded"
      ~align:"center" ~padding:"0 1" ~margin:"1 2" ~bold:true ()
  in
  let rendered = Style_cmd.render style ~trim:false "x" in
  let lipgloss = Gum_style.to_style style in
  (match Charm_lipgloss.Style.get_padding lipgloss with
  | Some sides -> side_equal (Charm_lipgloss.Sides.xy ~x:1 ~y:0) sides
  | None -> Alcotest.fail "padding was dropped");
  (match Charm_lipgloss.Style.get_margin lipgloss with
  | Some sides -> side_equal (Charm_lipgloss.Sides.xy ~x:2 ~y:1) sides
  | None -> Alcotest.fail "margin was dropped");
  (match Charm_lipgloss.Style.get_border lipgloss with
  | Some border ->
      Alcotest.(check bool) "rounded border" true (border = Charm_lipgloss.Border.rounded)
  | None -> Alcotest.fail "border was dropped");
  (match Charm_lipgloss.Style.get_align_horizontal lipgloss with
  | Some position ->
      Alcotest.(check bool)
        "center alignment" true
        (position = Charm_lipgloss.Position.center)
  | None -> Alcotest.fail "alignment was dropped");
  Alcotest.(check int)
    "rendered width including margin" 9
    (Charm_lipgloss.Layout.width rendered);
  Alcotest.(check int)
    "rendered height including margin" 5
    (Charm_lipgloss.Layout.height rendered)

let neutral_style_is_identity () =
  Alcotest.(check string)
    "neutral style" "x"
    (Style_cmd.render Gum_style.empty ~trim:false "x")

let cases =
  [
    Alcotest.test_case "trim lines" `Quick trims_each_line;
    Alcotest.test_case "line structure" `Quick keeps_line_structure;
    Alcotest.test_case "complete style" `Quick renders_complete_style;
    Alcotest.test_case "neutral style" `Quick neutral_style_is_identity;
  ]
