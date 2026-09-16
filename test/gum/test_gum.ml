let sides_equal (left : Charm_lipgloss.Sides.t) (right : Charm_lipgloss.Sides.t) =
  left.top = right.top && left.right = right.right && left.bottom = right.bottom
  && left.left = right.left

let sides_testable =
  Alcotest.testable
    (Fmt.of_to_string (fun (sides : Charm_lipgloss.Sides.t) ->
         Fmt.str "(%d,%d,%d,%d)" sides.top sides.right sides.bottom sides.left))
    sides_equal

let test_env_names () =
  let info = Gum_flag.env ~cmd:"choose" "cursor.foreground" in
  Alcotest.(check string)
    "dotted style env" "GUM_CHOOSE_CURSOR_FOREGROUND"
    (Cmdliner.Cmd.Env.info_var info);
  let info = Gum_flag.env ~cmd:"filter" "show-help" in
  Alcotest.(check string)
    "dashed env" "GUM_FILTER_SHOW_HELP"
    (Cmdliner.Cmd.Env.info_var info)

let test_padding_shapes () =
  let check input expected =
    match Gum_flag.parse_padding input with
    | Ok actual -> Alcotest.check sides_testable input expected actual
    | Error (`Msg message) -> Alcotest.fail message
  in
  check "3" (Charm_lipgloss.Sides.all 3);
  check "1 2" (Charm_lipgloss.Sides.v ~top:1 ~right:2 ~bottom:1 ~left:2 ());
  check "1,2,3" (Charm_lipgloss.Sides.v ~top:1 ~right:2 ~bottom:3 ~left:2 ());
  check "1,2,3,4" (Charm_lipgloss.Sides.v ~top:1 ~right:2 ~bottom:3 ~left:4 ())

let test_padding_rejects_malformed () =
  let malformed = [ ""; "1 2 3 4 5"; "one"; "1,broken" ] in
  List.iter
    (fun value ->
      match Gum_flag.parse_padding value with
      | Ok _ -> Alcotest.fail (Fmt.str "accepted malformed padding %S" value)
      | Error (`Msg _) -> ())
    malformed

let test_color_align_border () =
  let expected = Charm_ansi.Color.Rgb (170, 187, 204) in
  match (Gum_flag.color "#abc", Gum_flag.align "middle", Gum_flag.border "rounded") with
  | Ok (Some color), Some position, Some border ->
      Alcotest.(check bool) "hex color" true (Charm_ansi.Color.equal expected color);
      Alcotest.(check (float 0.)) "middle" 0.5 (Charm_lipgloss.Position.to_float position);
      Alcotest.(check string) "rounded top-left" "╭" border.top_left
  | _ -> Alcotest.fail "valid color, alignment, or border rejected"

let test_invalid_color () =
  match Gum_flag.color "#abcd" with
  | Error (`Msg _) -> ()
  | Ok _ -> Alcotest.fail "invalid hex color accepted"

let test_style_defaults () =
  let style = Gum_style.defaults ~foreground:"#abc" ~padding:"1,2,3,4" () in
  Alcotest.(check bool)
    "foreground" true
    (match Gum_style.foreground style with
    | Some color -> Charm_ansi.Color.equal color (Charm_ansi.Color.Rgb (170, 187, 204))
    | None -> false);
  let rendered = Gum_style.to_style style |> Charm_lipgloss.Style.get_padding in
  Alcotest.(check (option sides_testable))
    "padding"
    (Some (Charm_lipgloss.Sides.v ~top:1 ~right:2 ~bottom:3 ~left:4 ()))
    rendered

let cases =
  [
    Alcotest.test_case "environment names" `Quick test_env_names;
    Alcotest.test_case "padding shapes" `Quick test_padding_shapes;
    Alcotest.test_case "malformed padding" `Quick test_padding_rejects_malformed;
    Alcotest.test_case "color alignment border" `Quick test_color_align_border;
    Alcotest.test_case "invalid color" `Quick test_invalid_color;
    Alcotest.test_case "style defaults" `Quick test_style_defaults;
  ]

let () =
  Alcotest.run "gum"
    [
      ("shared", cases);
      ("format", Test_format.cases);
      ("join", Test_join.cases);
      ("log", Test_log.cases);
      ("style", Test_style.cases);
      ("version", Test_version.cases);
      ("choose", Test_choose.cases);
      ("filter", Test_filter.cases);
      ("confirm", Test_confirm.cases);
      ("input", Test_input.cases);
      ("write", Test_write.cases);
      ("pager", Test_pager.cases);
      ("csv", Test_csv.cases);
      ("table", Test_table.cases);
      ("file", Test_file.cases);
      ("spin", Test_spin.cases);
    ]
