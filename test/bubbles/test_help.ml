module Help = Charm_bubbles.Help
module Binding = Charm_bubbles.Key_binding
module Text = Charm_ansi.Text
module Layout = Charm_lipgloss.Layout

let bindings () =
  [
    Binding.v ~help:("up", "move") [ "up" ];
    Binding.v ~help:("down", "move") [ "down" ];
    Binding.v ~help:("q", "quit") [ "q" ];
  ]

let short_and_disabled () =
  let help = Help.v ~width:80 () in
  let short = Help.short_view help (bindings ()) |> Text.strip in
  Alcotest.(check string) "short help" "up move • down move • q quit" short;
  let disabled = Binding.v ~enabled:false ~help:("x", "hidden") [ "x" ] in
  let short = Help.short_view help (disabled :: bindings ()) |> Text.strip in
  Alcotest.(check string) "disabled binding omitted" "up move • down move • q quit" short

let short_width () =
  let help = Help.v ~width:12 () in
  let short = Help.short_view help (bindings ()) |> Text.strip in
  Alcotest.(check bool) "short view respects width" true (Layout.width short <= 12);
  Alcotest.(check bool)
    "short view keeps an item or ellipsis" true
    (String.ends_with ~suffix:"…" short || short = "up move")

let full_columns () =
  let help = Help.v ~width:40 ~full_separator:"|" ~show_all:true () in
  let keymap =
    {
      Help.short_help = [];
      full_help =
        [
          [
            Binding.v ~help:("up", "move") [ "up" ]; Binding.v ~help:("q", "quit") [ "q" ];
          ];
          [ Binding.v ~help:("enter", "select") [ "enter" ] ];
          [ Binding.v ~enabled:false ~help:("x", "hidden") [ "x" ] ];
        ];
    }
  in
  let full = Help.view help keymap |> Text.strip in
  Alcotest.(check bool) "full help has first column" true (String.contains full 'u');
  Alcotest.(check bool) "full help skips disabled column" false (String.contains full 'x');
  Alcotest.(check bool) "full help stays in width" true (Layout.width full <= 40)

let cases =
  [
    Alcotest.test_case "short help" `Quick short_and_disabled;
    Alcotest.test_case "short width" `Quick short_width;
    Alcotest.test_case "full columns" `Quick full_columns;
  ]
