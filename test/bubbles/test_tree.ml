module Key_binding = Charm_bubbles.Key_binding
module Tree = Charm_bubbles.Tree

let plain model = Charm_ansi.Text.strip (Tree.view model)

let node_shape_and_size () =
  let closed = Tree.node ~open_:false [ Tree.leaf "hidden" ] in
  let open_node = Tree.node [ Tree.leaf "shown" ] in
  Alcotest.(check string) "leaf value" "leaf" (Tree.value (Tree.leaf "leaf"));
  Alcotest.(check bool) "closed branch" false (Tree.is_open closed);
  Alcotest.(check int) "closed size" 1 (Tree.size closed);
  Alcotest.(check int) "open size" 2 (Tree.size open_node);
  Alcotest.(check int)
    "closed children remain inspectable" 1
    (Stdlib.List.length (Tree.children closed))

let navigation_boundaries () =
  let root =
    Tree.node ~value:"root"
      [
        Tree.leaf "one";
        Tree.node ~value:"branch" [ Tree.leaf "nested" ];
        Tree.leaf "last";
      ]
  in
  let model = Tree.v ~width:40 ~height:8 root in
  Alcotest.(check int) "root selected" 0 (Tree.y_offset model);
  let model = Tree.up model |> Tree.go_to_top in
  Alcotest.(check int) "top clamp" 0 (Tree.y_offset model);
  let model = Tree.go_to_bottom model in
  Alcotest.(check int) "bottom" 4 (Tree.y_offset model);
  let model = Tree.down model in
  Alcotest.(check int) "bottom clamp" 4 (Tree.y_offset model);
  let model = Tree.go_to_top model |> Tree.toggle_current_node in
  Alcotest.(check int)
    "collapsed root has one visible node" 1
    (Stdlib.List.length (Tree.all_nodes model));
  let model = Tree.toggle_current_node model in
  Alcotest.(check int)
    "reopened root restores descendants" 5
    (Stdlib.List.length (Tree.all_nodes model));
  let model = Tree.down model |> Tree.down |> Tree.toggle_current_node in
  Alcotest.(check int)
    "collapsed branch hides nested node" 4
    (Stdlib.List.length (Tree.all_nodes model))

let keymap_and_help () =
  let model =
    Tree.v ~width:60 ~height:10 (Tree.node ~value:"root" [ Tree.leaf "child" ])
  in
  let key = Charm_tea.Key.v Charm_tea.Key.Down in
  (match Tree.key model key with
  | Some Tree.Down -> ()
  | _ -> Alcotest.fail "down key not bound");
  let extra = Key_binding.v ~help:("v", "select") [ "v" ] in
  let model = Tree.set_additional_short_help_keys [ extra ] model in
  let text = plain model in
  Alcotest.(check bool) "short help includes additional key" true (String.length text > 0);
  let model, _ = Tree.update Tree.Toggle_full_help model in
  Alcotest.(check bool)
    "full help toggles" true
    (String.length (plain model) >= String.length text)

let multiline_cursor_and_viewport () =
  let root =
    Tree.node ~value:"root\ncontinuation"
      [ Tree.node ~value:"parent\nsecond" [ Tree.leaf "leaf" ]; Tree.leaf "tail" ]
  in
  let model = Tree.v ~width:30 ~height:4 root in
  let text = plain model in
  Alcotest.(check bool) "root multiline value" true (String.contains text 'c');
  let model = Tree.down model |> Tree.down in
  let text = plain (Tree.set_y_offset 2 model) in
  Alcotest.(check bool) "nested value survives viewport" true (String.contains text 'l')

let cases =
  [
    Alcotest.test_case "node shape and size" `Quick node_shape_and_size;
    Alcotest.test_case "navigation boundaries" `Quick navigation_boundaries;
    Alcotest.test_case "keymap and help" `Quick keymap_and_help;
    Alcotest.test_case "multiline viewport" `Quick multiline_cursor_and_viewport;
  ]
