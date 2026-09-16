let key name =
  match Charm_tea.Key.of_string name with
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.fail message

let enabled_and_matching () =
  let binding = Charm_bubbles.Key_binding.v ~help:("k", "confirm") [ "k"; "up" ] in
  Alcotest.(check bool)
    "enabled with keys" true
    (Charm_bubbles.Key_binding.enabled binding);
  Alcotest.(check bool)
    "matching key" true
    (Charm_bubbles.Key_binding.matches (key "k") binding);
  Alcotest.(check bool)
    "other key does not match" false
    (Charm_bubbles.Key_binding.matches (key "down") binding)

let disabled_and_unbound () =
  let binding = Charm_bubbles.Key_binding.v ~help:("k", "confirm") [ "k" ] in
  let binding = Charm_bubbles.Key_binding.set_enabled false binding in
  Alcotest.(check bool)
    "disabled binding does not match" false
    (Charm_bubbles.Key_binding.matches (key "k") binding);
  let binding = Charm_bubbles.Key_binding.set_enabled true binding in
  Alcotest.(check bool)
    "re-enabled binding matches" true
    (Charm_bubbles.Key_binding.matches (key "k") binding);
  let unbound = Charm_bubbles.Key_binding.unbind binding in
  Alcotest.(check bool)
    "unbind disables" false
    (Charm_bubbles.Key_binding.enabled unbound);
  Alcotest.(check (pair string string))
    "unbind clears help" ("", "") unbound.Charm_bubbles.Key_binding.help;
  let rebound = Charm_bubbles.Key_binding.set_enabled true unbound in
  Alcotest.(check bool)
    "empty keys remain disabled" false
    (Charm_bubbles.Key_binding.matches (key "k") rebound)

let matching_ignores_event_fields () =
  let binding = Charm_bubbles.Key_binding.v [ "k" ] in
  let pressed = key "k" in
  let released =
    { pressed with Charm_tea.Key.event = Charm_tea.Key.Release; text = "k" }
  in
  Alcotest.(check bool)
    "event and text are ignored" true
    (Charm_bubbles.Key_binding.matches released binding)

let of_keys_and_updates () =
  let first = key "k" in
  let second = key "up" in
  let binding = Charm_bubbles.Key_binding.of_keys ~enabled:false [ first ] in
  let binding = Charm_bubbles.Key_binding.set_keys [ second ] binding in
  Alcotest.(check bool)
    "explicit enablement is preserved" false
    (Charm_bubbles.Key_binding.matches second binding);
  let binding = Charm_bubbles.Key_binding.set_enabled true binding in
  let binding = Charm_bubbles.Key_binding.set_help ("up", "move") binding in
  Alcotest.(check bool)
    "updated key matches" true
    (Charm_bubbles.Key_binding.matches second binding);
  Alcotest.(check (pair string string))
    "help is replaced" ("up", "move") binding.Charm_bubbles.Key_binding.help

let matches_any () =
  let bindings =
    [ Charm_bubbles.Key_binding.v [ "left" ]; Charm_bubbles.Key_binding.v [ "right" ] ]
  in
  Alcotest.(check bool)
    "one matching binding is enough" true
    (Charm_bubbles.Key_binding.matches_any (key "right") bindings);
  Alcotest.(check bool)
    "no binding matches" false
    (Charm_bubbles.Key_binding.matches_any (key "up") bindings)

let cases =
  [
    Alcotest.test_case "enabled and matching" `Quick enabled_and_matching;
    Alcotest.test_case "disabled and unbound" `Quick disabled_and_unbound;
    Alcotest.test_case "matching ignores event fields" `Quick
      matching_ignores_event_fields;
    Alcotest.test_case "of_keys and updates" `Quick of_keys_and_updates;
    Alcotest.test_case "matches_any" `Quick matches_any;
  ]
