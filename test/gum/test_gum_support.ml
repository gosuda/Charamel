(* Shared helpers for the gum scripted-UI suites. *)

let key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message
