let keyboard =
  {
    Charamel_tea.View.disambiguate = true;
    report_events = true;
    report_alternates = false;
    report_all_keys = false;
    report_text = false;
  }

let prompt =
  "Press any key to see its details printed to the terminal. Press 'ctrl+c' to quit."

let describe key =
  let name = Charamel_tea.Key.to_string key in
  let text = key.Charamel_tea.Key.text in
  if String.equal text "" then Fmt.str "You pressed: %s" name
  else Fmt.str "You pressed: %s (text: %S)" name text

let app : (string list, Charamel_tea.Key.t) Charamel_tea.app =
  {
    init = (fun () -> ([], Charamel_tea.Cmd.none));
    update =
      (fun key lines ->
        if String.equal (Charamel_tea.Key.to_string key) "ctrl+c" then
          (lines, Charamel_tea.Cmd.quit)
        else (lines @ [ describe key ], Charamel_tea.Cmd.none));
    view =
      (fun lines ->
        let content =
          match lines with
          | [] -> prompt
          | _ :: _ -> String.concat "\n" lines ^ "\n" ^ prompt
        in
        Charamel_tea.View.v ~keyboard content);
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app
    [ Smoke.key "a" ]
    [ "You pressed: a"; "Press any key to see its details" ]
  @ Smoke.expect app
      [ Smoke.key "up"; Smoke.key "right" ]
      [ "You pressed: up"; "You pressed: right" ]
  @ Smoke.expect app [] [ prompt ]
