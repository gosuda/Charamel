type msg = Key of Charamel_tea.Key.t | Editor of int
type model = { altscreen_active : bool; err : string option }

let open_editor () =
  Charamel_tea.Cmd.exec ~argv:(Charamel_os.Editor.editor ()) (fun code -> Editor code)

let on_key key model =
  match Charamel_tea.Key.to_string key with
  | "q" | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
  | "a" ->
      ({ model with altscreen_active = not model.altscreen_active }, Charamel_tea.Cmd.none)
  | "e" -> (model, open_editor ())
  | _ -> (model, Charamel_tea.Cmd.none)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ altscreen_active = false; err = None }, Charamel_tea.Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key key -> on_key key model
        | Editor code ->
            if code = 0 then (model, Charamel_tea.Cmd.none)
            else
              ( { model with err = Some (Fmt.str "exit status %d" code) },
                Charamel_tea.Cmd.quit ));
    view =
      (fun model ->
        let content =
          match model.err with
          | Some err -> "Error: " ^ err ^ "\n"
          | None ->
              "Press 'e' to open your EDITOR.\n\
               Press 'a' to toggle the altscreen\n\
               Press 'q' to quit.\n"
        in
        Charamel_tea.View.v ~alt_screen:model.altscreen_active content);
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app
    [ Smoke.key "q" ]
    [ "Press 'e' to open your EDITOR."; "Press 'a' to toggle the altscreen" ]
  @ Smoke.expect app
      [ `Msg (Editor 0); Smoke.key "a"; Smoke.key "q" ]
      [ "Press 'e' to open your EDITOR." ]
  @ Smoke.expect app [ `Msg (Editor 1) ] [ "Error: exit status 1" ]
