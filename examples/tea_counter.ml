type message = Key of Charamel_tea.Key.t

let app : (int, message) Charamel_tea.app =
  {
    init = (fun () -> (0, Charamel_tea.Cmd.none));
    update =
      (fun message count ->
        match message with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "up" -> (count + 1, Charamel_tea.Cmd.none)
            | "down" -> (count - 1, Charamel_tea.Cmd.none)
            | "q" | "ctrl+c" -> (count, Charamel_tea.Cmd.quit)
            | _ -> (count, Charamel_tea.Cmd.none)));
    view =
      (fun count ->
        Charamel_tea.View.v
          (Fmt.str "Counter: %d\n\nUse up/down to change the value. Press q to quit."
             count));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () =
  Eio_main.run (fun env ->
      match Charamel_tea.run ~clock:env#clock app env with Ok _ -> () | Error _ -> ())

let () = main ()
