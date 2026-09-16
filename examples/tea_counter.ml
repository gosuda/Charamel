type message = Key of Charm_tea.Key.t

let app : (int, message) Charm_tea.app =
  {
    init = (fun () -> (0, Charm_tea.Cmd.none));
    update =
      (fun message count ->
        match message with
        | Key key -> (
            match Charm_tea.Key.to_string key with
            | "up" -> (count + 1, Charm_tea.Cmd.none)
            | "down" -> (count - 1, Charm_tea.Cmd.none)
            | "q" | "ctrl+c" -> (count, Charm_tea.Cmd.quit)
            | _ -> (count, Charm_tea.Cmd.none)));
    view =
      (fun count ->
        Charm_tea.View.v
          (Fmt.str "Counter: %d\n\nUse up/down to change the value. Press q to quit."
             count));
    subscriptions = (fun _ -> Charm_tea.Sub.key (fun key -> Key key));
  }

let main () =
  Eio_main.run (fun env ->
      match Charm_tea.run ~clock:env#clock app env with Ok _ -> () | Error _ -> ())

let () = main ()
