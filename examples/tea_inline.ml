type message = Tick of Mtime.t | Finished | Key of Charm_tea.Key.t

let app : (int, message) Charm_tea.app =
  {
    init =
      (fun () ->
        ( 0,
          Charm_tea.Cmd.seq
            [
              Charm_tea.Cmd.print "first line";
              Charm_tea.Cmd.print "second line";
              Charm_tea.Cmd.print "third line";
              Charm_tea.Cmd.after 3.0 (fun () -> Finished);
            ] ));
    update =
      (fun message ticks ->
        match message with
        | Tick _ -> (ticks + 1, Charm_tea.Cmd.none)
        | Finished -> (ticks, Charm_tea.Cmd.quit)
        | Key key ->
            if Charm_tea.Key.to_string key = "ctrl+c" then (ticks, Charm_tea.Cmd.interrupt)
            else (ticks, Charm_tea.Cmd.none));
    view =
      (fun ticks ->
        Charm_tea.View.v
          (Fmt.str "Working%s\n\nPress Ctrl-C to stop." (String.make (ticks mod 4) '.')));
    subscriptions =
      (fun _ ->
        Charm_tea.Sub.batch
          [
            Charm_tea.Sub.every 0.1 (fun at -> Tick at);
            Charm_tea.Sub.key (fun key -> Key key);
          ]);
  }

let main () =
  Eio_main.run (fun env ->
      match Charm_tea.run ~clock:env#clock app env with Ok _ -> () | Error _ -> ())

let () = main ()
