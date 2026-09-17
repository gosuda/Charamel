type message = Tick of Mtime.t | Finished | Key of Charamel_tea.Key.t

let app : (int, message) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( 0,
          Charamel_tea.Cmd.seq
            [
              Charamel_tea.Cmd.print "first line";
              Charamel_tea.Cmd.print "second line";
              Charamel_tea.Cmd.print "third line";
              Charamel_tea.Cmd.after 3.0 (fun () -> Finished);
            ] ));
    update =
      (fun message ticks ->
        match message with
        | Tick _ -> (ticks + 1, Charamel_tea.Cmd.none)
        | Finished -> (ticks, Charamel_tea.Cmd.quit)
        | Key key ->
            if Charamel_tea.Key.to_string key = "ctrl+c" then
              (ticks, Charamel_tea.Cmd.interrupt)
            else (ticks, Charamel_tea.Cmd.none));
    view =
      (fun ticks ->
        Charamel_tea.View.v
          (Fmt.str "Working%s\n\nPress Ctrl-C to stop." (String.make (ticks mod 4) '.')));
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.every 0.1 (fun at -> Tick at);
            Charamel_tea.Sub.key (fun key -> Key key);
          ]);
  }

let main () =
  Eio_main.run (fun env ->
      match Charamel_tea.run ~clock:env#clock app env with Ok _ -> () | Error _ -> ())

let () = main ()
