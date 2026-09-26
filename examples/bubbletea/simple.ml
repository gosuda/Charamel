type msg = Tick | Key of Charamel_tea.Key.t

let tick = Charamel_tea.Cmd.after 1.0 (fun () -> Tick)

let app : (int, msg) Charamel_tea.app =
  {
    init = (fun () -> (5, tick));
    update =
      (fun msg count ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "ctrl+c" | "q" -> (count, Charamel_tea.Cmd.quit)
            | "ctrl+z" -> (count, Charamel_tea.Cmd.suspend)
            | _ -> (count, Charamel_tea.Cmd.none))
        | Tick ->
            let count = count - 1 in
            if count <= 0 then (count, Charamel_tea.Cmd.quit) else (count, tick));
    view =
      (fun count ->
        Charamel_tea.View.v
          (Fmt.str
             "Hi. This program will exit in %d seconds.\n\n\
              To quit sooner press ctrl-c, or press ctrl-z to suspend...\n"
             count));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [ Smoke.key "q" ] [ "exit in 5 seconds" ]
  @ Smoke.expect app [ `Wait 2.5; Smoke.key "q" ] [ "exit in 3 seconds" ]
  @ Smoke.expect app [] [ "exit in 0 seconds" ]
