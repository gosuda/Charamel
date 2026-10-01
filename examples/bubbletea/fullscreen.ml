type msg = Tick | Key of Charamel_tea.Key.t

let is_quit key =
  let name = Charamel_tea.Key.to_string key in
  String.equal name "q" || String.equal name "escape" || String.equal name "ctrl+c"

let tick = Charamel_tea.Cmd.after 1. (fun () -> Tick)

let app : (int, msg) Charamel_tea.app =
  {
    init = (fun () -> (5, tick));
    update =
      (fun msg count ->
        match msg with
        | Key key ->
            if is_quit key then (count, Charamel_tea.Cmd.quit)
            else (count, Charamel_tea.Cmd.none)
        | Tick ->
            let count = count - 1 in
            (count, if count <= 0 then Charamel_tea.Cmd.quit else tick));
    view =
      (fun count ->
        Charamel_tea.View.v ~alt_screen:true
          (Fmt.str "\n\n     Hi. This program will exit in %d seconds..." count));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [ Smoke.key "q" ] [ "exit in 5 seconds" ]
  @ Smoke.expect app [ `Wait 2.1; Smoke.key "q" ] [ "exit in 3 seconds" ]
