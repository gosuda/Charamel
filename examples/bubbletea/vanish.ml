type msg = Key of Charamel_tea.Key.t

let prompt =
  "Press any key to quit.\n(When this program quits, it will vanish without a trace.)"

let app : (bool, msg) Charamel_tea.app =
  {
    init = (fun () -> (false, Charamel_tea.Cmd.none));
    update = (fun msg _ -> match msg with Key _ -> (true, Charamel_tea.Cmd.quit));
    view = (fun vanished -> Charamel_tea.View.v (if vanished then "" else prompt));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [
      "Press any key to quit.";
      "(When this program quits, it will vanish without a trace.)";
    ]
  @ Smoke.expect app [ Smoke.key "q" ] [ "" ]
