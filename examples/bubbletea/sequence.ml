type msg = Key of Charamel_tea.Key.t | Printed of string

let sleep_println text seconds = Charamel_tea.Cmd.after seconds (fun () -> Printed text)

let phase_one =
  Charamel_tea.Cmd.batch
    [
      Charamel_tea.Cmd.seq [ sleep_println "1-1-1" 1.; sleep_println "1-1-2" 1. ];
      Charamel_tea.Cmd.batch [ sleep_println "1-2-1" 1.5; sleep_println "1-2-2" 1.25 ];
    ]

let phase_three =
  Charamel_tea.Cmd.seq
    [
      Charamel_tea.Cmd.batch [ sleep_println "3-1-1" 0.5; sleep_println "3-1-2" 1. ];
      Charamel_tea.Cmd.seq [ sleep_println "3-2-1" 0.75; sleep_println "3-2-2" 0.5 ];
    ]

let app : (string list, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( [],
          Charamel_tea.Cmd.seq
            [
              phase_one;
              Charamel_tea.Cmd.msg (Printed "2");
              phase_three;
              Charamel_tea.Cmd.quit;
            ] ));
    update =
      (fun msg lines ->
        match msg with
        | Key _ -> (lines, Charamel_tea.Cmd.quit)
        | Printed text -> (lines @ [ text ], Charamel_tea.Cmd.none));
    view = (fun lines -> Charamel_tea.View.v (String.concat "\n" lines));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "1-1-1\n1-2-2\n1-2-1\n1-1-2\n2\n3-1-1\n3-1-2\n3-2-1\n3-2-2" ]
  @ Smoke.expect app [ `Wait 1.3; Smoke.key "x" ] [ "1-1-1\n1-2-2" ]
