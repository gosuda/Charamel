let debounce_duration = 1.
let prompt = "To exit press any key, then wait for one second without pressing anything."

type model = { tag : int }
type msg = Key of Charamel_tea.Key.t | Debounced of int

let update msg model =
  match msg with
  | Key _ ->
      let tag = model.tag + 1 in
      ({ tag }, Charamel_tea.Cmd.after debounce_duration (fun () -> Debounced tag))
  | Debounced tag ->
      if Int.equal tag model.tag then (model, Charamel_tea.Cmd.quit)
      else (model, Charamel_tea.Cmd.none)

let view model = Charamel_tea.View.v (Fmt.str "Key presses: %d\n%s" model.tag prompt)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ tag = 0 }, Charamel_tea.Cmd.none));
    update;
    view;
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Key presses: 0" ]
  @ Smoke.expect app [ Smoke.key "a"; Smoke.key "b" ] [ "Key presses: 2" ]
  @ Smoke.expect app [ Smoke.key "a"; `Wait 0.5; Smoke.key "b" ] [ "Key presses: 2" ]
  @ Smoke.expect app
      [ Smoke.key "ctrl+c"; Smoke.key "q" ]
      [ "Key presses: 2"; "wait for one second without pressing anything" ]
