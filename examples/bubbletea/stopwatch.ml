module Help = Charamel_bubbles.Help
module Key_binding = Charamel_bubbles.Key_binding
module Stopwatch = Charamel_bubbles.Stopwatch

type keymap = {
  start : Key_binding.t;
  stop : Key_binding.t;
  reset : Key_binding.t;
  quit : Key_binding.t;
}

type model = {
  stopwatch : Stopwatch.t;
  quitting : bool;
  start_enabled : bool;
  stop_enabled : bool;
}

type msg = Key of Charamel_tea.Key.t | Stopwatch_msg of Stopwatch.msg

let keymap model =
  {
    start = Key_binding.v ~help:("s", "start") ~enabled:model.start_enabled [ "s" ];
    stop = Key_binding.v ~help:("s", "stop") ~enabled:model.stop_enabled [ "s" ];
    reset = Key_binding.v ~help:("r", "reset") [ "r" ];
    quit = Key_binding.v ~help:("q", "quit") [ "ctrl+c"; "q" ];
  }

let step msg model =
  let stopwatch, cmd = Stopwatch.update msg model.stopwatch in
  ({ model with stopwatch }, Charamel_tea.Cmd.map (fun msg -> Stopwatch_msg msg) cmd)

let update msg model =
  match msg with
  | Key key ->
      let km = keymap model in
      if Key_binding.matches key km.quit then
        ({ model with quitting = true }, Charamel_tea.Cmd.quit)
      else if Key_binding.matches key km.reset then step Stopwatch.Reset model
      else if Key_binding.matches_any key [ km.start; km.stop ] then
        let running = Stopwatch.running model.stopwatch in
        ( {
            model with
            stopwatch = Stopwatch.toggle model.stopwatch;
            stop_enabled = not running;
            start_enabled = running;
          },
          Charamel_tea.Cmd.none )
      else (model, Charamel_tea.Cmd.none)
  | Stopwatch_msg msg -> step msg model

let view model =
  let km = keymap model in
  let text = Stopwatch.view model.stopwatch ^ "\n" in
  let text =
    if model.quitting then text
    else
      "Elapsed: " ^ text ^ "\n"
      ^ Help.short_view (Help.v ()) [ km.start; km.stop; km.reset; km.quit ]
  in
  Charamel_tea.View.v text

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( {
            stopwatch = Stopwatch.v ~interval:0.001 ();
            quitting = false;
            start_enabled = false;
            stop_enabled = true;
          },
          Charamel_tea.Cmd.none ));
    update;
    view;
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map
              (fun msg -> Stopwatch_msg msg)
              (Stopwatch.subscriptions model.stopwatch);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Elapsed: 0s"; "s stop"; "r reset"; "q quit" ]
  @ Smoke.expect app [ Smoke.key "s"; `Wait 1.0 ] [ "Elapsed: 999ms" ]
  @ Smoke.expect app [ Smoke.key "s"; `Wait 1.0; Smoke.key "r" ] [ "Elapsed: 0s" ]
