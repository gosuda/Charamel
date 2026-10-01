module Help = Charamel_bubbles.Help
module Key_binding = Charamel_bubbles.Key_binding
module Timer = Charamel_bubbles.Timer

let timeout = 5.
let interval = 0.001

type keymap = {
  start : Key_binding.t;
  stop : Key_binding.t;
  reset : Key_binding.t;
  quit : Key_binding.t;
}

type model = {
  timer : Timer.t;
  quitting : bool;
  start_enabled : bool;
  stop_enabled : bool;
}

type msg = Key of Charamel_tea.Key.t | Timer_msg of Timer.msg

let keymap model =
  {
    start = Key_binding.v ~help:("s", "start") ~enabled:model.start_enabled [ "s" ];
    stop = Key_binding.v ~help:("s", "stop") ~enabled:model.stop_enabled [ "s" ];
    reset = Key_binding.v ~help:("r", "reset") [ "r" ];
    quit = Key_binding.v ~help:("q", "quit") [ "q"; "ctrl+c" ];
  }

let reset_timer timer =
  let fresh = Timer.v ~interval ~timeout () in
  if Timer.running timer then fresh else Timer.stop fresh

let update msg model =
  match msg with
  | Key key ->
      let km = keymap model in
      if Key_binding.matches key km.quit then
        ({ model with quitting = true }, Charamel_tea.Cmd.quit)
      else if Key_binding.matches key km.reset then
        ({ model with timer = reset_timer model.timer }, Charamel_tea.Cmd.none)
      else if Key_binding.matches_any key [ km.start; km.stop ] then
        let timer = Timer.toggle model.timer in
        let running = Timer.running timer in
        ( { model with timer; start_enabled = not running; stop_enabled = running },
          Charamel_tea.Cmd.none )
      else (model, Charamel_tea.Cmd.none)
  | Timer_msg msg ->
      let timer, cmd = Timer.update msg model.timer in
      if Timer.timed_out timer then
        ({ model with timer; quitting = true }, Charamel_tea.Cmd.quit)
      else ({ model with timer }, Charamel_tea.Cmd.map (fun msg -> Timer_msg msg) cmd)

let view model =
  let km = keymap model in
  let remaining =
    if Timer.timed_out model.timer then "All done!" else Timer.view model.timer
  in
  let text = remaining ^ "\n" in
  let text =
    if model.quitting then text
    else
      "Exiting in " ^ text ^ "\n"
      ^ Help.short_view (Help.v ()) [ km.start; km.stop; km.reset; km.quit ]
  in
  Charamel_tea.View.v text

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( {
            timer = Timer.v ~interval ~timeout ();
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
              (fun msg -> Timer_msg msg)
              (Timer.subscriptions model.timer);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Exiting in 5s"; "s stop"; "r reset"; "q quit" ]
  @ Smoke.expect app [ `Wait 1.0 ] [ "Exiting in 4.001s" ]
  @ Smoke.expect app [ Smoke.key "s" ] [ "s start" ]
  @ Smoke.expect app [ Smoke.key "r"; `Wait 2.0 ] [ "Exiting in 3s" ]
