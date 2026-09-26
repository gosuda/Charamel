type model = { disambiguation : bool; event_types : bool }

type msg =
  | Key of Charamel_tea.Key.t
  | Release of Charamel_tea.Key.t
  | Report of Charamel_tea.Event.t

let keyboard : Charamel_tea.View.keyboard =
  {
    disambiguate = true;
    report_events = true;
    report_alternates = false;
    report_all_keys = false;
    report_text = false;
  }

let update msg model =
  match msg with
  | Report (Charamel_tea.Event.Kitty_flags flags) ->
      ({ disambiguation = true; event_types = flags land 2 <> 0 }, Charamel_tea.Cmd.none)
  | Report _ -> (model, Charamel_tea.Cmd.none)
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
      | name -> (model, Charamel_tea.Cmd.print ("  press: " ^ name ^ "\n")))
  | Release key ->
      (model, Charamel_tea.Cmd.print ("release: " ^ Charamel_tea.Key.to_string key))

let view model =
  Charamel_tea.View.v ~keyboard
    (Fmt.str
       "Terminal supports key releases: %b\n\
        Terminal supports key disambiguation: %b\n\
        This demo logs key events. Press ctrl+c to quit.\n"
       model.event_types model.disambiguation)

let subscriptions _ =
  Charamel_tea.Sub.batch
    [
      Charamel_tea.Sub.key (fun key -> Key key);
      Charamel_tea.Sub.key_release (fun key -> Release key);
      Charamel_tea.Sub.terminal (fun event -> Report event);
    ]

let init () =
  ({ disambiguation = false; event_types = false }, Charamel_tea.Cmd.query `Kitty_flags)

let app : (model, msg) Charamel_tea.app = { init; update; view; subscriptions }
let main () = Smoke.run_ app
let flags value = `Msg (Report (Charamel_tea.Event.Kitty_flags value))

let smoke () =
  Smoke.expect app [] [ "key releases: false"; "key disambiguation: false" ]
  @ Smoke.expect app [ flags 3 ] [ "key releases: true"; "key disambiguation: true" ]
  @ Smoke.expect app [ flags 1 ] [ "key releases: false"; "key disambiguation: true" ]
