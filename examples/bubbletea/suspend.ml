let prompt = "\nPress ctrl-z to suspend, ctrl+c to interrupt, q, or esc to exit\n"

type model = { quitting : bool; suspending : bool }
type msg = Key of Charamel_tea.Key.t | Suspended | Resumed

let update msg model =
  match msg with
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "q" | "escape" -> ({ model with quitting = true }, Charamel_tea.Cmd.quit)
      | "ctrl+c" -> ({ model with quitting = true }, Charamel_tea.Cmd.interrupt)
      | "ctrl+z" ->
          ( model,
            Charamel_tea.Cmd.batch
              [ Charamel_tea.Cmd.msg Suspended; Charamel_tea.Cmd.suspend ] )
      | _ -> (model, Charamel_tea.Cmd.none))
  | Suspended -> ({ model with suspending = true }, Charamel_tea.Cmd.none)
  | Resumed -> ({ model with suspending = false }, Charamel_tea.Cmd.none)

let view model =
  let content = if model.suspending || model.quitting then "" else prompt in
  Charamel_tea.View.v content

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ quitting = false; suspending = false }, Charamel_tea.Cmd.none));
    update;
    view;
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resume (fun () -> Resumed);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [ "Press ctrl-z to suspend, ctrl+c to interrupt, q, or esc to exit" ]
  @ Smoke.expect app
      [ `Msg Suspended; `Msg Resumed ]
      [ "Press ctrl-z to suspend, ctrl+c to interrupt, q, or esc to exit" ]
