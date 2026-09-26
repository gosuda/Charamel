open Lwt.Syntax
module Spinner = Charamel_bubbles.Spinner

type model = { responses : int; spinner : Spinner.t }
type msg = Activity | Key of Charamel_tea.Key.t | Spin of Spinner.msg

let spin_cmd cmd = Charamel_tea.Cmd.map (fun message -> Spin message) cmd
let spin_sub sub = Charamel_tea.Sub.map (fun message -> Spin message) sub

let app activity_stream : (model, msg) Charamel_tea.app =
  let activity =
    Charamel_tea.Sub.map (fun () -> Activity) (Charamel_tea.Sub.stream activity_stream)
  in
  {
    init = (fun () -> ({ responses = 0; spinner = Spinner.v () }, Charamel_tea.Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key _ -> (model, Charamel_tea.Cmd.quit)
        | Activity ->
            ({ model with responses = model.responses + 1 }, Charamel_tea.Cmd.none)
        | Spin message ->
            let spinner, cmd = Spinner.update message model.spinner in
            ({ model with spinner }, spin_cmd cmd));
    view =
      (fun model ->
        Charamel_tea.View.v
          (Fmt.str "\n %s Events received: %d\n\n Press any key to exit\n"
             (Spinner.view model.spinner) model.responses));
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            spin_sub (Spinner.subscriptions model.spinner);
            activity;
          ]);
  }

let rec activity_loop push =
  let* () = Charamel_os.Time.sleep Charamel_os.Time.lwt (0.1 +. Random.float 0.9) in
  push (Some ());
  activity_loop push

let main () =
  let activity, push = Lwt_stream.create () in
  Lwt.async (fun () -> activity_loop push);
  Smoke.run_ (app activity)

let smoke () =
  Smoke.expect
    (app (Lwt_stream.of_list []))
    [ `Msg Activity; `Msg Activity; `Msg Activity ]
    [ "Events received: 3"; "Press any key to exit" ]
  @ Smoke.expect
      (app (Lwt_stream.of_list []))
      [ `Msg Activity; `Msg (Key (Charamel_tea.Key.v Enter)) ]
      [ "Events received: 1" ]
