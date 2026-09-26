open Lwt.Syntax
module Spinner = Charamel_bubbles.Spinner

type model = { ticks : int; spinner : Spinner.t }
type msg = Tick_count of int | Key of Charamel_tea.Key.t | Spin of Spinner.msg

let interval = 0.1

let app source : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ ticks = 0; spinner = Spinner.v () }, Charamel_tea.Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key key when Stdlib.List.mem (Charamel_tea.Key.to_string key) [ "q"; "ctrl+c" ]
          ->
            (model, Charamel_tea.Cmd.quit)
        | Key _ -> (model, Charamel_tea.Cmd.none)
        | Tick_count ticks -> ({ model with ticks }, Charamel_tea.Cmd.none)
        | Spin message ->
            let spinner, cmd = Spinner.update message model.spinner in
            ({ model with spinner }, Charamel_tea.Cmd.map (fun m -> Spin m) cmd));
    view =
      (fun model ->
        Charamel_tea.View.v
          (Fmt.str "%s Ticks: %d\n\nPress q to quit\n" (Spinner.view model.spinner)
             model.ticks));
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map (fun m -> Spin m) (Spinner.subscriptions model.spinner);
            Charamel_tea.Sub.map (fun n -> Tick_count n) (Charamel_tea.Sub.stream source);
          ]);
  }

let rec producer push count =
  let* () = Charamel_os.Time.sleep Charamel_os.Time.lwt interval in
  push (Some count);
  producer push (count + 1)

let main () =
  let source, push = Lwt_stream.create () in
  Lwt.async (fun () -> producer push 1);
  Smoke.run_ (app source)

let smoke () =
  Smoke.expect (app (Lwt_stream.of_list [])) [] [ "Ticks: 0"; "Press q to quit" ]
  @ Smoke.expect
      (app (Lwt_stream.of_list []))
      [ `Msg (Tick_count 1); `Msg (Tick_count 2) ]
      [ "Ticks: 2" ]
  @ Smoke.expect
      (app (Lwt_stream.of_list []))
      [ `Msg (Tick_count 1); Smoke.key "q" ]
      [ "Ticks: 1" ]
