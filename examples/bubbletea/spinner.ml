module Spinner = Charamel_bubbles.Spinner

type model = { spinner : Spinner.t; quitting : bool }
type msg = Key of Charamel_tea.Key.t | Spin of Spinner.msg

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( {
            spinner =
              Spinner.v ~kind:Dot
                ~style:Charamel_lipgloss.Style.(empty |> foreground (Indexed 205))
                ();
            quitting = false;
          },
          Charamel_tea.Cmd.none ));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "q" | "escape" | "ctrl+c" ->
                ({ model with quitting = true }, Charamel_tea.Cmd.quit)
            | _ -> (model, Charamel_tea.Cmd.none))
        | Spin msg ->
            let spinner, cmd = Spinner.update msg model.spinner in
            ({ model with spinner }, Charamel_tea.Cmd.map (fun msg -> Spin msg) cmd));
    view =
      (fun model ->
        let text =
          Fmt.str "\n\n   %s Loading forever...press q to quit\n\n"
            (Spinner.view model.spinner)
        in
        Charamel_tea.View.v (if model.quitting then text ^ "\n" else text));
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map
              (fun msg -> Spin msg)
              (Spinner.subscriptions model.spinner);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  let first = List.hd (Spinner.frames Dot) and second = List.nth (Spinner.frames Dot) 1 in
  Smoke.expect app [ Smoke.key "q" ] [ first ^ " Loading forever...press q to quit" ]
  @ Smoke.expect app [ `Wait 0.15; Smoke.key "q" ] [ second ^ " Loading forever" ]
