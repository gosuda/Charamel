let prompt =
  "\nWhen you're done press q to quit.\nPress any other key to query the window-size.\n"

let is_quit key =
  let name = Charamel_tea.Key.to_string key in
  String.equal name "ctrl+c" || String.equal name "q" || String.equal name "escape"

type size = { rows : int; cols : int }
type msg = Key of Charamel_tea.Key.t | Win of size

let update msg lines =
  match msg with
  | Key key ->
      if is_quit key then (lines, Charamel_tea.Cmd.quit)
      else (lines, Charamel_tea.Cmd.window_size)
  | Win { rows; cols } ->
      (lines @ [ Fmt.str "The window size is: %dx%d" cols rows ], Charamel_tea.Cmd.none)

let view lines =
  let content =
    match lines with [] -> prompt | _ :: _ -> String.concat "\n" lines ^ "\n" ^ prompt
  in
  Charamel_tea.View.v content

let app : (string list, msg) Charamel_tea.app =
  {
    init = (fun () -> ([], Charamel_tea.Cmd.none));
    update;
    view;
    subscriptions =
      (fun _ ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Win { rows; cols });
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "When you're done press q to quit"; "The window size is: 80x24" ]
  @ Smoke.expect app ~size:(12, 50) [] [ "The window size is: 50x12" ]
  @ Smoke.expect app
      [ Smoke.key "x" ]
      [ "The window size is: 80x24\nThe window size is: 80x24" ]
  @ Smoke.expect app [ `Resize (30, 100) ] [ "The window size is: 100x30" ]
