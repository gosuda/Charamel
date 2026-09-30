module Cmd = Charamel_tea.Cmd
module Key = Charamel_tea.Key
module Picker = Charamel_bubbles.Filepicker
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View

type model = {
  filepicker : Picker.t;
  selected_file : string;
  quitting : bool;
  err : string option;
}

type msg = Key of Key.t | Pick of Picker.msg | Clear_error | Win of { rows : int }

let allowed_types = [ ".mod"; ".sum"; ".go"; ".txt"; ".md" ]

let apply_pick msg model =
  let selected = Picker.did_select_file msg model.filepicker in
  let rejected = Picker.did_select_disabled_file msg model.filepicker in
  let filepicker, cmd = Picker.update msg model.filepicker in
  let model = { model with filepicker } in
  let cmd = Cmd.map (fun msg -> Pick msg) cmd in
  match selected with
  | Some path -> ({ model with selected_file = path }, cmd)
  | None -> (
      match rejected with
      | None -> (model, cmd)
      | Some path ->
          ( { model with err = Some (path ^ " is not valid."); selected_file = "" },
            Cmd.batch [ cmd; Cmd.after 2.0 (fun () -> Clear_error) ] ))

let on_key key model =
  match Key.to_string key with
  | "ctrl+c" | "q" -> ({ model with quitting = true }, Cmd.quit)
  | _ -> (
      match Picker.key model.filepicker key with
      | Some msg -> apply_pick msg model
      | None -> (model, Cmd.none))

let view model =
  if model.quitting then View.v ~alt_screen:true ""
  else
    let head =
      match model.err with
      | Some message -> Style.render Picker.default_styles.disabled_file message
      | None ->
          if model.selected_file = "" then "Pick a file:"
          else
            "Selected file: "
            ^ Style.render Picker.default_styles.selected model.selected_file
    in
    View.v ~alt_screen:true ("\n  " ^ head ^ "\n\n" ^ Picker.view model.filepicker ^ "\n")

let app ~root : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        let filepicker, cmd = Picker.init (Picker.v ~root ~allowed_types ()) in
        ( { filepicker; selected_file = ""; quitting = false; err = None },
          Cmd.map (fun msg -> Pick msg) cmd ));
    update =
      (fun msg model ->
        match msg with
        | Key key -> on_key key model
        | Pick pick_msg -> apply_pick pick_msg model
        | Clear_error -> ({ model with err = None }, Cmd.none)
        | Win { rows } -> apply_pick (Picker.Resize rows) model);
    view;
    subscriptions =
      (fun _ ->
        Sub.batch
          [ Sub.key (fun key -> Key key); Sub.resize (fun ~rows ~cols:_ -> Win { rows }) ]);
  }

let home () = Option.value (Sys.getenv_opt "HOME") ~default:"."

let main () =
  Lwt.map
    (fun result ->
      let selected = match result with Some model -> model.selected_file | None -> "" in
      print_string
        ("\n  You selected: "
        ^ Style.render Picker.default_styles.selected selected
        ^ "\n"))
    (Smoke.run (app ~root:(home ())))

let fixture_files = [ "alpha.go"; "beta.md"; "gamma.txt"; "delta.bin" ]

let write_file path text =
  let channel = open_out path in
  output_string channel text;
  close_out channel

let make_fixture () =
  let dir = Filename.temp_file "charamel-file-picker" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  List.iter (fun name -> write_file (Filename.concat dir name) "sample\n") fixture_files;
  let nested = Filename.concat dir "nested" in
  Unix.mkdir nested 0o700;
  write_file (Filename.concat nested "inner.go") "sample\n";
  dir

let remove_tree dir =
  let clean path =
    try if Sys.is_directory path then Unix.rmdir path else Unix.unlink path
    with Unix.Unix_error _ | Sys_error _ -> ()
  in
  (try Array.iter (fun name -> clean (Filename.concat dir name)) (Sys.readdir dir)
   with Unix.Unix_error _ | Sys_error _ -> ());
  clean dir

let smoke () =
  let dir = make_fixture () in
  let app = app ~root:dir in
  let pairs =
    Smoke.expect app []
      [
        "Pick a file:";
        (* The directory's rendered size is its [st_size], which depends on the
           filesystem (60B on APFS, 4KiB on ext4), so the needle stops before it;
           Windows reports directories 0o777. *)
        (if Sys.win32 then "> drwxrwxrwx" else "> drwx------");
        "nested";
        "alpha.go";
        "beta.md";
        "gamma.txt";
        "delta.bin";
      ]
    @ Smoke.expect app
        [ `Wait 0.5; Smoke.key "j"; Smoke.key "enter" ]
        [ "Selected file: alpha.go" ]
    @ Smoke.expect app
        [ `Wait 0.5; Smoke.key "G"; Smoke.key "enter" ]
        [ "Selected file: gamma.txt" ]
    @ Smoke.expect app [ `Wait 0.5; Smoke.key "enter" ] [ "Pick a file:"; "inner.go" ]
    @ Smoke.expect app
        [ `Wait 0.5; Smoke.key "j"; Smoke.key "j"; Smoke.key "enter" ]
        [ "Selected file: beta.md" ]
  in
  remove_tree dir;
  pairs
