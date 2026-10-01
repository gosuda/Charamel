module Color = Charamel_ansi.Color
module Textarea = Charamel_bubbles.Textarea

type model = { ta : Textarea.t }
type msg = Key of Charamel_tea.Key.t | Ta of Textarea.msg | Bg of bool | Other

let textarea () =
  Textarea.v ~placeholder:"Schnrr..." ~show_line_numbers:true ~dynamic_height:true
    ~min_height:3 ~max_height:15 ~max_content_height:20 ~width:60 ~virtual_cursor:false ()

let status ta =
  Fmt.str "\nHeight: %d · Lines: %d · Cursor: (%d, %d) · Scroll: %.0f%%"
    (Textarea.height ta) (Textarea.line_count ta) (Textarea.line ta) (Textarea.column ta)
    (Textarea.scroll_percent ta *. 100.)

let route_key model key =
  match Textarea.key model.ta key with
  | Some input_msg ->
      let ta, cmd = Textarea.update input_msg model.ta in
      let msg_cmd = Charamel_tea.Cmd.map (fun m -> Ta m) cmd in
      (({ ta } : model), msg_cmd)
  | None -> (model, Charamel_tea.Cmd.none)

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        let ta, focus_cmd = Textarea.focus (textarea ()) in
        let cmds =
          [
            Charamel_tea.Cmd.map (fun m -> Ta m) focus_cmd;
            Charamel_tea.Cmd.query `Background;
          ]
        in
        (({ ta } : model), Charamel_tea.Cmd.batch cmds));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
            | _ -> route_key model key)
        | Ta input_msg ->
            let ta, cmd = Textarea.update input_msg model.ta in
            let msg_cmd = Charamel_tea.Cmd.map (fun m -> Ta m) cmd in
            (({ ta } : model), msg_cmd)
        | Bg is_dark ->
            let styles = Textarea.default_styles ~is_dark in
            ({ ta = Textarea.set_styles styles model.ta }, Charamel_tea.Cmd.none)
        | Other -> (model, Charamel_tea.Cmd.none));
    view =
      (fun model ->
        let content =
          "\n"
          ^ String.concat "\n"
              [ Textarea.view model.ta; status model.ta; "\n(ctrl+c to quit)" ]
        in
        let cursor =
          match Textarea.cursor model.ta with
          | Some c -> Some { c with Charamel_tea.Cursor.row = c.row + 1 }
          | None -> None
        in
        Charamel_tea.View.v ?cursor content);
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map (fun m -> Ta m) (Textarea.subscriptions model.ta);
            Charamel_tea.Sub.terminal (function
              | Charamel_tea.Event.Background_color c -> Bg (Color.is_dark c)
              | _ -> Other);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Schnrr..."; "Height: 3" ]
  @ Smoke.expect app [ `Text "hello" ] [ "hello"; "Cursor: (0, 5)" ]
  @ Smoke.expect app [ `Text "one"; Smoke.key "enter"; `Text "two" ] [ "Lines: 2" ]
  @ Smoke.expect app
      [
        `Text "a";
        Smoke.key "enter";
        `Text "b";
        Smoke.key "enter";
        `Text "c";
        Smoke.key "enter";
        `Text "d";
      ]
      [ "Lines: 4" ]
