module Textarea = Charamel_bubbles.Textarea
module Layout = Charamel_lipgloss.Layout
module Color = Charamel_ansi.Color

let header_view = "Tell me a story.\n"
let footer_view = "\n(ctrl+c to quit)\n"

let initial_area =
  Textarea.v ~placeholder:"Once upon a time..." ~virtual_cursor:false
    ~styles:(Textarea.default_styles ~is_dark:true)
    ()

type msg =
  | Key of Charamel_tea.Key.t
  | Area of Textarea.msg
  | Report of Charamel_tea.Event.t

let area_cmd cmd = Charamel_tea.Cmd.map (fun msg -> Area msg) cmd

let edit msg area =
  let area, cmd = Textarea.update msg area in
  (area, area_cmd cmd)

let refocus area =
  if Textarea.focused area then (area, Charamel_tea.Cmd.none)
  else
    let area, cmd = Textarea.focus area in
    (area, area_cmd cmd)

let type_key key area =
  let area, focus = refocus area in
  match Textarea.key area key with
  | Some msg ->
      let area, cmd = Textarea.update msg area in
      (area, Charamel_tea.Cmd.batch [ focus; area_cmd cmd ])
  | None -> (area, focus)

let update msg area =
  match msg with
  | Area msg -> edit msg area
  | Report (Charamel_tea.Event.Background_color color) ->
      let styles = Textarea.default_styles ~is_dark:(Color.is_dark color) in
      (Textarea.set_styles styles area, Charamel_tea.Cmd.none)
  | Report _ -> (area, Charamel_tea.Cmd.none)
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "ctrl+c" -> (area, Charamel_tea.Cmd.quit)
      | "esc" ->
          if Textarea.focused area then (Textarea.blur area, Charamel_tea.Cmd.none)
          else (area, Charamel_tea.Cmd.none)
      | _ -> type_key key area)

let view area =
  let content = String.concat "\n" [ header_view; Textarea.view area; footer_view ] in
  let cursor =
    match Textarea.cursor area with
    | Some cursor ->
        Some
          { cursor with Charamel_tea.Cursor.row = cursor.row + Layout.height header_view }
    | None -> None
  in
  Charamel_tea.View.v ?cursor content

let subscriptions area =
  Charamel_tea.Sub.batch
    [
      Charamel_tea.Sub.key (fun key -> Key key);
      Charamel_tea.Sub.map (fun msg -> Area msg) (Textarea.subscriptions area);
      Charamel_tea.Sub.terminal (fun event -> Report event);
    ]

let init () =
  let area, cmd = Textarea.focus initial_area in
  (area, Charamel_tea.Cmd.batch [ area_cmd cmd; Charamel_tea.Cmd.query `Background ])

let app : (Textarea.t, msg) Charamel_tea.app = { init; update; view; subscriptions }
let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [ Smoke.key "ctrl+c" ] [ "Tell me a story."; "Once upon a time..." ]
  @ Smoke.expect app [ `Text "Pikachu"; Smoke.key "ctrl+c" ] [ "Pikachu" ]
  @ Smoke.expect app
      [ `Text "Pikachu"; Smoke.key "esc"; Smoke.key "ctrl+c" ]
      [ "Pikachu"; "(ctrl+c to quit)" ]
  @ Smoke.expect app
      [ `Text "abc"; Smoke.key "enter"; `Text "d"; Smoke.key "ctrl+c" ]
      [ "abc"; "d" ]
