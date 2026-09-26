module Border = Charamel_lipgloss.Border
module Color = Charamel_ansi.Color
module Layout = Charamel_lipgloss.Layout
module Position = Charamel_lipgloss.Position
module Sides = Charamel_lipgloss.Sides
module Sides_color = Charamel_lipgloss.Sides_color
module Style = Charamel_lipgloss.Style

let highlight =
  Charamel_lipgloss.light_dark ~is_dark:true ~light:(Color.of_hex_or "#874BFD")
    ~dark:(Color.of_hex_or "#7D56F4")

let tab_border_with_bottom ~left ~middle ~right =
  let border = Border.rounded in
  { border with bottom_left = left; bottom = middle; bottom_right = right }

let inactive_tab_border = tab_border_with_bottom ~left:"┴" ~middle:"─" ~right:"┴"
let active_tab_border = tab_border_with_bottom ~left:"┘" ~middle:" " ~right:"└"
let doc_style = Style.(empty |> padding (Sides.xy ~x:2 ~y:2))

let inactive_tab =
  Style.(
    empty |> border inactive_tab_border
    |> border_foreground (Sides_color.all highlight)
    |> padding (Sides.xy ~x:1 ~y:0))

let active_tab =
  Style.(
    empty |> border active_tab_border
    |> border_foreground (Sides_color.all highlight)
    |> padding (Sides.xy ~x:1 ~y:0))

let window_style =
  Style.(
    empty
    |> border_foreground (Sides_color.all highlight)
    |> padding (Sides.xy ~x:0 ~y:2)
    |> align Position.center |> border Border.normal |> unset_border_top)

let render_tab ~first ~last ~active name =
  let style = if active then active_tab else inactive_tab in
  let border =
    match Style.get_border style with
    | Some border when first && active -> { border with bottom_left = "│" }
    | Some border when first -> { border with bottom_left = "├" }
    | Some border when last && active -> { border with bottom_right = "│" }
    | Some border when last -> { border with bottom_right = "┤" }
    | Some border -> border
    | None -> Border.none
  in
  Style.render (Style.border border style) name

type model = { tabs : string list; contents : string list; active : int }
type msg = Key of Charamel_tea.Key.t

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( {
            tabs = [ "Lip Gloss"; "Blush"; "Eye Shadow"; "Mascara"; "Foundation" ];
            contents =
              [
                "Lip Gloss Tab";
                "Blush Tab";
                "Eye Shadow Tab";
                "Mascara Tab";
                "Foundation Tab";
              ];
            active = 0;
          },
          Charamel_tea.Cmd.none ));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "q" | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
            | "right" | "l" | "n" | "tab" ->
                ( {
                    model with
                    active = min (model.active + 1) (List.length model.tabs - 1);
                  },
                  Charamel_tea.Cmd.none )
            | "left" | "h" | "p" | "shift+tab" ->
                ({ model with active = max (model.active - 1) 0 }, Charamel_tea.Cmd.none)
            | _ -> (model, Charamel_tea.Cmd.none)));
    view =
      (fun model ->
        let last = List.length model.tabs - 1 in
        let rendered =
          List.mapi
            (fun i name ->
              render_tab ~first:(i = 0) ~last:(i = last) ~active:(i = model.active) name)
            model.tabs
        in
        let row = Layout.join_horizontal ~pos:Position.top rendered in
        let panel = List.nth model.contents model.active in
        let window = Style.render (Style.width (Layout.width row) window_style) panel in
        Charamel_tea.View.v (Style.render doc_style (row ^ "\n" ^ window)));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Lip Gloss Tab" ]
  @ Smoke.expect app [ Smoke.key "tab" ] [ "Blush Tab" ]
  @ Smoke.expect app
      [ Smoke.key "tab"; Smoke.key "tab"; Smoke.key "right" ]
      [ "Mascara Tab" ]
  @ Smoke.expect app [ Smoke.key "left" ] [ "Lip Gloss Tab" ]
  @ Smoke.expect app [ Smoke.key "tab"; Smoke.key "shift+tab" ] [ "Lip Gloss Tab" ]
