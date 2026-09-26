module Bubble_list = Charamel_bubbles.List
module Color = Charamel_ansi.Color
module Sides = Charamel_lipgloss.Sides
module Style = Charamel_lipgloss.Style

let list_height = 14
let item_style = Style.(empty |> padding (Sides.v ~left:4 ()))

let selected_item_style =
  Style.(empty |> padding (Sides.v ~left:2 ()) |> foreground (Color.Indexed 170))

let quit_text_style = Style.(empty |> margin (Sides.v ~top:1 ~bottom:2 ~left:4 ()))

let render_item (ctx : string Bubble_list.item_context) name =
  let line = Fmt.str "%d. %s" (ctx.index + 1) name in
  if ctx.selected then Style.render selected_item_style ("> " ^ line)
  else Style.render item_style line

let item_delegate : (string, _) Bubble_list.delegate =
  Bubble_list.
    {
      height = 1;
      spacing = 0;
      render = render_item;
      update = (fun _ _ _ -> None);
      short_help = [];
      full_help = [];
    }

let items =
  [
    "Ramen";
    "Tomato Soup";
    "Hamburgers";
    "Cheeseburgers";
    "Currywurst";
    "Okonomiyaki";
    "Pasta";
    "Fillet Mignon";
    "Caviar";
    "Just Wine";
  ]

let styles () =
  let base = Bubble_list.default_styles ~is_dark:true in
  {
    base with
    title = Style.(base.title |> margin (Sides.v ~left:2 ()));
    pagination_style = Style.(base.pagination_style |> padding (Sides.v ~left:4 ()));
    help_style = Style.(base.help_style |> padding (Sides.v ~left:4 ~bottom:1 ()));
  }

let make_list () =
  Bubble_list.v ~title:"What do you want for dinner?" ~width:20 ~height:list_height
    ~show_status_bar:false ~filtering_enabled:false ~styles:(styles ())
    ~delegate:item_delegate
    ~filter_value:(fun _ -> "")
    items

type model = { list : string Bubble_list.t; choice : string; quitting : bool }

type msg =
  | Key of Charamel_tea.Key.t
  | List_msg of string Bubble_list.msg
  | Resize of int * int

let route_key model key =
  match Bubble_list.key model.list key with
  | Some list_msg ->
      let list, cmd = Bubble_list.update list_msg model.list in
      ({ model with list }, Charamel_tea.Cmd.map (fun m -> List_msg m) cmd)
  | None -> (model, Charamel_tea.Cmd.none)

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ({ list = make_list (); choice = ""; quitting = false }, Charamel_tea.Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "q" | "ctrl+c" -> ({ model with quitting = true }, Charamel_tea.Cmd.quit)
            | "enter" ->
                let choice =
                  match Bubble_list.selected_item model.list with
                  | Some item -> item
                  | None -> model.choice
                in
                ({ model with choice }, Charamel_tea.Cmd.quit)
            | _ -> route_key model key)
        | List_msg list_msg ->
            let list, cmd = Bubble_list.update list_msg model.list in
            ({ model with list }, Charamel_tea.Cmd.map (fun m -> List_msg m) cmd)
        | Resize (_, cols) ->
            ( { model with list = Bubble_list.set_width cols model.list },
              Charamel_tea.Cmd.none ));
    view =
      (fun model ->
        let content =
          if model.choice <> "" then
            Style.render quit_text_style (model.choice ^ "? Sounds good to me.")
          else if model.quitting then
            Style.render quit_text_style "Not hungry? That’s cool."
          else "\n" ^ Bubble_list.view model.list
        in
        Charamel_tea.View.v content);
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map
              (fun m -> List_msg m)
              (Bubble_list.subscriptions model.list);
            Charamel_tea.Sub.resize (fun ~rows ~cols -> Resize (rows, cols));
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "What do you want for dinner?"; "> 1. Ramen" ]
  @ Smoke.expect app [ Smoke.key "down" ] [ "> 2. Tomato Soup" ]
  @ Smoke.expect app [ Smoke.key "down"; Smoke.key "down" ] [ "> 3. Hamburgers" ]
  @ Smoke.expect app [ Smoke.key "enter" ] [ "Ramen? Sounds good to me." ]
  @ Smoke.expect app [ Smoke.key "q" ] [ "Not hungry? That’s cool." ]
