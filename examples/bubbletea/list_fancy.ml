module Cmd = Charamel_tea.Cmd
module Color = Charamel_ansi.Color
module Event = Charamel_tea.Event
module Key = Charamel_tea.Key
module Key_binding = Charamel_bubbles.Key_binding
module Listing = Charamel_bubbles.List
module Sides = Charamel_lipgloss.Sides
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View

type item = { title : string; description : string }
type styles = { app : Style.t; title : Style.t; status_message : Style.t }
type generator = { title_index : int; desc_index : int }

type model = {
  list : item Listing.t;
  styles : styles;
  width : int;
  height : int;
  generator : generator;
  remove_enabled : bool;
}

type msg =
  | Key of Key.t
  | Items of item Listing.msg
  | Theme of Event.t
  | Win of { rows : int; cols : int }

let titles =
  [|
    "Artichoke";
    "Baking Flour";
    "Bananas";
    "Barley";
    "Bean Sprouts";
    "Bitter Melon";
    "Black Cod";
    "Blood Orange";
    "Brown Sugar";
    "Cashew Apple";
    "Cashews";
    "Cat Food";
    "Coconut Milk";
    "Cucumber";
    "Curry Paste";
    "Currywurst";
    "Dill";
    "Dragonfruit";
    "Dried Shrimp";
    "Eggs";
    "Fish Cake";
    "Furikake";
    "Garlic";
    "Gherkin";
    "Ginger";
    "Granulated Sugar";
    "Grapefruit";
    "Green Onion";
    "Hazelnuts";
    "Heavy whipping cream";
    "Honey Dew";
    "Horseradish";
    "Jicama";
    "Kohlrabi";
    "Leeks";
    "Lentils";
    "Licorice Root";
    "Meyer Lemons";
    "Milk";
    "Molasses";
    "Muesli";
    "Nectarine";
    "Niagamo Root";
    "Nopal";
    "Nutella";
    "Oat Milk";
    "Oatmeal";
    "Olives";
    "Papaya";
    "Party Gherkin";
    "Peppers";
    "Persian Lemons";
    "Pickle";
    "Pineapple";
    "Plantains";
    "Pocky";
    "Powdered Sugar";
    "Quince";
    "Radish";
    "Ramps";
    "Star Anise";
    "Sweet Potato";
    "Tamarind";
    "Unsalted Butter";
    "Watermelon";
    "Weißwurst";
    "Yams";
    "Yeast";
    "Yuzu";
    "Snow Peas";
  |]

let descs =
  [|
    "A little weird";
    "Bold flavor";
    "Can’t get enough";
    "Delectable";
    "Expensive";
    "Expired";
    "Exquisite";
    "Fresh";
    "Gimme";
    "In season";
    "Kind of spicy";
    "Looks fresh";
    "Looks good to me";
    "Maybe not";
    "My favorite";
    "Oh my";
    "On sale";
    "Organic";
    "Questionable";
    "Really fresh";
    "Refreshing";
    "Salty";
    "Scrumptious";
    "Delectable";
    "Slightly sweet";
    "Smells great";
    "Tasty";
    "Too ripe";
    "At last";
    "What?";
    "Wow";
    "Yum";
    "Maybe";
    "Sure, why not?";
  |]

let new_styles ~is_dark =
  let status_color =
    Charamel_lipgloss.light_dark ~is_dark ~light:(Color.of_hex_or "#04B575")
      ~dark:(Color.of_hex_or "#04B575")
  in
  {
    app = Style.(empty |> padding (Sides.xy ~x:2 ~y:1));
    title =
      Style.(
        empty
        |> foreground (Color.of_hex_or "#FFFDF5")
        |> background (Color.of_hex_or "#25A065")
        |> padding (Sides.xy ~x:1 ~y:0));
    status_message = Style.(empty |> foreground status_color);
  }

let keymap_insert = Key_binding.v ~help:("a", "add item") [ "a" ]
let keymap_toggle_spinner = Key_binding.v ~help:("s", "toggle spinner") [ "s" ]
let keymap_toggle_title = Key_binding.v ~help:("T", "toggle title") [ "T" ]
let keymap_toggle_status = Key_binding.v ~help:("S", "toggle status") [ "S" ]
let keymap_toggle_pagination = Key_binding.v ~help:("P", "toggle pagination") [ "P" ]
let keymap_toggle_help = Key_binding.v ~help:("H", "toggle help") [ "H" ]
let delegate_keys_choose = Key_binding.v ~help:("enter", "choose") [ "enter" ]
let delegate_keys_remove = Key_binding.v ~help:("x", "delete") [ "x"; "backspace" ]

let delegate =
  let base =
    Listing.default_delegate
      ~title:(fun (i : item) -> i.title)
      ~description:(fun (i : item) -> i.description)
      ()
  in
  {
    base with
    short_help = [ delegate_keys_choose; delegate_keys_remove ];
    full_help = [ [ delegate_keys_choose; delegate_keys_remove ] ];
  }

let next generator =
  let item =
    { title = titles.(generator.title_index); description = descs.(generator.desc_index) }
  in
  ( {
      title_index = (generator.title_index + 1) mod Array.length titles;
      desc_index = (generator.desc_index + 1) mod Array.length descs;
    },
    item )

let take count generator =
  let rec go generator acc = function
    | 0 -> (List.rev acc, generator)
    | n ->
        let generator, item = next generator in
        go generator (item :: acc) (n - 1)
  in
  go generator [] count

let list_with_title_style ~title list =
  Listing.set_styles { (Listing.styles list) with title } list

let update_list_properties model =
  let frame_width, frame_height = Style.get_frame_size model.styles.app in
  let list =
    Listing.set_size ~width:(model.width - frame_width)
      ~height:(model.height - frame_height) model.list
  in
  { model with list }

let initial_model () =
  let styles = new_styles ~is_dark:false in
  let items, generator = take 24 { title_index = 0; desc_index = 0 } in
  let list =
    Listing.v ~title:"Groceries" ~delegate ~filter_value:(fun (i : item) -> i.title) items
    |> list_with_title_style ~title:styles.title
    |> Listing.set_additional_full_help_keys
         [
           keymap_toggle_spinner;
           keymap_insert;
           keymap_toggle_title;
           keymap_toggle_status;
           keymap_toggle_pagination;
           keymap_toggle_help;
         ]
  in
  { list; styles; width = 0; height = 0; generator; remove_enabled = true }

let apply_items model msg =
  let list, cmd = Listing.update msg model.list in
  ({ model with list }, Cmd.map (fun msg -> Items msg) cmd)

let with_status model text =
  let list, cmd =
    Listing.new_status_message (Style.render model.styles.status_message text) model.list
  in
  ({ model with list }, Cmd.map (fun msg -> Items msg) cmd)

let on_toggle_title model =
  let show = not (Listing.show_title model.list) in
  let list =
    model.list |> Listing.set_show_title show |> Listing.set_show_filter show
    |> Listing.set_filtering_enabled show
  in
  ({ model with list }, Cmd.none)

let on_insert model =
  let generator, item = next model.generator in
  let model =
    {
      model with
      generator;
      remove_enabled = true;
      list = Listing.insert_item 0 item model.list;
    }
  in
  with_status model ("Added " ^ item.title)

let on_choose model =
  match Listing.selected_item model.list with
  | None -> (model, Cmd.none)
  | Some item -> with_status model ("You chose " ^ item.title)

let on_remove model =
  if not model.remove_enabled then (model, Cmd.none)
  else
    let index = Listing.index model.list in
    match Listing.selected_item model.list with
    | None -> (model, Cmd.none)
    | Some item ->
        let list = Listing.remove_item index model.list in
        let model = { model with list; remove_enabled = Listing.items list <> [] } in
        with_status model ("Deleted " ^ item.title)

let on_key key model =
  match Key.to_string key with
  | "ctrl+c" -> (model, Cmd.quit)
  | ("q" | "escape") when Listing.filter_state model.list <> Listing.Filtering ->
      (model, Cmd.quit)
  | _ when Listing.filter_state model.list = Listing.Filtering -> (
      match Listing.key model.list key with
      | Some msg -> apply_items model msg
      | None -> (model, Cmd.none))
  | _ when Key_binding.matches key keymap_toggle_spinner ->
      ({ model with list = Listing.toggle_spinner model.list }, Cmd.none)
  | _ when Key_binding.matches key keymap_toggle_title -> on_toggle_title model
  | _ when Key_binding.matches key keymap_toggle_status ->
      let show = not (Listing.show_status_bar model.list) in
      ({ model with list = Listing.set_show_status_bar show model.list }, Cmd.none)
  | _ when Key_binding.matches key keymap_toggle_pagination ->
      let show = not (Listing.show_pagination model.list) in
      ({ model with list = Listing.set_show_pagination show model.list }, Cmd.none)
  | _ when Key_binding.matches key keymap_toggle_help ->
      let show = not (Listing.show_help model.list) in
      ({ model with list = Listing.set_show_help show model.list }, Cmd.none)
  | _ when Key_binding.matches key keymap_insert -> on_insert model
  | _ when Key_binding.matches key delegate_keys_choose -> on_choose model
  | _ when Key_binding.matches key delegate_keys_remove -> on_remove model
  | _ -> (
      match Listing.key model.list key with
      | Some msg -> apply_items model msg
      | None -> (model, Cmd.none))

let on_theme color model =
  let styles = new_styles ~is_dark:(Charamel_lipgloss.Color_util.is_dark color) in
  let model = { model with styles } in
  let model =
    { model with list = list_with_title_style ~title:styles.title model.list }
  in
  (update_list_properties model, Cmd.none)

let update msg model =
  match msg with
  | Key key -> on_key key model
  | Items items_msg -> apply_items model items_msg
  | Theme (Background_color color) -> on_theme color model
  | Theme _ -> (model, Cmd.none)
  | Win { rows; cols } ->
      (update_list_properties { model with width = cols; height = rows }, Cmd.none)

let view model =
  View.v ~alt_screen:true
    ?cursor:(Listing.view_cursor model.list)
    (Style.render model.styles.app (Listing.view model.list))

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (initial_model (), Cmd.query `Background));
    update;
    view;
    subscriptions =
      (fun model ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.resize (fun ~rows ~cols -> Win { rows; cols });
            Sub.terminal (fun event -> Theme event);
            Sub.map (fun msg -> Items msg) (Listing.subscriptions model.list);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app
    [ Smoke.key "q" ]
    [ "Groceries"; "Artichoke"; "A little weird"; "24 items" ]
  @ Smoke.expect app [ Smoke.key "a"; Smoke.key "q" ] [ "Added Ginger"; "Ginger" ]
  @ Smoke.expect app [ Smoke.key "x"; Smoke.key "q" ] [ "Deleted Artichoke"; "23 items" ]
  @ Smoke.expect app [ Smoke.key "enter"; Smoke.key "q" ] [ "You chose Artichoke" ]
  @ Smoke.expect app
      [ Smoke.key "/"; `Text "bana"; Smoke.key "enter"; Smoke.key "q" ]
      [ "“bana” 1 item"; "23 filtered" ]
