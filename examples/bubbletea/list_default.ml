module Cmd = Charamel_tea.Cmd
module Key = Charamel_tea.Key
module Listing = Charamel_bubbles.List
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View

type item = { title : string; desc : string }
type model = { list : item Listing.t }
type msg = Key of Key.t | Items of item Listing.msg | Win of { rows : int; cols : int }

let doc_style = Style.(empty |> margin (Charamel_lipgloss.Sides.xy ~x:2 ~y:1))

let items =
  [
    { title = "Raspberry Pi’s"; desc = "I have ’em all over my house" };
    { title = "Nutella"; desc = "It's good on toast" };
    { title = "Bitter melon"; desc = "It cools you down" };
    { title = "Nice socks"; desc = "And by that I mean socks without holes" };
    { title = "Eight hours of sleep"; desc = "I had this once" };
    { title = "Cats"; desc = "Usually" };
    { title = "Plantasia, the album"; desc = "My plants love it too" };
    { title = "Pour over coffee"; desc = "It takes forever to make though" };
    { title = "VR"; desc = "Virtual reality...what is there to say?" };
    { title = "Noguchi Lamps"; desc = "Such pleasing organic forms" };
    { title = "Linux"; desc = "Pretty much the best OS" };
    { title = "Business school"; desc = "Just kidding" };
    { title = "Pottery"; desc = "Wet clay is a great feeling" };
    { title = "Shampoo"; desc = "Nothing like clean hair" };
    { title = "Table tennis"; desc = "It’s surprisingly exhausting" };
    { title = "Milk crates"; desc = "Great for packing in your extra stuff" };
    { title = "Afternoon tea"; desc = "Especially the tea sandwich part" };
    { title = "Stickers"; desc = "The thicker the vinyl the better" };
    { title = "20° Weather"; desc = "Celsius, not Fahrenheit" };
    { title = "Warm light"; desc = "Like around 2700 Kelvin" };
    { title = "The vernal equinox"; desc = "The autumnal equinox is pretty good too" };
    { title = "Gaffer’s tape"; desc = "Basically sticky fabric" };
    { title = "Terrycloth"; desc = "In other words, towel fabric" };
  ]

let delegate =
  Listing.default_delegate
    ~title:(fun item -> item.title)
    ~description:(fun item -> item.desc)
    ()

let new_list () =
  Listing.v ~title:"My Fave Things" ~delegate ~filter_value:(fun item -> item.title) items

let apply_items model msg =
  let list, cmd = Listing.update msg model.list in
  ({ list }, Cmd.map (fun msg -> Items msg) cmd)

let on_key key model =
  match Key.to_string key with
  | "ctrl+c" -> (model, Cmd.quit)
  | ("q" | "escape") when Listing.filter_state model.list <> Listing.Filtering ->
      (model, Cmd.quit)
  | _ -> (
      match Listing.key model.list key with
      | Some msg -> apply_items model msg
      | None -> (model, Cmd.none))

let on_resize rows cols model =
  let frame_h, frame_v = Style.get_frame_size doc_style in
  let list =
    Listing.set_size ~width:(cols - frame_h) ~height:(rows - frame_v) model.list
  in
  ({ list }, Cmd.none)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ list = new_list () }, Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key key -> on_key key model
        | Items items_msg -> apply_items model items_msg
        | Win { rows; cols } -> on_resize rows cols model);
    view =
      (fun model ->
        View.v ~alt_screen:true
          ?cursor:(Listing.view_cursor model.list)
          (Style.render doc_style (Listing.view model.list)));
    subscriptions =
      (fun _ ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.resize (fun ~rows ~cols -> Win { rows; cols });
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [ "My Fave Things"; "Raspberry Pi’s"; "I have ’em all over my house"; "23 items" ]
  @ Smoke.expect app [ Smoke.key "end" ] [ "Terrycloth"; "In other words, towel fabric" ]
  @ Smoke.expect app
      [ Smoke.key "/"; `Text "nutella"; Smoke.key "enter"; Smoke.key "q" ]
      [ "Nutella"; "“nutella” 1 item"; "22 filtered" ]
