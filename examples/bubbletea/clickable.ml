module C = Charamel_lipgloss.Compositor
module Cmd = Charamel_tea.Cmd
module La = Charamel_lipgloss.Layout
module Layer = Charamel_lipgloss.Layer
module Mouse = Charamel_tea.Mouse
module P = Charamel_lipgloss.Position
module Raster = Charamel_ansi.Raster
module Sc = Charamel_lipgloss.Sides_color
module Si = Charamel_lipgloss.Sides
module St = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module V = Charamel_tea.View

let max_dialogs = 999
let button_label = "Run Away"
let hex = Charamel_ansi.Color.of_hex_or
let indexed color = Charamel_ansi.Color.Indexed color
let bg_text_style = St.(empty |> foreground (indexed 239) |> padding (Si.xy ~x:2 ~y:1))
let whitespace = ("/", St.(empty |> foreground (indexed 238)))
let dialog_word_style = St.(empty |> foreground (hex "#E7E1CC"))

let dialog_style hovered =
  St.(
    empty
    |> foreground (hex "#E7E1CC")
    |> width 36 |> height 8
    |> padding (Si.xy ~x:3 ~y:1)
    |> border Charamel_lipgloss.Border.rounded
    |> border_foreground (Sc.all (hex (if hovered then "#F25D94" else "#874BFD"))))

let special_word_style dark =
  St.(empty |> foreground (hex (if dark then "#73F59F" else "#43BF6D")))

let button_style hovered =
  St.(
    empty
    |> padding (Si.xy ~x:3 ~y:0)
    |> foreground (hex "#FFF7DB")
    |> background (hex (if hovered then "#FF5F87" else "#6124DF")))

let uncapitalized = [ "of"; "a"; "an"; "and"; "’n’" ]

let adjectives =
  [|
    "a hot";
    "a cute";
    "a fresh";
    "a nice";
    "a lovely";
    "an eager";
    "a soft";
    "an expensive";
    "a new";
    "an old";
    "a happy";
    "a messy";
    "a good";
    "a bad";
    "a cheesy";
    "a friendly";
    "a free";
    "a cold";
    "a gorgeous";
    "a glamorous";
    "a handsome";
    "an exquisite";
    "a tantalizing";
    "a suspicious";
    "an american";
    "a wooden";
    "a golden";
    "a dirty";
    "a hairy";
    "a lukewarm";
    "a burning hot";
    "a shiny";
    "a rogue";
    "a green";
    "a late night";
    "a mass produced";
    "a handmade";
    "a wild";
    "a clean";
    "a rugged";
    "the #1";
    "the best";
    "the worst";
    "a famous";
    "an infamous";
    "a clever";
    "a microwaved";
    "a 3D printed";
    "your favorite";
    "your least favorite";
    "someone’s";
    "a precious";
    "a fake";
    "a genuine";
    "a bejeweled";
    "a good-smelling";
  |]

let nouns =
  [|
    "pear";
    "banana";
    "bowl of ramen";
    "currywurst";
    "quince";
    "pie";
    "cake";
    "burrito";
    "sushi";
    "basket of fish ’n’ chips";
    "burger";
    "kohlrabi";
    "pineapple";
    "cantaloupe";
    "sausage roll";
    "yuzu";
    "grapefruit";
    "espresso shot";
    "sandwich";
    "bowl of chow mein";
    "lemon";
    "cup of coffee";
    "bottle of hot sauce";
    "can of beer";
    "glass of wine";
    "muffin";
    "bagel";
    "glass of champagne";
    "bottle of rosé";
    "pengu";
    "badger";
    "mango";
    "okonomiyaki";
    "meatball";
    "box of wine";
    "artichoke";
    "TUI";
    "linux distro";
    "dotfile";
    "weißwurst";
    "computer";
  |]

let is_word_rune c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || Char.code c > 127

let title_word word =
  let buffer = Buffer.create (String.length word) in
  let previous = ref false in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') && not !previous then
        Buffer.add_char buffer (Char.uppercase_ascii c)
      else Buffer.add_char buffer c;
      previous := is_word_rune c)
    word;
  Buffer.contents buffer

let capitalize phrase =
  String.split_on_char ' ' phrase
  |> Stdlib.List.mapi (fun index word ->
      if index > 0 && Stdlib.List.mem word uncapitalized then word else title_word word)
  |> String.concat " "

let word_at adjectives_at nouns_at =
  capitalize (adjectives.(adjectives_at) ^ " " ^ nouns.(nouns_at))

type dialog = {
  id : string;
  button_id : string;
  x : int;
  y : int;
  text : string;
  hovering : bool;
  hovering_button : bool;
}

let button_view d = St.render (button_style d.hovering_button) button_label

let window_view ~special d =
  let styled =
    St.render special d.text ^ St.render dialog_word_style " draws near. Command?"
  in
  St.render (dialog_style d.hovering) styled

let clamp n ~lo ~hi = if n < lo then lo else if n > hi then hi else n
let h_gap = 3
let v_gap = 1

let dialog_layer ~z ~special d =
  let window = window_view ~special d in
  let button = button_view d in
  let button_x = La.width window - La.width button - 1 - h_gap in
  let button_y = La.height window - La.height button - 1 - v_gap in
  Layer.add
    (Layer.v ~id:d.id ~x:d.x ~y:d.y ~z window)
    [ Layer.v ~id:d.button_id ~x:button_x ~y:button_y ~z button ]

let body_text dialogs =
  let n = Stdlib.List.length dialogs in
  let buffer = Buffer.create 96 in
  if n > 0 then Buffer.add_string buffer "Drag to move. ";
  if n = 0 && n < max_dialogs then Buffer.add_string buffer "Click to spawn."
  else if n >= 1 && n < max_dialogs then
    Buffer.add_string buffer (Fmt.str "Click to spawn up to %d more." (max_dialogs - n));
  Buffer.add_string buffer "\n\nPress q to quit.";
  Buffer.contents buffer

let layers ~width ~height ~special dialogs =
  let bg =
    La.place ~h:P.left ~v:P.top ~width ~height ~whitespace
      (St.render bg_text_style (body_text dialogs))
  in
  Layer.v ~id:"bg" bg
  :: Stdlib.List.mapi (fun index d -> dialog_layer ~z:(index + 1) ~special d) dialogs

let rec flatten layer ~x ~y acc =
  let x = x + Layer.x layer and y = y + Layer.y layer in
  Stdlib.List.fold_left
    (fun acc child -> flatten child ~x ~y acc)
    ((layer, x, y) :: acc) (Layer.children layer)

let paint grid ~x ~y content =
  let width = La.width content and height = La.height content in
  if width > 0 && height > 0 then
    Array.iteri
      (fun row line ->
        let target = y + row in
        if target >= 0 && target < Array.length grid then
          Array.iteri
            (fun column cell ->
              let place = x + column in
              if place >= 0 && place < Array.length grid.(target) then
                grid.(target).(place) <- cell)
            line)
      (Raster.layout ~width ~max_rows:height content)

let compose ~width ~height ~special dialogs =
  let grid = Array.init (max 0 height) (fun _ -> Array.make (max 0 width) Raster.blank) in
  let flat =
    Stdlib.List.concat_map
      (fun layer -> Stdlib.List.rev (flatten layer ~x:0 ~y:0 []))
      (layers ~width ~height ~special dialogs)
  in
  let ordered =
    Stdlib.List.stable_sort
      (fun (a, _, _) (b, _, _) -> compare (Layer.z a) (Layer.z b))
      flat
  in
  Stdlib.List.iter (fun (layer, x, y) -> paint grid ~x ~y (Layer.content layer)) ordered;
  Raster.to_string ~trim:true grid

type model = {
  width : int;
  height : int;
  dialogs : dialog list;
  mouse_down : bool;
  press_id : string;
  drag_id : string;
  drag_offset_x : int;
  drag_offset_y : int;
  special : St.t;
  adjectives_at : int;
  nouns_at : int;
  next_id : int;
}

type msg =
  | Key of Charamel_tea.Key.t
  | Win of { rows : int; cols : int }
  | Terminal of Charamel_tea.Event.t
  | Mouse_event of Mouse.t

let hit_id model ~x ~y =
  let layers =
    layers ~width:model.width ~height:model.height ~special:model.special model.dialogs
  in
  match C.hit (C.v layers) ~x ~y with Some hit -> hit.C.id | None -> ""

let new_dialog model ~x ~y =
  let blank =
    {
      id = "";
      button_id = "";
      x = 0;
      y = 0;
      text = "";
      hovering = false;
      hovering_button = false;
    }
  in
  let w, h = La.size (window_view ~special:model.special blank) in
  let adjectives_at = (model.adjectives_at + 1) mod Stdlib.Array.length adjectives in
  let nouns_at = (model.nouns_at + 1) mod Stdlib.Array.length nouns in
  let next_id = model.next_id + 1 in
  let dialog =
    {
      blank with
      x = clamp (x - (w / 2)) ~lo:0 ~hi:(model.width - w);
      y = clamp (y - (h / 2)) ~lo:0 ~hi:(model.height - h);
      text = word_at adjectives_at nouns_at;
      id = Fmt.str "dialog-%d" next_id;
      button_id = Fmt.str "button-%d" next_id;
    }
  in
  (dialog, { model with adjectives_at; nouns_at; next_id })

let drag_dialogs model mouse =
  if model.mouse_down && not (String.equal model.drag_id "") then
    Stdlib.List.map
      (fun d ->
        if String.equal d.id model.drag_id then begin
          let w, h = La.size (window_view ~special:model.special d) in
          {
            d with
            x = clamp (mouse.Mouse.x - model.drag_offset_x) ~lo:0 ~hi:(model.width - w);
            y = clamp (mouse.Mouse.y - model.drag_offset_y) ~lo:0 ~hi:(model.height - h);
          }
        end
        else d)
      model.dialogs
  else model.dialogs

let hover_dialogs dialogs ~id =
  Stdlib.List.map
    (fun d ->
      if String.equal d.button_id id then
        { d with hovering = true; hovering_button = true }
      else if String.equal d.id id then
        { d with hovering = true; hovering_button = false }
      else { d with hovering = false; hovering_button = false })
    dialogs

let start_press id mouse model =
  let pressed = { model with mouse_down = true; press_id = id } in
  match Stdlib.List.find_opt (fun d -> String.equal d.id id) pressed.dialogs with
  | None -> (pressed, Cmd.none)
  | Some d ->
      let dragging =
        {
          pressed with
          drag_id = id;
          drag_offset_x = mouse.Mouse.x - d.x;
          drag_offset_y = mouse.Mouse.y - d.y;
        }
      in
      if Stdlib.List.length dragging.dialogs < 2 then (dragging, Cmd.none)
      else
        let rest =
          Stdlib.List.filter
            (fun other -> not (String.equal other.id id))
            dragging.dialogs
        in
        ({ dragging with dialogs = rest @ [ d ] }, Cmd.none)

let finish_release id mouse model =
  if String.equal model.press_id "" then (model, Cmd.none)
  else if String.equal id "bg" && String.equal model.press_id "bg" then begin
    let settled = { model with mouse_down = false; drag_id = ""; press_id = "" } in
    if Stdlib.List.length settled.dialogs >= max_dialogs then (settled, Cmd.none)
    else begin
      let dialog, model = new_dialog settled ~x:mouse.Mouse.x ~y:mouse.Mouse.y in
      ({ model with dialogs = model.dialogs @ [ dialog ] }, Cmd.none)
    end
  end
  else
    let dialogs =
      Stdlib.List.filter
        (fun d ->
          not (String.equal id d.button_id && String.equal model.press_id d.button_id))
        model.dialogs
    in
    ({ model with dialogs; mouse_down = false; drag_id = ""; press_id = "" }, Cmd.none)

let layer_hit id mouse model =
  match mouse.Mouse.action with
  | Mouse.Press -> (
      match mouse.Mouse.button with
      | Mouse.Left when not model.mouse_down -> start_press id mouse model
      | _ -> (model, Cmd.none))
  | Mouse.Motion ->
      let dialogs = hover_dialogs (drag_dialogs model mouse) ~id in
      ({ model with dialogs }, Cmd.none)
  | Mouse.Release -> finish_release id mouse model

let update msg model =
  match msg with
  | Win { rows; cols } -> ({ model with width = cols; height = rows }, Cmd.none)
  | Terminal (Charamel_tea.Event.Background_color background) ->
      ( {
          model with
          special = special_word_style (Charamel_ansi.Color.is_dark background);
        },
        Cmd.none )
  | Terminal _ -> (model, Cmd.none)
  | Key key -> (
      match Charamel_tea.Key.to_string key with
      | "q" | "ctrl+c" | "escape" -> (model, Cmd.quit)
      | _ -> (model, Cmd.none))
  | Mouse_event mouse ->
      let id = hit_id model ~x:mouse.Mouse.x ~y:mouse.Mouse.y in
      if String.equal id "" then (model, Cmd.none) else layer_hit id mouse model

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( {
            width = 0;
            height = 0;
            dialogs = [];
            mouse_down = false;
            press_id = "";
            drag_id = "";
            drag_offset_x = 0;
            drag_offset_y = 0;
            special = St.empty;
            adjectives_at = 0;
            nouns_at = 0;
            next_id = 0;
          },
          Cmd.query `Background ));
    update;
    view =
      (fun model ->
        V.v ~alt_screen:true ~mouse:V.Mouse_all
          (compose ~width:model.width ~height:model.height ~special:model.special
             model.dialogs));
    subscriptions =
      (fun _ ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.mouse (fun mouse -> Mouse_event mouse);
            Sub.resize (fun ~rows ~cols -> Win { rows; cols });
            Sub.terminal (fun event -> Terminal event);
          ]);
  }

let main () = Smoke.run_ app

let no_mods : Charamel_tea.Key.mods =
  {
    shift = false;
    alt = false;
    ctrl = false;
    meta = false;
    super = false;
    hyper = false;
    caps_lock = false;
    num_lock = false;
  }

let mouse ~action x y = { Mouse.x; y; button = Mouse.Left; action; mods = no_mods }
let press = mouse ~action:Mouse.Press
let motion = mouse ~action:Mouse.Motion
let release = mouse ~action:Mouse.Release
let rows = 24
let columns = 80

let blank_dialog =
  {
    id = "";
    button_id = "";
    x = 0;
    y = 0;
    text = "";
    hovering = false;
    hovering_button = false;
  }

let dialog_w, dialog_h = La.size (window_view ~special:St.empty blank_dialog)
let button_w = La.width (button_view blank_dialog)
let button_h = La.height (button_view blank_dialog)
let spawn_x = 40
let spawn_y = 12
let spawned_at_x = clamp (spawn_x - (dialog_w / 2)) ~lo:0 ~hi:(columns - dialog_w)
let spawned_at_y = clamp (spawn_y - (dialog_h / 2)) ~lo:0 ~hi:(rows - dialog_h)

let spawned =
  {
    blank_dialog with
    id = "dialog-1";
    button_id = "button-1";
    x = spawned_at_x;
    y = spawned_at_y;
    text = "A Cute Banana";
  }

let grab_x = spawned_at_x + 8
let grab_y = spawned_at_y + 2
let drag_x = clamp (10 - (grab_x - spawned_at_x)) ~lo:0 ~hi:(columns - dialog_w)
let drag_y = clamp (5 - (grab_y - spawned_at_y)) ~lo:0 ~hi:(rows - dialog_h)
let dragged = { spawned with x = drag_x; y = drag_y }
let button_x = spawned_at_x + (dialog_w - button_w - 1 - h_gap)
let button_y = spawned_at_y + (dialog_h - button_h - 1 - v_gap)

let frame_at dialogs =
  Charamel_ansi.Text.strip (compose ~width:columns ~height:rows ~special:St.empty dialogs)

let row frame index =
  match Stdlib.List.nth_opt (String.split_on_char '\n' frame) index with
  | Some line -> line
  | None -> ""

let dragged_row = row (frame_at [ dragged ]) (drag_y + 2)

let smoke () =
  Smoke.expect app ~size:(rows, columns) [] [ "Click to spawn."; "Press q to quit." ]
  @ Smoke.expect app ~size:(rows, columns)
      [
        `Msg (Mouse_event (press spawn_x spawn_y));
        `Msg (Mouse_event (release spawn_x spawn_y));
      ]
      [
        "Drag to move. Click to spawn up to 998 more.";
        "A Cute Banana draws near.";
        "Run Away";
      ]
  @ Smoke.expect app ~size:(rows, columns)
      [
        `Msg (Mouse_event (press spawn_x spawn_y));
        `Msg (Mouse_event (release spawn_x spawn_y));
        `Msg (Mouse_event (press grab_x grab_y));
        `Msg (Mouse_event (motion 10 5));
        `Msg (Mouse_event (release 10 5));
      ]
      [ dragged_row; "A Cute Banana draws near." ]
  @ Smoke.expect app ~size:(rows, columns)
      [
        `Msg (Mouse_event (press spawn_x spawn_y));
        `Msg (Mouse_event (release spawn_x spawn_y));
        `Msg (Mouse_event (press (button_x + 2) button_y));
        `Msg (Mouse_event (release (button_x + 2) button_y));
      ]
      [ "Click to spawn."; "Press q to quit." ]
