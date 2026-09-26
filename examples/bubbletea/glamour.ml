module Color = Charamel_ansi.Color
module Event = Charamel_tea.Event
module Glamour = Charamel_glamour
module Style = Charamel_lipgloss.Style
module Viewport = Charamel_bubbles.Viewport

let lines =
  [
    "";
    "# Today’s Menu";
    "";
    "## Appetizers";
    "";
    "| Name        | Price | Notes                           |";
    "| ---         | ---   | ---                             |";
    "| Tsukemono   | $2    | Just an appetizer               |";
    "| Tomato Soup | $4    | Made with San Marzano tomatoes  |";
    "| Okonomiyaki | $4    | Takes a few minutes to make     |";
    "| Curry       | $3    | We can add squash if you’d like |";
    "";
    "## Seasonal Dishes";
    "";
    "| Name                 | Price | Notes              |";
    "| ---                  | ---   | ---                |";
    "| Steamed bitter melon | $2    | Not so bitter      |";
    "| Takoyaki             | $3    | Fun to eat         |";
    "| Winter squash        | $3    | Today it's pumpkin |";
    "";
    "## Desserts";
    "";
    "| Name         | Price | Notes                 |";
    "| ---          | ---   | ---                   |";
    "| Dorayaki     | $4    | Looks good on rabbits |";
    "| Banana Split | $5    | A classic             |";
    "| Cream Puff   | $3    | Pretty creamy!        |";
    "";
    "All our dishes are made in-house by Karen, our chef. Most of our ingredients are \
     from our garden or the fish market down the street.";
    "";
    "Some famous people that have eaten here lately:";
    "";
    "* [x] René Redzepi";
    "* [x] David Chang";
    "* [ ] Jiro Ono (maybe some day)";
    "";
    "Bon appétit!";
    "";
  ]

let content = String.concat "\n" lines
let width = 78
let height = 20
let glamour_gutter = 3

let frame_style =
  Style.padding_side `Right 2
    (Style.border_foreground
       (Charamel_lipgloss.Sides_color.all (Color.Indexed 62))
       (Style.border Charamel_lipgloss.Border.rounded Style.empty))

let render_width = width - Style.get_horizontal_frame_size frame_style - glamour_gutter

let render is_dark =
  Glamour.render ~width:render_width ~theme:(Glamour.Theme.auto ~is_dark) content

let help_style = Style.foreground (Color.Indexed 241) Style.empty

type model = { viewport : Viewport.t; is_dark : bool }
type msg = Key of Charamel_tea.Key.t | Viewport_msg of Viewport.msg | Report of Event.t

let step msg model =
  let viewport, cmd = Viewport.update msg model.viewport in
  ({ model with viewport }, Charamel_tea.Cmd.map (fun msg -> Viewport_msg msg) cmd)

let is_quit key =
  match Charamel_tea.Key.to_string key with
  | "q" | "ctrl+c" | "escape" -> true
  | _ -> false

let recolour is_dark model =
  ( { is_dark; viewport = Viewport.set_content (render is_dark) model.viewport },
    Charamel_tea.Cmd.none )

let update msg model =
  match msg with
  | Key key -> (
      if is_quit key then (model, Charamel_tea.Cmd.quit)
      else
        match Viewport.key model.viewport key with
        | Some msg -> step msg model
        | None -> (model, Charamel_tea.Cmd.none))
  | Viewport_msg msg -> step msg model
  | Report (Event.Background_color color) ->
      let is_dark = Color.is_dark color in
      if Bool.equal is_dark model.is_dark then (model, Charamel_tea.Cmd.none)
      else recolour is_dark model
  | Report _ -> (model, Charamel_tea.Cmd.none)

let view model =
  Charamel_tea.View.v
    (Viewport.view model.viewport
    ^ Style.render help_style "\n  ↑/↓: Navigate • q: Quit\n")

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        ( {
            viewport =
              Viewport.set_content (render true)
                (Viewport.v ~width ~height ~style:frame_style ());
            is_dark = true;
          },
          Charamel_tea.Cmd.query `Background ));
    update;
    view;
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map
              (fun msg -> Viewport_msg msg)
              (Viewport.subscriptions model.viewport);
            Charamel_tea.Sub.terminal (fun event -> Report event);
          ]);
  }

let main () = Smoke.run_ app
let scrolled count = List.init count (fun _ -> Smoke.key "down")

let smoke () =
  Smoke.expect app [] [ "Today’s Menu"; "Appetizers"; "Tsukemono" ]
  @ Smoke.expect app (scrolled 20) [ "Desserts"; "Dorayaki" ]
