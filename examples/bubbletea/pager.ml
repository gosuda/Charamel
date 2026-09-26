module Color = Charamel_ansi.Color
module Cmd = Charamel_tea.Cmd
module Layout = Charamel_lipgloss.Layout
module Border = Charamel_lipgloss.Border
module Position = Charamel_lipgloss.Position
module Sides = Charamel_lipgloss.Sides
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module Text = Charamel_ansi.Text
module View = Charamel_tea.View
module Viewport = Charamel_bubbles.Viewport

let content =
  {|Glow
====

A casual introduction. 你好世界!

## Let’s talk about artichokes

The _artichoke_ is mentioned as a garden plant in the 8th century BC by Homer
**and** Hesiod. The naturally occurring variant of the artichoke, the cardoon,
which is native to the Mediterranean area, also has records of use as a food
among the ancient Greeks and Romans. Pliny the Elder mentioned growing of
_carduus_ in Carthage and Cordoba.

> He holds him with a skinny hand,
> ‘There was a ship,’ quoth he.
> ‘Hold off! unhand me, grey-beard loon!’
> An artichoke, dropt he.

--Samuel Taylor Coleridge, [The Rime of the Ancient Mariner][rime]

[rime]: https://poetryfoundation.org/poems/43997/

## Other foods worth mentioning

1. Carrots
1. Celery
1. Tacos
    * Soft
    * Hard
1. Cucumber

## Things to eat today

* [x] Carrots
* [x] Ramen
* [ ] Currywurst

### Power levels of the aforementioned foods

| Name       | Power | Comment          |
| ---        | ---   | ---              |
| Carrots    | 9001  | It’s over 9000?! |
| Ramen      | 9002  | Also over 9000?! |
| Currywurst | 10000 | What?!           |

## Currying Artichokes

Here’s a bit of code in [Haskell](https://haskell.org), because we are fancy.
Remember that to compile Haskell you’ll need `ghc`.

```haskell
module Main where

import Data.Function ( (&) )
import Data.List ( intercalculate )

hello :: String -> String
hello s =
    "Hello, " ++ s ++ "."

main :: IO ()
main =
    map hello [ "artichoke", "alcachofa" ] & intercalculate "\n" & putStrLn
```

***

_Alcachofa_, if you were wondering, is artichoke in Spanish.|}

let dash n = String.concat "" (Stdlib.List.init n (fun _ -> "─"))
let pad = Sides.v ~top:0 ~right:1 ~bottom:0 ~left:1 ()

let title_style =
  Style.(empty |> border { Border.rounded with right = "├" } |> padding pad)

let info_style = Style.(empty |> border { Border.rounded with left = "┤" } |> padding pad)

let title_text = Style.render title_style "Mr. Pager"
let line text = Layout.height text
let chrome = line title_text + line (Style.render info_style "0%:0%")

let header viewport =
  let fill = dash (max 0 (Viewport.width viewport - Text.width title_text)) in
  Layout.join_horizontal ~pos:Position.center [ title_text; fill ]

let scroll_text viewport =
  let percent = truncate (Viewport.scroll_percent viewport *. 100.) in
  let horizontal = truncate (Viewport.horizontal_scroll_percent viewport *. 100.) in
  Fmt.str "%3d%%:%3d%%" percent horizontal

let footer viewport =
  let info = Style.render info_style (scroll_text viewport) in
  let fill = dash (max 0 (Viewport.width viewport - Text.width info)) in
  Layout.join_horizontal ~pos:Position.center [ fill; info ]

let gutter (context : Viewport.gutter_context) =
  if context.soft then "     │ "
  else if context.index >= context.total_lines then "   ~ │ "
  else Fmt.str "%4d │ " (context.index + 1)

let matches text pattern =
  let length = String.length pattern in
  let last = String.length text - length in
  let rec go position acc =
    if position > last then Stdlib.List.rev acc
    else if String.equal (String.sub text position length) pattern then
      go (position + length) ((position, position + length) :: acc)
    else go (position + 1) acc
  in
  go 0 []

let highlight_style =
  Style.(empty |> foreground (Color.Indexed 238) |> background (Color.Indexed 34))

let selected_style =
  Style.(empty |> foreground (Color.Indexed 238) |> background (Color.Indexed 47))

let new_viewport rows cols =
  let empty =
    Viewport.v ~width:cols
      ~height:(max 0 (rows - chrome))
      ~left_gutter:gutter ~mouse_wheel_enabled:true ~highlight_style
      ~selected_highlight_style:selected_style ()
  in
  let viewport = Viewport.set_content content empty in
  let ranges =
    Viewport.grapheme_ranges_of_byte_ranges viewport (matches content "artichoke")
  in
  viewport |> Viewport.set_highlights ranges |> Viewport.highlight_next

type model = { viewport : Viewport.t option }

type msg =
  | Key of Charamel_tea.Key.t
  | Mouse of Charamel_tea.Mouse.t
  | Win of int * int
  | Scrolled of Viewport.msg

let resize rows cols model =
  match model.viewport with
  | None -> ({ viewport = Some (new_viewport rows cols) }, Cmd.none)
  | Some viewport ->
      let resized = viewport |> Viewport.set_width cols |> Viewport.set_height rows in
      let reserved = line (header resized) + line (footer resized) in
      let viewport = Viewport.set_height (max 0 (rows - reserved)) resized in
      ({ viewport = Some viewport }, Cmd.none)

let scroll message model =
  match model.viewport with
  | None -> (model, Cmd.none)
  | Some viewport ->
      let viewport, cmd = Viewport.update message viewport in
      ({ viewport = Some viewport }, Cmd.map (fun message -> Scrolled message) cmd)

let on_key key model =
  match Charamel_tea.Key.to_string key with
  | "ctrl+c" | "q" | "escape" -> (model, Cmd.quit)
  | _ -> (
      match model.viewport with
      | None -> (model, Cmd.none)
      | Some viewport -> (
          match Viewport.key viewport key with
          | Some message -> scroll message model
          | None -> (model, Cmd.none)))

let on_mouse mouse model =
  match model.viewport with
  | None -> (model, Cmd.none)
  | Some viewport -> (
      match Viewport.mouse viewport mouse with
      | Some message -> scroll message model
      | None -> (model, Cmd.none))

let update msg model =
  match msg with
  | Win (rows, cols) -> resize rows cols model
  | Scrolled message -> scroll message model
  | Mouse mouse -> on_mouse mouse model
  | Key key -> on_key key model

let view model =
  match model.viewport with
  | None -> View.v ~alt_screen:true "\n  Initializing..."
  | Some viewport ->
      View.v ~alt_screen:true ~mouse:View.Mouse_motion
        (header viewport ^ "\n" ^ Viewport.view viewport ^ "\n" ^ footer viewport)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ viewport = None }, Cmd.none));
    update;
    view;
    subscriptions =
      (fun _ ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.mouse (fun mouse -> Mouse mouse);
            Sub.resize (fun ~rows ~cols -> Win (rows, cols));
          ]);
  }

let main () = Smoke.run_ app
let page_down = [ Smoke.key "f"; Smoke.key "f"; Smoke.key "f" ]

let smoke () =
  Smoke.expect app [] [ "Mr. Pager"; "Glow"; "  0%:  0%" ]
  @ Smoke.expect app [ Smoke.key "f" ] [ "Other foods worth mentioning"; " 36%:  0%" ]
  @ Smoke.expect app page_down [ "_Alcachofa_, if you were wondering"; "100%:  0%" ]
  @ Smoke.expect app [ Smoke.key "j" ] [ "  2 │ ===="; "   2%:  0%" ]
  @ Smoke.expect app ~size:(12, 40) [] [ " 8 │ The _artichoke_ is"; " 11%" ]
