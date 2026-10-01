module Pager = Charamel_bubbles.Paginator
module Color = Charamel_ansi.Color
module Event = Charamel_tea.Event
module Key = Charamel_tea.Key
module Style = Charamel_lipgloss.Style
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View

type model = { paginator : Pager.t; items : string array }
type msg = Key of Key.t | Page of Pager.msg | Theme of Event.t

let dot color = Style.render Style.(empty |> foreground color) "•"

let retint ~is_dark paginator =
  let active =
    Charamel_lipgloss.light_dark ~is_dark ~light:(Color.Indexed 235)
      ~dark:(Color.Indexed 252)
  in
  let inactive =
    Charamel_lipgloss.light_dark ~is_dark ~light:(Color.Indexed 250)
      ~dark:(Color.Indexed 238)
  in
  paginator |> Pager.set_active_dot (dot active) |> Pager.set_inactive_dot (dot inactive)

let new_model () =
  let items = Array.init 100 (fun i -> Fmt.str "Item %d" (i + 1)) in
  let paginator = Pager.v ~kind:Dots ~per_page:10 ~total_pages:100 () in
  { items; paginator = retint ~is_dark:true paginator }

let page model msg =
  let paginator, cmd = Pager.update msg model.paginator in
  ({ model with paginator }, Cmd.map (fun msg -> Page msg) cmd)

let on_key key model =
  match Key.to_string key with
  | "q" | "escape" | "ctrl+c" -> (model, Cmd.quit)
  | _ -> (
      match Pager.key model.paginator key with
      | Some msg -> page model msg
      | None -> (model, Cmd.none))

let update msg model =
  match msg with
  | Page page_msg -> page model page_msg
  | Key key -> on_key key model
  | Theme (Background_color bg) ->
      ( {
          model with
          paginator =
            retint ~is_dark:(Charamel_lipgloss.Color_util.is_dark bg) model.paginator;
        },
        Cmd.none )
  | Theme _ -> (model, Cmd.none)

let view model =
  let start, stop =
    Pager.slice_bounds ~length:(Array.length model.items) model.paginator
  in
  let body =
    List.init (stop - start) (fun i -> "  • " ^ model.items.(start + i) ^ "\n\n")
    |> String.concat ""
  in
  View.v
    ("\n  Paginator Example\n\n" ^ body ^ "  " ^ Pager.view model.paginator
   ^ "\n\n  h/l ←/→ page • q: quit\n")

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (new_model (), Cmd.query `Background));
    update;
    view;
    subscriptions =
      (fun _ ->
        Sub.batch
          [ Sub.key (fun key -> Key key); Sub.terminal (fun event -> Theme event) ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app []
    [ "  Paginator Example"; "  • Item 1\n"; "  • Item 10\n"; "  h/l ←/→ page • q: quit" ]
  @ Smoke.expect app [ Smoke.key "l" ] [ "  • Item 11\n"; "  • Item 20\n" ]
  @ Smoke.expect app [ Smoke.key "l"; Smoke.key "h" ] [ "  • Item 1\n"; "  • Item 10\n" ]
