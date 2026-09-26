module Cmd = Charamel_tea.Cmd
module Color = Charamel_ansi.Color
module Key = Charamel_tea.Key
module Progress = Charamel_bubbles.Progress
module Sides = Charamel_lipgloss.Sides
module Spin = Charamel_bubbles.Spinner
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub
module Text = Charamel_ansi.Text
module View = Charamel_tea.View

let package_names =
  [
    "vegeutils";
    "libgardening";
    "currykit";
    "spicerack";
    "fullenglish";
    "eggy";
    "bad-kitty";
    "chai";
    "hojicha";
    "libtacos";
    "babys-monads";
    "libpurring";
    "currywurst-devel";
    "xmodmeow";
    "licorice-utils";
    "cashew-apple";
    "rock-lobster";
    "standmixer";
    "coffee-CUPS";
    "libesszet";
    "zeichenorientierte-benutzerschnittstellen";
    "schnurrkit";
    "old-socks-devel";
    "jalapeño";
    "molasses-utils";
    "xkohlrabi";
    "party-gherkin";
    "snow-peas";
    "libyuzu";
  ]

let packages =
  List.mapi
    (fun i name -> Fmt.str "%s-%d.%d.%d" name (i mod 10) (i * 3 mod 10) (i * 7 mod 10))
    package_names

let install_delay = 0.25
let current_pkg_name_style = Style.(empty |> foreground (Color.Indexed 211))
let done_style = Style.(empty |> margin (Sides.xy ~x:2 ~y:1))
let check_mark = Style.render Style.(empty |> foreground (Color.Indexed 42)) "✓"
let spinner_style = Style.(empty |> foreground (Color.Indexed 63))

type model = {
  index : int;
  width : int;
  spinner : Spin.t;
  progress : Progress.t;
  finished : bool;
}

type msg =
  | Key of Key.t
  | Installed of string
  | Spin of Spin.msg
  | Frame of Progress.msg
  | Win of { cols : int }

let download_and_install pkg = Cmd.after install_delay (fun () -> Installed pkg)

let new_model () =
  {
    index = 0;
    width = 0;
    spinner = Spin.v ~style:spinner_style ();
    progress = Progress.v ~width:40 ~show_percentage:false ();
    finished = false;
  }

let pad_left width text = String.make (max 0 (width - String.length text)) ' ' ^ text

let on_installed pkg model =
  let count = List.length packages in
  let message = Cmd.print (check_mark ^ " " ^ pkg) in
  if model.index >= count - 1 then
    ({ model with finished = true }, Cmd.seq [ message; Cmd.quit ])
  else
    let index = model.index + 1 in
    let progress = Progress.set_percent (float index /. float count) model.progress in
    ( { model with index; progress },
      Cmd.batch [ message; download_and_install (List.nth packages index) ] )

let on_key key model =
  match Key.to_string key with
  | "ctrl+c" | "escape" | "q" -> (model, Cmd.quit)
  | _ -> (model, Cmd.none)

let view model =
  let count = List.length packages in
  if model.finished then
    View.v (Style.render done_style (Fmt.str "Done! Installed %d packages.\n" count))
  else
    let digits = Text.width (string_of_int count) in
    let pkg_count =
      " "
      ^ pad_left digits (string_of_int model.index)
      ^ "/"
      ^ pad_left digits (string_of_int count)
    in
    let spin = Spin.view model.spinner ^ " " in
    let prog = Progress.view model.progress in
    let cells_avail = max 0 (model.width - Text.width (spin ^ prog ^ pkg_count)) in
    let pkg_name = Style.render current_pkg_name_style (List.nth packages model.index) in
    let info =
      Style.render Style.(empty |> max_width cells_avail) ("Installing " ^ pkg_name)
    in
    let cells_remaining =
      max 0 (model.width - Text.width (spin ^ info ^ prog ^ pkg_count))
    in
    View.v (spin ^ info ^ String.make cells_remaining ' ' ^ prog ^ pkg_count)

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (new_model (), download_and_install (List.hd packages)));
    update =
      (fun msg model ->
        match msg with
        | Key key -> on_key key model
        | Installed pkg -> on_installed pkg model
        | Spin spin_msg ->
            let spinner, cmd = Spin.update spin_msg model.spinner in
            ({ model with spinner }, Cmd.map (fun msg -> Spin msg) cmd)
        | Frame frame_msg ->
            let progress, cmd = Progress.update frame_msg model.progress in
            ({ model with progress }, Cmd.map (fun msg -> Frame msg) cmd)
        | Win { cols } -> ({ model with width = cols }, Cmd.none));
    view;
    subscriptions =
      (fun model ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.resize (fun ~rows:_ ~cols -> Win { cols });
            Sub.map (fun msg -> Spin msg) (Spin.subscriptions model.spinner);
            Sub.map (fun msg -> Frame msg) (Progress.subscriptions model.progress);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [ Smoke.key "q" ] [ "Installing vegeutils-0.0.0"; "  0/29"; "░" ]
  @ Smoke.expect app
      [ `Wait 0.3; Smoke.key "q" ]
      [ "Installing libgardening-1.3.7"; "  1/29" ]
  @ Smoke.expect app
      [ `Wait 1.0; Smoke.key "q" ]
      [ "Installing spicerack-3.9.1"; "  3/29" ]
  @ Smoke.expect app [] [ "Done! Installed 29 packages." ]
