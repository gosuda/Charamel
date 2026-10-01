module Color = Charamel_ansi.Color
module Textinput = Charamel_bubbles.Textinput

let styles =
  let default = Textinput.default_styles ~is_dark:true in
  {
    default with
    Charamel_bubbles.Textinput.cursor = { default.cursor with color = Color.Indexed 63 };
  }

type model = { user_input : Textinput.t }
type msg = Key of Charamel_tea.Key.t | Input_msg of Textinput.msg

let step msg model =
  let user_input, cmd = Textinput.update msg model.user_input in
  ({ user_input }, Charamel_tea.Cmd.map (fun msg -> Input_msg msg) cmd)

let is_quit key =
  match Charamel_tea.Key.to_string key with
  | "ctrl+c" | "escape" | "enter" -> true
  | _ -> false

let update msg model =
  match msg with
  | Key key -> (
      if is_quit key then (model, Charamel_tea.Cmd.quit)
      else
        match Textinput.key model.user_input key with
        | Some msg -> step msg model
        | None -> (model, Charamel_tea.Cmd.none))
  | Input_msg msg -> step msg model

let view model =
  Charamel_tea.View.v
    (Fmt.str "\nYou piped in: %s\n\nPress ^C to exit" (Textinput.view model.user_input))

let app value : (model, msg) Charamel_tea.app =
  let input =
    Textinput.v ~prompt:"" ~width:48 ~styles ~value () |> Textinput.cursor_end
  in
  {
    init =
      (fun () ->
        let user_input, cmd = Textinput.focus input in
        ({ user_input }, Charamel_tea.Cmd.map (fun msg -> Input_msg msg) cmd));
    update;
    view;
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map
              (fun msg -> Input_msg msg)
              (Textinput.subscriptions model.user_input);
          ]);
  }

let main () =
  let open Lwt.Syntax in
  if Charamel_os.Tty.is_tty_stdin then
    let* () = Lwt_io.write Lwt_io.stdout "Try piping in some text.\n" in
    Lwt.return (exit 1)
  else
    let* piped = Lwt_io.read Lwt_io.stdin in
    Smoke.run_ (app (String.trim piped))

let smoke () =
  let script events = Smoke.expect (app "the piped text") events in
  script [] [ "You piped in: the piped text"; "Press ^C to exit" ]
  @ script [ `Text " and more" ] [ "You piped in: the piped text and more" ]
  @ script [ Smoke.key "q" ] [ "You piped in: the piped textq" ]
