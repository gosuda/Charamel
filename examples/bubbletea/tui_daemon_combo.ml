module Spinner = Charamel_bubbles.Spinner
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color

type result = { duration : float; emoji : string }
type model = { spinner : Spinner.t; results : result list; quitting : bool }
type msg = Key of Charamel_tea.Key.t | Spin of Spinner.msg | Finished of float

let help = Style.foreground (Color.Indexed 241) Style.empty
let main_style = Style.margin (Charamel_lipgloss.Sides.v ~left:1 ()) Style.empty
let blank = { duration = 0.0; emoji = "" }
let dots = "........................"

let emojis =
  [|
    "🍦";
    "🧋";
    "🍡";
    "🤠";
    "👾";
    "😭";
    "🦊";
    "🐯";
    "🦆";
    "🥨";
    "🎏";
    "🍔";
    "🍒";
    "🍥";
    "🎮";
    "📦";
    "🦁";
    "🐶";
    "😸";
    "🍕";
    "🥐";
    "🧲";
    "🚒";
    "🥇";
    "🏆";
    "🌽";
  |]

let random_emoji () = emojis.(Random.int (Array.length emojis))

let trim_zeros text =
  let last = ref (String.length text - 1) in
  while !last > 0 && text.[!last] = '0' do
    decr last
  done;
  if text.[!last] = '.' then String.sub text 0 !last else String.sub text 0 (!last + 1)

let show_duration seconds =
  let ms = int_of_float (Float.round (seconds *. 1000.0)) in
  if ms < 1000 then Fmt.str "%dms" ms
  else Fmt.str "%ss" (trim_zeros (Fmt.str "%.3f" (float_of_int ms /. 1000.0)))

let row_view row =
  if row.duration <= 0.0 then dots
  else Fmt.str "%s Job finished in %s" row.emoji (show_duration row.duration)

let pretend_process () =
  let seconds = float_of_int (Random.int 900 + 100) /. 1000.0 in
  Charamel_tea.Cmd.after seconds (fun () -> Finished seconds)

let app ~log : (model, msg) Charamel_tea.app =
  let update msg model =
    match msg with
    | Key _ -> ({ model with quitting = true }, Charamel_tea.Cmd.quit)
    | Spin spin_msg ->
        let spinner, cmd = Spinner.update spin_msg model.spinner in
        ({ model with spinner }, Charamel_tea.Cmd.map (fun msg -> Spin msg) cmd)
    | Finished duration ->
        let row = { duration; emoji = random_emoji () } in
        let older = match model.results with _ :: rest -> rest | [] -> [] in
        ( { model with results = older @ [ row ] },
          Charamel_tea.Cmd.batch [ log (row_view row); pretend_process () ] )
  in
  {
    init =
      (fun () ->
        ( {
            spinner =
              Spinner.v ~style:(Style.foreground (Color.Indexed 206) Style.empty) ();
            results = List.init 5 (fun _ -> blank);
            quitting = false;
          },
          Charamel_tea.Cmd.batch [ log "Starting work..."; pretend_process () ] ));
    update;
    view =
      (fun model ->
        let content =
          List.fold_left
            (fun acc row -> acc ^ row_view row ^ "\n")
            (Fmt.str "\n%s Doing some work...\n\n" (Spinner.view model.spinner))
            model.results
        in
        let content = content ^ Style.render help "\nPress any key to exit\n" in
        let content = if model.quitting then content ^ "\n" else content in
        Charamel_tea.View.v (Style.render main_style content));
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map
              (fun msg -> Spin msg)
              (Spinner.subscriptions model.spinner);
          ]);
  }

let tui = app ~log:(fun _message -> Charamel_tea.Cmd.none)

let daemon_mode () =
  List.mem "-d" (Array.to_list Sys.argv) || not (Unix.isatty Unix.stdout)

let main () =
  if daemon_mode () then
    Lwt.map ignore (Smoke.run ~renderer:`None (app ~log:Charamel_tea.Cmd.print))
  else Lwt.map ignore (Smoke.run ~renderer:`Terminal tui)

let smoke () =
  Smoke.expect tui
    [ Smoke.key "q" ]
    [ "Doing some work..."; dots; "Press any key to exit" ]
  @ Smoke.expect tui [ `Msg (Finished 1.5); Smoke.key "q" ] [ "Job finished in 1.5s" ]
  @ Smoke.expect tui
      [ `Msg (Finished 0.25); `Msg (Finished 1.0); Smoke.key "q" ]
      [ "Job finished in 250ms"; "Job finished in 1s" ]
