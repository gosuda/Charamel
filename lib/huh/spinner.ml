type 'e error = [ `Interrupted | `Failed of 'e ]

type ('a, 'e) msg =
  | Tick of Charamel_bubbles.Spinner.msg
  | Done of ('a, 'e) result
  | Key of Charamel_tea.Key.t
  | Terminal of Charamel_tea.Event.t

type ('a, 'e) model = {
  spinner : Charamel_bubbles.Spinner.t;
  title : string;
  is_dark : bool;
  outcome : ('a, 'e) result option;
}

let color value =
  match Charamel_ansi.Color.of_hex value with
  | Some color -> color
  | None -> invalid_arg ("invalid Huh spinner color: " ^ value)

let default_theme ~is_dark =
  let spinner =
    Charamel_lipgloss.Style.empty |> Charamel_lipgloss.Style.foreground (color "#F780E2")
  in
  let title =
    Charamel_lipgloss.Style.empty
    |> Charamel_lipgloss.Style.foreground
         (Charamel_lipgloss.light_dark ~is_dark ~light:(color "#00020A")
            ~dark:(color "#FFFDF5"))
  in
  (spinner, title)

let is_tty base =
  let stdin_fd = Eio_unix.Resource.fd base#stdin in
  Eio_unix.Fd.use_exn "isatty" stdin_fd Unix.isatty

let term_is_dumb () = match Sys.getenv_opt "TERM" with Some "dumb" -> true | _ -> false

let run ?(title = "Loading...") ?style ?(accessible = false) ?theme ~clock action base =
  let terminal = Charamel_tea.Terminal.local ~output:`Stderr base in
  let forced_accessible = accessible || (not (is_tty base)) || term_is_dumb () in
  let theme = Option.value theme ~default:default_theme in
  if forced_accessible then begin
    Eio.Flow.copy_string (title ^ "\n") base#stdout;
    Result.map_error (fun error -> `Failed error) (action ())
  end
  else
    Eio.Switch.run (fun sw ->
        let promise, resolver = Eio.Promise.create () in
        Eio.Fiber.fork_daemon ~sw (fun () ->
            Eio.Promise.resolve resolver (action ());
            `Stop_daemon);
        let spinner_style, _ = theme ~is_dark:true in
        let spinner_kind = Option.value style ~default:Charamel_bubbles.Spinner.Dot in
        let spinner =
          Charamel_bubbles.Spinner.v ~kind:spinner_kind ~style:spinner_style ()
        in
        let init () =
          let model = { spinner; title; is_dark = true; outcome = None } in
          ( model,
            Charamel_tea.Cmd.batch
              [
                Charamel_tea.Cmd.query `Background;
                Charamel_tea.Cmd.perform (fun () -> Done (Eio.Promise.await promise));
              ] )
        in
        let update message model =
          match message with
          | Tick tick ->
              let spinner, command = Charamel_bubbles.Spinner.update tick model.spinner in
              ({ model with spinner }, Charamel_tea.Cmd.map (fun m -> Tick m) command)
          | Done outcome -> ({ model with outcome = Some outcome }, Charamel_tea.Cmd.quit)
          | Key key ->
              if
                key.Charamel_tea.Key.mods.Charamel_tea.Key.ctrl
                && Uchar.equal
                     (match key.Charamel_tea.Key.code with
                     | Charamel_tea.Key.Char c -> c
                     | _ -> Uchar.of_char '\000')
                     (Uchar.of_char 'c')
              then (model, Charamel_tea.Cmd.interrupt)
              else (model, Charamel_tea.Cmd.none)
          | Terminal (Charamel_tea.Event.Background_color background) ->
              let is_dark =
                match Charamel_ansi.Color.to_rgb background with
                | None -> true
                | Some (red, green, blue) ->
                    ((0.299 *. float red)
                    +. (0.587 *. float green)
                    +. (0.114 *. float blue))
                    /. 255.
                    < 0.5
              in
              let spinner_style, _ = theme ~is_dark in
              ( {
                  model with
                  is_dark;
                  spinner = Charamel_bubbles.Spinner.set_style spinner_style model.spinner;
                },
                Charamel_tea.Cmd.none )
          | Terminal _ -> (model, Charamel_tea.Cmd.none)
        in
        let view model =
          let _, title_style = theme ~is_dark:model.is_dark in
          Charamel_tea.View.v ~alt_screen:false
            (Charamel_bubbles.Spinner.view model.spinner
            ^ " "
            ^ Charamel_lipgloss.Style.render title_style model.title)
        in
        let subscriptions model =
          Charamel_tea.Sub.batch
            [
              Charamel_tea.Sub.map
                (fun tick -> Tick tick)
                (Charamel_bubbles.Spinner.subscriptions model.spinner);
              Charamel_tea.Sub.key (fun key -> Key key);
              Charamel_tea.Sub.terminal (fun event -> Terminal event);
            ]
        in
        let app = { Charamel_tea.init; update; view; subscriptions } in
        match Charamel_tea.run ~terminal ~clock app base with
        | Ok model -> (
            match model.outcome with
            | Some (Ok value) -> Ok value
            | Some (Error error) -> Error (`Failed error)
            | None -> Error `Interrupted)
        | Error `Interrupted | Error `Killed -> Error `Interrupted
        | Error (`Exn (exception_, backtrace)) ->
            Printexc.raise_with_backtrace exception_ backtrace)
