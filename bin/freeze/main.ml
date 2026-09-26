open Charamel_cli

let write_flow channel text = Lwt_io.write channel text

let read_line_opt channel =
  Lwt.catch
    (fun () -> Lwt.bind (Lwt_io.read_line channel) (fun line -> Lwt.return (Some line)))
    (function End_of_file -> Lwt.return None | exn -> Lwt.fail exn)

let prompt ~input ~output label current parse =
  Lwt.bind
    (write_flow output (Fmt.str "%s [%s]: " label current))
    (fun () ->
      Lwt.bind (read_line_opt input) (function
        | None -> Lwt.return current
        | Some value when String.trim value = "" -> Lwt.return current
        | Some value ->
            Lwt.return (Option.value (parse (String.trim value)) ~default:current)))

let interactive_config (env : Env.t) (config : Freeze_core.Config.t) =
  let input = env.Env.stdin in
  let output = env.Env.stderr in
  Lwt.bind
    (prompt ~input ~output "Background" config.Freeze_core.Config.background Option.some)
    (fun background ->
      Lwt.bind (prompt ~input ~output "Theme" config.Freeze_core.Config.theme Option.some)
        (fun theme ->
          Lwt.bind
            (prompt ~input ~output "Language" config.Freeze_core.Config.language
               Option.some) (fun language ->
              Lwt.bind
                (prompt ~input ~output "Output" config.Freeze_core.Config.output
                   Option.some) (fun output_path ->
                  Lwt.return
                    { config with background; theme; language; output = output_path }))))

let output_error (error : Freeze_core.Pty.error) =
  match error with
  | `Invalid_command message -> message
  | `Spawn message -> message
  | `Exit (code, output) ->
      if output = "" then Fmt.str "command exited with status %d" code
      else Fmt.str "command exited with status %d\n%s" code output
  | `Signaled (signal, output) ->
      if output = "" then Fmt.str "command terminated by signal %d" signal
      else Fmt.str "command terminated by signal %d\n%s" signal output
  | `Timeout output ->
      if output = "" then "command timed out" else Fmt.str "command timed out\n%s" output

let with_status (env : Env.t) path = write_flow env.Env.stdout (Fmt.str "WROTE %s\n" path)

let save_svg (env : Env.t) path svg =
  let resolved =
    if Filename.is_relative path then Filename.concat env.Env.cwd path else path
  in
  Lwt.catch
    (fun () ->
      Lwt.bind
        (Charamel_os.Fs.with_open_out ~perm:0o644 resolved (fun channel ->
             Lwt_io.write channel svg))
        (fun () -> Lwt.return (Ok ())))
    (function
      | Charamel_os.Fs.E _ ->
          Lwt.return (Error (Fmt.str "could not write output %s" path))
      | Unix.Unix_error (error, function_name, argument) ->
          Lwt.return
            (Error
               (Fmt.str "could not write output %s: %s (%s %s)" path
                  (Unix.error_message error) function_name argument))
      | exn -> Lwt.fail exn)

let default_output = "freeze.png"

let run_render (env : Env.t) (config : Freeze_core.Config.t) ~path ~raw =
  if raw = "" then Charamel_cli.error "No input"
  else
    let render_config : Freeze_core.Config.t =
      if config.Freeze_core.Config.output = "" then
        { config with output = default_output }
      else config
    in
    let ansi =
      Freeze_core.Input.is_ansi ~language:render_config.Freeze_core.Config.language raw
    in
    let language =
      Freeze_core.Input.language ~override:render_config.Freeze_core.Config.language ~path
    in
    if (not ansi) && Option.is_none language then
      Charamel_cli.error "Language Unknown: specify a language with the --language flag"
    else
      match
        Freeze_core.Svg.render ~fs_root:env.Env.cwd ~config:render_config ~language
          ~text:raw ~is_ansi:ansi
      with
      | Error message -> Charamel_cli.error message
      | Ok ({ svg; _ } : Freeze_core.Svg.rendered) -> (
          let output =
            if render_config.Freeze_core.Config.output <> "" then
              Some render_config.Freeze_core.Config.output
            else None
          in
          match output with
          | None -> write_flow env.Env.stdout svg
          | Some path when Filename.check_suffix path ".png" ->
              Lwt.bind (Freeze_core.Png.convert ~svg ~output:path) (function
                | Ok () -> with_status env path
                | Error message -> Charamel_cli.error message)
          | Some path when Filename.check_suffix path ".svg" ->
              Lwt.bind (save_svg env path svg) (function
                | Ok () -> with_status env path
                | Error message -> Charamel_cli.error message)
          | Some _ -> Charamel_cli.error "unsupported output format")

let run_execute (env : Env.t) (config : Freeze_core.Config.t) execute =
  Lwt.bind
    (Freeze_core.Pty.execute ~env:(Unix.environment ())
       ?width:
         (if config.Freeze_core.Config.width > 0. then
            Some (int_of_float config.Freeze_core.Config.width)
          else None)
       ?height:
         (if config.Freeze_core.Config.height > 0. then
            Some (int_of_float config.Freeze_core.Config.height)
          else None)
       ~timeout:config.Freeze_core.Config.execute_timeout execute)
    (function
      | Error (`Timeout output) ->
          Charamel_cli.error ~code:124 (output_error (`Timeout output))
      | Error error -> Charamel_cli.error (output_error error)
      | Ok output ->
          if output = "" then Charamel_cli.error "no command output"
          else
            let config = { config with language = "ansi" } in
            run_render env config ~path:None ~raw:output)

let run_from_source (env : Env.t) (config : Freeze_core.Config.t) =
  let source =
    if config.Freeze_core.Config.input = "" || config.Freeze_core.Config.input = "-" then
      Freeze_core.Input.Stdin
    else Freeze_core.Input.File config.Freeze_core.Config.input
  in
  Lwt.bind (Freeze_core.Input.read ~fs_root:env.Env.cwd ~stdin:env.Env.stdin source)
    (function
    | Error message -> Charamel_cli.error message
    | Ok ({ text; path } : Freeze_core.Input.loaded) ->
        run_render env config ~path ~raw:text)

let run (env : Env.t) (cli : Freeze_core.Config.cli) =
  Lwt.bind
    (Freeze_core.Config.load ~fs_root:env.Env.cwd ~name:cli.Freeze_core.Config.config)
    (function
    | Error message -> Charamel_cli.error message
    | Ok (base : Freeze_core.Config.t) ->
        let config = Freeze_core.Config.apply_cli base cli in
        let configured =
          if config.Freeze_core.Config.interactive then interactive_config env config
          else Lwt.return config
        in
        Lwt.bind configured (fun (config : Freeze_core.Config.t) ->
            let saved =
              if
                config.Freeze_core.Config.interactive
                && cli.Freeze_core.Config.config = "default"
              then
                Lwt.bind (Freeze_core.Config.save_user config) (function
                  | Ok () -> Lwt.return_unit
                  | Error message -> Charamel_cli.error message)
              else Lwt.return_unit
            in
            Lwt.bind saved (fun () ->
                let execute = String.trim config.Freeze_core.Config.execute in
                if execute <> "" then run_execute env config execute
                else run_from_source env config)))

let default (env : Env.t) =
  let action = run env in
  Cmdliner.Term.(const action $ Freeze_core.Config.cli_term)

let () =
  Charamel_cli.run ~name:"freeze" ~version:Charamel_cli.Version.current
    ~doc:"Generate an SVG or PNG image of source code and terminal output." ~default []
