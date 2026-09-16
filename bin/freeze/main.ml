let write_flow flow text = Eio.Flow.copy_string text flow

let root_for_path (env : Eio_unix.Stdenv.base) (path : string) : Eio.Fs.dir_ty Eio.Path.t
    =
  if Filename.is_relative path then env#cwd else env#fs

let path_for env path = Eio.Path.(root_for_path env path / path)

let prompt ~input ~output label current parse =
  write_flow output (Fmt.str "%s [%s]: " label current);
  match Eio.Buf_read.line input with
  | exception End_of_file -> current
  | value when String.trim value = "" -> current
  | value -> Option.value (parse (String.trim value)) ~default:current

let interactive_config (env : Eio_unix.Stdenv.base) (config : Freeze_core.Config.t) =
  let input = Eio.Buf_read.of_flow ~max_size:65_536 env#stdin in
  let output = env#stderr in
  let background = prompt ~input ~output "Background" config.background Option.some in
  let theme = prompt ~input ~output "Theme" config.theme Option.some in
  let language = prompt ~input ~output "Language" config.language Option.some in
  let output_path = prompt ~input ~output "Output" config.output Option.some in
  { config with background; theme; language; output = output_path }

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

let with_status (env : Eio_unix.Stdenv.base) path =
  write_flow env#stdout (Fmt.str "WROTE %s\n" path)

let save_svg (env : Eio_unix.Stdenv.base) path svg =
  try
    Eio.Cancel.protect (fun () ->
        Eio.Path.save ~create:(`Or_truncate 0o644) (path_for env path) svg);
    Ok ()
  with
  | Eio.Io _ as exception_ ->
      Error (Fmt.str "could not write output %s: %a" path Eio.Exn.pp exception_)
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (Fmt.str "could not write output %s: %s (%s %s)" path (Unix.error_message error)
           function_name argument)

let default_output = "freeze.png"

let run_render (env : Eio_unix.Stdenv.base) ~sw (config : Freeze_core.Config.t) ~path ~raw
    =
  if raw = "" then Charm_cli.error "No input"
  else
    let render_config : Freeze_core.Config.t =
      if config.output = "" then { config with output = default_output } else config
    in

    let ansi = Freeze_core.Input.is_ansi ~language:render_config.language raw in
    let language = Freeze_core.Input.language ~override:render_config.language ~path in
    if (not ansi) && Option.is_none language then
      Charm_cli.error "Language Unknown: specify a language with the --language flag"
    else
      let svg_fs =
        if render_config.font.file = "" then env#cwd
        else root_for_path env render_config.font.file
      in
      match
        Freeze_core.Svg.render ~fs:svg_fs ~config:render_config ~language ~text:raw
          ~is_ansi:ansi
      with
      | Error message -> Charm_cli.error message
      | Ok ({ svg; _ } : Freeze_core.Svg.rendered) -> (
          let output =
            if render_config.output <> "" then Some render_config.output else None
          in
          match output with
          | None -> write_flow env#stdout svg
          | Some path when Filename.check_suffix path ".png" -> (
              match
                Freeze_core.Png.convert ~sw ~process_mgr:env#process_mgr ~svg ~output:path
              with
              | Ok () -> with_status env path
              | Error message -> Charm_cli.error message)
          | Some path when Filename.check_suffix path ".svg" -> (
              match save_svg env path svg with
              | Ok () -> with_status env path
              | Error message -> Charm_cli.error message)
          | Some _ -> Charm_cli.error "unsupported output format")

let run (env : Eio_unix.Stdenv.base) (cli : Freeze_core.Config.cli) =
  let config_fs = if cli.config = "user" then env#fs else root_for_path env cli.config in
  match Freeze_core.Config.load ~fs:config_fs ~name:cli.config with
  | Error message -> Charm_cli.error message
  | Ok (base : Freeze_core.Config.t) ->
      let config : Freeze_core.Config.t = Freeze_core.Config.apply_cli base cli in
      let config : Freeze_core.Config.t =
        if config.interactive then interactive_config env config else config
      in
      (if config.interactive && cli.config = "default" then
         match Freeze_core.Config.save_user ~fs:env#fs config with
         | Ok () -> ()
         | Error message -> Charm_cli.error message);
      Eio.Switch.run (fun sw ->
          let execute = String.trim config.execute in
          if execute <> "" then
            match
              Freeze_core.Pty.execute ~sw ~clock:env#clock ~process_mgr:env#process_mgr
                ~env:(Unix.environment ())
                ?width:
                  (if config.width > 0. then Some (int_of_float config.width) else None)
                ?height:
                  (if config.height > 0. then Some (int_of_float config.height) else None)
                ~timeout:config.execute_timeout execute
            with
            | Error (`Timeout output) ->
                Charm_cli.error ~code:124 (output_error (`Timeout output))
            | Error error -> Charm_cli.error (output_error error)
            | Ok output ->
                if output = "" then Charm_cli.error "no command output"
                else
                  let config = { config with language = "ansi" } in
                  run_render env ~sw config ~path:None ~raw:output
          else
            let source =
              if config.input = "" || config.input = "-" then Freeze_core.Input.Stdin
              else Freeze_core.Input.File config.input
            in
            let input_fs =
              match source with
              | Freeze_core.Input.File path -> root_for_path env path
              | Freeze_core.Input.Stdin | Freeze_core.Input.Execute _ -> env#cwd
            in
            match Freeze_core.Input.read ~fs:input_fs ~stdin:env#stdin source with
            | Error message -> Charm_cli.error message
            | Ok ({ text; path } : Freeze_core.Input.loaded) ->
                run_render env ~sw config ~path ~raw:text)

let default (env : Eio_unix.Stdenv.base) =
  let action = run env in
  Cmdliner.Term.(const action $ Freeze_core.Config.cli_term)

let () =
  Charm_cli.run ~name:"freeze" ~version:Charm_cli.Version.current
    ~doc:"Generate an SVG or PNG image of source code and terminal output." ~default []
