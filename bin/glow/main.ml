open Charamel_cli

type options = {
  source : string option;
  config : string option;
  style : string option;
  width : int option;
  pager : bool option;
  tui : bool option;
  all : bool option;
  line_numbers : bool option;
  preserve_new_lines : bool option;
  mouse : bool option;
}

let path_for ~cwd path =
  if Filename.is_relative path then Filename.concat cwd path else path

let read_file_sync path =
  let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
    (fun () ->
      let length = (Unix.fstat fd).Unix.st_size in
      let bytes = Bytes.create length in
      let rec read_all offset =
        if offset >= length then ()
        else
          let count = Unix.read fd bytes offset (length - offset) in
          if count = 0 then () else read_all (offset + count)
      in
      read_all 0;
      Bytes.unsafe_to_string bytes)

let config_read ~cwd path =
  let resolved = path_for ~cwd path in
  try Some (Ok (read_file_sync resolved)) with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> None
  | Unix.Unix_error (error, operation, argument) ->
      Some (Error (Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument))

let config_error = function
  | `Io (path, message) -> Fmt.str "glow: unable to read config %s: %s" path message
  | `Json (path, message) -> Fmt.str "glow: invalid JSON in %s: %s" path message
  | `Env (path, message) -> Fmt.str "glow: invalid environment in %s: %s" path message

let bool_term ~name ~short =
  let positive_names =
    match short with None -> [ name ] | Some value -> [ String.make 1 value; name ]
  in
  let positive =
    Cmdliner.Arg.info positive_names ~doc:(Fmt.str "Enable %s." name)
      ~env:
        (Cmdliner.Cmd.Env.info
           ("GLOW_"
           ^ String.uppercase_ascii
               (String.map (fun c -> if c = '-' then '_' else c) name)))
  in
  let negative = Cmdliner.Arg.info [ "no-" ^ name ] ~doc:(Fmt.str "Disable %s." name) in
  let values =
    Cmdliner.Arg.(value (vflag_all [] [ (true, positive); (false, negative) ]))
  in
  Cmdliner.Term.(const (function [] -> None | value :: _ -> Some value) $ values)

let option_term names ~doc ?env converter =
  Cmdliner.Arg.(value (opt (some converter) None (info names ~doc ?env)))

let width_term =
  option_term [ "width"; "w" ] ~doc:"Word-wrap width. Zero uses the terminal width."
    ~env:(Cmdliner.Cmd.Env.info "GLOW_WIDTH")
    Cmdliner.Arg.int

let style_term =
  option_term [ "style"; "s" ]
    ~doc:"Theme: auto, dark, light, dracula, tokyo-night, pink, ascii, or notty."
    ~env:(Cmdliner.Cmd.Env.info "GLOW_STYLE")
    Cmdliner.Arg.string

let config_term =
  Cmdliner.Arg.(
    value (opt (some string) None (info [ "config" ] ~doc:"Configuration JSON path.")))

let options_term =
  let open Cmdliner in
  let open Term.Syntax in
  let+ source =
    Arg.(
      value
        (pos 0 (some string) None
           (info [] ~docv:"SOURCE" ~doc:"Markdown file, directory, URL, or - for stdin.")))
  and+ config = config_term
  and+ style = style_term
  and+ width = width_term
  and+ pager = bool_term ~name:"pager" ~short:(Some 'p')
  and+ tui = bool_term ~name:"tui" ~short:(Some 't')
  and+ all = bool_term ~name:"all" ~short:(Some 'a')
  and+ line_numbers = bool_term ~name:"line-numbers" ~short:(Some 'l')
  and+ preserve_new_lines = bool_term ~name:"preserve-new-lines" ~short:(Some 'n')
  and+ mouse = bool_term ~name:"mouse" ~short:(Some 'm') in
  {
    source;
    config;
    style;
    width;
    pager;
    tui;
    all;
    line_numbers;
    preserve_new_lines;
    mouse;
  }

let resolved_config (env : Env.t) options =
  match
    Config.load ~explicit:options.config ~cwd:env.Env.cwd ~env:Sys.getenv_opt
      ~read:(config_read ~cwd:env.Env.cwd)
  with
  | Error error -> Error (config_error error)
  | Ok (config, path) ->
      let config =
        Config.apply config
          {
            Config.style = options.style;
            width = options.width;
            pager = options.pager;
            tui = options.tui;
            all = options.all;
            line_numbers = options.line_numbers;
            preserve_new_lines = options.preserve_new_lines;
            mouse = options.mouse;
          }
      in
      Ok (config, path)

let source_error = function
  | `Invalid message -> Fmt.str "glow: %s" message
  | `Io (path, message) -> Fmt.str "glow: %s: %s" path message
  | `Http message -> Fmt.str "glow: %s" message

let terminal_width () =
  match Charamel_os.Tty.size_stdout () with
  | Some (_, cols) -> min 120 (max 1 cols)
  | None -> 80

let render_document ~is_tty:stdout_is_tty (config : Config.t) document =
  let width =
    if config.Config.width > 0 then config.Config.width
    else if stdout_is_tty then terminal_width ()
    else 80
  in
  let body =
    if document.Source.markdown then Source.remove_frontmatter document.Source.content
    else "```\n" ^ document.Source.content ^ "\n```"
  in
  let is_dark = Charamel_cli.is_dark ~env:Sys.getenv_opt in
  let theme =
    match String.lowercase_ascii config.Config.style with
    | "dark" -> Charamel_glamour.Theme.dark
    | "light" -> Charamel_glamour.Theme.light
    | "dracula" -> Charamel_glamour.Theme.dracula
    | "tokyo-night" | "tokyo_night" -> Charamel_glamour.Theme.tokyo_night
    | "pink" -> Charamel_glamour.Theme.pink
    | "ascii" -> Charamel_glamour.Theme.ascii
    | "notty" -> Charamel_glamour.Theme.notty
    | "auto" -> Charamel_glamour.Theme.auto ~is_dark
    | value -> Fmt.failwith "glow: unknown style %S" value
  in
  Charamel_glamour.render ~width ~theme ?base_url:document.Source.base_url
    ~preserve_newlines:config.Config.preserve_new_lines body

let pager_words () =
  match Sys.getenv_opt "PAGER" with
  | Some value when String.trim value <> "" -> Charamel_os.Shell.split_words value
  | _ -> [ "less"; "-r" ]

let exit_message ~label code =
  if code > 128 then Fmt.str "glow: %s terminated by signal %d" label (code - 128)
  else Fmt.str "glow: %s exited with status %d" label code

let run_pager text =
  match pager_words () with
  | [] -> Lwt.return (Error "glow: PAGER is empty")
  | argv ->
      Lwt.catch
        (fun () ->
          let process = Charamel_os.Process.spawn ~stdin:`Pipe argv in
          let stdin_w = Charamel_os.Process.stdin_w process in
          Lwt.bind (Lwt_io.write stdin_w text) (fun () ->
              Lwt.bind (Lwt_io.close stdin_w) (fun () ->
                  Lwt.bind (Charamel_os.Process.await process) (fun code ->
                      if code = 0 then Lwt.return (Ok ())
                      else Lwt.return (Error (exit_message ~label:"pager" code))))))
        (function
          | Unix.Unix_error (error, operation, argument) ->
              Lwt.return
                (Error
                   (Fmt.str "glow: unable to run pager: %s (%s %s)"
                      (Unix.error_message error) operation argument))
          | exn -> Lwt.fail exn)

let run_editor path =
  let command =
    match Sys.getenv_opt "EDITOR" with
    | Some value when String.trim value <> "" -> Charamel_os.Shell.split_words value
    | _ -> [ "vi" ]
  in
  match command with
  | [] -> Lwt.return (Error "glow: EDITOR is empty")
  | command ->
      Lwt.catch
        (fun () ->
          let process = Charamel_os.Process.spawn (command @ [ path ]) in
          Lwt.bind (Charamel_os.Process.await process) (fun code ->
              if code = 0 then Lwt.return (Ok ())
              else Lwt.return (Error (exit_message ~label:"editor" code))))
        (function
          | Unix.Unix_error (error, operation, argument) ->
              Lwt.return
                (Error
                   (Fmt.str "glow: unable to run editor: %s (%s %s)"
                      (Unix.error_message error) operation argument))
          | exn -> Lwt.fail exn)

let write_config (env : Env.t) options =
  match
    Config.config_path ~explicit:options.config ~cwd:env.Env.cwd ~env:Sys.getenv_opt
  with
  | None -> Lwt.return (Error "glow: HOME or XDG_CONFIG_HOME is not set")
  | Some path ->
      let resolved = path_for ~cwd:env.Env.cwd path in
      let directory = Filename.dirname resolved in
      Lwt.catch
        (fun () ->
          Lwt.bind (Charamel_os.Fs.mkdir_p directory) (function
            | Error _ -> Lwt.return (Error "glow: unable to write config")
            | Ok () ->
                let write_default () =
                  if Sys.file_exists resolved then Lwt.return_unit
                  else
                    Charamel_os.Fs.with_open_out ~perm:0o600 resolved (fun channel ->
                        Lwt_io.write channel Config.default_json)
                in
                Lwt.bind (write_default ()) (fun () ->
                    Lwt.bind (run_editor resolved) (function
                      | Error _ as error -> Lwt.return error
                      | Ok () -> Lwt.return (Ok (Fmt.str "Wrote config file to: %s" path))))))
        (function
          | Charamel_os.Fs.E _ -> Lwt.return (Error "glow: unable to write config")
          | Unix.Unix_error (error, operation, argument) ->
              Lwt.return
                (Error
                   (Fmt.str "glow: unable to write config: %s (%s %s)"
                      (Unix.error_message error) operation argument))
          | exn -> Lwt.fail exn)

let run_config (env : Env.t) options =
  Lwt.bind (write_config env options) (function
    | Ok message -> Lwt_io.write env.Env.stdout (message ^ "\n")
    | Error message -> Charamel_cli.error message)

let run_default (env : Env.t) options =
  match resolved_config env options with
  | Error message -> Charamel_cli.error message
  | Ok (config, _) -> (
      if config.Config.pager && config.Config.tui then
        Charamel_cli.error "glow: cannot use both --pager and --tui";
      let stdout_is_tty = Charamel_os.Tty.is_tty_stdout in
      let stdin_is_tty = Charamel_os.Tty.is_tty_stdin in
      let cwd = env.Env.cwd in
      match Source.classify ~argument:options.source ~cwd ~stdin_is_tty with
      | Error error -> Charamel_cli.error (source_error error)
      | Ok location ->
          let ui_required =
            config.Config.tui
            || match location with Source.Directory _ -> true | _ -> false
          in
          if ui_required then
            if not stdout_is_tty then Charamel_cli.error "glow: --tui needs a terminal"
            else
              Lwt.bind (Ui.run env ~config ~location) (function
                | Ok () -> Lwt.return_unit
                | Error message -> Charamel_cli.error (Fmt.str "glow: %s" message))
          else
            Lwt.bind (Source.read ~cwd:env.Env.cwd ~stdin:env.Env.stdin location)
              (function
              | Error error -> Charamel_cli.error (source_error error)
              | Ok document ->
                  let output = render_document ~is_tty:stdout_is_tty config document in
                  if config.Config.pager then
                    Lwt.bind (run_pager output) (function
                      | Ok () -> Lwt.return_unit
                      | Error message -> Charamel_cli.error message)
                  else Lwt_io.write env.Env.stdout output))

let default env =
  let action = run_default env in
  Cmdliner.Term.(const action $ options_term)

let config_command (env : Env.t) =
  let open Cmdliner in
  let config_arg =
    Arg.(
      value (opt (some string) None (info [ "config" ] ~doc:"Configuration JSON path.")))
  in
  let action path =
    run_config env
      ({
         source = None;
         config = path;
         style = None;
         width = None;
         pager = None;
         tui = None;
         all = None;
         line_numbers = None;
         preserve_new_lines = None;
         mouse = None;
       }
        : options)
  in
  let term = Term.(const action $ config_arg) in
  Cmd.v (Cmd.info "config" ~doc:"Create and open the Glow JSON configuration.") term

let () =
  Charamel_cli.run ~name:"glow" ~version:Charamel_cli.Version.current
    ~doc:"Render Markdown on the command line, with a terminal browser and pager."
    ~default [ config_command ]
