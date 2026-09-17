open Result.Syntax

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

let is_tty flow =
  try Eio_unix.Fd.use_exn "isatty" (Eio_unix.Resource.fd flow) Unix.isatty
  with Eio.Io _ | Unix.Unix_error (_, _, _) -> false

let root_for_path env path = if Filename.is_relative path then env#cwd else env#fs
let path_for env path = Eio.Path.(root_for_path env path / path)

let config_read env path =
  try Some (Ok (Eio.Path.load (path_for env path))) with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None
  | Eio.Io _ as exception_ -> Some (Error (Fmt.str "%a" Eio.Exn.pp exception_))
  | Unix.Unix_error (error, operation, argument) ->
      Some (Error (Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument))
  | Sys_error message -> Some (Error message)

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

let resolved_config env options =
  let cwd = Eio.Path.native_exn env#cwd in
  match
    Config.load ~explicit:options.config ~cwd ~env:Sys.getenv_opt ~read:(config_read env)
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

let terminal_width _env =
  try
    let size : Eio_unix.Pty.winsize = Eio_unix.Pty.get_window_size Eio_unix.Fd.stdout in
    min 120 (max 1 size.Eio_unix.Pty.cols)
  with Unix.Unix_error _ | Eio.Io _ -> 80

let render_document ~env ~is_tty:stdout_is_tty (config : Config.t) document =
  let width =
    if config.Config.width > 0 then config.Config.width
    else if stdout_is_tty then terminal_width env
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

let executable program =
  if String.contains program '/' then program
  else
    let path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
    let rec find = function
      | [] -> program
      | directory :: rest -> (
          let candidate =
            Filename.concat (if directory = "" then "." else directory) program
          in
          try
            Unix.access candidate [ Unix.X_OK ];
            candidate
          with Unix.Unix_error _ -> find rest)
    in
    find (String.split_on_char ':' path)

let pager_words () =
  match Sys.getenv_opt "PAGER" with
  | Some value when String.trim value <> "" -> Ui.split_words value
  | _ -> [ "less"; "-r" ]

let run_pager text =
  let argv = pager_words () in
  match argv with
  | [] -> Error "glow: PAGER is empty"
  | program :: _ -> (
      try
        let status =
          Eio_unix.run_in_systhread (fun () ->
              let input_r, input_w = Unix.pipe ~cloexec:true () in
              let argv_array = Array.of_list argv in
              let pid =
                Unix.create_process (executable program) argv_array input_r Unix.stdout
                  Unix.stderr
              in
              Unix.close input_r;
              let channel = Unix.out_channel_of_descr input_w in
              output_string channel text;
              flush channel;
              close_out_noerr channel;
              snd (Unix.waitpid [] pid))
        in
        match status with
        | Unix.WEXITED 0 -> Ok ()
        | Unix.WEXITED code -> Error (Fmt.str "glow: pager exited with status %d" code)
        | Unix.WSIGNALED signal ->
            Error (Fmt.str "glow: pager terminated by signal %d" signal)
        | Unix.WSTOPPED signal ->
            Error (Fmt.str "glow: pager stopped by signal %d" signal)
      with
      | Unix.Unix_error (error, operation, argument) ->
          Error
            (Fmt.str "glow: unable to run pager: %s (%s %s)" (Unix.error_message error)
               operation argument)
      | Sys_error message -> Error (Fmt.str "glow: unable to run pager: %s" message))

let run_editor path =
  let command =
    match Sys.getenv_opt "EDITOR" with
    | Some value when String.trim value <> "" -> Ui.split_words value
    | _ -> [ "vi" ]
  in
  match command with
  | [] -> Error "glow: EDITOR is empty"
  | program :: _ -> (
      try
        let status =
          Eio_unix.run_in_systhread (fun () ->
              let argv = Array.of_list (command @ [ path ]) in
              let pid =
                Unix.create_process (executable program) argv Unix.stdin Unix.stdout
                  Unix.stderr
              in
              snd (Unix.waitpid [] pid))
        in
        match status with
        | Unix.WEXITED 0 -> Ok ()
        | Unix.WEXITED code -> Error (Fmt.str "glow: editor exited with status %d" code)
        | Unix.WSIGNALED signal ->
            Error (Fmt.str "glow: editor terminated by signal %d" signal)
        | Unix.WSTOPPED signal ->
            Error (Fmt.str "glow: editor stopped by signal %d" signal)
      with
      | Unix.Unix_error (error, operation, argument) ->
          Error
            (Fmt.str "glow: unable to run editor: %s (%s %s)" (Unix.error_message error)
               operation argument)
      | Sys_error message -> Error (Fmt.str "glow: unable to run editor: %s" message))

let write_config env options =
  match
    Config.config_path ~explicit:options.config ~cwd:(Eio.Path.native_exn env#cwd)
      ~env:Sys.getenv_opt
  with
  | None -> Error "glow: HOME or XDG_CONFIG_HOME is not set"
  | Some path -> (
      let directory = Filename.dirname path in
      try
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (path_for env directory);
        if not (Sys.file_exists path) then
          Eio.Cancel.protect (fun () ->
              Eio.Path.save ~create:(`Or_truncate 0o600) (path_for env path)
                Config.default_json);
        let* () = run_editor path in
        Ok (Fmt.str "Wrote config file to: %s" path)
      with
      | Eio.Io _ as exception_ ->
          Error (Fmt.str "glow: unable to write config: %a" Eio.Exn.pp exception_)
      | Unix.Unix_error (error, operation, argument) ->
          Error
            (Fmt.str "glow: unable to write config: %s (%s %s)" (Unix.error_message error)
               operation argument))

let run_config env options =
  match write_config env options with
  | Ok message -> Eio.Flow.copy_string (message ^ "\n") env#stdout
  | Error message -> Charamel_cli.error message

let run_default env options =
  match resolved_config env options with
  | Error message -> Charamel_cli.error message
  | Ok (config, _) -> (
      if config.Config.pager && config.Config.tui then
        Charamel_cli.error "glow: cannot use both --pager and --tui";
      let stdout_is_tty = is_tty env#stdout in
      let stdin_is_tty = is_tty env#stdin in
      let cwd = Eio.Path.native_exn env#cwd in
      match Source.classify ~argument:options.source ~cwd ~stdin_is_tty with
      | Error error -> Charamel_cli.error (source_error error)
      | Ok location -> (
          let ui_required =
            config.Config.tui
            || match location with Source.Directory _ -> true | _ -> false
          in
          if ui_required then
            if not stdout_is_tty then Charamel_cli.error "glow: --tui needs a terminal"
            else
              match Ui.run env ~config ~location with
              | Ok () -> ()
              | Error message -> Charamel_cli.error (Fmt.str "glow: %s" message)
          else
            match Source.read ~env ~clock:env#clock ~net:env#net location with
            | Error error -> Charamel_cli.error (source_error error)
            | Ok document ->
                let output = render_document ~env ~is_tty:stdout_is_tty config document in
                if config.Config.pager then
                  match run_pager output with
                  | Ok () -> ()
                  | Error message -> Charamel_cli.error message
                else Eio.Flow.copy_string output env#stdout))

let default env =
  let action = run_default env in
  Cmdliner.Term.(const action $ options_term)

let config_command env =
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
