module Env = Charamel_cli.Env

type options = {
  path : string;
  cursor : string;
  all : bool;
  permissions : bool;
  size : bool;
  file : bool;
  directory : bool;
  show_help : bool;
  timeout : float option;
  header : string;
  height : int;
  padding : string;
  cursor_style : Gum_style.t;
  symlink_style : Gum_style.t;
  directory_style : Gum_style.t;
  file_style : Gum_style.t;
  permissions_style : Gum_style.t;
  selected_style : Gum_style.t;
  file_size_style : Gum_style.t;
  header_style : Gum_style.t;
}

let default_options =
  {
    path = ".";
    cursor = ">";
    all = false;
    permissions = true;
    size = true;
    file = true;
    directory = false;
    show_help = true;
    timeout = None;
    header = "";
    height = 10;
    padding = "0 0";
    cursor_style = Gum_style.defaults ~foreground:"212" ();
    symlink_style = Gum_style.defaults ~foreground:"36" ();
    directory_style = Gum_style.defaults ~foreground:"99" ();
    file_style = Gum_style.defaults ();
    permissions_style = Gum_style.defaults ~foreground:"244" ();
    selected_style = Gum_style.defaults ~foreground:"212" ~bold:true ();
    file_size_style = Gum_style.defaults ~foreground:"240" ~width:8 ~align:"right" ();
    header_style = Gum_style.defaults ~foreground:"99" ();
  }

type status = Running | Selected of string | Quit | Aborted
type model = { picker : Charamel_bubbles.Filepicker.t; status : status }

type msg =
  | Picker of Charamel_bubbles.Filepicker.msg
  | Key of Charamel_tea.Key.t
  | Resize of int

let help_line = "↑/↓ navigate • enter select • esc quit"

let apply_styles (options : options) picker =
  let defaults = Charamel_bubbles.Filepicker.default_styles in
  let styles =
    {
      defaults with
      cursor = Gum_style.to_style options.cursor_style;
      symlink = Gum_style.to_style options.symlink_style;
      directory = Gum_style.to_style options.directory_style;
      file = Gum_style.to_style options.file_style;
      permission = Gum_style.to_style options.permissions_style;
      selected = Gum_style.to_style options.selected_style;
      file_size = Gum_style.to_style options.file_size_style;
    }
  in
  Charamel_bubbles.Filepicker.set_styles styles picker

let app ~(env : Charamel_cli.Env.t) (options : options) ~padding ~directory =
  let picker =
    Charamel_bubbles.Filepicker.v ~root:env.Env.fs_root ~current_directory:directory
      ~height:options.height ~auto_height:(options.height = 0) ~cursor:options.cursor
      ~dir_allowed:options.directory ~file_allowed:options.file
      ~show_permissions:options.permissions ~show_size:options.size
      ~show_hidden:options.all ()
  in
  let picker = apply_styles options picker in
  let picker, init_cmd = Charamel_bubbles.Filepicker.init picker in
  let initial = { picker; status = Running } in
  let init_cmd = Charamel_tea.Cmd.map (fun message -> Picker message) init_cmd in
  let update message model =
    match message with
    | Key key when Gum_flag.is_abort key ->
        ({ model with status = Aborted }, Charamel_tea.Cmd.interrupt)
    | Key key when Gum_flag.is_quit key ->
        ({ model with status = Quit }, Charamel_tea.Cmd.quit)
    | Key key -> (
        match Charamel_bubbles.Filepicker.key model.picker key with
        | None -> (model, Charamel_tea.Cmd.none)
        | Some message -> (
            match Charamel_bubbles.Filepicker.did_select_file message model.picker with
            | Some path -> ({ model with status = Selected path }, Charamel_tea.Cmd.quit)
            | None ->
                let picker, command =
                  Charamel_bubbles.Filepicker.update message model.picker
                in
                ( { model with picker },
                  Charamel_tea.Cmd.map (fun message -> Picker message) command )))
    | Resize rows ->
        let picker, command =
          Charamel_bubbles.Filepicker.update (Charamel_bubbles.Filepicker.Resize rows)
            model.picker
        in
        ( { model with picker },
          Charamel_tea.Cmd.map (fun message -> Picker message) command )
    | Picker message -> (
        match Charamel_bubbles.Filepicker.did_select_file message model.picker with
        | Some path -> ({ model with status = Selected path }, Charamel_tea.Cmd.quit)
        | None ->
            let picker, command =
              Charamel_bubbles.Filepicker.update message model.picker
            in
            ( { model with picker },
              Charamel_tea.Cmd.map (fun message -> Picker message) command ))
  in
  let frame body =
    let parts =
      (if options.header = "" then []
       else
         [
           ( Gum_style.to_style options.header_style |> fun style ->
             Charamel_lipgloss.Style.render style options.header );
         ])
      @ [ body ]
    in
    let parts = if options.show_help then parts @ [ help_line ] else parts in
    let content = String.concat "\n" parts in
    Charamel_lipgloss.Style.render
      (Charamel_lipgloss.Style.padding padding Charamel_lipgloss.Style.empty)
      content
  in
  let cursor model =
    Gum_io.place_cursor ~frame (Charamel_bubbles.Filepicker.selection_cursor model.picker)
  in
  let view model =
    let view =
      Charamel_tea.View.v ~alt_screen:false
        (frame (Charamel_bubbles.Filepicker.view model.picker))
    in
    { view with cursor = cursor model }
  in
  let subscriptions _ =
    Charamel_tea.Sub.batch
      [
        Charamel_tea.Sub.key (fun key -> Key key);
        Charamel_tea.Sub.resize (fun ~rows ~cols:_ -> Resize rows);
      ]
  in
  ( { Charamel_tea.init = (fun () -> (initial, init_cmd)); update; view; subscriptions },
    () )

let absolute_directory path =
  let path =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
  in
  Lwt_preemptive.detach (fun () -> Unix.realpath path) ()

let is_directory path =
  Lwt.map
    (function Ok { Unix.st_kind = Unix.S_DIR; _ } -> true | Ok _ | Error _ -> false)
    (Charamel_os.Fs.stat path)

let run env (options : options) =
  if (not options.file) && not options.directory then
    Charamel_cli.error "at least one between --file and --directory must be set";
  Lwt.bind
    (Lwt.catch
       (fun () ->
         Lwt.map
           (fun path -> Ok path)
           (absolute_directory (if options.path = "" then "." else options.path)))
       (function
         | Unix.Unix_error (error, _, _) -> Lwt.return (Error error) | exn -> Lwt.fail exn))
    (function
      | Error error ->
          Charamel_cli.error (Fmt.str "file not found: %s" (Unix.error_message error))
      | Ok directory ->
          Lwt.bind (is_directory directory) (fun directory_ok ->
              if not directory_ok then
                Charamel_cli.error (Fmt.str "file not found: %s" directory);
              let padding =
                match Gum_flag.parse_padding options.padding with
                | Ok value -> value
                | Error (`Msg message) -> Charamel_cli.error message
              in
              let app, () = app ~env options ~padding ~directory in
              Lwt.bind
                (Gum_run.run_tui ~name:"file" ?timeout:options.timeout env app
                   ~finished:(fun model ->
                     match model.status with
                     | Selected _ -> Gum_run.Submitted
                     | Quit -> Gum_run.Quit
                     | Aborted -> Gum_run.Aborted
                     | Running -> Gum_run.Quit))
                (fun model ->
                  match model.status with
                  | Selected path -> Gum_io.print_raw env path
                  | Quit | Running -> Charamel_cli.error "no file selected"
                  | Aborted -> Charamel_cli.exit 130)))

let options path cursor all permissions size file directory show_help timeout header
    height padding cursor_style symlink_style directory_style file_style permissions_style
    selected_style file_size_style header_style =
  {
    path;
    cursor;
    all;
    permissions;
    size;
    file;
    directory;
    show_help;
    timeout;
    header;
    height;
    padding;
    cursor_style;
    symlink_style;
    directory_style;
    file_style;
    permissions_style;
    selected_style;
    file_size_style;
    header_style;
  }

let cmd env =
  let open Cmdliner in
  let bool cmd name ~default ~doc = Gum_flag.negatable ~cmd ~default ~doc name in
  let path_arg =
    Arg.(
      value (pos 0 (some string) None (info [] ~docv:"PATH" ~doc:"Directory to browse.")))
  in
  let path =
    let env_name = Cmdliner.Cmd.Env.info_var (Gum_flag.env ~cmd:"file" "path") in
    Term.(
      const (fun path ->
          Option.value path ~default:(Option.value (Sys.getenv_opt env_name) ~default:"."))
      $ path_arg)
  in
  let cursor =
    Arg.(
      value
        (opt string default_options.cursor
           (info [ "cursor"; "c" ]
              ~env:(Gum_flag.env ~cmd:"file" "cursor")
              ~doc:"Cursor marker.")))
  in
  let all =
    Gum_flag.flag ~cmd:"file" ~short:'a' ~default:false ~doc:"Show hidden entries." "all"
  in
  let permissions =
    Gum_flag.negatable ~cmd:"file" ~short:'p' ~env_name:"permission" ~default:true
      ~doc:"Show permissions." "permissions"
  in
  let size =
    Gum_flag.negatable ~cmd:"file" ~short:'s' ~env_name:"size" ~default:true
      ~doc:"Show file sizes." "size"
  in
  let typed_padding =
    Gum_flag.validated_padding_term ~doc:"Padding."
      ~pp:(fun formatter _ -> Stdlib.Format.pp_print_string formatter "")
      ~cmd:"file" ()
  in
  let file = bool "file" "file" ~default:true ~doc:"Allow file selection." in
  let directory =
    bool "file" "directory" ~default:false ~doc:"Allow directory selection."
  in
  let show_help = bool "file" "show-help" ~default:true ~doc:"Show help." in
  let timeout =
    Gum_flag.seconds ~cmd:"file" ~doc:"Abort after this duration." "timeout"
  in
  let header =
    Arg.(
      value
        (opt string ""
           (info [ "header" ]
              ~env:(Gum_flag.env ~cmd:"file" "header")
              ~doc:"Header text.")))
  in
  let height =
    Arg.(
      value
        (opt int default_options.height
           (info [ "height" ]
              ~env:(Gum_flag.env ~cmd:"file" "height")
              ~doc:"Maximum entries.")))
  in
  let cursor_style =
    Gum_style.term ~cmd:"file" ~prefix:"cursor" ~defaults:default_options.cursor_style ()
  in
  let symlink_style =
    Gum_style.term ~cmd:"file" ~prefix:"symlink" ~defaults:default_options.symlink_style
      ()
  in
  let directory_style =
    Gum_style.term ~cmd:"file" ~prefix:"directory"
      ~defaults:default_options.directory_style ()
  in
  let file_style =
    Gum_style.term ~cmd:"file" ~prefix:"file" ~defaults:default_options.file_style ()
  in
  let permissions_style =
    Gum_style.term ~cmd:"file" ~prefix:"permissions"
      ~defaults:default_options.permissions_style ()
  in
  let selected_style =
    Gum_style.term ~cmd:"file" ~prefix:"selected" ~defaults:default_options.selected_style
      ()
  in
  let file_size_style =
    Gum_style.term ~cmd:"file" ~prefix:"file-size"
      ~defaults:default_options.file_size_style ()
  in
  let header_style =
    Gum_style.term ~cmd:"file" ~prefix:"header" ~defaults:default_options.header_style ()
  in
  let action path cursor all permissions size file directory show_help timeout header
      height padding cursor_style symlink_style directory_style file_style
      permissions_style selected_style file_size_style header_style =
    run env
      (options path cursor all permissions size file directory show_help timeout header
         height padding cursor_style symlink_style directory_style file_style
         permissions_style selected_style file_size_style header_style)
  in
  let term =
    Term.(
      const action $ path $ cursor $ all $ permissions $ size $ file $ directory
      $ show_help $ timeout $ header $ height $ typed_padding $ cursor_style
      $ symlink_style $ directory_style $ file_style $ permissions_style $ selected_style
      $ file_size_style $ header_style)
  in
  Cmd.v (Cmd.info "file" ~doc:"Choose a file or directory.") term
