module Cmd = Charamel_tea.Cmd
module Env = Charamel_cli.Env
module Sub = Charamel_tea.Sub
module Key = Charamel_tea.Key
module View = Charamel_tea.View
module Viewport = Charamel_bubbles.Viewport
module Textinput = Charamel_bubbles.Textinput
module List_view = Charamel_bubbles.List
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color
module Layout = Charamel_lipgloss.Layout

let error_text = function
  | `Invalid message -> message
  | `Io (path, message) -> Fmt.str "%s: %s" path message
  | `Http message -> message

let key_name key = Key.to_string key

let theme ~is_dark style =
  match Charamel_glamour.Theme.of_name ~is_dark style with
  | Ok theme -> theme
  | Error value -> Fmt.failwith "glow: unknown style %S" value

let line_prefix_style = Style.foreground (Color.Indexed 244) Style.empty
let search_style = Style.foreground (Color.Indexed 212) (Style.bold true Style.empty)

let search_styles =
  let base = Textinput.default_styles ~is_dark:true in
  {
    base with
    Textinput.focused = { base.Textinput.focused with prompt = search_style };
    blurred = { base.Textinput.blurred with prompt = search_style };
  }

let make_search width = Textinput.v ~prompt:"/" ~width ~styles:search_styles ()

let source_label document =
  match document.Source.path with None -> "stdin" | Some path -> path

type pager = {
  env : Charamel_cli.Env.t option;
  config : Config.t;
  document : Source.document;
  origin_directory : string option;
  viewport : Viewport.t;
  width : int;
  height : int;
  is_dark : bool;
  search_mode : bool;
  search : Textinput.t;
  matches : int list;
  match_index : int;
  show_help : bool;
  status : string option;
}

type browser = {
  env : Charamel_cli.Env.t;
  config : Config.t;
  root : string;
  listing : string List_view.t;
  width : int;
  height : int;
}

type model = Browser of browser | Pager of pager

type msg =
  | Key of Key.t
  | Mouse of Charamel_tea.Mouse.t
  | Resize of int * int
  | Browser_child of string List_view.msg
  | Pager_child of Viewport.msg
  | Search_child of Textinput.msg
  | Loaded of Source.document
  | Failed of string
  | Editor_done of int

let tty_size (_ : Charamel_cli.Env.t) =
  match Charamel_os.Tty.size_stdout () with
  | Some (rows, cols) -> (max 1 rows, max 1 cols)
  | None -> (80, 24)

let width_for env (config : Config.t) =
  if config.Config.width > 0 then config.Config.width
  else
    let _, columns = tty_size env in
    min 120 columns

let code_fence document =
  if document.Source.markdown then Source.remove_frontmatter document.Source.content
  else "```\n" ^ document.Source.content ^ "\n```"

let rendered_text ~is_dark ~width ~(config : Config.t) document =
  let theme = theme ~is_dark config.Config.style in
  Charamel_glamour.render ~width ~theme ?base_url:document.Source.base_url
    ~preserve_newlines:config.Config.preserve_new_lines (code_fence document)

let line_gutter =
  Some
    (fun ({ Viewport.index; _ } : Viewport.gutter_context) ->
      Style.render line_prefix_style (Fmt.str "%4d " (index + 1)))

let content_for (pager : pager) =
  rendered_text ~is_dark:pager.is_dark ~width:pager.width ~config:pager.config
    pager.document

let with_content (pager : pager) ?(keep_offset = true) () =
  let old_offset = Viewport.y_offset pager.viewport in
  let viewport =
    pager.viewport
    |> Viewport.set_width pager.width
    |> Viewport.set_height (max 1 (pager.height - 2))
    |> Viewport.set_left_gutter
         (if pager.config.Config.line_numbers then line_gutter else None)
    |> Viewport.set_content (content_for pager)
  in
  let viewport =
    if keep_offset then Viewport.set_y_offset old_offset viewport else viewport
  in
  { pager with viewport }

let make_pager env (config : Config.t) ?origin_directory document ~width ~height =
  let is_dark = Charamel_cli.is_dark ~env:Sys.getenv_opt in
  let pager =
    {
      env = Some env;
      config;
      document;
      origin_directory;
      viewport = Viewport.v ~width ~height:2 ();
      width;
      height;
      is_dark;
      search_mode = false;
      search = make_search width;
      matches = [];
      match_index = -1;
      show_help = false;
      status = None;
    }
  in
  with_content pager ~keep_offset:false ()

let relative root path =
  let prefix = if String.ends_with ~suffix:"/" root then root else root ^ "/" in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix) (String.length path - String.length prefix)
  else path

let make_browser env (config : Config.t) root ~width ~height =
  let files = Source.discover_markdown ~root ~show_hidden:config.Config.all in
  let delegate =
    List_view.default_delegate
      ~is_dark:(Charamel_cli.is_dark ~env:Sys.getenv_opt)
      ~title:Filename.basename
      ~description:(fun path -> relative root path)
      ()
  in
  let listing =
    List_view.v ~title:"Markdown files" ~width
      ~height:(max 4 (height - 2))
      ~is_dark:(Charamel_cli.is_dark ~env:Sys.getenv_opt)
      ~delegate ~filter_value:(relative root) files
  in
  { env; config; root; listing; width; height }

let load_result = function
  | Ok document -> Loaded document
  | Error error -> Failed (error_text error)

let load_command (env : Charamel_cli.Env.t) location =
  Cmd.await
    (Lwt.map load_result (Source.read ~cwd:env.Env.cwd ~stdin:env.Env.stdin location))

let refresh_browser browser =
  make_browser browser.env browser.config browser.root ~width:browser.width
    ~height:browser.height

let browser_update browser message =
  match message with
  | Key key
    when (key_name key = "q" || key_name key = "esc")
         && not (List_view.setting_filter browser.listing) ->
      (Browser browser, Cmd.quit)
  | Key key -> (
      match key_name key with
      | "r" when not (List_view.setting_filter browser.listing) ->
          (Browser (refresh_browser browser), Cmd.none)
      | "enter" when not (List_view.setting_filter browser.listing) -> (
          match List_view.selected_item browser.listing with
          | None -> (Browser browser, Cmd.none)
          | Some path -> (Browser browser, load_command browser.env (Source.File path)))
      | _ -> (
          match List_view.key browser.listing key with
          | None -> (Browser browser, Cmd.none)
          | Some child ->
              let listing, command = List_view.update child browser.listing in
              ( Browser { browser with listing },
                Cmd.map (fun value -> Browser_child value) command )))
  | Browser_child child ->
      let listing, command = List_view.update child browser.listing in
      ( Browser { browser with listing },
        Cmd.map (fun value -> Browser_child value) command )
  | Resize (rows, columns) ->
      let listing =
        List_view.set_size ~width:columns ~height:(max 4 (rows - 2)) browser.listing
      in
      (Browser { browser with listing; width = columns; height = rows }, Cmd.none)
  | Mouse _ -> (Browser browser, Cmd.none)
  | Loaded document ->
      ( Pager
          (make_pager browser.env browser.config ~origin_directory:browser.root document
             ~width:browser.width ~height:browser.height),
        Cmd.none )
  | Failed message ->
      ( Browser { browser with listing = List_view.set_title message browser.listing },
        Cmd.none )
  | Pager_child _ | Search_child _ | Editor_done _ -> (Browser browser, Cmd.none)

let search_lines query text =
  if query = "" then []
  else
    let needle = String.lowercase_ascii query in
    let rec contains_at line offset =
      if offset + String.length needle > String.length line then false
      else if String.sub line offset (String.length needle) = needle then true
      else contains_at line (offset + 1)
    in
    String.split_on_char '\n' text
    |> List.mapi (fun index line ->
        if contains_at (String.lowercase_ascii (Charamel_ansi.Text.strip line)) 0 then
          Some index
        else None)
    |> List.filter_map Fun.id

let goto_match pager index =
  match List.nth_opt pager.matches index with
  | None -> { pager with status = Some "No matches" }
  | Some line ->
      let viewport = Viewport.set_y_offset line pager.viewport in
      {
        pager with
        viewport;
        match_index = index;
        status = Some (Fmt.str "match %d/%d" (index + 1) (List.length pager.matches));
      }

let leave_search pager =
  let search = Textinput.blur pager.search in
  { pager with search; search_mode = false }

let update_search (pager : pager) (key : Key.t) =
  match key.Key.code with
  | Key.Escape -> (leave_search { pager with status = None }, Cmd.none)
  | Key.Enter ->
      let matches =
        search_lines (Textinput.value pager.search) (Viewport.content pager.viewport)
      in
      let left = leave_search pager in
      let pager = { left with matches; match_index = -1; status = None } in
      (goto_match pager 0, Cmd.none)
  | _ -> (
      match Textinput.key pager.search key with
      | None -> (pager, Cmd.none)
      | Some message ->
          let search, command = Textinput.update message pager.search in
          ({ pager with search }, Cmd.map (fun value -> Search_child value) command))

let reload_command (pager : pager) =
  Cmd.await
    (match (pager.env, pager.document.Source.path) with
    | None, _ -> Lwt.return (Failed "filesystem access is unavailable in scripted mode")
    | Some _, None -> Lwt.return (Failed "the current source has no local file")
    | Some env, Some path ->
        Lwt.map load_result
          (Source.read ~cwd:env.Charamel_cli.Env.cwd ~stdin:env.Charamel_cli.Env.stdin
             (Source.File path)))

let editor_command pager =
  match pager.document.Source.path with
  | None -> Cmd.msg (Failed "the current source has no local file")
  | Some path ->
      let argv = Charamel_os.Editor.editor () @ [ path ] in
      Cmd.seq [ Cmd.exec ~argv (fun code -> Editor_done code); reload_command pager ]

let pager_update (pager : pager) message =
  match message with
  | Key key when pager.search_mode -> (Pager (update_search pager key |> fst), Cmd.none)
  | Key key -> (
      match key_name key with
      | "q" | "esc" -> (
          match (pager.origin_directory, pager.env) with
          | Some root, Some env ->
              ( Browser
                  (make_browser env pager.config root ~width:pager.width
                     ~height:pager.height),
                Cmd.none )
          | _ -> (Pager pager, Cmd.quit))
      | "?" -> (Pager { pager with show_help = not pager.show_help }, Cmd.none)
      | "home" | "g" ->
          (Pager { pager with viewport = Viewport.goto_top pager.viewport }, Cmd.none)
      | "end" | "G" | "shift+g" ->
          (Pager { pager with viewport = Viewport.goto_bottom pager.viewport }, Cmd.none)
      | "/" ->
          let search, command = Textinput.focus (Textinput.set_value "" pager.search) in
          ( Pager
              {
                pager with
                search;
                search_mode = true;
                status = Some "/ search  enter accept  esc cancel";
              },
            Cmd.map (fun value -> Search_child value) command )
      | "n" when pager.matches <> [] ->
          ( Pager
              (goto_match pager ((pager.match_index + 1) mod List.length pager.matches)),
            Cmd.none )
      | ("N" | "shift+n") when pager.matches <> [] ->
          ( Pager
              (goto_match pager
                 ((pager.match_index - 1 + List.length pager.matches)
                 mod List.length pager.matches)),
            Cmd.none )
      | "l" ->
          ( Pager
              (with_content
                 {
                   pager with
                   config =
                     {
                       pager.config with
                       line_numbers = not pager.config.Config.line_numbers;
                     };
                 }
                 ()),
            Cmd.none )
      | "r" -> (Pager (with_content pager ~keep_offset:false ()), Cmd.none)
      | "e" -> (Pager pager, editor_command pager)
      | _ -> (
          match Viewport.key pager.viewport key with
          | None -> (Pager pager, Cmd.none)
          | Some child ->
              let viewport, command = Viewport.update child pager.viewport in
              ( Pager { pager with viewport },
                Cmd.map (fun value -> Pager_child value) command )))
  | Search_child child ->
      let search, command = Textinput.update child pager.search in
      (Pager { pager with search }, Cmd.map (fun value -> Search_child value) command)
  | Pager_child child ->
      let viewport, command = Viewport.update child pager.viewport in
      (Pager { pager with viewport }, Cmd.map (fun value -> Pager_child value) command)
  | Mouse mouse -> (
      match Viewport.mouse pager.viewport mouse with
      | None -> (Pager pager, Cmd.none)
      | Some child ->
          let viewport, command = Viewport.update child pager.viewport in
          (Pager { pager with viewport }, Cmd.map (fun value -> Pager_child value) command)
      )
  | Resize (rows, columns) ->
      let search = Textinput.set_width columns pager.search in
      ( Pager (with_content { pager with search; width = columns; height = rows } ()),
        Cmd.none )
  | Loaded document -> (
      match pager.env with
      | Some env ->
          ( Pager
              (make_pager env pager.config ?origin_directory:pager.origin_directory
                 document ~width:pager.width ~height:pager.height),
            Cmd.none )
      | None ->
          (Pager (with_content { pager with document } ~keep_offset:false ()), Cmd.none))
  | Failed message -> (Pager { pager with status = Some message }, Cmd.none)
  | Editor_done code ->
      ( Pager { pager with status = Some (Fmt.str "editor exited with %d" code) },
        reload_command pager )
  | Browser_child _ -> (Pager pager, Cmd.none)

let update message model =
  match model with
  | Browser browser -> browser_update browser message
  | Pager pager -> pager_update pager message

let status_style =
  Style.background (Color.Indexed 236) (Style.foreground (Color.Indexed 252) Style.empty)

let help_style = Style.foreground (Color.Indexed 244) Style.empty

let place_cursor ~above cursor =
  match cursor with
  | None -> None
  | Some (cursor : Charamel_tea.Cursor.t) -> Some { cursor with row = cursor.row + above }

let browser_view browser =
  let footer =
    Style.render help_style "↑/↓ select  enter open  / filter  r refresh  q quit"
  in
  let view =
    View.v ~alt_screen:true
      ~mouse:(if browser.config.Config.mouse then View.Mouse_all else View.Mouse_off)
      ~title:"Glow"
      (List_view.view browser.listing ^ "\n" ^ footer)
  in
  { view with cursor = List_view.view_cursor browser.listing }

let pager_prefix (pager : pager) =
  let body = Viewport.view pager.viewport in
  let percent = int_of_float (100. *. Viewport.scroll_percent pager.viewport) in
  let note = Option.value pager.status ~default:(source_label pager.document) in
  let footer =
    Style.render status_style (Fmt.str " %s  %d%%  ? help  q back " note percent)
  in
  let help =
    if pager.show_help then
      "\n"
      ^ Style.render help_style
          "g/home top  G/end bottom  d/u page  / search  n/N next/prev  l numbers  r \
           rerender  e editor"
    else ""
  in
  body ^ "\n" ^ footer ^ help

let pager_view (pager : pager) =
  let prefix = pager_prefix pager in
  let search = if pager.search_mode then "\n" ^ Textinput.view pager.search else "" in
  let view =
    View.v ~alt_screen:true
      ~mouse:(if pager.config.Config.mouse then View.Mouse_all else View.Mouse_off)
      ~title:"Glow" (prefix ^ search)
  in
  let cursor =
    if not pager.search_mode then None
    else place_cursor ~above:(Layout.height prefix) (Textinput.cursor pager.search)
  in
  { view with cursor }

let view = function
  | Browser browser -> browser_view browser
  | Pager pager -> pager_view pager

let subscriptions = function
  | Browser browser ->
      let resize = Sub.resize (fun ~rows ~cols -> Resize (rows, cols)) in
      let key = Sub.key (fun key -> Key key) in
      let mouse =
        if browser.config.Config.mouse then Sub.mouse (fun value -> Mouse value)
        else Sub.none
      in
      Sub.batch [ resize; key; mouse ]
  | Pager pager ->
      let resize = Sub.resize (fun ~rows ~cols -> Resize (rows, cols)) in
      let key = Sub.key (fun key -> Key key) in
      let mouse =
        if pager.config.Config.mouse then Sub.mouse (fun value -> Mouse value)
        else Sub.none
      in
      Sub.batch [ resize; key; mouse ]

let app initial =
  { Charamel_tea.init = (fun () -> (initial, Cmd.none)); update; view; subscriptions }

type test_event =
  [ `Key of Key.t | `Text of string | `Resize of int * int | `Wait of float ]

let scripted ~(config : Config.t) ~content ~events ~size =
  let rows, columns = size in
  let document = { Source.content; path = None; base_url = None; markdown = true } in
  let pager =
    let pager =
      {
        env = None;
        config;
        document;
        origin_directory = None;
        viewport = Viewport.v ~width:columns ~height:2 ();
        width = columns;
        height = rows;
        is_dark = Charamel_cli.is_dark ~env:Sys.getenv_opt;
        search_mode = false;
        search = make_search columns;
        matches = [];
        match_index = -1;
        show_help = false;
        status = None;
      }
    in
    with_content pager ~keep_offset:false ()
  in
  let events =
    List.map
      (function
        | `Key key -> `Key key
        | `Text text -> `Text text
        | `Resize (rows, columns) -> `Resize (rows, columns)
        | `Wait seconds -> `Wait seconds)
      events
  in
  let _, frame = Charamel_tea.Test.run (app (Pager pager)) ~events ~size in
  frame

let run (env : Charamel_cli.Env.t) ~(config : Config.t) ~location =
  let initial_width = width_for env config in
  let _, initial_height = tty_size env in
  let initial =
    match location with
    | Source.Directory root ->
        Lwt.return
          (Ok
             (Browser
                (make_browser env config root ~width:initial_width ~height:initial_height)))
    | Source.Stdin | Source.File _ | Source.Url _ ->
        Lwt.bind (Source.read ~cwd:env.Env.cwd ~stdin:env.Env.stdin location) (function
          | Error error -> Lwt.return (Error (error_text error))
          | Ok document ->
              Lwt.return
                (Ok
                   (Pager
                      (make_pager env config document ~width:initial_width
                         ~height:initial_height))))
  in
  Lwt.bind initial (function
    | Error _ as error -> Lwt.return error
    | Ok initial ->
        Lwt.bind
          (Charamel_tea.run
             ~terminal:(Charamel_tea.Terminal.local ())
             ~clock:env.Env.clock (app initial))
          (function
            | Ok _ -> Lwt.return (Ok ())
            | Error `Interrupted -> Lwt.return (Error "interrupted")
            | Error (`Exn (exception_, _)) ->
                Lwt.return (Error (Fmt.str "%s" (Printexc.to_string exception_)))))
