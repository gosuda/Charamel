open Result.Syntax
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module Key = Charamel_tea.Key
module View = Charamel_tea.View
module Viewport = Charamel_bubbles.Viewport
module List_view = Charamel_bubbles.List
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color

let error_text = function
  | `Invalid message -> message
  | `Io (path, message) -> Fmt.str "%s: %s" path message
  | `Http message -> message

let key_name key = Key.to_string key

let split_words text =
  let length = String.length text in
  let rec loop index quote current acc =
    if index = length then
      let acc =
        if Buffer.length current = 0 then acc else Buffer.contents current :: acc
      in
      List.rev acc
    else
      let character = text.[index] in
      match quote with
      | Some delimiter when Char.equal character delimiter ->
          loop (index + 1) None current acc
      | Some _ ->
          Buffer.add_char current character;
          loop (index + 1) quote current acc
      | None ->
          if character = '\'' || character = '"' then
            loop (index + 1) (Some character) current acc
          else if Char.equal character ' ' || Char.equal character '\t' then
            if Buffer.length current = 0 then loop (index + 1) None current acc
            else loop (index + 1) None (Buffer.create 16) (Buffer.contents current :: acc)
          else (
            Buffer.add_char current character;
            loop (index + 1) None current acc)
  in
  loop 0 None (Buffer.create 32) []

let theme ~is_dark style =
  match String.lowercase_ascii style with
  | "dark" -> Charamel_glamour.Theme.dark
  | "light" -> Charamel_glamour.Theme.light
  | "dracula" -> Charamel_glamour.Theme.dracula
  | "tokyo-night" | "tokyo_night" -> Charamel_glamour.Theme.tokyo_night
  | "pink" -> Charamel_glamour.Theme.pink
  | "ascii" -> Charamel_glamour.Theme.ascii
  | "notty" -> Charamel_glamour.Theme.notty
  | "auto" -> Charamel_glamour.Theme.auto ~is_dark
  | value -> Fmt.failwith "glow: unknown style %S" value

let line_prefix_style = Style.foreground (Color.Indexed 244) Style.empty
let search_style = Style.foreground (Color.Indexed 212) (Style.bold true Style.empty)

let source_label document =
  match document.Source.path with None -> "stdin" | Some path -> path

type pager = {
  env : Eio_unix.Stdenv.base option;
  config : Config.t;
  document : Source.document;
  origin_directory : string option;
  viewport : Viewport.t;
  width : int;
  height : int;
  is_dark : bool;
  search_mode : bool;
  query : string;
  matches : int list;
  match_index : int;
  show_help : bool;
  status : string option;
}

type browser = {
  env : Eio_unix.Stdenv.base;
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
  | Loaded of Source.document
  | Failed of string
  | Editor_done of int

let tty_size _env =
  try
    let size : Eio_unix.Pty.winsize = Eio_unix.Pty.get_window_size Eio_unix.Fd.stdout in
    (max 1 size.Eio_unix.Pty.rows, max 1 size.Eio_unix.Pty.cols)
  with Unix.Unix_error _ | Eio.Io _ -> (80, 24)

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

let add_line_numbers text =
  let lines = String.split_on_char '\n' text in
  lines
  |> List.mapi (fun index line ->
      let prefix = Fmt.str "%4d " (index + 1) in
      Style.render line_prefix_style prefix ^ line)
  |> String.concat "\n"

let content_for (pager : pager) =
  let text =
    rendered_text ~is_dark:pager.is_dark ~width:pager.width ~config:pager.config
      pager.document
  in
  if pager.config.Config.line_numbers then add_line_numbers text else text

let with_content (pager : pager) ?(keep_offset = true) () =
  let old_offset = Viewport.y_offset pager.viewport in
  let viewport =
    pager.viewport
    |> Viewport.set_width pager.width
    |> Viewport.set_height (max 1 (pager.height - 2))
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
      query = "";
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

let load_command env location =
  Cmd.perform (fun () ->
      match Source.read ~env ~clock:env#clock ~net:env#net location with
      | Ok document -> Loaded document
      | Error error -> Failed (error_text error))

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
  | Pager_child _ | Editor_done _ -> (Browser browser, Cmd.none)

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
        if contains_at (String.lowercase_ascii line) 0 then Some index else None)
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

let update_search (pager : pager) (key : Key.t) =
  match key.Key.code with
  | Key.Escape -> ({ pager with search_mode = false; status = None }, Cmd.none)
  | Key.Enter ->
      let matches = search_lines pager.query pager.document.Source.content in
      let pager = { pager with search_mode = false; matches; match_index = -1 } in
      (goto_match pager 0, Cmd.none)
  | Key.Backspace ->
      let length = String.length pager.query in
      let query = if length = 0 then "" else String.sub pager.query 0 (length - 1) in
      ({ pager with query }, Cmd.none)
  | Key.Char uchar ->
      let text =
        if key.Key.text = "" then (
          let buffer = Buffer.create 4 in
          Buffer.add_utf_8_uchar buffer uchar;
          Buffer.contents buffer)
        else key.Key.text
      in
      ({ pager with query = pager.query ^ text }, Cmd.none)
  | _ -> (pager, Cmd.none)

let editor_words () =
  match Sys.getenv_opt "EDITOR" with
  | Some value when String.trim value <> "" -> split_words value
  | _ -> [ "vi" ]

let reload_command (pager : pager) =
  Cmd.perform (fun () ->
      match (pager.env, pager.document.Source.path) with
      | None, _ -> Failed "filesystem access is unavailable in scripted mode"
      | Some _, None -> Failed "the current source has no local file"
      | Some env, Some path -> (
          match Source.read ~env ~clock:env#clock ~net:env#net (Source.File path) with
          | Ok document -> Loaded document
          | Error error -> Failed (error_text error)))

let editor_command pager =
  match pager.document.Source.path with
  | None -> Cmd.msg (Failed "the current source has no local file")
  | Some path ->
      let argv = editor_words () @ [ path ] in
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
          ( Pager
              {
                pager with
                search_mode = true;
                query = "";
                status = Some "/ search  enter accept  esc cancel";
              },
            Cmd.none )
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
      (Pager (with_content { pager with width = columns; height = rows } ()), Cmd.none)
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

let browser_view browser =
  let footer =
    Style.render help_style "↑/↓ select  enter open  / filter  r refresh  q quit"
  in
  View.v ~alt_screen:true
    ~mouse:(if browser.config.Config.mouse then View.Mouse_all else View.Mouse_off)
    ~title:"Glow"
    (List_view.view browser.listing ^ "\n" ^ footer)

let pager_view (pager : pager) =
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
  let search =
    if pager.search_mode then Fmt.str "\n%s%s" (Style.render search_style "/") pager.query
    else ""
  in
  View.v ~alt_screen:true
    ~mouse:(if pager.config.Config.mouse then View.Mouse_all else View.Mouse_off)
    ~title:"Glow"
    (body ^ "\n" ^ footer ^ help ^ search)

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
        query = "";
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

let run env ~(config : Config.t) ~location =
  let initial_width = width_for env config in
  let _, initial_height = tty_size env in
  let initial =
    match location with
    | Source.Directory root ->
        Ok
          (Browser
             (make_browser env config root ~width:initial_width ~height:initial_height))
    | Source.Stdin | Source.File _ | Source.Url _ -> (
        match Source.read ~env ~clock:env#clock ~net:env#net location with
        | Error error -> Error (error_text error)
        | Ok document ->
            Ok
              (Pager
                 (make_pager env config document ~width:initial_width
                    ~height:initial_height)))
  in
  let* initial = initial in
  match
    Charamel_tea.run
      ~terminal:(Charamel_tea.Terminal.local env)
      ~clock:env#clock (app initial) env
  with
  | Ok _ -> Ok ()
  | Error `Interrupted -> Error "interrupted"
  | Error `Killed -> Error "killed"
  | Error (`Exn (exception_, _)) -> Error (Fmt.str "%s" (Printexc.to_string exception_))
