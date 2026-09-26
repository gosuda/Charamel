module Key = Charamel_tea.Key
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View
module Style = Charamel_lipgloss.Style
module Textinput = Charamel_bubbles.Textinput
module Viewport = Charamel_bubbles.Viewport

let k_up = Gum_flag.key ~cmd:"pager" "up"
let k_down = Gum_flag.key ~cmd:"pager" "down"
let k_pgup = Gum_flag.key ~cmd:"pager" "pgup"
let k_pgdown = Gum_flag.key ~cmd:"pager" "pgdown"
let k_space = Gum_flag.key ~cmd:"pager" " "
let k_f = Gum_flag.key ~cmd:"pager" "f"
let k_b = Gum_flag.key ~cmd:"pager" "b"
let k_u = Gum_flag.key ~cmd:"pager" "u"
let k_ctrl_u = Gum_flag.key ~cmd:"pager" "ctrl+u"
let k_d = Gum_flag.key ~cmd:"pager" "d"
let k_ctrl_d = Gum_flag.key ~cmd:"pager" "ctrl+d"
let k_k = Gum_flag.key ~cmd:"pager" "k"
let k_j = Gum_flag.key ~cmd:"pager" "j"
let k_h = Gum_flag.key ~cmd:"pager" "h"
let k_l = Gum_flag.key ~cmd:"pager" "l"
let k_home = Gum_flag.key ~cmd:"pager" "home"
let k_g = Gum_flag.key ~cmd:"pager" "g"
let k_end = Gum_flag.key ~cmd:"pager" "end"
let k_shift_g = Gum_flag.key ~cmd:"pager" "G"
let k_slash = Gum_flag.key ~cmd:"pager" "/"
let k_enter = Gum_flag.key ~cmd:"pager" "enter"
let k_n = Gum_flag.key ~cmd:"pager" "n"
let k_shift_n = Gum_flag.key ~cmd:"pager" "N"
let k_escape = Gum_flag.key ~cmd:"pager" "esc"
let k_ctrl_c = Gum_flag.key ~cmd:"pager" "ctrl+c"
let k_ctrl_d_key = Gum_flag.key ~cmd:"pager" "ctrl+d"
let k_q = Gum_flag.key ~cmd:"pager" "q"
let is_key = Gum_flag.is_key
let any_key = Gum_flag.any_key

type options = {
  content : string;
  style : Gum_style.t;
  show_line_numbers : bool;
  soft_wrap : bool;
  timeout : float option;
  line_number_style : Gum_style.t;
  match_style : Gum_style.t;
  match_highlight_style : Gum_style.t;
}

type msg =
  | Key of Key.t
  | Input of Textinput.msg
  | Viewport of Viewport.msg
  | Resize of int * int

type model = {
  options : options;
  content : string;
  viewport : Viewport.t;
  search : Textinput.t;
  search_active : bool;
  query : string;
  match_lines : int list;
  match_index : int;
  quitting : bool;
}

let default_options =
  {
    content = "";
    style =
      Gum_style.defaults ~border:"rounded" ~padding:"0 1" ~border_foreground:"212" ();
    show_line_numbers = true;
    soft_wrap = true;
    timeout = None;
    line_number_style = Gum_style.defaults ~foreground:"237" ();
    match_style = Gum_style.defaults ~foreground:"212" ~bold:true ();
    match_highlight_style =
      Gum_style.defaults ~foreground:"235" ~background:"225" ~bold:true ();
  }

let sanitize text =
  let output = Buffer.create (String.length text) in
  let remove_previous () =
    let length = Buffer.length output in
    if length > 0 then begin
      let index = ref (length - 1) in
      while !index > 0 && Char.code (Buffer.nth output !index) land 0xC0 = 0x80 do
        decr index
      done;
      Buffer.truncate output !index
    end
  in
  String.iter
    (fun character ->
      if character = '\b' then remove_previous () else Buffer.add_char output character)
    text;
  Buffer.contents output

let split_lines text = String.split_on_char '\n' text

let search_lines ~pattern content =
  if pattern = "" then []
  else
    let regexp =
      try Some (Re.Perl.compile_pat ~opts:[ `Caseless ] pattern)
      with Re.Perl.Parse_error | Re.Perl.Not_supported | Invalid_argument _ -> None
    in
    match regexp with
    | None -> []
    | Some regexp ->
        split_lines content
        |> List.mapi (fun index line -> if Re.execp regexp line then Some index else None)
        |> List.filter_map Fun.id

let style_literal_matches ~style ~pattern line =
  if pattern = "" then line
  else
    let regexp =
      try Some (Re.Perl.compile_pat ~opts:[ `Caseless ] pattern)
      with Re.Perl.Parse_error | Re.Perl.Not_supported | Invalid_argument _ -> None
    in
    match regexp with
    | None -> line
    | Some regexp ->
        let rec collect position acc =
          match Re.exec_opt ~pos:position regexp line with
          | None -> List.rev acc
          | Some groups ->
              let start, stop = Re.Group.offset groups 0 in
              let next = if stop <= start then start + 1 else stop in
              collect next ((start, stop) :: acc)
        in
        let positions = collect 0 [] in
        let rec render_from position = function
          | [] -> String.sub line position (String.length line - position)
          | (start, stop) :: rest ->
              let prefix = String.sub line position (start - position) in
              let match_text = String.sub line start (stop - start) in
              prefix ^ Style.render style match_text ^ render_from stop rest
        in
        render_from 0 positions

let configured_size style =
  let width = Option.value ~default:0 (Style.get_width style) in
  let height = Option.value ~default:0 (Style.get_height style) in
  (width, height)

let decorated_content model =
  let line_number_style = Gum_style.to_style model.options.line_number_style in
  let match_style = Gum_style.to_style model.options.match_style in
  let highlight_style = Gum_style.to_style model.options.match_highlight_style in
  split_lines model.content
  |> List.mapi (fun index line ->
      let line =
        if model.query = "" then line
        else
          let style =
            if
              List.mem index model.match_lines
              &&
              match List.nth_opt model.match_lines model.match_index with
              | Some current -> current = index
              | None -> false
            then highlight_style
            else match_style
          in
          style_literal_matches ~style ~pattern:model.query line
      in
      if model.options.show_line_numbers then
        Style.render line_number_style (Fmt.str "%4d " (index + 1)) ^ line
      else line)
  |> String.concat "\n"

let refresh_viewport model = Viewport.set_content (decorated_content model) model.viewport

let compute_matches model =
  let match_lines = search_lines ~pattern:model.query model.content in
  let match_index =
    if match_lines = [] then 0 else min model.match_index (List.length match_lines - 1)
  in
  let model = { model with match_lines; match_index } in
  { model with viewport = refresh_viewport model }

let new_search width =
  let search =
    Textinput.v ~prompt:"/" ~placeholder:"search" ~width
      ~styles:(Textinput.default_styles ~is_dark:true)
      ()
  in
  Textinput.focus search

let make (options : options) =
  let root_style = Gum_style.to_style options.style in
  let configured_width, configured_height = configured_size root_style in
  let viewport =
    Viewport.v ~width:configured_width ~height:configured_height
      ~soft_wrap:options.soft_wrap ~style:Style.empty ()
  in
  let search, _ = new_search configured_width in
  let model =
    {
      options;
      content = sanitize options.content;
      viewport;
      search;
      search_active = false;
      query = "";
      match_lines = [];
      match_index = 0;
      quitting = false;
    }
  in
  compute_matches model

let cancel_search model =
  let search = Textinput.blur model.search in
  { model with search; search_active = false }

let apply_search model =
  let query = Textinput.value model.search in
  compute_matches { model with query; search_active = false }

let update_input model message =
  let search, command = Textinput.update message model.search in
  ({ model with search }, Cmd.map (fun message -> Input message) command)

let jump_match model delta =
  match model.match_lines with
  | [] -> model
  | _ ->
      let length = List.length model.match_lines in
      let index = (model.match_index + delta) mod length in
      let index = if index < 0 then index + length else index in
      let line = List.nth model.match_lines index in
      {
        model with
        match_index = index;
        viewport = Viewport.set_y_offset line model.viewport;
      }

let update_viewport model message =
  let viewport, command = Viewport.update message model.viewport in
  ({ model with viewport }, Cmd.map (fun message -> Viewport message) command)

let handle_key model key =
  if model.search_active then
    if is_key key k_escape || is_key key k_ctrl_d_key then (cancel_search model, Cmd.none)
    else if is_key key k_ctrl_c then (cancel_search model, Cmd.none)
    else if is_key key k_enter then (apply_search model, Cmd.none)
    else
      match Textinput.key model.search key with
      | None -> (model, Cmd.none)
      | Some message -> update_input model message
  else if is_key key k_ctrl_c then ({ model with quitting = true }, Cmd.interrupt)
  else if is_key key k_slash then
    let search, command = Textinput.focus model.search in
    ( { model with search; search_active = true },
      Cmd.map (fun message -> Input message) command )
  else if is_key key k_escape || is_key key k_q then
    ({ model with quitting = true }, Cmd.quit)
  else if is_key key k_n then (jump_match model 1, Cmd.none)
  else if is_key key k_shift_n then (jump_match model (-1), Cmd.none)
  else if any_key key [ k_home; k_g ] then
    ({ model with viewport = Viewport.goto_top model.viewport }, Cmd.none)
  else if any_key key [ k_end; k_shift_g ] then
    ({ model with viewport = Viewport.goto_bottom model.viewport }, Cmd.none)
  else
    match Viewport.key model.viewport key with
    | None ->
        if
          any_key key
            [
              k_up;
              k_k;
              k_down;
              k_j;
              k_pgup;
              k_pgdown;
              k_space;
              k_f;
              k_b;
              k_u;
              k_ctrl_u;
              k_d;
              k_ctrl_d;
              k_h;
              k_l;
            ]
        then (model, Cmd.none)
        else (model, Cmd.none)
    | Some message -> update_viewport model message

let render model =
  if model.quitting then ""
  else
    let content =
      if model.search_active then
        Textinput.view model.search ^ "\n" ^ Viewport.view model.viewport
      else Viewport.view model.viewport
    in
    let content =
      if model.options.show_line_numbers || model.options.soft_wrap then
        content ^ "\n\n↑↓ scroll • / search • n/N next/prev • q quit"
      else content
    in
    Style.render (Gum_style.to_style model.options.style) content

let update message model =
  match message with
  | Key key -> handle_key model key
  | Input component_message ->
      if model.search_active then
        let model, command = update_input model component_message in
        let model = if Textinput.value model.search = "" then model else model in
        (model, command)
      else (model, Cmd.none)
  | Viewport component_message -> update_viewport model component_message
  | Resize (rows, cols) ->
      let width =
        match Style.get_width (Gum_style.to_style model.options.style) with
        | Some width when width > 0 -> width
        | _ -> cols
      in
      let height =
        match Style.get_height (Gum_style.to_style model.options.style) with
        | Some height when height > 0 -> height
        | _ -> rows
      in
      let viewport =
        Viewport.set_height height (Viewport.set_width width model.viewport)
      in
      let search = Textinput.set_width width model.search in
      ({ model with viewport; search }, Cmd.none)

let app options : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (make options, Cmd.none));
    update = (fun message model -> update message model);
    view = (fun model -> View.v ~alt_screen:true (render model));
    subscriptions =
      (fun model ->
        Sub.batch
          [
            Sub.key (fun key ->
                if model.search_active then
                  match Textinput.key model.search key with
                  | Some message -> Input message
                  | None -> Key key
                else Key key);
            Sub.resize (fun ~rows ~cols -> Resize (rows, cols));
            Sub.map
              (fun message -> Viewport message)
              (Viewport.subscriptions model.viewport);
          ]);
  }

let run env (options : options) =
  Lwt.bind
    (if options.content <> "" then Lwt.return options.content
     else
       Lwt.map
         (function
           | Ok value -> value
           | Error `Empty -> Charamel_cli.error "provide some content to display"
           | Error (`Read value) -> value)
         (Gum_io.read_stdin env))
    (fun content ->
      let content = sanitize content in
      if content = "" then Charamel_cli.error "provide some content to display";
      let options = { options with content } in
      Lwt.catch
        (fun () ->
          Lwt.map ignore
            (Gum_run.run ?timeout:options.timeout env (app options) ~finished:(fun _ ->
                 Gum_run.Submitted)))
        (function
          | Gum_io.No_tty -> Charamel_cli.error "pager: requires a terminal"
          | exn -> Lwt.fail exn))

let cmd env =
  let open Cmdliner in
  let open Term.Syntax in
  let content =
    Arg.(value (pos 0 string "" (info [] ~docv:"CONTENT" ~doc:"Content to page.")))
  in
  let term =
    let+ content = content
    and+ style =
      Gum_style.term ~cmd:"pager"
        ~defaults:
          (Gum_style.defaults ~border:"rounded" ~padding:"0 1" ~border_foreground:"212" ())
        ()
    and+ show_line_numbers =
      Gum_flag.negatable ~cmd:"pager" ~default:true ~doc:"Show line numbers."
        "show-line-numbers"
    and+ soft_wrap =
      Gum_flag.negatable ~cmd:"pager" ~default:true ~doc:"Soft wrap long lines."
        "soft-wrap"
    and+ timeout =
      Gum_flag.seconds ~cmd:"pager" ~doc:"Timeout until pager exits." "timeout"
    and+ line_number_style =
      Gum_style.term ~cmd:"pager" ~prefix:"line-number."
        ~defaults:(Gum_style.defaults ~foreground:"237" ())
        ()
    and+ match_style =
      Gum_style.term ~cmd:"pager" ~prefix:"match."
        ~defaults:(Gum_style.defaults ~foreground:"212" ~bold:true ())
        ()
    and+ match_highlight_style =
      Gum_style.term ~cmd:"pager" ~prefix:"match-highlight."
        ~defaults:(Gum_style.defaults ~foreground:"235" ~background:"225" ~bold:true ())
        ()
    in
    run env
      {
        content;
        style;
        show_line_numbers;
        soft_wrap;
        timeout;
        line_number_style;
        match_style;
        match_highlight_style;
      }
  in
  Cmd.v (Cmd.info "pager" ~doc:"Browse content with scrolling and search.") term
