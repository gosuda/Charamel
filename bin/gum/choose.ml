module Key = Charamel_tea.Key
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View
module Style = Charamel_lipgloss.Style
module Text = Charamel_ansi.Text

let k_down = Gum_flag.key ~cmd:"choose" "down"
let k_j = Gum_flag.key ~cmd:"choose" "j"
let k_ctrl_j = Gum_flag.key ~cmd:"choose" "ctrl+j"
let k_ctrl_n = Gum_flag.key ~cmd:"choose" "ctrl+n"
let k_up = Gum_flag.key ~cmd:"choose" "up"
let k_k = Gum_flag.key ~cmd:"choose" "k"
let k_ctrl_k = Gum_flag.key ~cmd:"choose" "ctrl+k"
let k_ctrl_p = Gum_flag.key ~cmd:"choose" "ctrl+p"
let k_right = Gum_flag.key ~cmd:"choose" "right"
let k_l = Gum_flag.key ~cmd:"choose" "l"
let k_ctrl_f = Gum_flag.key ~cmd:"choose" "ctrl+f"
let k_left = Gum_flag.key ~cmd:"choose" "left"
let k_h = Gum_flag.key ~cmd:"choose" "h"
let k_ctrl_b = Gum_flag.key ~cmd:"choose" "ctrl+b"
let k_home = Gum_flag.key ~cmd:"choose" "home"
let k_g = Gum_flag.key ~cmd:"choose" "g"
let k_end = Gum_flag.key ~cmd:"choose" "end"
let k_shift_g = Gum_flag.key ~cmd:"choose" "G"
let k_space = Gum_flag.key ~cmd:"choose" " "
let k_tab = Gum_flag.key ~cmd:"choose" "tab"
let k_x = Gum_flag.key ~cmd:"choose" "x"
let k_ctrl_at = Gum_flag.key ~cmd:"choose" "ctrl+@"
let k_a = Gum_flag.key ~cmd:"choose" "a"
let k_shift_a = Gum_flag.key ~cmd:"choose" "A"
let k_ctrl_a = Gum_flag.key ~cmd:"choose" "ctrl+a"
let k_enter = Gum_flag.key ~cmd:"choose" "enter"
let k_ctrl_q = Gum_flag.key ~cmd:"choose" "ctrl+q"
let k_escape = Gum_flag.key ~cmd:"choose" "esc"
let k_ctrl_c = Gum_flag.key ~cmd:"choose" "ctrl+c"
let any_key = Gum_flag.any_key

let find_substring ~needle text =
  let needle_length = String.length needle in
  let text_length = String.length text in
  if needle_length = 0 then Some 0
  else
    let rec scan index =
      if index + needle_length > text_length then None
      else if String.sub text index needle_length = needle then Some index
      else scan (index + 1)
    in
    scan 0

let parse_option ~delimiter option =
  if delimiter = "" then Ok (option, option)
  else
    match find_substring ~needle:delimiter option with
    | None -> Error (Fmt.str "invalid option format: %S" option)
    | Some index ->
        let label = String.sub option 0 index in
        let value_start = index + String.length delimiter in
        let value = String.sub option value_start (String.length option - value_start) in
        Ok (label, value)

type item = { label : string; value : string; selected : bool; order : int }

type options = {
  options : string list;
  limit : int;
  no_limit : bool;
  ordered : bool;
  height : int;
  cursor : string;
  show_help : bool;
  timeout : float option;
  header : string;
  cursor_prefix : string;
  selected_prefix : string;
  unselected_prefix : string;
  selected : string list;
  select_if_one : bool;
  input_delimiter : string;
  output_delimiter : string;
  label_delimiter : string;
  strip_ansi : bool;
  padding : string;
  cursor_style : Gum_style.t;
  header_style : Gum_style.t;
  item_style : Gum_style.t;
  selected_style : Gum_style.t;
}

type msg = Key of Key.t | Resize of int * int

type model = {
  options : options;
  items : item list;
  index : int;
  paginator : Charamel_bubbles.Paginator.t;
  submitted : bool;
  quitting : bool;
  padding : Charamel_lipgloss.Sides.t;
}

let default_options =
  {
    options = [];
    limit = 1;
    no_limit = false;
    ordered = false;
    height = 10;
    cursor = "> ";
    show_help = true;
    timeout = None;
    header = "Choose:";
    cursor_prefix = "• ";
    selected_prefix = "✓ ";
    unselected_prefix = "• ";
    selected = [];
    select_if_one = false;
    input_delimiter = "\n";
    output_delimiter = "\n";
    label_delimiter = "";
    strip_ansi = true;
    padding = "0 0";
    cursor_style = Gum_style.defaults ~foreground:"212" ();
    header_style = Gum_style.defaults ~foreground:"99" ();
    item_style = Gum_style.empty;
    selected_style = Gum_style.defaults ~foreground:"212" ();
  }

let parse_options ~delimiter options =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | option :: rest -> (
        match parse_option ~delimiter option with
        | Error _ as error -> error
        | Ok (label, value) ->
            loop ({ label; value; selected = false; order = -1 } :: acc) rest)
  in
  loop [] options

let single_option (options : options) =
  match parse_options ~delimiter:options.label_delimiter options.options with
  | Error message -> Error message
  | Ok [ item ] when options.select_if_one -> Ok (Some item.value)
  | Ok _ -> Ok None

let effective_limit (options : options) count =
  if options.no_limit then count + 1 else max 1 options.limit

let make (options : options) =
  let raw_items =
    match parse_options ~delimiter:options.label_delimiter options.options with
    | Ok items -> items
    | Error message -> invalid_arg message
  in
  let raw_items =
    if options.ordered then
      List.sort (fun a b -> String.compare a.label b.label) raw_items
    else raw_items
  in
  let limit = effective_limit options (List.length raw_items) in
  let options =
    if limit = 1 && not options.no_limit then
      { options with cursor_prefix = ""; selected_prefix = ""; unselected_prefix = "" }
    else options
  in
  let select_all = options.selected = [ "*" ] in
  let selected_values value = select_all || List.mem value options.selected in
  let rec seed index order count acc = function
    | [] -> (List.rev acc, index, order)
    | item :: rest ->
        let is_requested = selected_values item.label in
        if limit = 1 then
          let cursor = if is_requested then index else -1 in
          let item = { item with selected = false; order = -1 } in
          let items, cursor', order' = seed (index + 1) order count (item :: acc) rest in
          (items, (if cursor >= 0 then cursor else cursor'), order')
        else if is_requested && count < limit then
          let item = { item with selected = true; order } in
          let items, cursor, order' =
            seed (index + 1) (order + 1) (count + 1) (item :: acc) rest
          in
          (items, cursor, order')
        else
          let items, cursor, order' = seed (index + 1) order count (item :: acc) rest in
          (items, cursor, order')
  in
  let items, requested_cursor, _ = seed 0 0 0 [] raw_items in
  let index = if requested_cursor < 0 then 0 else requested_cursor in
  let height = max 1 options.height in
  let total_pages = max 1 ((List.length items + height - 1) / height) in
  let paginator =
    Charamel_bubbles.Paginator.v ~kind:Charamel_bubbles.Paginator.Dots ~per_page:height
      ~total_pages ()
  in
  let paginator = Charamel_bubbles.Paginator.set_page (index / height) paginator in
  {
    options;
    items;
    index;
    paginator;
    submitted = false;
    quitting = false;
    padding = Gum_flag.parsed_padding options.padding;
  }

let selected model =
  let selected = List.filter (fun (item : item) -> item.selected) model.items in
  let selected =
    if model.options.ordered then List.sort (fun a b -> compare a.order b.order) selected
    else selected
  in
  List.map (fun item -> item.value) selected

let submitted model = model.submitted

let page_sync model index =
  let page = index / max 1 model.options.height in
  Charamel_bubbles.Paginator.set_page page model.paginator

let update_item index f items =
  List.mapi (fun current item -> if current = index then f item else item) items

let toggle_at model index =
  let limit = effective_limit model.options (List.length model.items) in
  let selected_count =
    List.fold_left
      (fun count (item : item) -> if item.selected then count + 1 else count)
      0 model.items
  in
  match List.nth_opt model.items index with
  | None -> model
  | Some _ when limit = 1 -> model
  | Some item when item.selected ->
      {
        model with
        items =
          update_item index
            (fun item -> { item with selected = false; order = -1 })
            model.items;
      }
  | Some _ when selected_count >= limit -> model
  | Some _ ->
      let order =
        List.fold_left (fun maximum item -> max maximum item.order) (-1) model.items + 1
      in
      {
        model with
        items =
          update_item index (fun item -> { item with selected = true; order }) model.items;
      }

let toggle_all model =
  let limit = effective_limit model.options (List.length model.items) in
  if limit = 1 then model
  else
    let selected_count =
      List.fold_left
        (fun count (item : item) -> if item.selected then count + 1 else count)
        0 model.items
    in
    if selected_count < min limit (List.length model.items) then
      let rec select order count = function
        | [] -> []
        | item :: rest when count >= limit -> item :: select order count rest
        | (item : item) :: rest when item.selected -> item :: select order count rest
        | item :: rest ->
            { item with selected = true; order } :: select (order + 1) (count + 1) rest
      in
      { model with items = select 0 selected_count model.items }
    else
      {
        model with
        items =
          List.map (fun item -> { item with selected = false; order = -1 }) model.items;
      }

let move model delta =
  let length = List.length model.items in
  if length = 0 then model
  else
    let index = (model.index + delta) mod length in
    let index = if index < 0 then index + length else index in
    { model with index; paginator = page_sync model index }

let jump_page model delta = move model (delta * max 1 model.options.height)

let handle_key model key =
  if any_key key [ k_ctrl_c ] then ({ model with quitting = true }, Cmd.interrupt)
  else if any_key key [ k_escape ] then ({ model with quitting = true }, Cmd.quit)
  else if any_key key [ k_enter; k_ctrl_q ] then
    let items =
      if
        effective_limit model.options (List.length model.items) = 1 && selected model = []
      then
        update_item model.index
          (fun item -> { item with selected = true; order = 0 })
          model.items
      else model.items
    in
    ({ model with items; submitted = true; quitting = true }, Cmd.quit)
  else if any_key key [ k_down; k_j; k_ctrl_j; k_ctrl_n ] then (move model 1, Cmd.none)
  else if any_key key [ k_up; k_k; k_ctrl_k; k_ctrl_p ] then (move model (-1), Cmd.none)
  else if any_key key [ k_right; k_l; k_ctrl_f ] then (jump_page model 1, Cmd.none)
  else if any_key key [ k_left; k_h; k_ctrl_b ] then (jump_page model (-1), Cmd.none)
  else if any_key key [ k_home; k_g ] then
    ({ model with index = 0; paginator = page_sync model 0 }, Cmd.none)
  else if any_key key [ k_end; k_shift_g ] then
    let index = max 0 (List.length model.items - 1) in
    ({ model with index; paginator = page_sync model index }, Cmd.none)
  else if any_key key [ k_space; k_tab; k_x; k_ctrl_at ] then
    (toggle_at model model.index, Cmd.none)
  else if any_key key [ k_a; k_shift_a; k_ctrl_a ] then (toggle_all model, Cmd.none)
  else (model, Cmd.none)

let render model =
  if model.quitting then ""
  else
    let start =
      Charamel_bubbles.Paginator.page model.paginator * max 1 model.options.height
    in
    let visible =
      model.items
      |> List.mapi (fun index item -> (index, item))
      |> List.filter (fun (index, _) ->
          index >= start && index < start + max 1 model.options.height)
    in
    let cursor_style = Gum_style.to_style model.options.cursor_style in
    let header_style = Gum_style.to_style model.options.header_style in
    let item_style = Gum_style.to_style model.options.item_style in
    let selected_style = Gum_style.to_style model.options.selected_style in
    let lines =
      List.map
        (fun (index, (item : item)) ->
          let cursor =
            if index = model.index then model.options.cursor
            else String.make (Text.width model.options.cursor) ' '
          in
          let cursor =
            if index = model.index then Style.render cursor_style cursor else cursor
          in
          let marker, body_style =
            if item.selected then (model.options.selected_prefix, selected_style)
            else if index = model.index then (model.options.cursor_prefix, cursor_style)
            else (model.options.unselected_prefix, item_style)
          in
          cursor ^ Style.render body_style (marker ^ item.label))
        visible
    in
    let lines = String.concat "\n" lines in
    let lines =
      if model.paginator |> Charamel_bubbles.Paginator.total_pages > 1 then
        lines ^ "\n  " ^ Charamel_bubbles.Paginator.view model.paginator
      else lines
    in
    let lines =
      if model.options.header = "" then lines
      else Style.render header_style model.options.header ^ "\n" ^ lines
    in
    let lines =
      if model.options.show_help then
        lines ^ "\n\nenter submit • esc quit • ↑↓ navigate • space toggle"
      else lines
    in
    Style.render (Style.padding model.padding Style.empty) lines

let update message model =
  match message with
  | Key key -> handle_key model key
  | Resize (rows, _cols) ->
      let height =
        max 1 (if model.options.height = 0 then rows else model.options.height)
      in
      let options =
        if model.options.height = 0 then { model.options with height } else model.options
      in
      let paginator =
        Charamel_bubbles.Paginator.v ~kind:Charamel_bubbles.Paginator.Dots
          ~per_page:height
          ~total_pages:(max 1 ((List.length model.items + height - 1) / height))
          ()
      in
      let paginator =
        Charamel_bubbles.Paginator.set_page (model.index / height) paginator
      in
      ({ model with options; paginator }, Cmd.none)

let app options : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (make options, Cmd.none));
    update = (fun message model -> update message model);
    view = (fun model -> View.v (render model));
    subscriptions =
      (fun _ ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.resize (fun ~rows ~cols -> Resize (rows, cols));
          ]);
  }

let read_text env (options : options) =
  match Gum_io.read_stdin ~strip_ansi:options.strip_ansi env with
  | Ok value -> value
  | Error `Empty -> ""
  | Error (`Read value) -> value

let normalized_options env (options : options) =
  let input = read_text env options in
  let positional = options.options in
  let options, selected =
    if positional = [] then
      let values =
        if input = "" then [] else Gum_io.split ~delimiter:options.input_delimiter input
      in
      ({ options with options = values }, options.selected)
    else if options.selected = [] && input <> "" then
      (options, Gum_io.split ~delimiter:options.input_delimiter input)
    else (options, options.selected)
  in
  { options with selected }

let run env (options : options) =
  let options = normalized_options env options in
  if options.options = [] then
    Charamel_cli.error "no options provided, see `gum choose --help`";
  match single_option options with
  | Error message -> Charamel_cli.error message
  | Ok (Some value) -> Gum_io.print_raw env value
  | Ok None ->
      let model =
        try
          Gum_run.run ?timeout:options.timeout env (app options) ~finished:(fun model ->
              if submitted model then Gum_run.Submitted else Gum_run.Quit)
        with Gum_io.No_tty -> Charamel_cli.error "choose: requires a terminal"
      in
      if not (submitted model) then Charamel_cli.error "nothing selected";
      let values = selected model in
      if values = [] then Charamel_cli.error "nothing selected"
      else Gum_io.println env (String.concat options.output_delimiter values)

let cmd env =
  let open Cmdliner in
  let open Term.Syntax in
  let options_arg =
    Arg.(value (pos_all string [] (info [] ~docv:"OPTION" ~doc:"Option to choose.")))
  in
  let selected_arg =
    Arg.(
      value
        (opt_all string []
           (info [ "selected" ] ~doc:"Options initially selected."
              ~env:(Gum_flag.env ~cmd:"choose" "selected"))))
  in
  let cursor_style =
    Gum_style.term ~cmd:"choose" ~prefix:"cursor."
      ~defaults:(Gum_style.defaults ~foreground:"212" ())
      ()
  in
  let header_style =
    Gum_style.term ~cmd:"choose" ~prefix:"header."
      ~defaults:(Gum_style.defaults ~foreground:"99" ())
      ()
  in
  let item_style =
    Gum_style.term ~cmd:"choose" ~prefix:"item." ~defaults:Gum_style.empty ()
  in
  let selected_style =
    Gum_style.term ~cmd:"choose" ~prefix:"selected."
      ~defaults:(Gum_style.defaults ~foreground:"212" ())
      ()
  in
  let term =
    let+ options = options_arg
    and+ limit =
      Gum_flag.int_arg ~cmd:"choose" "limit" ~default:1
        ~doc:"Maximum number of options to pick."
    and+ no_limit = Gum_flag.flag ~cmd:"choose" ~doc:"Pick unlimited options." "no-limit"
    and+ ordered = Gum_flag.flag ~cmd:"choose" ~doc:"Maintain selection order." "ordered"
    and+ height =
      Gum_flag.int_arg ~cmd:"choose" "height" ~default:10
        ~doc:"Number of options per page."
    and+ cursor =
      Gum_flag.string_arg ~cmd:"choose" "cursor" ~default:"> " ~doc:"Cursor prefix."
    and+ show_help =
      Gum_flag.negatable ~cmd:"choose" ~default:true ~doc:"Show help keybinds."
        "show-help"
    and+ timeout =
      Gum_flag.seconds ~cmd:"choose" ~doc:"Timeout until selection." "timeout"
    and+ header =
      Gum_flag.string_arg ~cmd:"choose" "header" ~default:"Choose:" ~doc:"Header value."
    and+ cursor_prefix =
      Gum_flag.string_arg ~cmd:"choose" "cursor-prefix" ~default:"• "
        ~doc:"Cursor item prefix."
    and+ selected_prefix =
      Gum_flag.string_arg ~cmd:"choose" "selected-prefix" ~default:"✓ "
        ~doc:"Selected item prefix."
    and+ unselected_prefix =
      Gum_flag.string_arg ~cmd:"choose" "unselected-prefix" ~default:"• "
        ~doc:"Unselected item prefix."
    and+ selected = selected_arg
    and+ select_if_one =
      Gum_flag.flag ~cmd:"choose" ~doc:"Select a sole option without a terminal."
        "select-if-one"
    and+ input_delimiter =
      Gum_flag.delimiter ~cmd:"choose" ~default:"\n" ~doc:"Input option delimiter."
        "input-delimiter"
    and+ output_delimiter =
      Gum_flag.delimiter ~cmd:"choose" ~default:"\n" ~doc:"Output option delimiter."
        "output-delimiter"
    and+ label_delimiter =
      Gum_flag.string_arg ~cmd:"choose" "label-delimiter" ~default:""
        ~doc:"Label/value delimiter."
    and+ strip_ansi =
      Gum_flag.negatable ~cmd:"choose" ~default:true ~doc:"Strip ANSI from stdin."
        "strip-ansi"
    and+ padding = Gum_flag.validated_padding_term ~cmd:"choose" ()
    and+ cursor_style = cursor_style
    and+ header_style = header_style
    and+ item_style = item_style
    and+ selected_style = selected_style in
    run env
      {
        options;
        limit;
        no_limit;
        ordered;
        height;
        cursor;
        show_help;
        timeout;
        header;
        cursor_prefix;
        selected_prefix;
        unselected_prefix;
        selected;
        select_if_one;
        input_delimiter;
        output_delimiter;
        label_delimiter;
        strip_ansi;
        padding;
        cursor_style;
        header_style;
        item_style;
        selected_style;
      }
  in
  Cmd.v (Cmd.info "choose" ~doc:"Choose one or more options.") term
