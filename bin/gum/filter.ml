module Key = Charm_tea.Key
module Cmd = Charm_tea.Cmd
module Sub = Charm_tea.Sub
module View = Charm_tea.View
module Style = Charm_lipgloss.Style
module Text = Charm_ansi.Text
module Layout = Charm_lipgloss.Layout
module Fuzzy = Charm_bubbles.Fuzzy
module Textinput = Charm_bubbles.Textinput
module Viewport = Charm_bubbles.Viewport

let key name =
  match Key.of_string name with
  | Ok value -> value
  | Error (`Msg message) -> invalid_arg (Fmt.str "invalid filter key %s: %s" name message)

let k_down = key "down"
let k_ctrl_j = key "ctrl+j"
let k_ctrl_n = key "ctrl+n"
let k_up = key "up"
let k_ctrl_k = key "ctrl+k"
let k_ctrl_p = key "ctrl+p"
let k_j = key "j"
let k_k = key "k"
let k_home = key "home"
let k_g = key "g"
let k_end = key "end"
let k_shift_g = key "G"
let k_tab = key "tab"
let k_shift_tab = key "shift+tab"
let k_ctrl_at = key "ctrl+@"
let k_ctrl_a = key "ctrl+a"
let k_slash = key "/"
let k_escape = key "esc"
let k_enter = key "enter"
let k_ctrl_q = key "ctrl+q"
let k_ctrl_c = key "ctrl+c"
let is_key actual expected = Key.matches actual expected
let any_key actual expected = List.exists (is_key actual) expected

type options = {
  options : string list;
  indicator : string;
  limit : int;
  no_limit : bool;
  select_if_one : bool;
  selected : string list;
  show_help : bool;
  strict : bool;
  selected_prefix : string;
  unselected_prefix : string;
  header : string;
  placeholder : string;
  prompt : string;
  width : int;
  height : int;
  value : string;
  reverse : bool;
  fuzzy : bool;
  fuzzy_sort : bool;
  timeout : float option;
  input_delimiter : string;
  output_delimiter : string;
  strip_ansi : bool;
  padding : string;
  indicator_style : Gum_style.t;
  selected_prefix_style : Gum_style.t;
  unselected_prefix_style : Gum_style.t;
  header_style : Gum_style.t;
  text_style : Gum_style.t;
  cursor_text_style : Gum_style.t;
  match_style : Gum_style.t;
  prompt_style : Gum_style.t;
  placeholder_style : Gum_style.t;
}

type match_ = { text : string; value : string; matched : int list; score : int }

type msg =
  | Key of Key.t
  | Input of Textinput.msg
  | Viewport of Viewport.msg
  | Resize of int * int

type candidate = { text : string; value : string }

type model = {
  options : options;
  candidates : candidate list;
  matches : match_ list;
  input : Textinput.t;
  viewport : Viewport.t;
  cursor : int;
  selected : string list;
  submitted : bool;
  quitting : bool;
  padding : Charm_lipgloss.Sides.t;
}

let default_options =
  {
    options = [];
    indicator = "•";
    limit = 1;
    no_limit = false;
    select_if_one = false;
    selected = [];
    show_help = true;
    strict = true;
    selected_prefix = " ◉ ";
    unselected_prefix = " ○ ";
    header = "";
    placeholder = "Filter...";
    prompt = "> ";
    width = 0;
    height = 0;
    value = "";
    reverse = false;
    fuzzy = true;
    fuzzy_sort = true;
    timeout = None;
    input_delimiter = "\n";
    output_delimiter = "\n";
    strip_ansi = true;
    padding = "0 0";
    indicator_style = Gum_style.defaults ~foreground:"212" ();
    selected_prefix_style = Gum_style.defaults ~foreground:"212" ();
    unselected_prefix_style = Gum_style.defaults ~foreground:"240" ();
    header_style = Gum_style.defaults ~foreground:"99" ();
    text_style = Gum_style.empty;
    cursor_text_style = Gum_style.empty;
    match_style = Gum_style.defaults ~foreground:"212" ();
    prompt_style = Gum_style.defaults ~foreground:"240" ();
    placeholder_style = Gum_style.defaults ~foreground:"240" ();
  }

let matched_ranges indices =
  let rec loop current_start current_end acc = function
    | [] -> List.rev ((current_start, current_end) :: acc)
    | index :: rest when index = current_end + 1 -> loop current_start index acc rest
    | index :: rest -> loop index index ((current_start, current_end) :: acc) rest
  in
  match indices with [] -> [] | first :: rest -> loop first first [] rest

let grapheme_indices_for_bytes text start stop =
  let clusters = Charm_ansi.Width.graphemes text in
  let rec loop byte_pos grapheme_pos acc = function
    | [] -> List.rev acc
    | cluster :: rest ->
        let next = byte_pos + String.length cluster in
        let acc = if next > start && byte_pos < stop then grapheme_pos :: acc else acc in
        loop next (grapheme_pos + 1) acc rest
  in
  loop 0 0 [] clusters

let exact_matches ~pattern candidates =
  if pattern = "" then
    List.map (fun value -> { text = value; value; matched = []; score = 0 }) candidates
  else
    let lowered_pattern = String.lowercase_ascii pattern in
    let rec one acc = function
      | [] -> List.rev acc
      | value :: rest -> (
          let lowered_value = String.lowercase_ascii value in
          let pattern_length = String.length lowered_pattern in
          let value_length = String.length lowered_value in
          let rec find index =
            if index + pattern_length > value_length then None
            else if String.sub lowered_value index pattern_length = lowered_pattern then
              Some index
            else find (index + 1)
          in
          match find 0 with
          | None -> one acc rest
          | Some index ->
              let matched =
                grapheme_indices_for_bytes value index (index + pattern_length)
              in
              one
                ({ text = value; value; matched; score = List.length matched } :: acc)
                rest)
    in
    one [] candidates

let exact_candidate_matches ~pattern candidates =
  List.filter_map
    (fun candidate ->
      match exact_matches ~pattern [ candidate.text ] with
      | [ match_ ] -> Some { match_ with value = candidate.value }
      | _ -> None)
    candidates

let candidate_matches (options : options) candidates query =
  let query = Text.strip query in
  if query = "" then
    List.map
      (fun candidate ->
        { text = candidate.text; value = candidate.value; matched = []; score = 0 })
      candidates
  else
    let source =
      if options.strict then candidates else { text = query; value = query } :: candidates
    in
    if options.fuzzy then
      let source_text = List.map (fun candidate -> candidate.text) source in
      let ranks =
        if options.fuzzy_sort then Fuzzy.find ~pattern:query source_text
        else Fuzzy.find_unsorted ~pattern:query source_text
      in
      List.filter_map
        (fun (rank : Fuzzy.match_) ->
          match List.nth_opt source rank.index with
          | None -> None
          | Some candidate ->
              Some
                {
                  text = candidate.text;
                  value = candidate.value;
                  matched = rank.matched;
                  score = rank.score;
                })
        ranks
    else exact_candidate_matches ~pattern:query source

let effective_limit (options : options) count =
  if options.no_limit then max 1 count else max 1 options.limit

let parsed_padding value =
  match Gum_flag.parse_padding value with
  | Ok sides -> sides
  | Error (`Msg message) -> invalid_arg message

let input_styles (options : options) =
  let styles = Textinput.default_styles ~is_dark:true in
  let focused =
    {
      styles.focused with
      prompt = Gum_style.to_style options.prompt_style;
      placeholder = Gum_style.to_style options.placeholder_style;
    }
  in
  let blurred =
    {
      styles.blurred with
      prompt = Gum_style.to_style options.prompt_style;
      placeholder = Gum_style.to_style options.placeholder_style;
    }
  in
  { styles with focused; blurred }

let set_viewport_content model =
  let content =
    model.matches
    |> List.map (fun (match_ : match_) -> match_.value)
    |> String.concat "\n"
  in
  Viewport.set_content content model.viewport

let source_candidates (options : options) =
  List.map (fun value -> { text = Charm_ansi.Text.strip value; value }) options.options

let single_option (options : options) =
  if (not options.select_if_one) || options.value = "" then Ok None
  else
    let candidates = source_candidates options in
    match candidate_matches options candidates options.value with
    | [ match_ ] -> Ok (Some match_.value)
    | _ -> Ok None

let recompute model =
  let query = Textinput.value model.input in
  let matches = candidate_matches model.options model.candidates query in
  let cursor = min model.cursor (max 0 (List.length matches - 1)) in
  let viewport = set_viewport_content { model with matches } in
  { model with matches; cursor; viewport }

let make (options : options) =
  let candidates = source_candidates options in
  let input =
    Textinput.v ~prompt:options.prompt ~placeholder:options.placeholder ~char_limit:64
      ~width:options.width ~value:options.value ~styles:(input_styles options) ()
  in
  let input, _ = Textinput.focus input in
  let viewport =
    Viewport.v ~width:options.width ~height:options.height ~soft_wrap:false
      ~style:Style.empty ()
  in
  let model =
    {
      options;
      candidates;
      matches = [];
      input;
      viewport;
      cursor = 0;
      selected = [];
      submitted = false;
      quitting = false;
      padding = parsed_padding options.padding;
    }
  in
  let model = recompute model in
  let selected =
    if options.selected = [ "*" ] then
      model.matches
      |> List.filteri (fun index _ ->
          index < effective_limit options (List.length model.matches))
      |> List.map (fun (match_ : match_) -> match_.text)
    else
      let limit = effective_limit options (List.length model.matches) in
      let rec requested count acc = function
        | [] -> List.rev acc
        | _ when count >= limit -> List.rev acc
        | (match_ : match_) :: rest when List.mem match_.text options.selected ->
            requested (count + 1) (match_.text :: acc) rest
        | _ :: rest -> requested count acc rest
      in
      requested 0 [] model.matches
  in
  let cursor =
    match selected with
    | [] -> model.cursor
    | first :: _ -> (
        match
          List.find_index (fun (match_ : match_) -> match_.text = first) model.matches
        with
        | Some index -> index
        | None -> model.cursor)
  in
  { model with selected; cursor }

let selected model =
  let is_selected text = List.mem text model.selected in
  let selected_original =
    model.candidates
    |> List.filter (fun candidate -> is_selected candidate.text)
    |> List.map (fun candidate -> candidate.value)
  in
  let synthetic =
    if (not model.options.strict) && List.mem (Textinput.value model.input) model.selected
    then [ Textinput.value model.input ]
    else []
  in
  synthetic @ selected_original

let current_match model = List.nth_opt model.matches model.cursor

let output_values model =
  let selected = selected model in
  if selected <> [] then selected
  else match current_match model with None -> [] | Some match_ -> [ match_.value ]

let submitted model = model.submitted

let toggle_current model =
  match current_match model with
  | None -> model
  | Some match_ ->
      let limit = effective_limit model.options (List.length model.matches) in
      if List.mem match_.text model.selected then
        {
          model with
          selected = List.filter (fun text -> text <> match_.text) model.selected;
        }
      else if List.length model.selected >= limit then model
      else { model with selected = model.selected @ [ match_.text ] }

let toggle_all model =
  let limit = effective_limit model.options (List.length model.matches) in
  let all =
    model.matches
    |> List.filteri (fun index _ -> index < limit)
    |> List.map (fun (match_ : match_) -> match_.text)
  in
  if List.length model.selected >= List.length all then { model with selected = [] }
  else { model with selected = all }

let ensure_visible model cursor =
  let height = max 1 (Viewport.height model.viewport) in
  let offset = Viewport.y_offset model.viewport in
  let offset =
    if cursor < offset then cursor
    else if cursor >= offset + height then cursor - height + 1
    else offset
  in
  { model with viewport = Viewport.set_y_offset (max 0 offset) model.viewport }

let move model delta =
  let length = List.length model.matches in
  if length = 0 then model
  else
    let cursor = (model.cursor + delta) mod length in
    let cursor = if cursor < 0 then cursor + length else cursor in
    ensure_visible { model with cursor } cursor

let update_component model component_message =
  let input, command = Textinput.update component_message model.input in
  let model = recompute { model with input } in
  (model, Cmd.map (fun message -> Input message) command)

let handle_key model key =
  if any_key key [ k_ctrl_c ] then ({ model with quitting = true }, Cmd.interrupt)
  else if is_key key k_escape then ({ model with quitting = true }, Cmd.quit)
  else if is_key key k_slash then
    let input, command = Textinput.focus model.input in
    ({ model with input }, Cmd.map (fun message -> Input message) command)
  else if any_key key [ k_home; k_g ] && not (Textinput.focused model.input) then
    (ensure_visible { model with cursor = 0 } 0, Cmd.none)
  else if any_key key [ k_end; k_shift_g ] && not (Textinput.focused model.input) then
    let cursor = max 0 (List.length model.matches - 1) in
    (ensure_visible { model with cursor } cursor, Cmd.none)
  else if any_key key [ k_enter; k_ctrl_q ] then
    ({ model with submitted = true; quitting = true }, Cmd.quit)
  else if any_key key [ k_down; k_ctrl_j; k_ctrl_n ] then (move model 1, Cmd.none)
  else if any_key key [ k_up; k_ctrl_k; k_ctrl_p ] then (move model (-1), Cmd.none)
  else if any_key key [ k_j ] && not (Textinput.focused model.input) then
    (move model 1, Cmd.none)
  else if any_key key [ k_k ] && not (Textinput.focused model.input) then
    (move model (-1), Cmd.none)
  else if any_key key [ k_tab ] then
    if effective_limit model.options (List.length model.matches) = 1 then (model, Cmd.none)
    else (move (toggle_current model) 1, Cmd.none)
  else if is_key key k_shift_tab then
    if effective_limit model.options (List.length model.matches) = 1 then (model, Cmd.none)
    else (move (toggle_current model) (-1), Cmd.none)
  else if is_key key k_ctrl_at then (toggle_current model, Cmd.none)
  else if is_key key k_ctrl_a then (toggle_all model, Cmd.none)
  else
    match Textinput.key model.input key with
    | None -> (model, Cmd.none)
    | Some component_message -> update_component model component_message

let render_match model index (match_ : match_) =
  let cursor = index = model.cursor in
  let indicator_style = Gum_style.to_style model.options.indicator_style in
  let selected_style = Gum_style.to_style model.options.selected_prefix_style in
  let unselected_style = Gum_style.to_style model.options.unselected_prefix_style in
  let text_style =
    if cursor then Gum_style.to_style model.options.cursor_text_style
    else Gum_style.to_style model.options.text_style
  in
  let prefix, prefix_style =
    if List.mem match_.text model.selected then
      (model.options.selected_prefix, selected_style)
    else if effective_limit model.options (List.length model.matches) > 1 then
      (model.options.unselected_prefix, unselected_style)
    else (" ", Style.empty)
  in
  let indicator =
    if cursor then Style.render indicator_style model.options.indicator else " "
  in
  let body =
    if match_.matched = [] then Style.render text_style match_.value
    else
      Layout.style_runes
        (Gum_style.to_style model.options.match_style)
        text_style match_.value ~indices:match_.matched
  in
  indicator ^ Style.render prefix_style prefix ^ body

let render model =
  if model.quitting then ""
  else
    let lines =
      model.matches
      |> List.mapi (fun index match_ -> (index, render_match model index match_))
      |> (fun lines -> if model.options.reverse then List.rev lines else lines)
      |> List.mapi (fun display_index (_, line) -> (display_index, line))
      |> List.filter (fun (index, _) ->
          let start = Viewport.y_offset model.viewport in
          let stop = start + max 1 (Viewport.height model.viewport) in
          index >= start && index < stop)
      |> List.map snd |> String.concat "\n"
    in
    let header =
      if model.options.header = "" then ""
      else
        Style.render (Gum_style.to_style model.options.header_style) model.options.header
        ^ "\n"
    in
    let view = header ^ Textinput.view model.input ^ "\n" ^ lines in
    let view =
      if model.options.show_help then
        view ^ "\n\nenter submit • / search • tab toggle • esc quit"
      else view
    in
    Style.render (Style.padding model.padding Style.empty) view

let update message model =
  match message with
  | Key key -> handle_key model key
  | Input component_message -> update_component model component_message
  | Viewport component_message ->
      let viewport, command = Viewport.update component_message model.viewport in
      ({ model with viewport }, Cmd.map (fun message -> Viewport message) command)
  | Resize (rows, cols) ->
      let width = if model.options.width = 0 then cols else model.options.width in
      let height = if model.options.height = 0 then rows else model.options.height in
      let input = Textinput.set_width width model.input in
      let viewport =
        Viewport.set_height height (Viewport.set_width width model.viewport)
      in
      ({ model with input; viewport }, Cmd.none)

let app options : (model, msg) Charm_tea.app =
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
  | Ok text -> text
  | Error `Empty -> ""
  | Error (`Read text) -> text

let normalized_options env (options : options) =
  if options.options <> [] then options
  else
    let input = read_text env options in
    if input <> "" then
      { options with options = Gum_io.split ~delimiter:options.input_delimiter input }
    else { options with options = Gum_io.list_files env }

let run env (options : options) =
  let options = normalized_options env options in
  if options.options = [] then
    Charm_cli.error "no options provided, see `gum filter --help`";
  match single_option options with
  | Error message -> Charm_cli.error message
  | Ok (Some value) -> Gum_io.println env value
  | Ok None ->
      let model =
        try
          Gum_run.run ?timeout:options.timeout env (app options) ~finished:(fun model ->
              if submitted model then Gum_run.Submitted else Gum_run.Quit)
        with Gum_io.No_tty -> Charm_cli.error "filter: requires a terminal"
      in
      if not (submitted model) then Charm_cli.error "nothing selected";
      let values = output_values model in
      if values = [] then Charm_cli.error "nothing selected"
      else Gum_io.println env (String.concat options.output_delimiter values)

let validated_padding_term ~cmd =
  let open Cmdliner in
  let parse value =
    match Gum_flag.parse_padding value with
    | Ok _ -> Ok value
    | Error (`Msg message) -> Error (`Msg message)
  in
  let padding_conv =
    Arg.conv (parse, fun ppf value -> Stdlib.Format.pp_print_string ppf value)
  in
  Arg.(
    value
      (opt padding_conv "0 0"
         (info [ "padding" ] ~doc:"Padding as one to four integers."
            ~env:(Gum_flag.env ~cmd "padding"))))

let string_arg ~cmd name ~default ~doc =
  Cmdliner.Arg.(
    value (opt string default (info [ name ] ~doc ~env:(Gum_flag.env ~cmd name))))

let int_arg ~cmd name ~default ~doc =
  Cmdliner.Arg.(
    value (opt int default (info [ name ] ~doc ~env:(Gum_flag.env ~cmd name))))

let cmd env =
  let open Cmdliner in
  let open Term.Syntax in
  let options_arg =
    Arg.(value (pos_all string [] (info [] ~docv:"OPTION" ~doc:"Option to filter.")))
  in
  let selected_arg =
    Arg.(
      value
        (opt_all string []
           (info [ "selected" ] ~doc:"Options initially selected."
              ~env:(Gum_flag.env ~cmd:"filter" "selected"))))
  in
  let indicator_style =
    Gum_style.term ~cmd:"filter" ~prefix:"indicator."
      ~defaults:(Gum_style.defaults ~foreground:"212" ())
      ()
  in
  let selected_prefix_style =
    Gum_style.term ~cmd:"filter" ~prefix:"selected-indicator."
      ~defaults:(Gum_style.defaults ~foreground:"212" ())
      ()
  in
  let unselected_prefix_style =
    Gum_style.term ~cmd:"filter" ~prefix:"unselected-prefix."
      ~defaults:(Gum_style.defaults ~foreground:"240" ())
      ()
  in
  let header_style =
    Gum_style.term ~cmd:"filter" ~prefix:"header."
      ~defaults:(Gum_style.defaults ~foreground:"99" ())
      ()
  in
  let text_style =
    Gum_style.term ~cmd:"filter" ~prefix:"text." ~defaults:Gum_style.empty ()
  in
  let cursor_text_style =
    Gum_style.term ~cmd:"filter" ~prefix:"cursor-text." ~defaults:Gum_style.empty ()
  in
  let match_style =
    Gum_style.term ~cmd:"filter" ~prefix:"match."
      ~defaults:(Gum_style.defaults ~foreground:"212" ())
      ()
  in
  let prompt_style =
    Gum_style.term ~cmd:"filter" ~prefix:"prompt."
      ~defaults:(Gum_style.defaults ~foreground:"240" ())
      ()
  in
  let placeholder_style =
    Gum_style.term ~cmd:"filter" ~prefix:"placeholder."
      ~defaults:(Gum_style.defaults ~foreground:"240" ())
      ()
  in
  let term =
    let+ options = options_arg
    and+ indicator =
      string_arg ~cmd:"filter" "indicator" ~default:"•" ~doc:"Selection indicator."
    and+ limit =
      int_arg ~cmd:"filter" "limit" ~default:1 ~doc:"Maximum number of options to pick."
    and+ no_limit = Gum_flag.flag ~cmd:"filter" ~doc:"Pick unlimited options." "no-limit"
    and+ select_if_one =
      Gum_flag.flag ~cmd:"filter" ~doc:"Select a sole match without a terminal."
        "select-if-one"
    and+ selected = selected_arg
    and+ show_help =
      Gum_flag.negatable ~cmd:"filter" ~default:true ~doc:"Show help keybinds."
        "show-help"
    and+ strict =
      Gum_flag.negatable ~cmd:"filter" ~default:true ~doc:"Require a matching option."
        "strict"
    and+ selected_prefix =
      string_arg ~cmd:"filter" "selected-prefix" ~default:" ◉ " ~doc:"Selected prefix."
    and+ unselected_prefix =
      string_arg ~cmd:"filter" "unselected-prefix" ~default:" ○ "
        ~doc:"Unselected prefix."
    and+ header = string_arg ~cmd:"filter" "header" ~default:"" ~doc:"Header value."
    and+ placeholder =
      string_arg ~cmd:"filter" "placeholder" ~default:"Filter..."
        ~doc:"Search placeholder."
    and+ prompt = string_arg ~cmd:"filter" "prompt" ~default:"> " ~doc:"Search prompt."
    and+ width =
      int_arg ~cmd:"filter" "width" ~default:0
        ~doc:"Input width (zero uses terminal width)."
    and+ height =
      int_arg ~cmd:"filter" "height" ~default:0
        ~doc:"List height (zero uses terminal height)."
    and+ value = string_arg ~cmd:"filter" "value" ~default:"" ~doc:"Initial filter value."
    and+ reverse =
      Gum_flag.flag ~cmd:"filter" ~doc:"Display matches from the bottom." "reverse"
    and+ fuzzy =
      Gum_flag.negatable ~cmd:"filter" ~default:true ~doc:"Enable fuzzy matching." "fuzzy"
    and+ fuzzy_sort =
      Gum_flag.negatable ~cmd:"filter" ~default:true ~doc:"Sort fuzzy matches by score."
        "fuzzy-sort"
    and+ timeout =
      Gum_flag.seconds ~cmd:"filter" ~doc:"Timeout until filtering." "timeout"
    and+ input_delimiter =
      Gum_flag.delimiter ~cmd:"filter" ~default:"\n" ~doc:"Input option delimiter."
        "input-delimiter"
    and+ output_delimiter =
      Gum_flag.delimiter ~cmd:"filter" ~default:"\n" ~doc:"Output option delimiter."
        "output-delimiter"
    and+ strip_ansi =
      Gum_flag.negatable ~cmd:"filter" ~default:true ~doc:"Strip ANSI from stdin."
        "strip-ansi"
    and+ padding = validated_padding_term ~cmd:"filter"
    and+ indicator_style = indicator_style
    and+ selected_prefix_style = selected_prefix_style
    and+ unselected_prefix_style = unselected_prefix_style
    and+ header_style = header_style
    and+ text_style = text_style
    and+ cursor_text_style = cursor_text_style
    and+ match_style = match_style
    and+ prompt_style = prompt_style
    and+ placeholder_style = placeholder_style in
    run env
      {
        options;
        indicator;
        limit;
        no_limit;
        select_if_one;
        selected;
        show_help;
        strict;
        selected_prefix;
        unselected_prefix;
        header;
        placeholder;
        prompt;
        width;
        height;
        value;
        reverse;
        fuzzy;
        fuzzy_sort;
        timeout;
        input_delimiter;
        output_delimiter;
        strip_ansi;
        padding;
        indicator_style;
        selected_prefix_style;
        unselected_prefix_style;
        header_style;
        text_style;
        cursor_text_style;
        match_style;
        prompt_style;
        placeholder_style;
      }
  in
  Cmd.v (Cmd.info "filter" ~doc:"Filter options with fuzzy or exact matching.") term
