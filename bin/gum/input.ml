module Key = Charamel_tea.Key
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View
module Style = Charamel_lipgloss.Style
module Textinput = Charamel_bubbles.Textinput

let key name =
  match Key.of_string name with
  | Ok value -> value
  | Error (`Msg message) -> invalid_arg (Fmt.str "invalid input key %s: %s" name message)

let k_enter = key "enter"
let k_escape = key "esc"
let k_ctrl_c = key "ctrl+c"
let is_key actual expected = Key.matches actual expected

type cursor_mode = Blink | Hide | Static

type options = {
  placeholder : string;
  prompt : string;
  cursor_mode : cursor_mode;
  value : string;
  char_limit : int;
  width : int;
  password : bool;
  show_help : bool;
  header : string;
  timeout : float option;
  strip_ansi : bool;
  padding : string;
  prompt_style : Gum_style.t;
  placeholder_style : Gum_style.t;
  cursor_style : Gum_style.t;
  header_style : Gum_style.t;
}

type msg = Key of Key.t | Input of Textinput.msg | Resize of int * int

type model = {
  options : options;
  input : Textinput.t;
  submitted : bool;
  quitting : bool;
  padding : Charamel_lipgloss.Sides.t;
}

let default_options =
  {
    placeholder = "Type something...";
    prompt = "> ";
    cursor_mode = Blink;
    value = "";
    char_limit = 400;
    width = 0;
    password = false;
    show_help = true;
    header = "";
    timeout = None;
    strip_ansi = true;
    padding = "0 0";
    prompt_style = Gum_style.empty;
    placeholder_style = Gum_style.defaults ~foreground:"240" ();
    cursor_style = Gum_style.defaults ~foreground:"212" ();
    header_style = Gum_style.defaults ~foreground:"240" ();
  }

let textinput_styles (options : options) : Textinput.styles =
  let styles = Textinput.default_styles ~is_dark:true in
  let prompt = Gum_style.to_style options.prompt_style in
  let placeholder = Gum_style.to_style options.placeholder_style in
  let focused = { styles.Textinput.focused with prompt; placeholder } in
  let blurred = { styles.Textinput.blurred with prompt; placeholder } in
  let cursor =
    {
      (match Gum_style.foreground options.cursor_style with
      | None -> styles.Textinput.cursor
      | Some color -> { styles.Textinput.cursor with color })
      with
      blink = options.cursor_mode = Blink;
    }
  in
  { focused; blurred; cursor }

let make (options : options) =
  let echo = if options.password then Textinput.Password else Textinput.Normal in
  let echo_character = if options.password then "•" else "*" in
  let input =
    Textinput.v ~prompt:options.prompt ~placeholder:options.placeholder ~echo
      ~echo_character ~char_limit:options.char_limit ~width:options.width
      ~value:options.value ~styles:(textinput_styles options) ()
  in
  let input, _ = Textinput.focus input in
  let input = Textinput.set_virtual_cursor (options.cursor_mode <> Hide) input in
  {
    options;
    input;
    submitted = false;
    quitting = false;
    padding = Gum_flag.parsed_padding options.padding;
  }

let initial_value env (options : options) =
  if options.value <> "" then options.value
  else
    match Gum_io.read_stdin ~strip_ansi:options.strip_ansi env with
    | Ok value -> value
    | Error `Empty -> ""
    | Error (`Read value) -> value

let value model = Textinput.value model.input
let submitted model = model.submitted

let update_component model component_message =
  let input, command = Textinput.update component_message model.input in
  ({ model with input }, Cmd.map (fun message -> Input message) command)

let handle_key model key =
  if is_key key k_ctrl_c then ({ model with quitting = true }, Cmd.interrupt)
  else if is_key key k_escape then ({ model with quitting = true }, Cmd.quit)
  else if is_key key k_enter then
    ({ model with quitting = true; submitted = true }, Cmd.quit)
  else
    match Textinput.key model.input key with
    | None -> (model, Cmd.none)
    | Some component_message -> update_component model component_message

let render model =
  if model.quitting then ""
  else
    let header =
      if model.options.header = "" then ""
      else
        Style.render (Gum_style.to_style model.options.header_style) model.options.header
        ^ "\n"
    in
    let content = header ^ Textinput.view model.input in
    let content =
      if model.options.show_help then content ^ "\n\nenter submit • esc cancel"
      else content
    in
    Style.render (Style.padding model.padding Style.empty) content

let update message model =
  match message with
  | Key key -> handle_key model key
  | Input component_message -> update_component model component_message
  | Resize (_rows, cols) ->
      let width = if model.options.width = 0 then max 0 cols else model.options.width in
      ({ model with input = Textinput.set_width width model.input }, Cmd.none)

let app options : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (make options, Cmd.none));
    update = (fun message model -> update message model);
    view = (fun model -> View.v (render model));
    subscriptions =
      (fun model ->
        Sub.batch
          [
            Sub.key (fun key ->
                match Textinput.key model.input key with
                | Some message -> Input message
                | None -> Key key);
            Sub.resize (fun ~rows ~cols -> Resize (rows, cols));
          ]);
  }

let run env (options : options) =
  let options = { options with value = initial_value env options } in
  let model =
    try
      Gum_run.run ?timeout:options.timeout env (app options) ~finished:(fun model ->
          if submitted model then Gum_run.Submitted else Gum_run.Quit)
    with Gum_io.No_tty -> Charamel_cli.error "input: requires a terminal"
  in
  if not (submitted model) then Charamel_cli.error "not submitted";
  Gum_io.print_raw env (value model)

let string_arg ~cmd name ~default ~doc =
  Cmdliner.Arg.(
    value (opt string default (info [ name ] ~doc ~env:(Gum_flag.env ~cmd name))))

let int_arg ~cmd name ~default ~doc =
  Cmdliner.Arg.(
    value (opt int default (info [ name ] ~doc ~env:(Gum_flag.env ~cmd name))))

let cmd env =
  let open Cmdliner in
  let open Term.Syntax in
  let cursor_mode =
    let converter =
      Gum_flag.enum ~docv:"MODE" [ ("blink", Blink); ("hide", Hide); ("static", Static) ]
    in
    let info =
      Arg.info [ "cursor.mode" ] ~doc:"Cursor mode."
        ~env:(Gum_flag.env ~cmd:"input" "cursor.mode")
    in
    Arg.value (Arg.opt converter Blink info)
  in
  let term =
    let+ placeholder =
      string_arg ~cmd:"input" "placeholder" ~default:"Type something..."
        ~doc:"Placeholder value."
    and+ prompt = string_arg ~cmd:"input" "prompt" ~default:"> " ~doc:"Prompt to display."
    and+ cursor_mode = cursor_mode
    and+ value = string_arg ~cmd:"input" "value" ~default:"" ~doc:"Initial value."
    and+ char_limit =
      int_arg ~cmd:"input" "char-limit" ~default:400
        ~doc:"Maximum value length (zero is unlimited)."
    and+ width =
      int_arg ~cmd:"input" "width" ~default:0
        ~doc:"Input width (zero uses terminal width)."
    and+ password = Gum_flag.flag ~cmd:"input" ~doc:"Mask input characters." "password"
    and+ show_help =
      Gum_flag.negatable ~cmd:"input" ~default:true ~doc:"Show help keybinds." "show-help"
    and+ header = string_arg ~cmd:"input" "header" ~default:"" ~doc:"Header value."
    and+ timeout =
      Gum_flag.seconds ~cmd:"input" ~doc:"Timeout until input aborts." "timeout"
    and+ strip_ansi =
      Gum_flag.negatable ~cmd:"input" ~default:true ~doc:"Strip ANSI from stdin."
        "strip-ansi"
    and+ padding = Gum_flag.validated_padding_term ~cmd:"input" ()
    and+ prompt_style =
      Gum_style.term ~cmd:"input" ~prefix:"prompt." ~defaults:Gum_style.empty ()
    and+ placeholder_style =
      Gum_style.term ~cmd:"input" ~prefix:"placeholder."
        ~defaults:(Gum_style.defaults ~foreground:"240" ())
        ()
    and+ cursor_style =
      Gum_style.term ~cmd:"input" ~prefix:"cursor."
        ~defaults:(Gum_style.defaults ~foreground:"212" ())
        ()
    and+ header_style =
      Gum_style.term ~cmd:"input" ~prefix:"header."
        ~defaults:(Gum_style.defaults ~foreground:"240" ())
        ()
    in
    run env
      {
        placeholder;
        prompt;
        cursor_mode;
        value;
        char_limit;
        width;
        password;
        show_help;
        header;
        timeout;
        strip_ansi;
        padding;
        prompt_style;
        placeholder_style;
        cursor_style;
        header_style;
      }
  in
  Cmd.v (Cmd.info "input" ~doc:"Read one line of text.") term
