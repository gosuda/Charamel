module Key = Charamel_tea.Key
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View
module Style = Charamel_lipgloss.Style
module Textarea = Charamel_bubbles.Textarea
module Key_binding = Charamel_bubbles.Key_binding

let key name =
  match Key.of_string name with
  | Ok value -> value
  | Error (`Msg message) -> invalid_arg (Fmt.str "invalid write key %s: %s" name message)

let k_enter = key "enter"
let k_escape = key "esc"
let k_ctrl_c = key "ctrl+c"
let k_ctrl_e = key "ctrl+e"
let is_key actual expected = Key.matches actual expected

type cursor_mode = Blink | Hide | Static

type options = {
  width : int;
  height : int;
  header : string;
  placeholder : string;
  prompt : string;
  show_cursor_line : bool;
  show_line_numbers : bool;
  value : string;
  char_limit : int;
  max_lines : int;
  show_help : bool;
  cursor_mode : cursor_mode;
  timeout : float option;
  strip_ansi : bool;
  padding : string;
  base_style : Gum_style.t;
  cursor_line_number_style : Gum_style.t;
  cursor_line_style : Gum_style.t;
  cursor_style : Gum_style.t;
  end_of_buffer_style : Gum_style.t;
  line_number_style : Gum_style.t;
  header_style : Gum_style.t;
  placeholder_style : Gum_style.t;
  prompt_style : Gum_style.t;
}

type msg =
  | Key of Key.t
  | Input of Textarea.msg
  | Resize of int * int
  | Editor_finished of string * int

type model = {
  options : options;
  textarea : Textarea.t;
  submitted : bool;
  quitting : bool;
  padding : Charamel_lipgloss.Sides.t;
}

let default_options =
  {
    width = 0;
    height = 5;
    header = "";
    placeholder = "Write something...";
    prompt = "┃ ";
    show_cursor_line = false;
    show_line_numbers = false;
    value = "";
    char_limit = 0;
    max_lines = 0;
    show_help = true;
    cursor_mode = Blink;
    timeout = None;
    strip_ansi = true;
    padding = "0 0";
    base_style = Gum_style.empty;
    cursor_line_number_style = Gum_style.defaults ~foreground:"7" ();
    cursor_line_style = Gum_style.empty;
    cursor_style = Gum_style.defaults ~foreground:"212" ();
    end_of_buffer_style = Gum_style.defaults ~foreground:"0" ();
    line_number_style = Gum_style.defaults ~foreground:"7" ();
    header_style = Gum_style.defaults ~foreground:"240" ();
    placeholder_style = Gum_style.defaults ~foreground:"240" ();
    prompt_style = Gum_style.defaults ~foreground:"7" ();
  }

let parsed_padding value =
  match Gum_flag.parse_padding value with
  | Ok sides -> sides
  | Error (`Msg message) -> invalid_arg message

let take count values =
  let rec loop remaining acc = function
    | [] -> List.rev acc
    | _ when remaining <= 0 -> List.rev acc
    | value :: rest -> loop (remaining - 1) (value :: acc) rest
  in
  loop count [] values

let remove_carriage_returns text =
  String.to_seq text |> Seq.filter (fun character -> character <> '\r') |> String.of_seq

let normalize_lines ~max_lines text =
  if max_lines <= 0 then text
  else String.concat "\n" (take max_lines (String.split_on_char '\n' text))

let initial_value env (options : options) =
  if options.value <> "" then remove_carriage_returns options.value
  else
    match Gum_io.read_stdin ~strip_ansi:options.strip_ansi env with
    | Ok value -> remove_carriage_returns value
    | Error `Empty -> ""
    | Error (`Read value) -> remove_carriage_returns value

let textarea_styles (options : options) : Textarea.styles =
  let styles = Textarea.default_styles ~is_dark:true in
  let map_state (state : Textarea.style_state) : Textarea.style_state =
    {
      state with
      base = Gum_style.to_style options.base_style;
      cursor_line =
        (if options.show_cursor_line then Gum_style.to_style options.cursor_line_style
         else Style.empty);
      cursor_line_number = Gum_style.to_style options.cursor_line_number_style;
      end_of_buffer = Gum_style.to_style options.end_of_buffer_style;
      line_number = Gum_style.to_style options.line_number_style;
      placeholder = Gum_style.to_style options.placeholder_style;
      prompt = Gum_style.to_style options.prompt_style;
    }
  in
  let cursor =
    {
      (match Gum_style.foreground options.cursor_style with
      | None -> styles.Textarea.cursor
      | Some color -> { styles.Textarea.cursor with color })
      with
      blink = options.cursor_mode = Blink;
    }
  in
  {
    focused = map_state styles.Textarea.focused;
    blurred = map_state styles.Textarea.blurred;
    cursor;
  }

let keymap =
  let insert_newline = Key_binding.v ~help:("ctrl+j", "insert newline") [ "ctrl+j" ] in
  { Textarea.default_keymap with insert_newline }

let make (options : options) =
  let textarea =
    Textarea.v ~prompt:options.prompt ~placeholder:options.placeholder
      ~show_line_numbers:options.show_line_numbers ~char_limit:options.char_limit
      ~max_height:10_000 ~width:options.width ~height:options.height ~keymap
      ~value:options.value ~styles:(textarea_styles options) ()
  in
  let textarea, _ = Textarea.focus textarea in
  let textarea = Textarea.set_virtual_cursor (options.cursor_mode <> Hide) textarea in
  {
    options;
    textarea;
    submitted = false;
    quitting = false;
    padding = parsed_padding options.padding;
  }

let value model = Textarea.value model.textarea
let submitted model = model.submitted

let within_line_limit (options : options) textarea =
  options.max_lines <= 0 || Textarea.line_count textarea <= options.max_lines

let update_component model component_message =
  let before = Textarea.value model.textarea in
  let textarea, command = Textarea.update component_message model.textarea in
  let textarea =
    if within_line_limit model.options textarea then textarea
    else Textarea.set_value before textarea
  in
  ({ model with textarea }, Cmd.map (fun message -> Input message) command)

let editor_command model =
  let path = Filename.temp_file "gum" ".md" in
  let output = open_out_bin path in
  output_string output (Textarea.value model.textarea);
  close_out output;
  let editor =
    match Sys.getenv_opt "EDITOR" with Some value when value <> "" -> value | _ -> "vi"
  in
  let command = Cmd.exec ~argv:[ editor; path ] (fun status -> status) in
  Cmd.map (fun status -> Editor_finished (path, status)) command

let read_editor path =
  let input = open_in_bin path in
  let length = in_channel_length input in
  let text = really_input_string input length in
  close_in input;
  text

let handle_key model key =
  if is_key key k_ctrl_c then ({ model with quitting = true }, Cmd.interrupt)
  else if is_key key k_escape then ({ model with quitting = true }, Cmd.quit)
  else if is_key key k_enter then
    ({ model with quitting = true; submitted = true }, Cmd.quit)
  else if is_key key k_ctrl_e then (model, editor_command model)
  else
    match Textarea.key model.textarea key with
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
    let content = header ^ Textarea.view model.textarea in
    let content =
      if model.options.show_help then
        content ^ "\n\nctrl+j newline • ctrl+e editor • enter submit • esc cancel"
      else content
    in
    Style.render (Style.padding model.padding Style.empty) content

let update message model =
  match message with
  | Key key -> handle_key model key
  | Input component_message -> update_component model component_message
  | Resize (rows, cols) ->
      let width = if model.options.width = 0 then max 0 cols else model.options.width in
      let height =
        if model.options.height = 0 then max 1 rows else model.options.height
      in
      let textarea = Textarea.set_width width model.textarea in
      let textarea = Textarea.set_height height textarea in
      ({ model with textarea }, Cmd.none)
  | Editor_finished (path, status) ->
      let content =
        if status = 0 then read_editor path else Textarea.value model.textarea
      in
      Sys.remove path;
      if status = 0 then
        ( {
            model with
            textarea =
              Textarea.set_value
                (normalize_lines ~max_lines:model.options.max_lines content)
                model.textarea;
          },
          Cmd.none )
      else ({ model with quitting = true }, Cmd.interrupt)

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

let run env (options : options) =
  let options =
    {
      options with
      value = normalize_lines ~max_lines:options.max_lines (initial_value env options);
    }
  in
  let model =
    try
      Gum_run.run ?timeout:options.timeout env (app options) ~finished:(fun model ->
          if submitted model then Gum_run.Submitted else Gum_run.Quit)
    with Gum_io.No_tty -> Charamel_cli.error "write: requires a terminal"
  in
  if not (submitted model) then Charamel_cli.error "not submitted";
  Gum_io.print_raw env (value model)

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
  let cursor_mode =
    let converter =
      Gum_flag.enum ~docv:"MODE" [ ("blink", Blink); ("hide", Hide); ("static", Static) ]
    in
    let info =
      Arg.info [ "cursor.mode" ] ~doc:"Cursor mode."
        ~env:(Gum_flag.env ~cmd:"write" "cursor.mode")
    in
    Arg.value (Arg.opt converter Blink info)
  in
  let term =
    let+ width = int_arg ~cmd:"write" "width" ~default:0 ~doc:"Text area width."
    and+ height = int_arg ~cmd:"write" "height" ~default:5 ~doc:"Text area height."
    and+ header = string_arg ~cmd:"write" "header" ~default:"" ~doc:"Header value."
    and+ placeholder =
      string_arg ~cmd:"write" "placeholder" ~default:"Write something..."
        ~doc:"Placeholder value."
    and+ prompt = string_arg ~cmd:"write" "prompt" ~default:"┃ " ~doc:"Prompt per line."
    and+ show_cursor_line =
      Gum_flag.flag ~cmd:"write" ~doc:"Highlight the cursor line." "show-cursor-line"
    and+ show_line_numbers =
      Gum_flag.flag ~cmd:"write" ~doc:"Show line numbers." "show-line-numbers"
    and+ value = string_arg ~cmd:"write" "value" ~default:"" ~doc:"Initial value."
    and+ char_limit =
      int_arg ~cmd:"write" "char-limit" ~default:0 ~doc:"Maximum character count."
    and+ max_lines =
      int_arg ~cmd:"write" "max-lines" ~default:0 ~doc:"Maximum logical lines."
    and+ show_help =
      Gum_flag.negatable ~cmd:"write" ~default:true ~doc:"Show help keybinds." "show-help"
    and+ cursor_mode = cursor_mode
    and+ timeout =
      Gum_flag.seconds ~cmd:"write" ~doc:"Timeout until write aborts." "timeout"
    and+ strip_ansi =
      Gum_flag.negatable ~cmd:"write" ~default:true ~doc:"Strip ANSI from stdin."
        "strip-ansi"
    and+ padding = validated_padding_term ~cmd:"write"
    and+ base_style =
      Gum_style.term ~cmd:"write" ~prefix:"base." ~defaults:Gum_style.empty ()
    and+ cursor_line_number_style =
      Gum_style.term ~cmd:"write" ~prefix:"cursor-line-number."
        ~defaults:(Gum_style.defaults ~foreground:"7" ())
        ()
    and+ cursor_line_style =
      Gum_style.term ~cmd:"write" ~prefix:"cursor-line." ~defaults:Gum_style.empty ()
    and+ cursor_style =
      Gum_style.term ~cmd:"write" ~prefix:"cursor."
        ~defaults:(Gum_style.defaults ~foreground:"212" ())
        ()
    and+ end_of_buffer_style =
      Gum_style.term ~cmd:"write" ~prefix:"end-of-buffer."
        ~defaults:(Gum_style.defaults ~foreground:"0" ())
        ()
    and+ line_number_style =
      Gum_style.term ~cmd:"write" ~prefix:"line-number."
        ~defaults:(Gum_style.defaults ~foreground:"7" ())
        ()
    and+ header_style =
      Gum_style.term ~cmd:"write" ~prefix:"header."
        ~defaults:(Gum_style.defaults ~foreground:"240" ())
        ()
    and+ placeholder_style =
      Gum_style.term ~cmd:"write" ~prefix:"placeholder."
        ~defaults:(Gum_style.defaults ~foreground:"240" ())
        ()
    and+ prompt_style =
      Gum_style.term ~cmd:"write" ~prefix:"prompt."
        ~defaults:(Gum_style.defaults ~foreground:"7" ())
        ()
    in
    run env
      {
        width;
        height;
        header;
        placeholder;
        prompt;
        show_cursor_line;
        show_line_numbers;
        value;
        char_limit;
        max_lines;
        show_help;
        cursor_mode;
        timeout;
        strip_ansi;
        padding;
        base_style;
        cursor_line_number_style;
        cursor_line_style;
        cursor_style;
        end_of_buffer_style;
        line_number_style;
        header_style;
        placeholder_style;
        prompt_style;
      }
  in
  Cmd.v (Cmd.info "write" ~doc:"Read multi-line text.") term
