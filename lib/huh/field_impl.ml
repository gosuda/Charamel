open Lwt.Syntax

type position = { is_first : bool; is_last : bool }

type ctx = {
  styles : Styles.t;
  keymap : Keymap.t;
  width : int;
  height : int;
  position : position;
  results : Results.t;
  env : Env.t;
}

type outcome = Stay | Next | Prev | Submit

type ('value, 'state, 'message) impl = {
  name : string;
  key : 'value Key.t option;
  msg_id : 'message Type.Id.t;
  init : ctx -> 'state -> 'state * 'message Charamel_tea.Cmd.t;
  reevaluate : ctx -> 'state -> 'state * 'message Charamel_tea.Cmd.t;
  on_key :
    ctx -> Charamel_tea.Key.t -> 'state -> 'state * 'message Charamel_tea.Cmd.t * outcome;
  on_paste : ctx -> string -> 'state -> 'state;
  update : ctx -> 'message -> 'state -> 'state * 'message Charamel_tea.Cmd.t;
  subscriptions : ctx -> 'state -> 'message Charamel_tea.Sub.t;
  view : ctx -> focused:bool -> 'state -> string;
  focus : ctx -> 'state -> 'state * 'message Charamel_tea.Cmd.t;
  blur : ctx -> 'state -> 'state;
  value : 'state -> 'value;
  error : 'state -> string option;
  skip : ctx -> 'state -> bool;
  zoom : 'state -> bool;
  key_binds : ctx -> 'state -> Charamel_bubbles.Key_binding.t list;
  run_accessible :
    name:string ->
    ctx ->
    out:(string -> unit Lwt.t) ->
    Accessible.reader ->
    'state ->
    'state Lwt.t;
  filtering_of : 'state -> bool option;
  set_filtering_of : bool -> 'state -> 'state;
  hovered : 'state -> string option;
}

type t = Field : ('value, 'state, 'message) impl * 'state -> t

let map_cmd id command =
  Charamel_tea.Cmd.map (fun message -> Field_msg.inject id message) command

let map_sub id subscription =
  Charamel_tea.Sub.map (fun message -> Field_msg.inject id message) subscription

let concat_nonempty parts =
  parts |> Stdlib.List.filter (fun s -> s <> "") |> String.concat "\n"

let field_label name = String.capitalize_ascii name ^ ":"

let field_style ctx focused =
  if focused then ctx.styles.Styles.focused else ctx.styles.Styles.blurred

let textinput_styles (styles : Styles.t) : Charamel_bubbles.Textinput.styles =
  let defaults = Charamel_bubbles.Textinput.default_styles ~is_dark:true in
  let state (default : Charamel_bubbles.Textinput.style_state)
      (custom : Styles.text_input) : Charamel_bubbles.Textinput.style_state =
    {
      text = custom.Styles.text;
      placeholder = custom.Styles.placeholder;
      suggestion = default.Charamel_bubbles.Textinput.suggestion;
      prompt = custom.Styles.prompt;
    }
  in
  {
    focused =
      state defaults.Charamel_bubbles.Textinput.focused
        styles.Styles.focused.Styles.text_input;
    blurred =
      state defaults.Charamel_bubbles.Textinput.blurred
        styles.Styles.blurred.Styles.text_input;
    cursor = defaults.Charamel_bubbles.Textinput.cursor;
  }

let textarea_styles (styles : Styles.t) : Charamel_bubbles.Textarea.styles =
  let defaults = Charamel_bubbles.Textarea.default_styles ~is_dark:true in
  let state (base : Charamel_bubbles.Textarea.style_state) (custom : Styles.text_input) :
      Charamel_bubbles.Textarea.style_state =
    {
      base = base.Charamel_bubbles.Textarea.base;
      text = custom.Styles.text;
      line_number = base.Charamel_bubbles.Textarea.line_number;
      cursor_line_number = base.Charamel_bubbles.Textarea.cursor_line_number;
      cursor_line = base.Charamel_bubbles.Textarea.cursor_line;
      end_of_buffer = base.Charamel_bubbles.Textarea.end_of_buffer;
      placeholder = custom.Styles.placeholder;
      prompt = custom.Styles.prompt;
      selection = base.Charamel_bubbles.Textarea.selection;
    }
  in
  {
    focused =
      state defaults.Charamel_bubbles.Textarea.focused
        styles.Styles.focused.Styles.text_input;
    blurred =
      state defaults.Charamel_bubbles.Textarea.blurred
        styles.Styles.blurred.Styles.text_input;
    cursor = defaults.Charamel_bubbles.Textarea.cursor;
  }

let render_title_description ctx ~focused ~title ~description ~error =
  let style = field_style ctx focused in
  let title =
    if title = "" then ""
    else
      let rendered = Charamel_lipgloss.Style.render style.Styles.title title in
      match error with
      | None -> rendered
      | Some _ ->
          rendered
          ^ Charamel_lipgloss.Style.render style.Styles.error_indicator
              style.Styles.indicators.Styles.error_indicator
  in
  let description =
    if description = "" then ""
    else
      Charamel_lipgloss.Style.render style.Styles.description
        (Charamel_ansi.Text.wrap ~width:(max 1 ctx.width) description)
  in
  concat_nonempty [ title; description ]

let raw_matches key (binding : Charamel_bubbles.Key_binding.t) =
  Stdlib.List.exists
    (fun candidate -> Charamel_tea.Key.matches key candidate)
    binding.Charamel_bubbles.Key_binding.keys

let binding_enabled binding enabled =
  Charamel_bubbles.Key_binding.set_enabled enabled binding

let navigation_binds ctx ~prev ~next ~submit =
  [
    binding_enabled prev (not ctx.position.is_first);
    binding_enabled next (not ctx.position.is_last);
    binding_enabled submit ctx.position.is_last;
  ]

let grapheme_count text = Stdlib.List.length (Charamel_ansi.Width.graphemes text)

let validate_string ~char_limit ~validate value =
  match char_limit with
  | Some limit when limit > 0 && grapheme_count value > limit ->
      Error (Fmt.str "Input cannot exceed %d characters" limit)
  | _ -> validate value

let noop_string _ = Ok ()
let noop_bool _ = Ok ()
let noop_list _ = Ok ()

type input_message = Input_edit of Charamel_bubbles.Textinput.msg

type input_state = {
  textinput : Charamel_bubbles.Textinput.t;
  title : string Dyn.t;
  description : string Dyn.t;
  placeholder : string Dyn.t;
  prompt : string;
  suggestions : string list Dyn.t;
  char_limit : int;
  echo : Charamel_bubbles.Textinput.echo;
  validate : string -> (unit, string) result;
  inline : bool;
  err : string option;
  last_version : int;
  width : int;
}

let input_msg_id : input_message Type.Id.t = Type.Id.make ()

let make_input_textinput ~prompt ~placeholder ~suggestions ~char_limit ~echo ~value ~width
    =
  Charamel_bubbles.Textinput.v ~prompt ~placeholder ~echo ~echo_character:"•" ~char_limit
    ~width ~show_suggestions:(suggestions <> []) ~suggestions ~value ()

let input_evaluate state results ~width ~styles =
  if state.last_version = Results.version results && state.width = width then state
  else
    let suggestions = Dyn.eval state.suggestions results in
    let textinput =
      state.textinput
      |> Charamel_bubbles.Textinput.set_suggestions suggestions
      |> Charamel_bubbles.Textinput.set_show_suggestions (suggestions <> [])
      |> Charamel_bubbles.Textinput.set_char_limit state.char_limit
      |> Charamel_bubbles.Textinput.set_prompt state.prompt
      |> Charamel_bubbles.Textinput.set_placeholder (Dyn.eval state.placeholder results)
      |> Charamel_bubbles.Textinput.set_width (max 1 width)
      |> Charamel_bubbles.Textinput.set_styles (textinput_styles styles)
    in
    { state with textinput; last_version = Results.version results; width }

let input_validate state =
  validate_string ~char_limit:(Some state.char_limit) ~validate:state.validate

let input_init (ctx : ctx) state =
  let state = input_evaluate state ctx.results ~width:ctx.width ~styles:ctx.styles in
  (state, Charamel_tea.Cmd.none)

let input_reevaluate (ctx : ctx) state =
  ( input_evaluate state ctx.results ~width:ctx.width ~styles:ctx.styles,
    Charamel_tea.Cmd.none )

let input_edit _ctx message state =
  let textinput, command = Charamel_bubbles.Textinput.update message state.textinput in
  let command = Charamel_tea.Cmd.map (fun child -> Input_edit child) command in
  ({ state with textinput; err = None }, command)

let input_on_key ctx key state =
  let state = { state with err = None } in
  let km = ctx.keymap.Keymap.input in
  if raw_matches key km.Keymap.prev && not ctx.position.is_first then
    (state, Charamel_tea.Cmd.none, Prev)
  else if raw_matches key km.Keymap.next && not ctx.position.is_last then
    match input_validate state (Charamel_bubbles.Textinput.value state.textinput) with
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
    | Ok () -> (state, Charamel_tea.Cmd.none, Next)
  else if raw_matches key km.Keymap.submit && ctx.position.is_last then
    match input_validate state (Charamel_bubbles.Textinput.value state.textinput) with
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
    | Ok () -> (state, Charamel_tea.Cmd.none, Submit)
  else if raw_matches key km.Keymap.accept_suggestion then
    let textinput, command =
      Charamel_bubbles.Textinput.update Charamel_bubbles.Textinput.Accept_suggestion
        state.textinput
    in
    let command = Charamel_tea.Cmd.map (fun child -> Input_edit child) command in
    ({ state with textinput }, command, Stay)
  else
    match Charamel_bubbles.Textinput.key state.textinput key with
    | None -> (state, Charamel_tea.Cmd.none, Stay)
    | Some message ->
        let state, command = input_edit ctx message state in
        (state, command, Stay)

let input_update _ctx message state =
  match message with
  | Input_edit child ->
      let state, command = input_edit _ctx child state in
      (state, command)

let input_paste _ text state =
  let textinput = Charamel_bubbles.Textinput.paste text state.textinput in
  { state with textinput; err = None }

let input_focus _ctx state =
  let textinput, command = Charamel_bubbles.Textinput.focus state.textinput in
  let command = Charamel_tea.Cmd.map (fun child -> Input_edit child) command in
  ({ state with textinput }, command)

let input_blur _ state =
  let textinput = Charamel_bubbles.Textinput.blur state.textinput in
  let err =
    match input_validate state (Charamel_bubbles.Textinput.value state.textinput) with
    | Ok () -> None
    | Error error -> Some error
  in
  { state with textinput; err }

let input_view ctx ~focused state =
  let title = Dyn.eval state.title ctx.results in
  let description = Dyn.eval state.description ctx.results in
  let text = render_title_description ctx ~focused ~title ~description ~error:state.err in
  let input = Charamel_bubbles.Textinput.view state.textinput in
  let content =
    if state.inline && title <> "" then String.concat " " [ text; input ]
    else concat_nonempty [ text; input ]
  in
  Charamel_lipgloss.Style.render (field_style ctx focused).Styles.base content

let input_key_binds ctx _state =
  let km = ctx.keymap.Keymap.input in
  navigation_binds ctx ~prev:km.Keymap.prev ~next:km.Keymap.next ~submit:km.Keymap.submit
  @ [ km.Keymap.accept_suggestion ]

let input_accessible ~name ctx ~out reader state =
  let title = Dyn.eval state.title ctx.results in
  let prompt = (if title = "" then field_label name else title) ^ " " in
  let validate value = input_validate state value in
  let* value =
    match state.echo with
    | Charamel_bubbles.Textinput.Password ->
        Accessible.prompt_password ~out reader ~prompt ~validate
    | Charamel_bubbles.Textinput.No_echo | Charamel_bubbles.Textinput.Normal ->
        Accessible.prompt_string ~out reader ~prompt
          ~default:(Charamel_bubbles.Textinput.value state.textinput)
          ~validate
  in
  let textinput = Charamel_bubbles.Textinput.set_value value state.textinput in
  Lwt.return { state with textinput; err = None }

let input_impl key title description placeholder prompt char_limit suggestions echo inline
    default validate =
  let textinput =
    make_input_textinput ~prompt
      ~placeholder:(Dyn.eval placeholder Results.empty)
      ~suggestions:[] ~char_limit ~echo ~value:default ~width:80
  in
  let state =
    {
      textinput;
      title;
      description;
      placeholder;
      prompt;
      suggestions;
      char_limit;
      echo;
      validate;
      inline;
      err = None;
      last_version = -1;
      width = 80;
    }
  in
  let impl =
    {
      name = "input";
      key = Some key;
      msg_id = input_msg_id;
      init = input_init;
      reevaluate = input_reevaluate;
      on_key = input_on_key;
      on_paste = input_paste;
      update = input_update;
      subscriptions =
        (fun _ state ->
          Charamel_tea.Sub.map
            (fun message -> Input_edit message)
            (Charamel_bubbles.Textinput.subscriptions state.textinput));
      view = input_view;
      focus = input_focus;
      blur = input_blur;
      value = (fun state -> Charamel_bubbles.Textinput.value state.textinput);
      error = (fun state -> state.err);
      skip = (fun _ _ -> false);
      zoom = (fun _ -> false);
      key_binds = input_key_binds;
      run_accessible = input_accessible;
      filtering_of = (fun _ -> None);
      set_filtering_of = (fun _ state -> state);
      hovered = (fun _ -> None);
    }
  in
  Field (impl, state)

type text_message =
  | Text_edit of Charamel_bubbles.Textarea.msg
  | Editor_done of string * int

type text_state = {
  textarea : Charamel_bubbles.Textarea.t;
  title : string Dyn.t;
  description : string Dyn.t;
  placeholder : string Dyn.t;
  lines : int;
  char_limit : int;
  show_line_numbers : bool;
  editor : bool;
  editor_extension : string;
  validate : string -> (unit, string) result;
  err : string option;
  last_version : int;
  counter : int;
  editor_path : string option;
  width : int;
}

let text_msg_id : text_message Type.Id.t = Type.Id.make ()

let make_textarea ~placeholder ~lines ~char_limit ~show_line_numbers ~value ~width =
  Charamel_bubbles.Textarea.v ~prompt:"┃ " ~placeholder ~show_line_numbers ~char_limit
    ~height:(max 1 lines) ~width ~value ()

let text_validate state =
  validate_string ~char_limit:(Some state.char_limit) ~validate:state.validate

let text_reevaluate (ctx : ctx) state =
  if state.last_version = Results.version ctx.results && state.width = ctx.width then
    state
  else
    {
      state with
      textarea =
        state.textarea
        |> Charamel_bubbles.Textarea.set_placeholder
             (Dyn.eval state.placeholder ctx.results)
        |> Charamel_bubbles.Textarea.set_char_limit state.char_limit
        |> Charamel_bubbles.Textarea.set_show_line_numbers state.show_line_numbers
        |> Charamel_bubbles.Textarea.set_width (max 1 ctx.width)
        |> Charamel_bubbles.Textarea.set_height (max 1 state.lines)
        |> Charamel_bubbles.Textarea.set_styles (textarea_styles ctx.styles);
      last_version = Results.version ctx.results;
      width = ctx.width;
    }

let text_init ctx state =
  let state = text_reevaluate ctx state in
  (state, Charamel_tea.Cmd.none)

let read_whole_file path =
  match Stdlib.open_in path with
  | exception Sys_error _ -> None
  | channel -> (
      match
        Fun.protect
          ~finally:(fun () -> Stdlib.close_in channel)
          (fun () ->
            Stdlib.really_input_string channel (Stdlib.in_channel_length channel))
      with
      | text -> Some text
      | exception Sys_error _ -> None)

let text_update ctx message state =
  match message with
  | Text_edit child_message ->
      let textarea, command =
        Charamel_bubbles.Textarea.update child_message state.textarea
      in
      let command = Charamel_tea.Cmd.map (fun child -> Text_edit child) command in
      ({ state with textarea; err = None }, command)
  | Editor_done (path, code) ->
      let state = { state with editor_path = None } in
      let file = Filename.concat ctx.env.Env.temp_dir path in
      let read_result = if code = 0 then read_whole_file file else None in
      let state =
        match read_result with
        | Some value ->
            {
              state with
              textarea = Charamel_bubbles.Textarea.set_value value state.textarea;
            }
        | None when code = 0 -> { state with err = Some "cannot read editor file" }
        | None -> state
      in
      (match Stdlib.Sys.remove file with () -> () | exception Sys_error _ -> ());
      (state, Charamel_tea.Cmd.none)

let editor_words env = match env.Env.editor with Some argv -> argv | None -> []
let editor_enabled env = editor_words env <> []

let write_new_file path text =
  let flags = [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] in
  let buffer = Bytes.of_string text in
  match Unix.openfile path flags 0o600 with
  | exception Unix.Unix_error (Unix.EEXIST, _, _) -> `Exists
  | exception Unix.Unix_error _ -> `Failed
  | fd ->
      let length = Bytes.length buffer in
      let rec write offset =
        if offset >= length then `Written
        else
          match Unix.write fd buffer offset (length - offset) with
          | 0 -> `Failed
          | written -> write (offset + written)
          | exception Unix.Unix_error _ -> `Failed
      in
      Fun.protect ~finally:(fun () -> Unix.close fd) (fun () -> write 0)

let text_make_editor_file ctx state =
  let rec attempt counter =
    if counter > state.counter + 100 then Error "cannot create editor file"
    else
      let filename =
        Fmt.str "huh-%d-%d.%s" (Unix.getpid ()) counter state.editor_extension
      in
      let path = Filename.concat ctx.env.Env.temp_dir filename in
      match write_new_file path (Charamel_bubbles.Textarea.value state.textarea) with
      | `Exists -> attempt (counter + 1)
      | `Failed -> Error "cannot create editor file"
      | `Written -> Ok (filename, counter + 1)
  in
  attempt state.counter

let text_on_key ctx key state =
  let state = { state with err = None } in
  let km = ctx.keymap.Keymap.text in
  if raw_matches key km.Keymap.prev && not ctx.position.is_first then
    (state, Charamel_tea.Cmd.none, Prev)
  else if raw_matches key km.Keymap.next && not ctx.position.is_last then
    match text_validate state (Charamel_bubbles.Textarea.value state.textarea) with
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
    | Ok () -> (state, Charamel_tea.Cmd.none, Next)
  else if raw_matches key km.Keymap.submit && ctx.position.is_last then
    match text_validate state (Charamel_bubbles.Textarea.value state.textarea) with
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
    | Ok () -> (state, Charamel_tea.Cmd.none, Submit)
  else if raw_matches key km.Keymap.new_line then
    let textarea = Charamel_bubbles.Textarea.insert_string "\n" state.textarea in
    ({ state with textarea }, Charamel_tea.Cmd.none, Stay)
  else if
    raw_matches key km.Keymap.editor
    && state.editor && state.editor_path = None && editor_enabled ctx.env
  then
    match text_make_editor_file ctx state with
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
    | Ok (path, counter) ->
        let command =
          Charamel_tea.Cmd.exec
            ~argv:(editor_words ctx.env @ [ Filename.concat ctx.env.Env.temp_dir path ])
            (fun code -> Editor_done (path, code))
        in
        ({ state with counter; editor_path = Some path }, command, Stay)
  else
    match Charamel_bubbles.Textarea.key state.textarea key with
    | None -> (state, Charamel_tea.Cmd.none, Stay)
    | Some child_message ->
        let textarea, command =
          Charamel_bubbles.Textarea.update child_message state.textarea
        in
        let command = Charamel_tea.Cmd.map (fun child -> Text_edit child) command in
        ({ state with textarea }, command, Stay)

let text_paste _ text state =
  {
    state with
    textarea = Charamel_bubbles.Textarea.paste text state.textarea;
    err = None;
  }

let text_focus _ctx state =
  let textarea, command = Charamel_bubbles.Textarea.focus state.textarea in
  let command = Charamel_tea.Cmd.map (fun child -> Text_edit child) command in
  ({ state with textarea }, command)

let text_blur _ state =
  let textarea = Charamel_bubbles.Textarea.blur state.textarea in
  let err =
    match text_validate state (Charamel_bubbles.Textarea.value state.textarea) with
    | Ok () -> None
    | Error error -> Some error
  in
  { state with textarea; err }

let text_view ctx ~focused state =
  let title = Dyn.eval state.title ctx.results in
  let description = Dyn.eval state.description ctx.results in
  let heading =
    render_title_description ctx ~focused ~title ~description ~error:state.err
  in
  let style = field_style ctx focused in
  let body =
    match state.editor_path with
    | None -> concat_nonempty [ heading; Charamel_bubbles.Textarea.view state.textarea ]
    | Some path ->
        concat_nonempty
          [
            heading;
            Charamel_bubbles.Textarea.view state.textarea;
            Charamel_lipgloss.Style.render style.Styles.next (Fmt.str "editing: %s" path);
          ]
  in
  Charamel_lipgloss.Style.render style.Styles.base body

let text_key_binds ctx _state =
  let km = ctx.keymap.Keymap.text in
  navigation_binds ctx ~prev:km.Keymap.prev ~next:km.Keymap.next ~submit:km.Keymap.submit
  @ [ km.Keymap.new_line; km.Keymap.editor ]

let text_accessible ~name ctx ~out reader state =
  let title = Dyn.eval state.title ctx.results in
  let prompt = (if title = "" then field_label name else title) ^ " " in
  let* value =
    Accessible.prompt_string ~out reader ~prompt
      ~default:(Charamel_bubbles.Textarea.value state.textarea)
      ~validate:(text_validate state)
  in
  Lwt.return
    {
      state with
      textarea = Charamel_bubbles.Textarea.set_value value state.textarea;
      err = None;
    }

let text_impl key title description placeholder lines char_limit show_line_numbers editor
    editor_extension default validate =
  let textarea =
    make_textarea
      ~placeholder:(Dyn.eval placeholder Results.empty)
      ~lines ~char_limit ~show_line_numbers ~value:default ~width:80
  in
  let state =
    {
      textarea;
      title;
      description;
      placeholder;
      lines;
      char_limit;
      show_line_numbers;
      editor;
      editor_extension;
      validate;
      err = None;
      last_version = -1;
      counter = 0;
      editor_path = None;
      width = 80;
    }
  in
  let impl =
    {
      name = "text";
      key = Some key;
      msg_id = text_msg_id;
      init = text_init;
      reevaluate = (fun ctx state -> (text_reevaluate ctx state, Charamel_tea.Cmd.none));
      on_key = text_on_key;
      on_paste = text_paste;
      update = text_update;
      subscriptions =
        (fun _ state ->
          Charamel_tea.Sub.map
            (fun message -> Text_edit message)
            (Charamel_bubbles.Textarea.subscriptions state.textarea));
      view = text_view;
      focus = text_focus;
      blur = text_blur;
      value = (fun state -> Charamel_bubbles.Textarea.value state.textarea);
      error = (fun state -> state.err);
      skip = (fun _ _ -> false);
      zoom = (fun _ -> false);
      key_binds = text_key_binds;
      run_accessible = text_accessible;
      filtering_of = (fun _ -> None);
      set_filtering_of = (fun _ state -> state);
      hovered = (fun _ -> None);
    }
  in
  Field (impl, state)

type 'a option_ = { key : string; value : 'a }
type arity = One | Many

type 'a picker_message =
  | Filter of Charamel_bubbles.Textinput.msg
  | Options_loaded of 'a option_ list

type 'a picker_state = {
  filter_input : Charamel_bubbles.Textinput.t;
  title : string Dyn.t;
  description : string Dyn.t;
  options : 'a option_ list Dyn.t;
  all : 'a option_ list;
  filtered : 'a option_ list;
  selected : bool list;
  height : int;
  limit : int;
  inline : bool;
  filterable : bool;
  explicit_width : int option;
  defaults : string list;
  validate_one : 'a -> (unit, string) result;
  validate_many : 'a list -> (unit, string) result;
  cursor : int;
  y_offset : int;
  filtering : bool;
  err : string option;
  last_version : int;
  width : int;
  loading_since : float option;
}

let lower_fold text =
  let buffer = Buffer.create (String.length text) in
  let length = String.length text in
  let rec loop offset =
    if offset >= length then ()
    else begin
      let decoded = String.get_utf_8_uchar text offset in
      if Uchar.utf_decode_is_valid decoded then
        let u = Uchar.utf_decode_uchar decoded in
        let mapped =
          match Uucp.Case.Map.to_lower u with
          | `Self -> u
          | `Uchars (first :: _) -> first
          | `Uchars [] -> u
        in
        Buffer.add_utf_8_uchar buffer mapped
      else Buffer.add_utf_8_uchar buffer (Uchar.of_int 0xfffd);
      loop (offset + max 1 (Uchar.utf_decode_length decoded))
    end
  in
  loop 0;
  Buffer.contents buffer

let filter_options query options =
  let query = lower_fold query in
  if query = "" then options
  else
    Stdlib.List.filter
      (fun option_ ->
        let key = lower_fold option_.key in
        let qlen = String.length query in
        let rec contains offset =
          if offset + qlen > String.length key then false
          else if String.sub key offset qlen = query then true
          else contains (offset + 1)
        in
        contains 0)
      options

let check_unique_options options =
  let rec loop seen = function
    | [] -> ()
    | option_ :: rest ->
        if Stdlib.List.mem option_.key seen then
          invalid_arg (Fmt.str "Charamel_huh.Field: duplicate option key %S" option_.key)
        else loop (option_.key :: seen) rest
  in
  loop [] options

let option_key_at index options =
  Option.map (fun option_ -> option_.key) (List.nth_opt options index)

let index_of_key key options =
  let rec loop index = function
    | [] -> None
    | option_ :: rest ->
        if String.equal key option_.key then Some index else loop (index + 1) rest
  in
  loop 0 options

let clamp_cursor cursor options =
  if options = [] then 0 else min (max 0 cursor) (Stdlib.List.length options - 1)

let bools_for_options options keys =
  Stdlib.List.map (fun option_ -> Stdlib.List.mem option_.key keys) options

let selected_values state =
  let rec loop values selected options =
    match (selected, options) with
    | [], _ | _, [] -> values
    | flag :: flags, option_ :: rest ->
        loop (if flag then values @ [ option_.value ] else values) flags rest
  in
  loop [] state.selected state.all

let picker_effective_width (state : _ picker_state) (ctx : ctx) =
  match state.explicit_width with
  | Some width -> max 1 (min width ctx.width)
  | None -> max 1 ctx.width

let picker_recompute ?(apply_default = true) arity (state : _ picker_state) all old_key =
  check_unique_options all;
  let filtered =
    filter_options (Charamel_bubbles.Textinput.value state.filter_input) all
  in
  let cursor_for filtered =
    match old_key with
    | Some key -> (
        match index_of_key key filtered with Some index -> index | None -> 0)
    | None -> clamp_cursor state.cursor filtered
  in
  match arity with
  | One ->
      let cursor =
        match old_key with
        | Some key -> (
            match index_of_key key filtered with Some index -> index | None -> 0)
        | None when apply_default -> (
            match state.defaults with
            | [ key ] -> (
                match index_of_key key filtered with Some index -> index | None -> 0)
            | _ -> clamp_cursor state.cursor filtered)
        | None -> 0
      in
      { state with all; filtered; cursor; y_offset = 0 }
  | Many ->
      let rec assoc key flags options =
        match (flags, options) with
        | flag :: flags, option_ :: options ->
            if String.equal key option_.key then Some flag else assoc key flags options
        | _ -> None
      in
      let selected =
        Stdlib.List.map
          (fun option_ ->
            match assoc option_.key state.selected state.all with
            | Some flag -> flag
            | None -> Stdlib.List.mem option_.key state.defaults)
          all
      in
      let cursor = cursor_for filtered in
      { state with all; filtered; selected; cursor; y_offset = 0 }

let picker_reevaluate arity (ctx : ctx) (state : _ picker_state) =
  if state.last_version = Results.version ctx.results && state.width = ctx.width then
    (state, Charamel_tea.Cmd.none)
  else
    let width = picker_effective_width state ctx in
    let resized state =
      {
        state with
        filter_input = Charamel_bubbles.Textinput.set_width width state.filter_input;
        width = ctx.width;
      }
    in
    match state.options with
    | Dyn.Of_results_async _ when state.loading_since <> None ->
        (state, Charamel_tea.Cmd.none)
    | Dyn.Of_results_async produce ->
        let results = ctx.results in
        let command =
          Charamel_tea.Cmd.perform (fun () -> Options_loaded (produce results))
        in
        let since = Charamel_os.Time.now ctx.env.Env.clock in
        (resized { state with loading_since = Some since }, command)
    | _ ->
        let old_key = option_key_at state.cursor state.filtered in
        let all = Dyn.eval state.options ctx.results in
        let state = picker_recompute arity state all old_key in
        let state =
          { state with last_version = Results.version ctx.results; loading_since = None }
        in
        (resized state, Charamel_tea.Cmd.none)

let picker_init arity (ctx : ctx) (state : _ picker_state) =
  let all = Dyn.eval state.options ctx.results in
  let state = picker_recompute arity state all None in
  let width = picker_effective_width state ctx in
  ( {
      state with
      filter_input = Charamel_bubbles.Textinput.set_width width state.filter_input;
      width = ctx.width;
      last_version = Results.version ctx.results;
    },
    Charamel_tea.Cmd.none )

let picker_move_by delta state =
  let length = Stdlib.List.length state.filtered in
  if length = 0 then state
  else { state with cursor = (state.cursor + delta + length) mod length }

let picker_set_filtering filtering state =
  let filter_input =
    if filtering then Charamel_bubbles.Textinput.focus state.filter_input |> fst
    else Charamel_bubbles.Textinput.blur state.filter_input
  in
  { state with filtering; filter_input }

let picker_selected_at index state =
  match List.nth_opt state.filtered index with
  | None -> false
  | Some option_ ->
      let rec lookup option_ flags options =
        match (flags, options) with
        | flag :: _, candidate :: _ when String.equal option_.key candidate.key -> flag
        | _ :: flags, _ :: options -> lookup option_ flags options
        | _ -> false
      in
      lookup option_ state.selected state.all

let picker_set_selected_key key value state =
  let selected =
    Stdlib.List.map2
      (fun flag option_ -> if String.equal option_.key key then value else flag)
      state.selected state.all
  in
  { state with selected }

let picker_all_filtered_selected state =
  state.filtered <> []
  && Stdlib.List.for_all
       (fun option_ ->
         match index_of_key option_.key state.filtered with
         | None -> false
         | Some index -> picker_selected_at index state)
       state.filtered

let picker_toggle_current state =
  match List.nth_opt state.filtered state.cursor with
  | None -> state
  | Some option_ ->
      let selected = picker_selected_at state.cursor state in
      if
        (not selected) && state.limit > 0
        && List.length (selected_values state) >= state.limit
      then state
      else picker_set_selected_key option_.key (not selected) state

let picker_validate arity state =
  match arity with
  | One -> (
      match List.nth_opt state.filtered state.cursor with
      | None -> Error "no options available"
      | Some option_ -> state.validate_one option_.value)
  | Many -> state.validate_many (selected_values state)

type picker_keymap = {
  prev : Keymap.binding;
  next : Keymap.binding;
  submit : Keymap.binding;
  up : Keymap.binding;
  down : Keymap.binding;
  left : Keymap.binding;
  right : Keymap.binding;
  filter : Keymap.binding;
  set_filter : Keymap.binding;
  clear_filter : Keymap.binding;
  half_page_up : Keymap.binding;
  half_page_down : Keymap.binding;
  goto_top : Keymap.binding;
  goto_bottom : Keymap.binding;
  toggle : Keymap.binding;
  select_all : Keymap.binding;
  select_none : Keymap.binding;
}

let picker_keymap ctx arity =
  let select = ctx.keymap.Keymap.select in
  let multi = ctx.keymap.Keymap.multi_select in
  match arity with
  | One ->
      {
        prev = select.prev;
        next = select.next;
        submit = select.submit;
        up = select.up;
        down = select.down;
        left = select.left;
        right = select.right;
        filter = select.filter;
        set_filter = select.set_filter;
        clear_filter = select.clear_filter;
        half_page_up = select.half_page_up;
        half_page_down = select.half_page_down;
        goto_top = select.goto_top;
        goto_bottom = select.goto_bottom;
        toggle = multi.toggle;
        select_all = multi.select_all;
        select_none = multi.select_none;
      }
  | Many ->
      {
        prev = multi.prev;
        next = multi.next;
        submit = multi.submit;
        up = multi.up;
        down = multi.down;
        left = select.left;
        right = select.right;
        filter = multi.filter;
        set_filter = multi.set_filter;
        clear_filter = multi.clear_filter;
        half_page_up = multi.half_page_up;
        half_page_down = multi.half_page_down;
        goto_top = multi.goto_top;
        goto_bottom = multi.goto_bottom;
        toggle = multi.toggle;
        select_all = multi.select_all;
        select_none = multi.select_none;
      }

let picker_on_key arity (ctx : ctx) key (state : _ picker_state) =
  let state = { state with err = None } in
  let km = picker_keymap ctx arity in
  let recompute ?(apply_default = true) state all old_key =
    picker_recompute ~apply_default arity state all old_key
  in
  let none = Charamel_tea.Cmd.none in
  if state.filtering then
    if
      raw_matches key km.set_filter
      || arity = One
         && Charamel_tea.Key.matches key (Charamel_tea.Key.v Charamel_tea.Key.Escape)
    then begin
      let state =
        if state.filtered = [] then
          let filter_input = Charamel_bubbles.Textinput.set_value "" state.filter_input in
          recompute ~apply_default:false { state with filter_input }
            (Dyn.eval state.options ctx.results)
            None
        else state
      in
      ({ state with filtering = false }, none, Stay)
    end
    else if raw_matches key km.up then (picker_move_by (-1) state, none, Stay)
    else if raw_matches key km.down then (picker_move_by 1 state, none, Stay)
    else if arity = One && raw_matches key km.goto_top then
      ({ state with cursor = 0 }, none, Stay)
    else if arity = One && raw_matches key km.goto_bottom then
      ({ state with cursor = max 0 (Stdlib.List.length state.filtered - 1) }, none, Stay)
    else
      match Charamel_bubbles.Textinput.key state.filter_input key with
      | None -> (state, none, Stay)
      | Some child ->
          let filter_input, command =
            Charamel_bubbles.Textinput.update child state.filter_input
          in
          let all = Dyn.eval state.options ctx.results in
          let state = { state with filter_input; err = None } in
          ( recompute ~apply_default:false state all None,
            Charamel_tea.Cmd.map (fun child -> Filter child) command,
            Stay )
  else if state.filterable && raw_matches key km.filter then
    (picker_set_filtering true state, none, Stay)
  else if
    raw_matches key km.clear_filter
    && Charamel_bubbles.Textinput.value state.filter_input <> ""
  then
    let filter_input = Charamel_bubbles.Textinput.set_value "" state.filter_input in
    ( recompute ~apply_default:false { state with filter_input }
        (Dyn.eval state.options ctx.results)
        None,
      none,
      Stay )
  else if arity = Many && raw_matches key km.toggle then
    let state = picker_toggle_current state in
    match picker_validate arity state with
    | Ok () -> (state, none, Stay)
    | Error error -> ({ state with err = Some error }, none, Stay)
  else if arity = Many && raw_matches key km.select_all && state.limit = 0 then
    let all_selected = picker_all_filtered_selected state in
    let selected =
      Stdlib.List.map
        (fun (flag, option_) ->
          if
            Stdlib.List.exists
              (fun visible -> String.equal visible.key option_.key)
              state.filtered
          then not all_selected
          else flag)
        (Stdlib.List.combine state.selected state.all)
    in
    let state = { state with selected } in
    match picker_validate arity state with
    | Ok () -> (state, none, Stay)
    | Error error -> ({ state with err = Some error }, none, Stay)
  else if arity = Many && raw_matches key km.select_none && state.limit = 0 then
    let selected =
      Stdlib.List.map
        (fun (flag, option_) ->
          if
            Stdlib.List.exists
              (fun visible -> String.equal visible.key option_.key)
              state.filtered
          then false
          else flag)
        (Stdlib.List.combine state.selected state.all)
    in
    let state = { state with selected } in
    match picker_validate arity state with
    | Ok () -> (state, none, Stay)
    | Error error -> ({ state with err = Some error }, none, Stay)
  else if raw_matches key km.prev && not ctx.position.is_first then (state, none, Prev)
  else if raw_matches key km.next && not ctx.position.is_last then
    match picker_validate arity state with
    | Ok () -> (state, none, Next)
    | Error error -> ({ state with err = Some error }, none, Stay)
  else if raw_matches key km.submit && ctx.position.is_last then
    match picker_validate arity state with
    | Ok () -> (state, none, Submit)
    | Error error -> ({ state with err = Some error }, none, Stay)
  else if arity = One && state.inline && (raw_matches key km.up || raw_matches key km.down)
  then (state, none, Stay)
  else if raw_matches key km.up then (picker_move_by (-1) state, none, Stay)
  else if raw_matches key km.down then (picker_move_by 1 state, none, Stay)
  else if arity = One && (raw_matches key km.left || raw_matches key km.right) then
    (picker_move_by (if raw_matches key km.left then -1 else 1) state, none, Stay)
  else if raw_matches key km.half_page_up then
    (picker_move_by (-max 1 (state.height / 2)) state, none, Stay)
  else if raw_matches key km.half_page_down then
    (picker_move_by (max 1 (state.height / 2)) state, none, Stay)
  else if raw_matches key km.goto_top then ({ state with cursor = 0 }, none, Stay)
  else if raw_matches key km.goto_bottom then
    ({ state with cursor = max 0 (List.length state.filtered - 1) }, none, Stay)
  else (state, none, Stay)

let picker_update arity (ctx : ctx) message (state : _ picker_state) =
  match message with
  | Filter child ->
      let filter_input, command =
        Charamel_bubbles.Textinput.update child state.filter_input
      in
      let all = Dyn.eval state.options ctx.results in
      let state = { state with filter_input; err = None } in
      ( picker_recompute ~apply_default:false arity state all None,
        Charamel_tea.Cmd.map (fun child -> Filter child) command )
  | Options_loaded all ->
      let old_key = option_key_at state.cursor state.filtered in
      let state = picker_recompute arity state all old_key in
      ( { state with last_version = Results.version ctx.results; loading_since = None },
        Charamel_tea.Cmd.none )

let picker_view arity (ctx : ctx) ~focused (state : _ picker_state) =
  let style = field_style ctx focused in
  let title = Dyn.eval state.title ctx.results in
  let description = Dyn.eval state.description ctx.results in
  let heading =
    render_title_description ctx ~focused ~title ~description ~error:state.err
  in
  let option_rows =
    match arity with
    | One ->
        let selector = style.Styles.indicators.Styles.select_selector in
        let height = if state.inline then 1 else max 1 state.height in
        let y_offset =
          min state.cursor
            (max 0 (state.y_offset + max 0 (state.cursor - state.y_offset) - height + 1))
        in
        let y_offset =
          max 0 (min y_offset (max 0 (Stdlib.List.length state.filtered - height)))
        in
        state.filtered
        |> Stdlib.List.mapi (fun index option_ -> (index, option_))
        |> Stdlib.List.filter (fun (index, _) ->
            index >= y_offset && index < y_offset + height)
        |> Stdlib.List.map (fun (index, option_) ->
            let selected = index = state.cursor in
            let selector =
              if selected then selector
              else String.make (Charamel_ansi.Text.width selector) ' '
            in
            let row_style =
              if selected then style.Styles.selected_option else style.Styles.option_
            in
            Charamel_lipgloss.Style.render style.Styles.select_selector selector
            ^ Charamel_lipgloss.Style.render row_style option_.key)
    | Many ->
        let selector = style.Styles.indicators.Styles.multi_select_selector in
        let y_offset =
          min state.cursor
            (max 0
               (state.y_offset + max 0 (state.cursor - state.y_offset) - state.height + 1))
        in
        state.filtered
        |> Stdlib.List.mapi (fun index option_ -> (index, option_))
        |> Stdlib.List.filter (fun (index, _) ->
            index >= y_offset && index < y_offset + max 1 state.height)
        |> Stdlib.List.map (fun (index, option_) ->
            let current = index = state.cursor in
            let selected = picker_selected_at index state in
            let selector =
              if current then selector
              else String.make (Charamel_ansi.Text.width selector) ' '
            in
            let prefix =
              if selected then style.Styles.indicators.Styles.selected_prefix
              else style.Styles.indicators.Styles.unselected_prefix
            in
            let prefix_style =
              if selected then style.Styles.selected_prefix
              else style.Styles.unselected_prefix
            in
            let option_style =
              if selected then style.Styles.selected_option
              else style.Styles.unselected_option
            in
            Charamel_lipgloss.Style.render style.Styles.multi_select_selector selector
            ^ Charamel_lipgloss.Style.render prefix_style prefix
            ^ Charamel_lipgloss.Style.render option_style option_.key)
  in
  let option_rows =
    match state.loading_since with
    | Some since when Charamel_os.Time.now ctx.env.Env.clock -. since > 0.025 ->
        [ Charamel_bubbles.Spinner.view (Charamel_bubbles.Spinner.v ()) ^ " Loading..." ]
    | _ -> option_rows
  in
  let rows =
    if state.filtering then
      Charamel_bubbles.Textinput.view state.filter_input :: option_rows
    else option_rows
  in
  let rows =
    if state.filtering || (arity = One && state.inline) then rows
    else rows @ List.init (max 0 (state.height - List.length rows)) (fun _ -> "")
  in
  let rows = if rows = [] && not state.filtering then [ "" ] else rows in
  let content = String.concat "\n" rows in
  let content =
    if arity = One && state.inline then
      Charamel_lipgloss.Style.render style.Styles.prev_indicator
        style.Styles.indicators.Styles.prev_indicator
      ^ content
      ^ Charamel_lipgloss.Style.render style.Styles.next_indicator
          style.Styles.indicators.Styles.next_indicator
    else content
  in
  Charamel_lipgloss.Style.render style.Styles.base (concat_nonempty [ heading; content ])

let picker_key_binds arity (ctx : ctx) (state : _ picker_state) =
  match arity with
  | One ->
      let km = ctx.keymap.Keymap.select in
      let navigation =
        navigation_binds ctx ~prev:km.prev ~next:km.next ~submit:km.submit
      in
      if state.filtering then
        navigation
        @ [
            binding_enabled km.set_filter true;
            km.up;
            km.down;
            km.goto_top;
            km.goto_bottom;
          ]
      else
        navigation
        @ [
            km.up;
            km.down;
            km.left;
            km.right;
            km.filter;
            binding_enabled km.clear_filter
              (Charamel_bubbles.Textinput.value state.filter_input <> "");
            km.half_page_up;
            km.half_page_down;
            km.goto_top;
            km.goto_bottom;
          ]
  | Many ->
      let km = ctx.keymap.Keymap.multi_select in
      let selected_any = List.exists Fun.id state.selected in
      let all_selected = picker_all_filtered_selected state in
      navigation_binds ctx ~prev:km.prev ~next:km.next ~submit:km.submit
      @ [
          km.toggle;
          km.up;
          km.down;
          km.filter;
          binding_enabled km.set_filter state.filtering;
          binding_enabled km.clear_filter
            (Charamel_bubbles.Textinput.value state.filter_input <> "");
          km.half_page_up;
          km.half_page_down;
          km.goto_top;
          km.goto_bottom;
          binding_enabled km.select_all (state.limit = 0 && not all_selected);
          binding_enabled km.select_none (state.limit = 0 && selected_any);
        ]

let picker_accessible_one ~name (ctx : ctx) ~out reader (state : _ picker_state) =
  let title = Dyn.eval state.title ctx.results in
  let options = Dyn.eval state.options ctx.results in
  check_unique_options options;
  let heading = (if title = "" then field_label name else title) ^ " \n" in
  let* () = out heading in
  let* () =
    Lwt_list.iteri_s
      (fun index option_ -> out (Fmt.str "%d. %s\n" (index + 1) option_.key))
      options
  in
  match options with
  | [] -> Lwt.return { state with err = Some "no options available" }
  | _ ->
      let prompt =
        if Stdlib.List.length options = 1 then
          "There is only one option available; enter the number 1: "
        else Fmt.str "Enter a number between 1 and %d: " (Stdlib.List.length options)
      in
      let rec loop state =
        let* choice =
          Accessible.prompt_int out reader ~prompt ~low:1
            ~high:(Stdlib.List.length options)
            ~default:(Some (state.cursor + 1))
        in
        let state = { state with cursor = choice - 1; filtered = options } in
        match picker_validate One state with
        | Ok () -> Lwt.return { state with err = None }
        | Error error ->
            let* () = out (error ^ "\n") in
            loop { state with err = Some error }
      in
      loop state

let picker_accessible_many ~name (ctx : ctx) ~out reader (state : _ picker_state) =
  let title = Dyn.eval state.title ctx.results in
  let options = Dyn.eval state.options ctx.results in
  check_unique_options options;
  let* () = out ((if title = "" then field_label name else title) ^ " \n") in
  if options = [] then Lwt.return { state with err = Some "no options available" }
  else
    let limit = if state.limit > 0 then state.limit else List.length options in
    let state =
      {
        state with
        all = options;
        filtered = options;
        selected = bools_for_options options state.defaults;
      }
    in
    let print_options state =
      Lwt_list.iteri_s
        (fun index option_ ->
          let selected =
            match index_of_key option_.key state.all with
            | None -> false
            | Some option_index -> List.nth state.selected option_index
          in
          out
            (Fmt.str "%d. [%s] %s\n" (index + 1)
               (if selected then "x" else " ")
               option_.key))
        options
    in
    let* () = out (Fmt.str "Select up to %d options.\n" limit) in
    let* () = print_options state in
    let* () = out "0. Confirm selection\n" in
    let rec loop state =
      let* choice =
        Accessible.prompt_int out reader
          ~prompt:(Fmt.str "Enter a number between 0 and %d: " (List.length options))
          ~low:0 ~high:(List.length options) ~default:(Some 0)
      in
      if choice = 0 then
        match picker_validate Many state with
        | Ok () -> Lwt.return { state with err = None }
        | Error error ->
            let* () = out (error ^ "\n") in
            loop { state with err = Some error }
      else
        let index = choice - 1 in
        let selected = picker_selected_at index state in
        if
          (not selected) && state.limit > 0
          && List.length (selected_values state) >= state.limit
        then begin
          let* () =
            out (Fmt.str "You can't select more than %d options.\n" state.limit)
          in
          loop state
        end
        else
          let option_ = List.nth options index in
          let state = picker_set_selected_key option_.key (not selected) state in
          let* () = print_options state in
          let* () = out "0. Confirm selection\n" in
          match picker_validate Many state with
          | Ok () -> loop { state with err = None }
          | Error error ->
              let* () = out (error ^ "\n") in
              loop { state with err = Some error }
    in
    loop state

let picker_accessible arity ~name (ctx : ctx) ~out reader (state : _ picker_state) =
  match arity with
  | One -> picker_accessible_one ~name ctx ~out reader state
  | Many -> picker_accessible_many ~name ctx ~out reader state

let picker_impl arity ~name ~key ~value title description height inline limit filterable
    explicit_width defaults validate_one validate_many options =
  let filter_input = Charamel_bubbles.Textinput.v ~prompt:"/ " ~width:80 () in
  let msg_id = Type.Id.make () in
  let state =
    {
      filter_input;
      title;
      description;
      options;
      all = [];
      filtered = [];
      selected = [];
      height = max 1 height;
      limit = max 0 limit;
      inline;
      filterable;
      explicit_width;
      defaults;
      validate_one;
      validate_many;
      cursor = 0;
      y_offset = 0;
      filtering = false;
      err = None;
      last_version = -1;
      width = 80;
      loading_since = None;
    }
  in
  let impl =
    {
      name;
      key = Some key;
      msg_id;
      init = picker_init arity;
      reevaluate = picker_reevaluate arity;
      on_key = picker_on_key arity;
      on_paste = (fun _ _ state -> state);
      update = picker_update arity;
      subscriptions =
        (fun _ state ->
          Charamel_tea.Sub.map
            (fun message -> Filter message)
            (Charamel_bubbles.Textinput.subscriptions state.filter_input));
      view = picker_view arity;
      focus = (fun _ state -> (state, Charamel_tea.Cmd.none));
      blur =
        (fun _ state ->
          match picker_validate arity state with
          | Ok () -> { state with err = None }
          | Error error -> { state with err = Some error });
      value;
      error = (fun state -> state.err);
      skip = (fun _ _ -> false);
      zoom = (fun _ -> false);
      key_binds = picker_key_binds arity;
      run_accessible = picker_accessible arity;
      filtering_of = (fun state -> Some state.filtering);
      set_filtering_of = (fun filtering state -> picker_set_filtering filtering state);
      hovered = (fun state -> option_key_at state.cursor state.filtered);
    }
  in
  Field (impl, state)

let select_impl key title description height inline filterable explicit_width default
    validate options =
  picker_impl One ~name:"select" ~key
    ~value:(fun state -> (List.nth state.filtered state.cursor).value)
    title description height inline 0 filterable explicit_width
    (match default with Some key -> [ key ] | None -> [])
    validate
    (fun _ -> Ok ())
    options

let multi_impl key title description height limit filterable explicit_width default
    validate options =
  picker_impl Many ~name:"multi_select" ~key ~value:selected_values title description
    height false limit filterable explicit_width default
    (fun _ -> Ok ())
    validate options

type confirm_message = unit

type confirm_state = {
  title : string Dyn.t;
  description : string Dyn.t;
  affirmative : string;
  negative : string option;
  inline : bool;
  button_alignment : [ `Left | `Center | `Right ];
  value : bool;
  validate : bool -> (unit, string) result;
  err : string option;
  last_version : int;
}

let confirm_msg_id : confirm_message Type.Id.t = Type.Id.make ()
let confirm_validate state value = state.validate value

let confirm_on_key ctx key state =
  let state = { state with err = None } in
  let km = ctx.keymap.Keymap.confirm in
  let finish value =
    let outcome = if ctx.position.is_last then Submit else Next in
    match confirm_validate state value with
    | Ok () -> ({ state with value }, Charamel_tea.Cmd.none, outcome)
    | Error error -> ({ state with value; err = Some error }, Charamel_tea.Cmd.none, Stay)
  in
  if raw_matches key km.Keymap.prev && not ctx.position.is_first then
    (state, Charamel_tea.Cmd.none, Prev)
  else if raw_matches key km.Keymap.accept then finish true
  else if raw_matches key km.Keymap.reject && state.negative <> None then finish false
  else if raw_matches key km.Keymap.toggle && state.negative <> None then
    ({ state with value = not state.value }, Charamel_tea.Cmd.none, Stay)
  else if raw_matches key km.Keymap.next && not ctx.position.is_last then
    match confirm_validate state state.value with
    | Ok () -> (state, Charamel_tea.Cmd.none, Next)
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
  else if raw_matches key km.Keymap.submit && ctx.position.is_last then
    match confirm_validate state state.value with
    | Ok () -> (state, Charamel_tea.Cmd.none, Submit)
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
  else (state, Charamel_tea.Cmd.none, Stay)

let confirm_update _ (_ : confirm_message) state = (state, Charamel_tea.Cmd.none)

let confirm_reevaluate ctx state =
  if state.last_version = Results.version ctx.results then state
  else { state with last_version = Results.version ctx.results }

let confirm_init ctx state = (confirm_reevaluate ctx state, Charamel_tea.Cmd.none)
let confirm_paste _ _ state = state
let confirm_focus _ state = (state, Charamel_tea.Cmd.none)

let confirm_blur _ state =
  match confirm_validate state state.value with
  | Ok () -> { state with err = None }
  | Error error -> { state with err = Some error }

let confirm_view ctx ~focused state =
  let style = field_style ctx focused in
  let title = Dyn.eval state.title ctx.results in
  let description = Dyn.eval state.description ctx.results in
  let heading =
    render_title_description ctx ~focused ~title ~description ~error:state.err
  in
  let button label active =
    Charamel_lipgloss.Style.render
      (if active then style.Styles.focused_button else style.Styles.blurred_button)
      label
  in
  let position =
    match state.button_alignment with
    | `Left -> Charamel_lipgloss.Position.left
    | `Center -> Charamel_lipgloss.Position.center
    | `Right -> Charamel_lipgloss.Position.right
  in
  let buttons =
    match state.negative with
    | None -> button state.affirmative state.value
    | Some negative ->
        Charamel_lipgloss.Layout.join_horizontal ~pos:position
          [ button state.affirmative state.value; button negative (not state.value) ]
  in
  let buttons =
    Charamel_lipgloss.Layout.place_horizontal ~pos:position
      ~width:(max (Charamel_ansi.Text.width heading) (Charamel_ansi.Text.width buttons))
      buttons
  in
  let content =
    if state.inline then concat_nonempty [ heading ^ " " ^ buttons ]
    else concat_nonempty [ heading; buttons ]
  in
  Charamel_lipgloss.Style.render style.Styles.base content

let confirm_key_binds ctx state =
  let km = ctx.keymap.Keymap.confirm in
  navigation_binds ctx ~prev:km.Keymap.prev ~next:km.Keymap.next ~submit:km.Keymap.submit
  @ [
      binding_enabled km.Keymap.toggle (state.negative <> None);
      km.Keymap.accept;
      binding_enabled km.Keymap.reject (state.negative <> None);
    ]

let confirm_accessible ~name ctx ~out reader state =
  let title = Dyn.eval state.title ctx.results in
  let prompt =
    (if title = "" then field_label name else title)
    ^ " "
    ^ if state.value then "[Y/n] " else "[y/N] "
  in
  let rec loop state =
    let* value = Accessible.prompt_bool out reader ~prompt ~default:state.value in
    match confirm_validate state value with
    | Ok () -> Lwt.return { state with value; err = None }
    | Error error ->
        let* () = out (error ^ "\n") in
        loop { state with value; err = Some error }
  in
  loop state

let confirm_impl key title description affirmative negative inline button_alignment
    default validate =
  let state =
    {
      title;
      description;
      affirmative;
      negative;
      inline;
      button_alignment;
      value = default;
      validate;
      err = None;
      last_version = -1;
    }
  in
  let impl =
    {
      name = "confirm";
      key = Some key;
      msg_id = confirm_msg_id;
      init = confirm_init;
      reevaluate =
        (fun ctx state -> (confirm_reevaluate ctx state, Charamel_tea.Cmd.none));
      on_key = confirm_on_key;
      on_paste = confirm_paste;
      update = confirm_update;
      subscriptions = (fun _ _ -> Charamel_tea.Sub.none);
      view = confirm_view;
      focus = confirm_focus;
      blur = confirm_blur;
      value = (fun state -> state.value);
      error = (fun state -> state.err);
      skip = (fun _ _ -> false);
      zoom = (fun _ -> false);
      key_binds = confirm_key_binds;
      run_accessible = confirm_accessible;
      filtering_of = (fun _ -> None);
      set_filtering_of = (fun _ state -> state);
      hovered = (fun _ -> None);
    }
  in
  Field (impl, state)

type note_state = {
  title : string Dyn.t;
  description : string Dyn.t;
  height : int;
  next : string option;
}

type note_message = unit

let note_msg_id : note_message Type.Id.t = Type.Id.make ()

let mini_markdown text =
  let buffer = Buffer.create (String.length text) in
  let rec parse index =
    if index >= String.length text then ()
    else
      match text.[index] with
      | '\\' when index + 1 < String.length text ->
          Buffer.add_char buffer text.[index + 1];
          parse (index + 2)
      | ('_' | '*' | '`') as delimiter ->
          let closing =
            try String.index_from text (index + 1) delimiter with Not_found -> -1
          in
          if closing < 0 then (
            Buffer.add_char buffer delimiter;
            parse (index + 1))
          else
            let inner = String.sub text (index + 1) (closing - index - 1) in
            let rendered =
              match delimiter with
              | '_' ->
                  Charamel_lipgloss.Style.render
                    (Charamel_lipgloss.Style.italic true Charamel_lipgloss.Style.empty)
                    inner
              | '*' ->
                  Charamel_lipgloss.Style.render
                    (Charamel_lipgloss.Style.bold true Charamel_lipgloss.Style.empty)
                    inner
              | '`' ->
                  Charamel_lipgloss.Style.render
                    (Charamel_lipgloss.Style.reverse true Charamel_lipgloss.Style.empty)
                    inner
              | _ -> inner
            in
            Buffer.add_string buffer rendered;
            parse (closing + 1)
      | c ->
          Buffer.add_char buffer c;
          parse (index + 1)
  in
  parse 0;
  Buffer.contents buffer

let note_init _ state = (state, Charamel_tea.Cmd.none)
let note_reevaluate _ state = state

let note_on_key ctx key state =
  let km = ctx.keymap.Keymap.note in
  if raw_matches key km.Keymap.prev && not ctx.position.is_first then
    (state, Charamel_tea.Cmd.none, Prev)
  else if ctx.position.is_last then (state, Charamel_tea.Cmd.none, Submit)
  else (state, Charamel_tea.Cmd.none, Next)

let note_update _ (_ : note_message) state = (state, Charamel_tea.Cmd.none)
let note_paste _ _ state = state
let note_focus _ state = (state, Charamel_tea.Cmd.none)
let note_blur _ state = state

let note_view ctx ~focused state =
  let style = field_style ctx focused in
  let title = Dyn.eval state.title ctx.results in
  let description = Dyn.eval state.description ctx.results |> mini_markdown in
  let card =
    concat_nonempty
      [ Charamel_lipgloss.Style.render style.Styles.note_title title; description ]
  in
  let card =
    match state.next with
    | None -> card
    | Some label -> card ^ "\n" ^ Charamel_lipgloss.Style.render style.Styles.next label
  in
  let card_lines = String.split_on_char '\n' card in
  let padded =
    card_lines @ List.init (max 0 (state.height - List.length card_lines)) (fun _ -> "")
  in
  Charamel_lipgloss.Style.render style.Styles.card (String.concat "\n" padded)

let note_key_binds ctx _state =
  let km = ctx.keymap.Keymap.note in
  navigation_binds ctx ~prev:km.Keymap.prev ~next:km.Keymap.next ~submit:km.Keymap.submit

let note_accessible ~name:_ ctx ~out _reader state =
  let title = Dyn.eval state.title ctx.results in
  let description = Dyn.eval state.description ctx.results in
  let* () = if title = "" then Lwt.return_unit else out (title ^ "\n") in
  let* () = if description = "" then Lwt.return_unit else out (description ^ "\n") in
  Lwt.return state

let note_impl title description height next =
  let state = { title; description; height; next } in
  let impl =
    {
      name = "note";
      key = None;
      msg_id = note_msg_id;
      init = note_init;
      reevaluate = (fun ctx state -> (note_reevaluate ctx state, Charamel_tea.Cmd.none));
      on_key = note_on_key;
      on_paste = note_paste;
      update = note_update;
      subscriptions = (fun _ _ -> Charamel_tea.Sub.none);
      view = note_view;
      focus = note_focus;
      blur = note_blur;
      value = (fun _ -> ());
      error = (fun _ -> None);
      skip = (fun ctx _ -> not (ctx.position.is_first && ctx.position.is_last));
      zoom = (fun _ -> false);
      key_binds = note_key_binds;
      run_accessible = note_accessible;
      filtering_of = (fun _ -> None);
      set_filtering_of = (fun _ state -> state);
      hovered = (fun _ -> None);
    }
  in
  Field (impl, state)

type file_message = File_picker of Charamel_bubbles.Filepicker.msg

type file_state = {
  title : string Dyn.t;
  description : string Dyn.t;
  dir : string;
  show_hidden : bool;
  show_size : bool;
  show_permissions : bool;
  allowed : string list;
  files : bool;
  dirs : bool;
  height : int;
  validate : string -> (unit, string) result;
  picker : Charamel_bubbles.Filepicker.t option;
  picking : bool;
  cursor : string option;
  selected : string;
  err : string option;
}

let file_msg_id : file_message Type.Id.t = Type.Id.make ()

let make_picker ctx state =
  Charamel_bubbles.Filepicker.v ~root:ctx.env.Env.fs_root ~current_directory:state.dir
    ~allowed_types:state.allowed ~show_permissions:state.show_permissions
    ~show_size:state.show_size ~show_hidden:state.show_hidden ~dir_allowed:state.dirs
    ~file_allowed:state.files ~height:(max 1 state.height) ?cursor:state.cursor ()

let file_init ctx state =
  let picker = make_picker ctx state in
  let picker, command = Charamel_bubbles.Filepicker.init picker in
  ( { state with picker = Some picker },
    Charamel_tea.Cmd.map (fun message -> File_picker message) command )

let file_reevaluate (ctx : ctx) state =
  let picker =
    Option.map
      (Charamel_bubbles.Filepicker.set_height
         (max 1 (if state.picking then ctx.height else state.height)))
      state.picker
  in
  { state with picker }

let file_validate ctx state input =
  if input = "" then Error "not a file"
  else
    let relative =
      if
        Filename.is_relative input && state.dir <> "."
        && not (String.starts_with ~prefix:(state.dir ^ "/") input)
      then Filename.concat state.dir input
      else input
    in
    let path =
      if Filename.is_relative relative then Filename.concat ctx.env.Env.fs_root relative
      else relative
    in
    match Unix.stat path with
    | { Unix.st_kind = Unix.S_REG; _ } ->
        if
          state.allowed <> []
          && not
               (Stdlib.List.exists
                  (fun suffix -> Filename.check_suffix input suffix)
                  state.allowed)
        then Error (Fmt.str "cannot select: %s" input)
        else state.validate input
    | { Unix.st_kind = _; _ } -> Error "not a file"
    | exception Unix.Unix_error _ -> Error "not a file"

let file_on_key ctx key state =
  let state = { state with err = None } in
  let km = ctx.keymap.Keymap.file in
  if (not state.picking) && raw_matches key km.Keymap.open_ then
    let picker, command =
      match state.picker with
      | Some picker -> Charamel_bubbles.Filepicker.init picker
      | None ->
          let picker = make_picker ctx state in
          Charamel_bubbles.Filepicker.init picker
    in
    ( { state with picker = Some picker; picking = true },
      Charamel_tea.Cmd.map (fun message -> File_picker message) command,
      Stay )
  else if state.picking then
    if raw_matches key km.Keymap.close then
      ({ state with picking = false }, Charamel_tea.Cmd.none, Stay)
    else
      match state.picker with
      | None -> (state, Charamel_tea.Cmd.none, Stay)
      | Some picker -> (
          match Charamel_bubbles.Filepicker.key picker key with
          | None -> (state, Charamel_tea.Cmd.none, Stay)
          | Some child -> (
              let selected = Charamel_bubbles.Filepicker.did_select_file child picker in
              let disabled =
                Charamel_bubbles.Filepicker.did_select_disabled_file child picker
              in
              let picker, command = Charamel_bubbles.Filepicker.update child picker in
              let command =
                Charamel_tea.Cmd.map (fun message -> File_picker message) command
              in
              match (selected, disabled) with
              | Some path, _ ->
                  ( { state with picker = Some picker; selected = path; picking = false },
                    command,
                    Next )
              | None, Some _ ->
                  let suffixes =
                    match state.allowed with
                    | [] -> ""
                    | [ one ] -> one
                    | many ->
                        let rev = Stdlib.List.rev many in
                        String.concat ", " (Stdlib.List.rev (Stdlib.List.tl rev))
                        ^ " and " ^ Stdlib.List.hd rev
                  in
                  let error =
                    if suffixes = "" then "cannot select file"
                    else Fmt.str "%s files only" suffixes
                  in
                  ({ state with picker = Some picker; err = Some error }, command, Stay)
              | None, None -> ({ state with picker = Some picker }, command, Stay)))
  else if raw_matches key km.Keymap.prev && not ctx.position.is_first then
    (state, Charamel_tea.Cmd.none, Prev)
  else if raw_matches key km.Keymap.next && not ctx.position.is_last then
    match file_validate ctx state state.selected with
    | Ok () -> (state, Charamel_tea.Cmd.none, Next)
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
  else if raw_matches key km.Keymap.submit && ctx.position.is_last then
    match file_validate ctx state state.selected with
    | Ok () -> (state, Charamel_tea.Cmd.none, Submit)
    | Error error -> ({ state with err = Some error }, Charamel_tea.Cmd.none, Stay)
  else (state, Charamel_tea.Cmd.none, Stay)

let file_update _ctx message state =
  match (message, state.picker) with
  | File_picker child, Some picker ->
      let selected = Charamel_bubbles.Filepicker.did_select_file child picker in
      let disabled = Charamel_bubbles.Filepicker.did_select_disabled_file child picker in
      let picker, command = Charamel_bubbles.Filepicker.update child picker in
      let state =
        match (selected, disabled) with
        | Some path, _ ->
            {
              state with
              picker = Some picker;
              selected = path;
              picking = false;
              err = None;
            }
        | None, Some _ ->
            let suffixes =
              match state.allowed with
              | [] -> ""
              | [ one ] -> one
              | many ->
                  let rev = Stdlib.List.rev many in
                  String.concat ", " (Stdlib.List.rev (Stdlib.List.tl rev))
                  ^ " and " ^ Stdlib.List.hd rev
            in
            let error =
              if suffixes = "" then "cannot select file"
              else Fmt.str "%s files only" suffixes
            in
            { state with picker = Some picker; err = Some error }
        | None, None -> { state with picker = Some picker }
      in
      (state, Charamel_tea.Cmd.map (fun message -> File_picker message) command)
  | _ -> (state, Charamel_tea.Cmd.none)

let file_view ctx ~focused state =
  let style = field_style ctx focused in
  let title = Dyn.eval state.title ctx.results in
  let description = Dyn.eval state.description ctx.results in
  let heading =
    render_title_description ctx ~focused ~title ~description ~error:state.err
  in
  let body =
    if state.picking then
      match state.picker with
      | Some picker -> Charamel_bubbles.Filepicker.view picker
      | None -> ""
    else if state.selected <> "" then
      Charamel_lipgloss.Style.render style.Styles.selected_option state.selected
    else
      Charamel_lipgloss.Style.render style.Styles.text_input.Styles.placeholder
        "No file selected."
  in
  Charamel_lipgloss.Style.render style.Styles.base (concat_nonempty [ heading; body ])

let file_key_binds ctx state =
  let km = ctx.keymap.Keymap.file in
  if state.picking then
    [
      km.Keymap.goto_top;
      km.Keymap.goto_bottom;
      km.Keymap.page_up;
      km.Keymap.page_down;
      km.Keymap.back;
      km.Keymap.select;
      km.Keymap.up;
      km.Keymap.down;
      km.Keymap.open_;
      km.Keymap.close;
    ]
  else
    navigation_binds ctx ~prev:km.Keymap.prev ~next:km.Keymap.next
      ~submit:km.Keymap.submit
    @ [ km.Keymap.open_ ]

let file_accessible ~name ctx ~out reader state =
  let title = Dyn.eval state.title ctx.results in
  let prompt = (if title = "" then field_label name else title) ^ " " in
  let* value =
    Accessible.prompt_string ~out reader ~prompt ~default:state.selected
      ~validate:(file_validate ctx state)
  in
  Lwt.return { state with selected = value; err = None }

let file_blur ctx state =
  match file_validate ctx state state.selected with
  | Ok () -> { state with err = None }
  | Error error -> { state with err = Some error }

let file_impl key title description dir show_hidden show_size show_permissions allowed
    files dirs height cursor picking validate =
  let state =
    {
      title;
      description;
      dir;
      show_hidden;
      show_size;
      show_permissions;
      allowed;
      files;
      dirs;
      height = max 1 height;
      validate;
      picker = None;
      picking;
      cursor;
      selected = "";
      err = None;
    }
  in
  let impl =
    {
      name = "file";
      key = Some key;
      msg_id = file_msg_id;
      init = file_init;
      reevaluate = (fun ctx state -> (file_reevaluate ctx state, Charamel_tea.Cmd.none));
      on_key = file_on_key;
      on_paste = (fun _ _ state -> state);
      update = file_update;
      subscriptions = (fun _ _ -> Charamel_tea.Sub.none);
      view = file_view;
      focus = (fun _ state -> (state, Charamel_tea.Cmd.none));
      blur = file_blur;
      value = (fun state -> state.selected);
      error = (fun state -> state.err);
      skip = (fun _ _ -> false);
      zoom = (fun state -> state.picking);
      key_binds = file_key_binds;
      run_accessible = file_accessible;
      filtering_of = (fun _ -> None);
      set_filtering_of = (fun _ state -> state);
      hovered = (fun _ -> None);
    }
  in
  Field (impl, state)

let init (Field (impl, state)) ctx =
  let state, command = impl.init ctx state in
  (Field (impl, state), map_cmd impl.msg_id command)

let reevaluate (Field (impl, state)) ctx =
  let state, command = impl.reevaluate ctx state in
  (Field (impl, state), map_cmd impl.msg_id command)

let step_key (Field (impl, state)) ctx key =
  let state, command, outcome = impl.on_key ctx key state in
  (Field (impl, state), map_cmd impl.msg_id command, outcome)

let step_msg (Field (impl, state)) ctx message =
  match Field_msg.project impl.msg_id message with
  | None -> (Field (impl, state), Charamel_tea.Cmd.none)
  | Some message ->
      let state, command = impl.update ctx message state in
      (Field (impl, state), map_cmd impl.msg_id command)

let step_paste (Field (impl, state)) ctx text = Field (impl, impl.on_paste ctx text state)

let subscriptions (Field (impl, state)) ctx =
  map_sub impl.msg_id (impl.subscriptions ctx state)

let view (Field (impl, state)) ctx ~focused = impl.view ctx ~focused state

let focus (Field (impl, state)) ctx =
  let state, command = impl.focus ctx state in
  (Field (impl, state), map_cmd impl.msg_id command)

let blur (Field (impl, state)) ctx = Field (impl, impl.blur ctx state)
let error (Field (impl, state)) = impl.error state
let skip (Field (impl, state)) ctx = impl.skip ctx state
let zoom (Field (impl, state)) = impl.zoom state
let key_name (Field (impl, _)) = Option.map Key.name impl.key
let key_binds (Field (impl, state)) ctx = impl.key_binds ctx state
let filtering (Field (impl, state)) = impl.filtering_of state

let set_filtering value (Field (impl, state)) =
  Field (impl, impl.set_filtering_of value state)

let hovered (Field (impl, state)) = impl.hovered state

let run_accessible (Field (impl, state)) ctx ~out reader =
  let* state = impl.run_accessible ~name:impl.name ctx ~out reader state in
  Lwt.return (Field (impl, state))

let commit (Field (impl, state)) results =
  match impl.key with
  | None -> results
  | Some key -> Results.add key (impl.value state) results

module Field = struct
  type nonrec t = t
  type nonrec 'a option_ = 'a option_

  let option_ ~key value = { key; value }
  let options_of_strings values = Stdlib.List.map (fun key -> { key; value = key }) values

  let input ?(title = Dyn.Const "") ?(description = Dyn.Const "")
      ?(placeholder = Dyn.Const "") ?(prompt = "> ") ?(char_limit = 0)
      ?(suggestions = Dyn.Const []) ?(echo = `Normal) ?(inline = false) ?(default = "")
      ?(validate = noop_string) key =
    let echo =
      match echo with
      | `Normal -> Charamel_bubbles.Textinput.Normal
      | `Password -> Charamel_bubbles.Textinput.Password
      | `None -> Charamel_bubbles.Textinput.No_echo
    in
    input_impl key title description placeholder prompt char_limit suggestions echo inline
      default validate

  let text ?(title = Dyn.Const "") ?(description = Dyn.Const "")
      ?(placeholder = Dyn.Const "") ?(lines = 5) ?(char_limit = 0)
      ?(show_line_numbers = false) ?(editor = true) ?(editor_extension = "md")
      ?(default = "") ?(validate = noop_string) key =
    text_impl key title description placeholder lines char_limit show_line_numbers editor
      editor_extension default validate

  let select ?(title = Dyn.Const "") ?(description = Dyn.Const "") ?(height = 10) ?width
      ?(inline = false) ?(filterable = false) ?default ?(validate = fun _ -> Ok ())
      ~options key =
    (match options with
    | Dyn.Const values -> check_unique_options values
    | Dyn.Of_results _ | Dyn.Of_results_async _ -> ());
    select_impl key title description height inline filterable width default validate
      options

  let multi_select ?(title = Dyn.Const "") ?(description = Dyn.Const "") ?(height = 10)
      ?width ?(limit = 0) ?(filterable = true) ?(default = []) ?(validate = noop_list)
      ~options key =
    (match options with
    | Dyn.Const values -> check_unique_options values
    | Dyn.Of_results _ | Dyn.Of_results_async _ -> ());
    multi_impl key title description height limit filterable width default validate
      options

  let confirm ?(title = Dyn.Const "") ?(description = Dyn.Const "") ?(affirmative = "Yes")
      ?(negative = Some "No") ?(inline = false) ?(button_alignment = `Center)
      ?(default = false) ?(validate = noop_bool) key =
    confirm_impl key title description affirmative negative inline button_alignment
      default validate

  let note ?(title = Dyn.Const "") ?(description = Dyn.Const "") ?(height = 0)
      ?(show_next = false) ?(next_label = "Next") () =
    note_impl title description height (if show_next then Some next_label else None)

  let file ?(title = Dyn.Const "") ?(description = Dyn.Const "") ?(dir = ".")
      ?(show_hidden = false) ?(show_size = false) ?(show_permissions = false)
      ?(allowed = []) ?(files = true) ?(dirs = false) ?(height = 10) ?cursor
      ?(picking = false) ?(validate = noop_string) key =
    file_impl key title description dir show_hidden show_size show_permissions allowed
      files dirs height cursor picking validate

  let key_name = key_name
  let filtering = filtering
  let set_filtering = set_filtering
  let hovered = hovered
end
