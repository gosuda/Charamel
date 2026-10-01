module Textarea = Charamel_bubbles.Textarea
module Help = Charamel_bubbles.Help
module Key_binding = Charamel_bubbles.Key_binding
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color
module Layout = Charamel_lipgloss.Layout

let initial_inputs = 2
let max_inputs = 6
let min_inputs = 1
let help_height = 5

let cursor_line_style =
  Style.background (Color.Indexed 57) (Style.foreground (Color.Indexed 230) Style.empty)

let placeholder_style = Style.foreground (Color.Indexed 238) Style.empty
let focused_placeholder_style = Style.foreground (Color.Indexed 99) Style.empty

let focused_border_style =
  Style.border_foreground
    (Charamel_lipgloss.Sides_color.all (Color.Indexed 238))
    (Style.border Charamel_lipgloss.Border.rounded Style.empty)

let blurred_border_style = Style.border Charamel_lipgloss.Border.hidden Style.empty
let end_of_buffer_style = Style.foreground (Color.Indexed 235) Style.empty

let styles =
  let base = Textarea.default_styles ~is_dark:true in
  let focused =
    {
      base.Textarea.focused with
      placeholder = focused_placeholder_style;
      cursor_line = cursor_line_style;
      cursor_line_number = cursor_line_style;
      base = focused_border_style;
      end_of_buffer = end_of_buffer_style;
    }
  in
  let blurred =
    {
      base.Textarea.blurred with
      placeholder = placeholder_style;
      base = blurred_border_style;
      end_of_buffer = end_of_buffer_style;
    }
  in
  {
    Textarea.focused;
    blurred;
    cursor = { base.Textarea.cursor with color = Color.Indexed 212 };
  }

let textarea_keymap =
  let map = Textarea.default_keymap in
  {
    map with
    delete_word_backward = Key_binding.set_enabled false map.delete_word_backward;
    line_next = Key_binding.v [ "down" ];
    line_previous = Key_binding.v [ "up" ];
  }

let new_textarea () =
  Textarea.v ~prompt:"" ~placeholder:"Type something" ~show_line_numbers:true
    ~virtual_cursor:true ~styles ~keymap:textarea_keymap ()

type keymap = {
  next : Key_binding.t;
  prev : Key_binding.t;
  add : Key_binding.t;
  remove : Key_binding.t;
  quit : Key_binding.t;
}

type model = {
  width : int;
  height : int;
  keymap : keymap;
  help : Help.t;
  inputs : Textarea.t list;
  focus : int;
}

type msg = Key of Charamel_tea.Key.t | Win of int * int | Input of int * Textarea.msg

let base_keymap =
  {
    next = Key_binding.v ~help:("tab", "next") [ "tab" ];
    prev = Key_binding.v ~help:("shift+tab", "prev") [ "shift+tab" ];
    add = Key_binding.v ~help:("ctrl+n", "add an editor") [ "ctrl+n" ];
    remove = Key_binding.v ~help:("ctrl+w", "remove an editor") [ "ctrl+w" ];
    quit = Key_binding.v ~help:("esc", "quit") [ "escape"; "ctrl+c" ];
  }

let bindings keymap = [ keymap.next; keymap.prev; keymap.add; keymap.remove; keymap.quit ]

let set_focus index model =
  let inputs =
    List.mapi
      (fun i input ->
        if i = index then fst (Textarea.focus input) else Textarea.blur input)
      model.inputs
  in
  { model with inputs; focus = index }

let blur_all model = { model with inputs = List.map Textarea.blur model.inputs }

let move_focus delta model =
  let count = List.length model.inputs in
  let focus = (((model.focus + delta) mod count) + count) mod count in
  set_focus focus model

let update_keybindings model =
  let count = List.length model.inputs in
  let keymap = model.keymap in
  {
    model with
    keymap =
      {
        keymap with
        add = Key_binding.set_enabled (count < max_inputs) keymap.add;
        remove = Key_binding.set_enabled (count > min_inputs) keymap.remove;
      };
  }

let size_inputs model =
  let count = List.length model.inputs in
  let per = if count = 0 then 0 else model.width / count in
  let height = model.height - help_height in
  {
    model with
    inputs =
      List.map
        (fun input -> Textarea.set_height height (Textarea.set_width per input))
        model.inputs;
  }

let route_key key model =
  let pairs = List.mapi (fun index input -> (index, input)) model.inputs in
  let inputs, cmds =
    List.fold_left
      (fun (inputs, cmds) (index, input) ->
        match Textarea.key input key with
        | Some msg ->
            let input, cmd = Textarea.update msg input in
            ( input :: inputs,
              Charamel_tea.Cmd.map (fun msg -> Input (index, msg)) cmd :: cmds )
        | None -> (input :: inputs, cmds))
      ([], []) pairs
  in
  ({ model with inputs = List.rev inputs }, Charamel_tea.Cmd.batch (List.rev cmds))

let remove_last model =
  let count = List.length model.inputs in
  let inputs = List.filteri (fun i _ -> i < count - 1) model.inputs in
  { model with inputs; focus = min model.focus (List.length inputs - 1) }

let on_key key model =
  let keymap = model.keymap in
  if Key_binding.matches key keymap.quit then (blur_all model, Charamel_tea.Cmd.quit)
  else if Key_binding.matches key keymap.next then route_key key (move_focus 1 model)
  else if Key_binding.matches key keymap.prev then route_key key (move_focus (-1) model)
  else if Key_binding.matches key keymap.add then
    route_key key { model with inputs = model.inputs @ [ new_textarea () ] }
  else if Key_binding.matches key keymap.remove then route_key key (remove_last model)
  else route_key key model

let start =
  {
    width = 0;
    height = 0;
    keymap = base_keymap;
    help = Help.v ();
    inputs = List.init initial_inputs (fun _ -> new_textarea ());
    focus = 0;
  }

let finish model cmd = (update_keybindings (size_inputs model), cmd)

let view model =
  Charamel_tea.View.v ~alt_screen:true
    (Layout.join_horizontal (List.map Textarea.view model.inputs)
    ^ "\n\n"
    ^ Help.short_view model.help (bindings model.keymap))

let input_subscriptions model =
  List.mapi
    (fun index input ->
      Charamel_tea.Sub.map (fun msg -> Input (index, msg)) (Textarea.subscriptions input))
    model.inputs

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (set_focus 0 (update_keybindings start), Charamel_tea.Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Win (rows, cols) ->
            finish { model with width = cols; height = rows } Charamel_tea.Cmd.none
        | Key key ->
            let model, cmd = on_key key model in
            finish model cmd
        | Input (index, input_msg) -> (
            match List.nth_opt model.inputs index with
            | None -> (model, Charamel_tea.Cmd.none)
            | Some input ->
                let input, cmd = Textarea.update input_msg input in
                let inputs =
                  List.mapi (fun i old -> if i = index then input else old) model.inputs
                in
                finish { model with inputs }
                  (Charamel_tea.Cmd.map (fun msg -> Input (index, msg)) cmd)));
    view;
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          (Charamel_tea.Sub.key (fun key -> Key key)
          :: Charamel_tea.Sub.resize (fun ~rows ~cols -> Win (rows, cols))
          :: input_subscriptions model));
  }

let main () = Smoke.run_ app
let long_text = "01234567890123456789012345678901234567890"

let smoke () =
  Smoke.expect app
    [ Smoke.key "escape" ]
    [ "Type something"; "tab next"; "ctrl+n add an editor" ]
  @ Smoke.expect app
      [ `Text "abc"; Smoke.key "tab"; `Text "xyz"; Smoke.key "escape" ]
      [ "abc"; "xyz" ]
  @ Smoke.expect app
      [ Smoke.key "ctrl+w"; `Text long_text; Smoke.key "escape" ]
      [ long_text ]
