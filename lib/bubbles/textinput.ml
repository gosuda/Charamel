module Key = Charamel_tea.Key
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color
module Text = Charamel_ansi.Text
module Width = Charamel_ansi.Width

let clamp n lo hi = max lo (min hi n)

let is_control u =
  let n = Uchar.to_int u in
  (n >= 0 && n <= 0x1f) || (n >= 0x7f && n <= 0x9f)

let sanitize s =
  let out = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else
      let decoded = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid decoded) then loop (i + 1)
      else
        let u = Uchar.utf_decode_uchar decoded in
        let n = Uchar.utf_decode_length decoded in
        if
          Uchar.equal u (Uchar.of_char '\t')
          || Uchar.equal u (Uchar.of_char '\n')
          || Uchar.equal u (Uchar.of_char '\r')
        then Buffer.add_char out ' '
        else if not (is_control u) then Buffer.add_utf_8_uchar out u;
        loop (i + n)
  in
  loop 0;
  Buffer.contents out

let lower_string s =
  let out = Buffer.create (String.length s) in
  let rec loop i =
    if i >= String.length s then ()
    else
      let d = String.get_utf_8_uchar s i in
      if not (Uchar.utf_decode_is_valid d) then loop (i + 1)
      else
        let u = Uchar.utf_decode_uchar d in
        let n = Uchar.utf_decode_length d in
        (match Uucp.Case.Map.to_lower u with
        | `Self -> Buffer.add_utf_8_uchar out u
        | `Uchars us -> Stdlib.List.iter (Buffer.add_utf_8_uchar out) us);
        loop (i + n)
  in
  loop 0;
  Buffer.contents out

let string_of_clusters xs = String.concat "" xs
let cluster_width s = Width.grapheme_width s
let clusters_width xs = Stdlib.List.fold_left (fun n x -> n + cluster_width x) 0 xs

let take n xs =
  let rec loop remaining acc = function
    | [] -> Stdlib.List.rev acc
    | _ when remaining <= 0 -> Stdlib.List.rev acc
    | x :: rest -> loop (remaining - 1) (x :: acc) rest
  in
  loop n [] xs

let drop n xs =
  let rec loop remaining = function
    | [] -> []
    | xs when remaining <= 0 -> xs
    | _ :: rest -> loop (remaining - 1) rest
  in
  loop n xs

let sub_clusters xs start len = take len (drop start xs)

let take_width budget xs =
  let rec loop used acc = function
    | [] -> Stdlib.List.rev acc
    | x :: rest ->
        let w = cluster_width x in
        if used = 0 && w > budget then Stdlib.List.rev acc
        else if budget <= 0 || used + w > budget then Stdlib.List.rev acc
        else loop (used + w) (x :: acc) rest
  in
  if budget <= 0 then [] else loop 0 [] xs

let repeat_string s n =
  let b = Buffer.create (String.length s * max 0 n) in
  for _ = 1 to max 0 n do
    Buffer.add_string b s
  done;
  Buffer.contents b

type echo = Normal | Password | No_echo

type style_state = {
  text : Style.t;
  placeholder : Style.t;
  suggestion : Style.t;
  prompt : Style.t;
}

type cursor_style = {
  color : Color.t;
  shape : Charamel_tea.Cursor.shape;
  blink : bool;
  blink_speed : float option;
}

type styles = { focused : style_state; blurred : style_state; cursor : cursor_style }

let default_styles ~is_dark =
  let color n = match Color.indexed n with Some c -> c | None -> Color.Default in
  let light_dark light dark = Charamel_lipgloss.light_dark ~is_dark ~light ~dark in
  {
    focused =
      {
        text = Style.empty;
        placeholder = Style.foreground (color 240) Style.empty;
        suggestion = Style.foreground (color 240) Style.empty;
        prompt = Style.foreground (color 7) Style.empty;
      };
    blurred =
      {
        text = Style.foreground (light_dark (color 245) (color 7)) Style.empty;
        placeholder = Style.foreground (color 240) Style.empty;
        suggestion = Style.foreground (color 240) Style.empty;
        prompt = Style.foreground (color 7) Style.empty;
      };
    cursor =
      {
        color = color 7;
        shape = Charamel_tea.Cursor.Block;
        blink = true;
        blink_speed = None;
      };
  }

type keymap = {
  character_forward : Key_binding.t;
  character_backward : Key_binding.t;
  word_forward : Key_binding.t;
  word_backward : Key_binding.t;
  delete_word_backward : Key_binding.t;
  delete_word_forward : Key_binding.t;
  delete_after_cursor : Key_binding.t;
  delete_before_cursor : Key_binding.t;
  delete_character_backward : Key_binding.t;
  delete_character_forward : Key_binding.t;
  line_start : Key_binding.t;
  line_end : Key_binding.t;
  paste : Key_binding.t;
  accept_suggestion : Key_binding.t;
  next_suggestion : Key_binding.t;
  prev_suggestion : Key_binding.t;
}

let default_keymap =
  {
    character_forward = Key_binding.v [ "right"; "ctrl+f" ];
    character_backward = Key_binding.v [ "left"; "ctrl+b" ];
    word_forward = Key_binding.v [ "alt+right"; "ctrl+right"; "alt+f" ];
    word_backward = Key_binding.v [ "alt+left"; "ctrl+left"; "alt+b" ];
    delete_word_backward = Key_binding.v [ "alt+backspace"; "ctrl+w"; "ctrl+backspace" ];
    delete_word_forward = Key_binding.v [ "alt+delete"; "alt+d"; "ctrl+delete" ];
    delete_after_cursor = Key_binding.v [ "ctrl+k" ];
    delete_before_cursor = Key_binding.v [ "ctrl+u" ];
    delete_character_backward = Key_binding.v [ "backspace"; "ctrl+h" ];
    delete_character_forward = Key_binding.v [ "delete"; "ctrl+d" ];
    line_start = Key_binding.v [ "home"; "ctrl+a" ];
    line_end = Key_binding.v [ "end"; "ctrl+e" ];
    paste = Key_binding.v [ "ctrl+v" ];
    accept_suggestion = Key_binding.v [ "tab" ];
    next_suggestion = Key_binding.v [ "down"; "ctrl+n" ];
    prev_suggestion = Key_binding.v [ "up"; "ctrl+p" ];
  }

type msg =
  | Character_forward
  | Character_backward
  | Word_forward
  | Word_backward
  | Delete_word_backward
  | Delete_word_forward
  | Delete_after_cursor
  | Delete_before_cursor
  | Delete_character_backward
  | Delete_character_forward
  | Line_start
  | Line_end
  | Accept_suggestion
  | Next_suggestion
  | Prev_suggestion
  | Insert of string
  | Paste_request
  | Cursor of Cursor.msg

type t = {
  prompt : string;
  placeholder : string;
  echo : echo;
  echo_character : string;
  char_limit : int;
  width : int;
  validate : (string -> (unit, string) result) option;
  show_suggestions : bool;
  suggestions : string list;
  matched_suggestions : string list;
  current_suggestion_index : int;
  keymap : keymap;
  styles : styles;
  virtual_cursor : bool;
  focused : bool;
  value : string list;
  position : int;
  offset : int;
  offset_right : int;
  error : string option;
  cursor : Cursor.t;
}

let active_style (m : t) = if m.focused then m.styles.focused else m.styles.blurred

let cursor_mode styles virtual_cursor =
  if not virtual_cursor then Cursor.Hide
  else if styles.blink then Cursor.Blink
  else Cursor.Static

let sync_cursor (m : t) =
  let cs = m.styles.cursor in
  let mode = cursor_mode cs m.virtual_cursor in
  let c = Cursor.set_style (Style.foreground cs.color Style.empty) m.cursor in
  let c = Cursor.set_text_style (active_style m).text c in
  let c =
    match cs.blink_speed with
    | Some speed -> Cursor.set_blink_speed speed c
    | None -> Cursor.set_blink_speed 0.53 c
  in
  let c = if Cursor.mode c = mode then c else Cursor.set_mode mode c in
  let c =
    if Cursor.focused c = m.focused then c
    else if m.focused then Cursor.focus c
    else Cursor.blur c
  in
  { m with cursor = c }

let displayed_width m clusters =
  match m.echo with
  | No_echo -> 0
  | Normal -> clusters_width clusters
  | Password ->
      let w = max 1 (Text.width m.echo_character) in
      clusters_width clusters * w

let segment_width m start len = displayed_width m (sub_clusters m.value start len)

let handle_overflow m =
  let n = Stdlib.List.length m.value in
  if m.width <= 0 || displayed_width m m.value <= m.width then
    { m with offset = 0; offset_right = n }
  else
    let pos = clamp m.position 0 n in
    let start0 = clamp m.offset 0 n in
    let start0 =
      if pos < start0 then pos else if pos > m.offset_right then pos else start0
    in
    let target_end = min n (if pos < n then pos + 1 else pos) in
    let start = ref (min start0 target_end) in
    while !start > 0 && segment_width m !start (target_end - !start) > m.width do
      incr start
    done;
    while
      !start > 0 && segment_width m (!start - 1) (target_end - (!start - 1)) <= m.width
    do
      decr start
    done;
    let right = ref target_end in
    while !right < n && segment_width m !start (!right + 1 - !start) <= m.width do
      incr right
    done;
    { m with offset = !start; offset_right = !right }

let validation_error m clusters =
  Option.bind m.validate (fun f ->
      match f (string_of_clusters clusters) with Ok () -> None | Error e -> Some e)

let v ?(prompt = "> ") ?(placeholder = "") ?(echo = Normal) ?(echo_character = "*")
    ?(char_limit = 0) ?(width = 0) ?validate ?(show_suggestions = false)
    ?(suggestions = []) ?(keymap = default_keymap) ?(is_dark = true) ?styles
    ?(virtual_cursor = true) ?(value = "") () =
  let styles = match styles with Some s -> s | None -> default_styles ~is_dark in
  let empty =
    {
      prompt;
      placeholder;
      echo;
      echo_character;
      char_limit = max 0 char_limit;
      width = max 0 width;
      validate;
      show_suggestions;
      suggestions;
      matched_suggestions = [];
      current_suggestion_index = 0;
      keymap;
      styles;
      virtual_cursor;
      focused = false;
      value = [];
      position = 0;
      offset = 0;
      offset_right = 0;
      error = None;
      cursor = Cursor.v ();
    }
  in
  let m = sync_cursor empty in
  let sanitized = sanitize value in
  let value =
    let xs = Width.graphemes sanitized in
    if m.char_limit > 0 then take m.char_limit xs else xs
  in
  let error = validation_error m value in
  let m = { m with value; position = Stdlib.List.length value; error } in
  let matches value =
    if value = "" then []
    else
      let folded = lower_string value in
      Stdlib.List.filter
        (fun candidate ->
          String.length candidate >= String.length value
          && String.sub (lower_string candidate) 0 (String.length folded) = folded)
        m.suggestions
  in
  sync_cursor
    (handle_overflow { m with matched_suggestions = matches (string_of_clusters value) })

let value m = string_of_clusters m.value
let error m = m.error
let position m = m.position
let width m = m.width
let prompt (m : t) = m.prompt
let placeholder (m : t) = m.placeholder
let echo m = m.echo
let char_limit m = m.char_limit
let keymap m = m.keymap
let styles m = m.styles
let virtual_cursor m = m.virtual_cursor
let focused (m : t) = m.focused
let show_suggestions m = m.show_suggestions
let available_suggestions m = m.suggestions
let matched_suggestions m = m.matched_suggestions
let current_suggestion_index m = m.current_suggestion_index

let current_suggestion m =
  match Stdlib.List.nth_opt m.matched_suggestions m.current_suggestion_index with
  | Some s -> s
  | None -> ""

let update_matches m =
  let current = value m in
  let folded = lower_string current in
  let matched =
    if (not m.show_suggestions) || current = "" then []
    else
      Stdlib.List.filter
        (fun candidate ->
          let candidate_folded = lower_string candidate in
          String.length candidate_folded >= String.length folded
          && String.sub candidate_folded 0 (String.length folded) = folded)
        m.suggestions
  in
  let index =
    if matched = m.matched_suggestions then
      min m.current_suggestion_index (max 0 (Stdlib.List.length matched - 1))
    else 0
  in
  { m with matched_suggestions = matched; current_suggestion_index = index }

let set_value s m =
  let clusters = Width.graphemes (sanitize s) in
  let old_empty = m.value = [] in
  let clusters = if m.char_limit > 0 then take m.char_limit clusters else clusters in
  let position =
    if (m.position = 0 && old_empty) || m.position > Stdlib.List.length clusters then
      Stdlib.List.length clusters
    else m.position
  in
  handle_overflow
    (update_matches
       { m with value = clusters; position; error = validation_error m clusters })

let set_cursor pos m =
  handle_overflow { m with position = clamp pos 0 (Stdlib.List.length m.value) }

let cursor_start m = set_cursor 0 m
let cursor_end m = set_cursor (Stdlib.List.length m.value) m
let set_width width m = handle_overflow { m with width = max 0 width }
let set_prompt prompt m = sync_cursor { m with prompt }
let set_placeholder placeholder m = sync_cursor { m with placeholder }
let set_echo echo m = sync_cursor (handle_overflow { m with echo })
let set_echo_character echo_character m = { m with echo_character } |> handle_overflow

let set_char_limit limit m =
  let char_limit = max 0 limit in
  let value = if char_limit > 0 then take char_limit m.value else m.value in
  handle_overflow
    (update_matches
       {
         m with
         char_limit;
         value;
         position = min m.position (Stdlib.List.length value);
         error = validation_error m value;
       })

let set_keymap keymap m = { m with keymap }
let set_styles styles m = sync_cursor { m with styles }
let set_virtual_cursor virtual_cursor m = sync_cursor { m with virtual_cursor }
let set_show_suggestions show_suggestions m = update_matches { m with show_suggestions }
let set_suggestions suggestions m = update_matches { m with suggestions }

let reset m =
  sync_cursor
    (handle_overflow
       {
         m with
         value = [];
         position = 0;
         offset = 0;
         offset_right = 0;
         error = None;
         matched_suggestions = [];
         current_suggestion_index = 0;
       })

let focus (m : t) =
  let m = { m with focused = true } in
  let m = sync_cursor m in
  ({ m with cursor = Cursor.show m.cursor }, Cmd.none)

let blur (m : t) = sync_cursor { m with focused = false }

let delete_range start stop m =
  let before = sub_clusters m.value 0 start in
  let after = drop stop m.value in
  let value = before @ after in
  let m = { m with value; position = start; error = validation_error m value } in
  update_matches (handle_overflow m)

let whitespace_cluster s =
  if s = "" then false
  else
    let d = String.get_utf_8_uchar s 0 in
    Uchar.utf_decode_is_valid d && Uucp.White.is_white_space (Uchar.utf_decode_uchar d)

let word_backward m =
  if m.echo <> Normal then cursor_start m
  else
    let i = ref m.position in
    while !i > 0 && whitespace_cluster (Stdlib.List.nth m.value (!i - 1)) do
      decr i
    done;
    while !i > 0 && not (whitespace_cluster (Stdlib.List.nth m.value (!i - 1))) do
      decr i
    done;
    set_cursor !i m

let word_forward m =
  if m.echo <> Normal then cursor_end m
  else
    let i = ref m.position in
    while
      !i < Stdlib.List.length m.value && whitespace_cluster (Stdlib.List.nth m.value !i)
    do
      incr i
    done;
    while
      !i < Stdlib.List.length m.value
      && not (whitespace_cluster (Stdlib.List.nth m.value !i))
    do
      incr i
    done;
    set_cursor !i m

let delete_word_backward m =
  if m.position = 0 then m
  else if m.echo <> Normal then delete_range 0 m.position m
  else
    let target = word_backward m in
    delete_range target.position m.position m

let delete_word_forward m =
  if m.position >= Stdlib.List.length m.value then m
  else if m.echo <> Normal then delete_range m.position (Stdlib.List.length m.value) m
  else
    let target = word_forward m in
    delete_range m.position target.position m

let delete_character_backward m =
  if m.position = 0 then m else delete_range (m.position - 1) m.position m

let delete_character_forward m =
  if m.position >= Stdlib.List.length m.value then m
  else delete_range m.position (m.position + 1) m

let insert_string s m =
  let sanitized = sanitize s in
  let incoming = Width.graphemes sanitized in
  let available =
    if m.char_limit > 0 then max 0 (m.char_limit - Stdlib.List.length m.value)
    else Stdlib.List.length incoming
  in
  let incoming = take available incoming in
  if incoming = [] then m
  else
    let value = sub_clusters m.value 0 m.position @ incoming @ drop m.position m.value in
    let position = m.position + Stdlib.List.length incoming in
    update_matches
      (handle_overflow { m with value; position; error = validation_error m value })

let paste s m = insert_string s m

let accept_suggestion m =
  match current_suggestion m with
  | "" -> m
  | suggestion ->
      let value = Width.graphemes (sanitize suggestion) in
      let value = if m.char_limit > 0 then take m.char_limit value else value in
      update_matches
        (handle_overflow
           {
             m with
             value;
             position = Stdlib.List.length value;
             error = validation_error m value;
           })

let update message (m : t) =
  if not m.focused then (m, Cmd.none)
  else
    let m =
      match message with
      | Cursor cursor_msg -> { m with cursor = Cursor.update cursor_msg m.cursor }
      | Character_forward ->
          if m.position < Stdlib.List.length m.value then set_cursor (m.position + 1) m
          else m
      | Character_backward -> if m.position > 0 then set_cursor (m.position - 1) m else m
      | Word_forward -> word_forward m
      | Word_backward -> word_backward m
      | Delete_word_backward -> delete_word_backward m
      | Delete_word_forward -> delete_word_forward m
      | Delete_after_cursor -> delete_range m.position (Stdlib.List.length m.value) m
      | Delete_before_cursor -> delete_range 0 m.position m
      | Delete_character_backward -> delete_character_backward m
      | Delete_character_forward -> delete_character_forward m
      | Line_start -> cursor_start m
      | Line_end -> cursor_end m
      | Accept_suggestion -> accept_suggestion m
      | Next_suggestion ->
          let count = Stdlib.List.length m.matched_suggestions in
          if count = 0 then m
          else
            {
              m with
              current_suggestion_index = (m.current_suggestion_index + 1) mod count;
            }
      | Prev_suggestion ->
          let count = Stdlib.List.length m.matched_suggestions in
          if count = 0 then m
          else
            {
              m with
              current_suggestion_index =
                (m.current_suggestion_index - 1 + count) mod count;
            }
      | Insert s -> insert_string s m
      | Paste_request -> m
    in
    let m =
      match message with Cursor _ -> m | _ -> { m with cursor = Cursor.show m.cursor }
    in
    (handle_overflow m, Cmd.none)

let plain_text style s = Style.render (Style.inline true style) s

let echo_transform m s =
  match m.echo with
  | Normal -> s
  | No_echo -> ""
  | Password ->
      let n = max 1 (Text.width m.echo_character) in
      repeat_string m.echo_character (Width.string_width s * n)

let render_virtual_cursor (m : t) ~char ~text_style =
  let c = Cursor.set_text_style text_style (Cursor.set_char char m.cursor) in
  Cursor.view c

let view (m : t) =
  let active = active_style m in
  let prompt = plain_text active.prompt m.prompt in
  let render_text s = plain_text active.text (echo_transform m s) in
  let render_suggestion s = plain_text active.suggestion (echo_transform m s) in
  let render_window () =
    let visible = sub_clusters m.value m.offset (m.offset_right - m.offset) in
    let before_count = clamp (m.position - m.offset) 0 (Stdlib.List.length visible) in
    let before = string_of_clusters (take before_count visible) in
    let after = string_of_clusters (drop before_count visible) in
    let value_view = ref (render_text before) in
    if m.virtual_cursor && m.focused then
      if before_count < Stdlib.List.length visible then (
        let under = Stdlib.List.nth visible before_count in
        value_view :=
          !value_view
          ^ render_virtual_cursor m ~char:(echo_transform m under) ~text_style:active.text;
        value_view :=
          !value_view ^ render_text (String.concat "" (drop (before_count + 1) visible)))
      else
        let suggestion = current_suggestion m in
        let suggestion_clusters = Width.graphemes suggestion in
        if suggestion <> "" && Stdlib.List.length suggestion_clusters > m.position then (
          let suffix = drop m.position suggestion_clusters in
          value_view :=
            !value_view
            ^ render_virtual_cursor m
                ~char:(echo_transform m (Stdlib.List.hd suffix))
                ~text_style:active.suggestion;
          value_view :=
            !value_view ^ render_suggestion (String.concat "" (Stdlib.List.tl suffix)))
        else
          value_view :=
            !value_view ^ render_virtual_cursor m ~char:" " ~text_style:active.text
    else value_view := !value_view ^ render_text after;
    let plain_width = displayed_width m visible in
    let padding =
      if m.width > 0 then
        let p = max 0 (m.width - plain_width) in
        if m.virtual_cursor && m.focused && m.position < Stdlib.List.length m.value then
          p + 1
        else p
      else 0
    in
    !value_view ^ plain_text active.text (String.make padding ' ')
  in
  if m.value = [] && m.placeholder <> "" then
    let placeholder_clusters = Width.graphemes m.placeholder in
    let shown =
      if m.width > 0 then take_width m.width placeholder_clusters
      else placeholder_clusters
    in
    let placeholder_text = String.concat "" shown in
    if m.virtual_cursor && m.focused then
      let first, rest =
        match shown with [] -> (" ", "") | x :: xs -> (x, String.concat "" xs)
      in
      prompt
      ^ render_virtual_cursor m ~char:first ~text_style:active.placeholder
      ^ plain_text active.placeholder rest
      ^ plain_text active.placeholder
          (String.make (max 0 (m.width - Width.string_width placeholder_text)) ' ')
    else prompt ^ plain_text active.placeholder placeholder_text
  else prompt ^ render_window ()

let key (m : t) (key : Key.t) =
  if not m.focused then None
  else
    let matches binding = Key_binding.matches key binding in
    if matches m.keymap.character_forward then Some Character_forward
    else if matches m.keymap.character_backward then Some Character_backward
    else if matches m.keymap.word_forward then Some Word_forward
    else if matches m.keymap.word_backward then Some Word_backward
    else if matches m.keymap.delete_word_backward then Some Delete_word_backward
    else if matches m.keymap.delete_word_forward then Some Delete_word_forward
    else if matches m.keymap.delete_after_cursor then Some Delete_after_cursor
    else if matches m.keymap.delete_before_cursor then Some Delete_before_cursor
    else if matches m.keymap.delete_character_backward then Some Delete_character_backward
    else if matches m.keymap.delete_character_forward then Some Delete_character_forward
    else if matches m.keymap.line_start then Some Line_start
    else if matches m.keymap.line_end then Some Line_end
    else if matches m.keymap.paste then Some Paste_request
    else if matches m.keymap.accept_suggestion then Some Accept_suggestion
    else if matches m.keymap.next_suggestion then Some Next_suggestion
    else if matches m.keymap.prev_suggestion then Some Prev_suggestion
    else if
      key.Key.mods.Key.ctrl || key.Key.mods.Key.alt || key.Key.mods.Key.meta
      || key.Key.mods.Key.super || key.Key.mods.Key.hyper
    then None
    else
      let text =
        if key.Key.text <> "" then key.Key.text
        else
          match key.Key.code with
          | Key.Char u ->
              let b = Buffer.create 4 in
              Buffer.add_utf_8_uchar b u;
              Buffer.contents b
          | Key.Space -> " "
          | _ -> ""
      in
      if text = "" then None else Some (Insert text)

let subscriptions (m : t) =
  if m.virtual_cursor && m.focused then
    Sub.map (fun msg -> Cursor msg) (Cursor.subscriptions m.cursor)
  else Sub.none

let cursor (m : t) =
  if m.virtual_cursor || not m.focused then None
  else
    let col = Text.width m.prompt + segment_width m m.offset (m.position - m.offset) in
    let cs = m.styles.cursor in
    Some { Charamel_tea.Cursor.row = 0; col; shape = cs.shape; blink = cs.blink }

let set_validate validator m =
  let m = { m with validate = validator } in
  { m with error = validation_error m m.value }
