(** Multi-line, grapheme-aware text editing.

    [t] keeps a logical line buffer, a cursor, a viewport, and an optional selection.
    Wrapping affects rendering only; [value] always preserves the logical newlines.
    Setters return new values and all editing transitions are exposed through {!update}.
*)

type style_state = {
  base : Charm_lipgloss.Style.t;
  text : Charm_lipgloss.Style.t;
  line_number : Charm_lipgloss.Style.t;
  cursor_line_number : Charm_lipgloss.Style.t;
  cursor_line : Charm_lipgloss.Style.t;
  end_of_buffer : Charm_lipgloss.Style.t;
  placeholder : Charm_lipgloss.Style.t;
  prompt : Charm_lipgloss.Style.t;
  selection : Charm_lipgloss.Style.t;
}
(** Styles for logical and rendered lines. *)

type cursor_style = Textinput.cursor_style
(** Cursor settings shared with {!Textinput}. *)

type styles = { focused : style_state; blurred : style_state; cursor : cursor_style }
(** Focused and blurred line styles plus cursor settings. *)

val default_styles : is_dark:bool -> styles
(** [default_styles ~is_dark] is the standard textarea palette. *)

type keymap = {
  character_backward : Key_binding.t;
  character_forward : Key_binding.t;
  delete_after_cursor : Key_binding.t;
  delete_before_cursor : Key_binding.t;
  delete_character_backward : Key_binding.t;
  delete_character_forward : Key_binding.t;
  delete_word_backward : Key_binding.t;
  delete_word_forward : Key_binding.t;
  insert_newline : Key_binding.t;
  line_end : Key_binding.t;
  line_next : Key_binding.t;
  line_previous : Key_binding.t;
  line_start : Key_binding.t;
  page_up : Key_binding.t;
  page_down : Key_binding.t;
  paste : Key_binding.t;
  word_backward : Key_binding.t;
  word_forward : Key_binding.t;
  input_begin : Key_binding.t;
  input_end : Key_binding.t;
  uppercase_word_forward : Key_binding.t;
  lowercase_word_forward : Key_binding.t;
  capitalize_word_forward : Key_binding.t;
  transpose_character_backward : Key_binding.t;
  select_character_forward : Key_binding.t;
  select_character_backward : Key_binding.t;
  select_word_forward : Key_binding.t;
  select_word_backward : Key_binding.t;
  select_line_up : Key_binding.t;
  select_line_down : Key_binding.t;
  select_all : Key_binding.t;
  copy_selection : Key_binding.t;
}
(** Key bindings for movement, editing, and selection. *)

val default_keymap : keymap
(** [default_keymap] contains every standard textarea key binding. *)

type line_info = {
  width : int;
  char_width : int;
  height : int;
  start_column : int;
  column_offset : int;
  row_offset : int;
  char_offset : int;
}
(** Measurements of the visual row containing the cursor. *)

type position = { row : int; col : int }
(** A logical buffer position, with grapheme-column [col]. *)

type prompt_info = { line_number : int; focused : bool }
(** Context passed to a dynamic prompt. *)

type key_action =
  | Character_backward
  | Character_forward
  | Delete_after_cursor
  | Delete_before_cursor
  | Delete_character_backward
  | Delete_character_forward
  | Delete_word_backward
  | Delete_word_forward
  | Insert_newline
  | Line_end
  | Line_next
  | Line_previous
  | Line_start
  | Page_up
  | Page_down
  | Word_backward
  | Word_forward
  | Input_begin
  | Input_end
  | Uppercase_word_forward
  | Lowercase_word_forward
  | Capitalize_word_forward
  | Transpose_character_backward
  | Select_character_forward
  | Select_character_backward
  | Select_word_forward
  | Select_word_backward
  | Select_line_up
  | Select_line_down
  | Select_all  (** An editing or selection action. *)

type msg =
  | Key_action of key_action
  | Insert of string
  | Copy_request of string
  | Paste_request
  | Cursor of Cursor.msg  (** Messages consumed by {!update}. *)

type t
(** The textarea model. *)

val v :
  ?prompt:string ->
  ?placeholder:string ->
  ?show_line_numbers:bool ->
  ?end_of_buffer_character:string ->
  ?char_limit:int ->
  ?max_height:int ->
  ?max_width:int ->
  ?width:int ->
  ?height:int ->
  ?dynamic_height:bool ->
  ?min_height:int ->
  ?max_content_height:int ->
  ?keymap:keymap ->
  ?is_dark:bool ->
  ?styles:styles ->
  ?virtual_cursor:bool ->
  ?value:string ->
  unit ->
  t
(** [v ?prompt ?placeholder ?show_line_numbers ?end_of_buffer_character ?char_limit
     ?max_height ?max_width ?width ?height ?dynamic_height ?min_height ?max_content_height
     ?keymap ?is_dark ?styles ?virtual_cursor ?value ()] constructs an unfocused textarea.
    Defaults are prompt ["┃ "], line numbers, end-of-buffer character [" "], no character
    limit, maximum height [99], maximum width [500], width [40], height [6], fixed height,
    minimum height [1], no visual content limit, dark styles, and a virtual cursor. *)

val update : msg -> t -> t * msg Charm_tea.Cmd.t
(** [update message t] applies a complete editing or cursor transition. A blurred textarea
    ignores the message. Clipboard requests are represented as messages; the parent
    supplies the clipboard operation. *)

val view : t -> string
(** [view t] renders wrapped logical lines, prompts, line numbers, selection, placeholder,
    cursor, and end-of-buffer rows. *)

val key : t -> Charm_tea.Key.t -> msg option
(** [key t key] maps an enabled binding or printable key to a message. *)

val subscriptions : t -> msg Charm_tea.Sub.t
(** [subscriptions t] subscribes to virtual cursor ticks while focused. *)

val focus : t -> t * msg Charm_tea.Cmd.t
(** [focus t] focuses the textarea. *)

val blur : t -> t
(** [blur t] removes focus. *)

val focused : t -> bool
(** [focused t] reports focus. *)

val value : t -> string
(** [value t] is the logical buffer, including its newlines. *)

val set_value : string -> t -> t
(** [set_value value t] replaces the logical buffer and places the cursor at its end,
    subject to character, line, and visual-content limits. *)

val insert_string : string -> t -> t
(** [insert_string value t] inserts Unicode text at the cursor. Newlines are preserved,
    control bytes are discarded, and the selection is replaced. *)

val paste : string -> t -> t
(** [paste value t] inserts a clipboard payload with the same Unicode and limit handling
    as {!insert_string}; newlines are preserved. *)

val length : t -> int
(** [length t] is the sum of display widths of all graphemes plus newlines. *)

val line_count : t -> int
(** [line_count t] is the number of logical lines. *)

val line : t -> int
(** [line t] is the cursor's zero-based logical line. *)

val column : t -> int
(** [column t] is the cursor's grapheme column within its logical line. *)

val line_info : t -> line_info
(** [line_info t] measures the visual row containing the cursor. *)

val cursor_down : t -> t
(** [cursor_down t] moves one visual row down while retaining a sticky column. *)

val cursor_up : t -> t
(** [cursor_up t] moves one visual row up while retaining a sticky column. *)

val set_cursor_column : int -> t -> t
(** [set_cursor_column column t] moves to a grapheme column on the current line. *)

val cursor_start : t -> t
(** [cursor_start t] moves to column zero of the current line. *)

val cursor_end : t -> t
(** [cursor_end t] moves after the last grapheme of the current line. *)

val move_to_begin : t -> t
(** [move_to_begin t] moves to the beginning of the buffer. *)

val move_to_end : t -> t
(** [move_to_end t] moves to the end of the buffer. *)

val page_up : t -> t
(** [page_up t] moves up by one viewport page. *)

val page_down : t -> t
(** [page_down t] moves down by one viewport page. *)

val reset : t -> t
(** [reset t] clears the buffer and selection. *)

val word : t -> string
(** [word t] returns the whitespace-delimited word at the cursor. *)

val width : t -> int
(** [width t] is the outer width. *)

val set_width : int -> t -> t
(** [set_width width t] changes the outer width and recomputes wrapping. *)

val height : t -> int
(** [height t] is the visible height. *)

val set_height : int -> t -> t
(** [set_height height t] clamps the visible height to configured bounds. *)

val set_prompt : string -> t -> t
(** [set_prompt prompt t] sets the static prompt. *)

val set_prompt_func : width:int -> (prompt_info -> string) -> t -> t
(** [set_prompt_func ~width f t] installs a prompt function padded to [width]. *)

val set_placeholder : string -> t -> t
(** [set_placeholder placeholder t] sets the empty-buffer placeholder. *)

val set_show_line_numbers : bool -> t -> t
(** [set_show_line_numbers enabled t] toggles line-number cells. *)

val set_end_of_buffer_character : string -> t -> t
(** [set_end_of_buffer_character char t] sets the filler character. *)

val set_char_limit : int -> t -> t
(** [set_char_limit limit t] sets the grapheme limit; [0] means unlimited. *)

val set_max_height : int -> t -> t
(** [set_max_height height t] sets the logical height ceiling. *)

val set_max_width : int -> t -> t
(** [set_max_width width t] sets the content-width ceiling. *)

val set_dynamic_height : bool -> t -> t
(** [set_dynamic_height enabled t] toggles height recalculation from content. *)

val set_min_height : int -> t -> t
(** [set_min_height height t] sets the dynamic-height floor. *)

val set_max_content_height : int -> t -> t
(** [set_max_content_height rows t] limits visual content rows; [0] is unlimited. *)

val keymap : t -> keymap
(** [keymap t] is the active keymap. *)

val set_keymap : keymap -> t -> t
(** [set_keymap keymap t] replaces the active keymap. *)

val styles : t -> styles
(** [styles t] is the active style set. *)

val set_styles : styles -> t -> t
(** [set_styles styles t] replaces styles and updates cursor behavior. *)

val set_virtual_cursor : bool -> t -> t
(** [set_virtual_cursor enabled t] selects embedded or real cursor output. *)

val cursor : t -> Charm_tea.Cursor.t option
(** [cursor t] returns a real cursor request when focused and virtual output is disabled.
*)

val scroll_y_offset : t -> int
(** [scroll_y_offset t] is the viewport's first visual row. *)

val scroll_percent : t -> float
(** [scroll_percent t] is the viewport's vertical scroll fraction. *)

val position_at : x:int -> y:int -> t -> position
(** [position_at ~x ~y t] maps textarea coordinates to a logical position. *)

val begin_selection : x:int -> y:int -> t -> t
(** [begin_selection ~x ~y t] starts a pointer selection. *)

val extend_selection : x:int -> y:int -> t -> t
(** [extend_selection ~x ~y t] extends a pointer selection. *)

val end_selection : t -> t
(** [end_selection t] completes a pointer selection. *)

val select_all : t -> t
(** [select_all t] selects the complete logical buffer. *)

val clear_selection : t -> t
(** [clear_selection t] removes the active selection. *)

val has_selection : t -> bool
(** [has_selection t] reports a non-empty selection. *)

val selection : t -> (position * position) option
(** [selection t] returns the normalized selected range, if any. *)

val selected_text : t -> string
(** [selected_text t] returns selected logical text joined by newlines. *)

val delete_selection : t -> t
(** [delete_selection t] removes the selected range and places the cursor at its start. *)
