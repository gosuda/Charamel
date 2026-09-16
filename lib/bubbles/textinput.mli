(** Single-line, grapheme-aware text input.

    [t] stores a Unicode-safe editing buffer, cursor, validation state, and optional
    completion candidates. Values are immutable: setters return a new model. *)

type echo = Normal | Password | No_echo  (** How the value is displayed. *)

type style_state = {
  text : Charm_lipgloss.Style.t;
  placeholder : Charm_lipgloss.Style.t;
  suggestion : Charm_lipgloss.Style.t;
  prompt : Charm_lipgloss.Style.t;
}
(** Styles selected according to focus state. *)

type cursor_style = {
  color : Charm_ansi.Color.t;
  shape : Charm_tea.Cursor.shape;
  blink : bool;
  blink_speed : float option;
}
(** Styles and timing for the virtual or real cursor. *)

type styles = { focused : style_state; blurred : style_state; cursor : cursor_style }
(** Focused and blurred text styles plus cursor settings. *)

val default_styles : is_dark:bool -> styles
(** [default_styles ~is_dark] is the standard dark or light palette. *)

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
(** Key bindings for editing and completion. *)

val default_keymap : keymap
(** [default_keymap] contains the standard text-input bindings. *)

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
  | Cursor of Cursor.msg  (** Messages consumed by {!update}. *)

type t
(** The text-input model. *)

val v :
  ?prompt:string ->
  ?placeholder:string ->
  ?echo:echo ->
  ?echo_character:string ->
  ?char_limit:int ->
  ?width:int ->
  ?validate:(string -> (unit, string) result) ->
  ?show_suggestions:bool ->
  ?suggestions:string list ->
  ?keymap:keymap ->
  ?is_dark:bool ->
  ?styles:styles ->
  ?virtual_cursor:bool ->
  ?value:string ->
  unit ->
  t
(** [v ?prompt ?placeholder ?echo ?echo_character ?char_limit ?width ?validate
     ?show_suggestions ?suggestions ?keymap ?is_dark ?styles ?virtual_cursor ?value ()]
    constructs an unfocused input. Defaults are prompt ["> "], normal echo, ["*"] as the
    password character, no character limit, unlimited width, no validator or suggestions,
    dark styles, and a virtual cursor. *)

val update : msg -> t -> t * msg Charm_tea.Cmd.t
(** [update message t] applies one editing transition. A blurred input ignores all
    messages. Editing always returns [Charm_tea.Cmd.none]. *)

val view : t -> string
(** [view t] renders the prompt, visible value window, placeholder or completion, and
    virtual cursor. *)

val key : t -> Charm_tea.Key.t -> msg option
(** [key t key] maps an enabled binding or printable key to a message. It returns [None]
    for a blurred input or an unbound key. *)

val subscriptions : t -> msg Charm_tea.Sub.t
(** [subscriptions t] subscribes to virtual-cursor ticks while focused. *)

val focus : t -> t * msg Charm_tea.Cmd.t
(** [focus t] focuses the input and returns a command for the cursor. *)

val blur : t -> t
(** [blur t] removes focus. *)

val focused : t -> bool
(** [focused t] reports focus. *)

val value : t -> string
(** [value t] is the sanitized logical value. *)

val set_value : string -> t -> t
(** [set_value value t] sanitizes, validates, and installs [value], preserving a valid
    cursor position and moving an out-of-range cursor to the end. *)

val paste : string -> t -> t
(** [paste payload t] inserts [payload] at the cursor with the same sanitization, limits,
    and validation as user input. *)

val reset : t -> t
(** [reset t] clears the value, validation error, suggestions, and cursor offset. *)

val error : t -> string option
(** [error t] is the most recent validator message, if any. *)

val position : t -> int
(** [position t] is the grapheme index of the cursor. *)

val set_cursor : int -> t -> t
(** [set_cursor position t] clamps and moves the cursor to a grapheme boundary. *)

val cursor_start : t -> t
(** [cursor_start t] moves the cursor to the beginning. *)

val cursor_end : t -> t
(** [cursor_end t] moves the cursor to the end. *)

val width : t -> int
(** [width t] is the content viewport width, or [0] for unlimited. *)

val set_width : int -> t -> t
(** [set_width width t] changes the horizontal viewport width. *)

val prompt : t -> string
(** [prompt t] is the prompt. *)

val set_prompt : string -> t -> t
(** [set_prompt prompt t] changes the prompt. *)

val placeholder : t -> string
(** [placeholder t] is the empty-input placeholder. *)

val set_placeholder : string -> t -> t
(** [set_placeholder placeholder t] changes the placeholder. *)

val echo : t -> echo
(** [echo t] is the display mode. *)

val set_echo : echo -> t -> t
(** [set_echo echo t] changes the display mode. *)

val set_echo_character : string -> t -> t
(** [set_echo_character char t] changes the password mask. *)

val char_limit : t -> int
(** [char_limit t] is the maximum number of grapheme clusters, or [0]. *)

val set_char_limit : int -> t -> t
(** [set_char_limit limit t] changes the limit and truncates an over-limit value. *)

val set_validate : (string -> (unit, string) result) option -> t -> t
(** [set_validate validator t] replaces the validator and records its result for the
    current value. *)

val keymap : t -> keymap
(** [keymap t] is the active keymap. *)

val set_keymap : keymap -> t -> t
(** [set_keymap keymap t] replaces the active keymap. *)

val styles : t -> styles
(** [styles t] is the active style set. *)

val set_styles : styles -> t -> t
(** [set_styles styles t] replaces styles and updates cursor behavior. *)

val set_virtual_cursor : bool -> t -> t
(** [set_virtual_cursor enabled t] selects embedded or real-terminal cursor output. *)

val virtual_cursor : t -> bool
(** [virtual_cursor t] reports whether the embedded cursor is enabled. *)

val cursor : t -> Charm_tea.Cursor.t option
(** [cursor t] returns a real cursor request when the input is focused and virtual cursor
    output is disabled. *)

val show_suggestions : t -> bool
(** [show_suggestions t] reports whether completions are rendered. *)

val set_show_suggestions : bool -> t -> t
(** [set_show_suggestions enabled t] enables or disables completion rendering. *)

val set_suggestions : string list -> t -> t
(** [set_suggestions suggestions t] installs completion candidates in order. *)

val available_suggestions : t -> string list
(** [available_suggestions t] returns all installed candidates. *)

val matched_suggestions : t -> string list
(** [matched_suggestions t] returns case-insensitive prefix matches in order. *)

val current_suggestion : t -> string
(** [current_suggestion t] is the selected completion, or [""] when none exists. *)

val current_suggestion_index : t -> int
(** [current_suggestion_index t] is the selected completion index. *)
