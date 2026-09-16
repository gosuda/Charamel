(** Terminal control sequence constructors.

    Cursor coordinates start at one. Text payloads must be valid UTF-8 and cannot contain
    ESC, BEL, or C1 control scalars. Invalid payloads raise [Invalid_argument]. Clipboard
    data is opaque and is base64-encoded. *)

(** {1 Cursor movement} *)

val cup : row:int -> col:int -> string
(** [cup ~row ~col] is the cursor-position sequence. When both coordinates are at most one
    it selects the home position. Otherwise positive coordinates are explicit and
    nonpositive coordinates are omitted. *)

val cuu : int -> string
(** [cuu n] is the sequence that moves up [n] cells. Values at most one select the default
    movement of one cell. *)

val cud : int -> string
(** [cud n] is the sequence that moves down [n] cells. Values at most one select the
    default movement of one cell. *)

val cuf : int -> string
(** [cuf n] is the sequence that moves forward [n] cells. Values at most one select the
    default movement of one cell. *)

val cub : int -> string
(** [cub n] is the sequence that moves backward [n] cells. Values at most one select the
    default movement of one cell. *)

val save_cursor : string
(** [save_cursor] is the DEC sequence that saves the cursor position. *)

val restore_cursor : string
(** [restore_cursor] is the DEC sequence that restores the saved cursor. *)

(** {1 Erase} *)

val el : [ `To_end | `To_start | `All ] -> string
(** [el extent] is the sequence that erases the selected part of the current line without
    moving the cursor. *)

val ed : [ `Below | `Above | `All | `Scrollback ] -> string
(** [ed extent] is the sequence that erases the selected part of the display.
    [`Scrollback] also erases saved scrollback lines. *)

(** {1 Modes} *)

val decset : int -> string
(** [decset mode] is the sequence that enables DEC private [mode]. *)

val decrst : int -> string
(** [decrst mode] is the sequence that disables DEC private [mode]. *)

val alt_screen : int
(** [alt_screen] is the alternate-screen mode number. *)

val cursor_visible : int
(** [cursor_visible] is the cursor-visibility mode number. *)

val mouse_click : int
(** [mouse_click] is the mouse button-reporting mode number. *)

val mouse_motion : int
(** [mouse_motion] is the mouse drag-reporting mode number. *)

val mouse_all : int
(** [mouse_all] is the all-motion reporting mode number. *)

val mouse_sgr : int
(** [mouse_sgr] is the SGR mouse-encoding mode number. *)

val mouse_pixels : int
(** [mouse_pixels] is the pixel-coordinate mouse mode number. *)

val bracketed_paste : int
(** [bracketed_paste] is the bracketed-paste mode number. *)

val focus : int
(** [focus] is the focus-reporting mode number. *)

val sync_output : int
(** [sync_output] is the synchronized-output mode number. *)

val grapheme_clustering : int
(** [grapheme_clustering] is the grapheme-clustering mode number. *)

val mouse_on : mode:[ `Click | `Motion | `All ] -> string
(** [mouse_on ~mode] is the sequence that enables the requested reporting mode with SGR
    mouse encoding. *)

val mouse_off : string
(** [mouse_off] is the sequence that disables mouse reporting and encoding. *)

(** {1 Window title and notifications} *)

val title : string -> string
(** [title s] is the OSC 2 sequence that sets the window title to [s]. *)

val clipboard_osc52 : string -> string
(** [clipboard_osc52 bytes] is the OSC 52 sequence containing base64-encoded [bytes].
    Empty data clears the clipboard. *)

val notify_osc9 : string -> string
(** [notify_osc9 text] is the OSC 9 desktop notification for [text]. *)

(** {1 Queries} *)

val bg_query : string
(** [bg_query] is the default background-color query. *)

val fg_query : string
(** [fg_query] is the default foreground-color query. *)

val cursor_color_query : string
(** [cursor_color_query] is the cursor-color query. *)

val da1 : string
(** [da1] is the primary device-attributes query. *)

val xtversion : string
(** [xtversion] is the terminal name and version query. *)

(** {1 Kitty keyboard} *)

val kitty_push : int -> string
(** [kitty_push flags] is the sequence that pushes keyboard [flags] onto the terminal's
    protocol stack. Nonpositive values select zero flags. *)

val kitty_pop : string
(** [kitty_pop] is the sequence that pops the keyboard protocol stack. *)
