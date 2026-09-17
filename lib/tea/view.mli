(** The declarative terminal frame a program renders each tick.

    [t] is everything about the terminal a program wants for one frame: the content grid,
    whether the application cursor shows, terminal modes such as the alternate screen and
    mouse reporting, and the OS-level chrome a program can ask for (window title, default
    foreground and background, a progress indicator). A renderer reconciles [t] against
    what it last applied and emits only the sequences that changed; nothing in this module
    talks to a terminal. *)

type mouse_mode =
  | Mouse_off
  | Mouse_click
  | Mouse_motion
  | Mouse_all
      (** The type for mouse reporting. [Mouse_click] reports button press and release
          only, [Mouse_motion] additionally reports motion while a button is held, and
          [Mouse_all] reports every motion event regardless of button state. *)

type keyboard = {
  disambiguate : bool;
  report_events : bool;
  report_alternates : bool;
  report_all_keys : bool;
  report_text : bool;
}
(** The type for the Kitty keyboard protocol enhancements a view requests. *)

type progress =
  | Progress_none
  | Progress_indeterminate
  | Progress_value of int
  | Progress_error of int
  | Progress_warning of int
      (** The type for an OS-level progress indicator (for example a taskbar progress
          bar). [Progress_value], [Progress_error] and [Progress_warning] carry a
          percentage clamped to [0, 100] wherever it is applied. *)

type t = {
  content : string;
  cursor : Cursor.t option;
  alt_screen : bool;
  mouse : mouse_mode;
  bracketed_paste : bool;
  report_focus : bool;
  title : string option;
  keyboard : keyboard;
  background : Charamel_ansi.Color.t option;
  foreground : Charamel_ansi.Color.t option;
  progress : progress;
}
(** The type for one frame. [cursor] is [None] when the application cursor should be
    hidden. *)

val v :
  ?cursor:Cursor.t ->
  ?alt_screen:bool ->
  ?mouse:mouse_mode ->
  ?bracketed_paste:bool ->
  ?report_focus:bool ->
  ?title:string ->
  ?keyboard:keyboard ->
  ?background:Charamel_ansi.Color.t ->
  ?foreground:Charamel_ansi.Color.t ->
  ?progress:progress ->
  string ->
  t
(** [v content] is a frame showing [content]. Defaults: [cursor] is [None] (hidden),
    [alt_screen] is [false], [mouse] is [Mouse_off], [bracketed_paste] is [true],
    [report_focus] is [false], [title] is [None], [keyboard] is
    [{ disambiguate = true; report_events = false; report_alternates = false;
     report_all_keys = false; report_text = false }], [background] and [foreground] are
    [None], and [progress] is [Progress_none]. *)
