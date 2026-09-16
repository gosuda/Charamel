(** Keyboard keys.

    [t] is a single key press, repeat, or release, decoded from terminal input or built by
    hand for keymap matching. [code] names the key; [mods] names the modifiers held with
    it; [text] carries the printable text the key produced, when any. *)

(** The key pressed. [Char u] is any printable Unicode scalar not named below, including
    letters, digits, and punctuation; ['A'] is reported as [Char (Uchar.of_char 'a')] with
    [mods.shift = true], never as a distinct upper-case code.

    Two upstream keypad codes have no slot here: the Kitty keyboard protocol's keypad
    separator (the numpad [,] key) and legacy application-keypad comma report as
    [Char (Uchar.of_char ',')] instead of a dedicated code, matching how every other
    printable keypad key (0-9, ., /, *, -, +) with no functional role beyond its glyph is
    already represented. *)
type code =
  | Char of Uchar.t
  | Enter
  | Tab
  | Backspace
  | Escape
  | Space
  | Insert
  | Delete
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | F of int  (** A function key, [F 1] through [F 63]. *)
  | Kp_0
  | Kp_1
  | Kp_2
  | Kp_3
  | Kp_4
  | Kp_5
  | Kp_6
  | Kp_7
  | Kp_8
  | Kp_9
  | Kp_decimal
  | Kp_divide
  | Kp_multiply
  | Kp_subtract
  | Kp_add
  | Kp_enter
  | Kp_equal
  | Kp_begin
      (** Also reported for the VT keypad/[CSI E]/[SS3 E] "begin" key, which upstream
          names distinctly but which no consumer in this repository treats differently
          from a keypad begin. *)
  | Caps_lock
  | Scroll_lock
  | Num_lock
  | Print_screen
  | Pause
  | Menu
  | Media_play
  | Media_pause
  | Media_play_pause
  | Media_stop
  | Media_next
  | Media_prev
  | Media_record
  | Media_fast_forward
  | Media_rewind
      (** Also reported for the Kitty keyboard protocol's distinct "media reverse" key
          (codepoint 57431), which every terminal that emits it uses to mean "skip
          backward", indistinguishable in effect from rewind for any consumer here. *)
  | Volume_up
  | Volume_down
  | Volume_mute
  | Left_shift
  | Left_ctrl
  | Left_alt
  | Left_super
  | Left_hyper
  | Left_meta
  | Right_shift
  | Right_ctrl
  | Right_alt
  | Right_super
  | Right_hyper
  | Right_meta
  | Iso_level3_shift
  | Iso_level5_shift

type mods = {
  shift : bool;
  alt : bool;
  ctrl : bool;
  meta : bool;
  super : bool;
  hyper : bool;
  caps_lock : bool;
  num_lock : bool;
}
(** The modifier keys held with a key press. [caps_lock] and [num_lock] report lock state,
    not a key held down; they are only ever set by the Kitty keyboard protocol or an
    explicit legacy modifier byte that carries their bits. *)

(** Whether a key was pressed, is auto-repeating, or was released. Only the Kitty keyboard
    protocol and a handful of legacy extensions report [Repeat] or [Release]; every other
    decode path reports [Press]. *)
type event = Press | Repeat | Release

type t = {
  code : code;
  mods : mods;
  text : string;
      (** The printable text the key produced, verbatim, or [""] for a key that has none
          (control keys, bare modifiers, or a printable key decoded without a text
          payload). Populated only for keys that represent printable character(s). *)
  shifted : Uchar.t option;
      (** The shifted form of [code] on the user's physical layout, when the decoding
          source reports it (Kitty keyboard protocol only). [None] otherwise. *)
  base : Uchar.t option;
      (** [code] under the standard PC-101 layout, when the decoding source reports it
          (Kitty keyboard protocol only). [None] otherwise. *)
  event : event;
}

val v : ?mods:mods -> code -> t
(** [v ?mods code] is a key press with [code] and [mods] (default: no modifiers held). A
    space character is represented by [Space]. Modifier flags named by [code] and lock
    flags for [Caps_lock] or [Num_lock] are cleared so the canonical binding is unique. *)

val to_string : t -> string
(** [to_string k] is the canonical textual representation of [k]. Held modifiers are
    written in the fixed order ctrl, alt, shift, meta, super, hyper, caps_lock, num_lock,
    followed by the name of [k.code]. A modifier named by [k.code] is omitted. A plus
    character is written as ["plus"] when modifiers precede it and as ["+"] otherwise.

    Named codes use their lower-case constructor name with underscores kept (["page_up"],
    ["kp_enter"], ["media_fast_forward"], ["iso_level3_shift"]); [F n] prints as
    ["f" ^ string_of_int n]. A space is written as ["space"]. *)

val of_string : string -> (t, [ `Msg of string ]) result
(** [of_string s] parses [s] as produced by {!to_string}. Modifier names and a code name
    are joined by ["+"], or a single Unicode scalar denotes a printable key. Every name
    {!to_string} can produce is accepted. ["esc"], ["pgup"], and ["pgdown"] are aliases of
    ["escape"], ["page_up"], and ["page_down"]. The final component is interpreted as the
    code, so lock-key names remain distinct from lock-state modifiers. The returned key
    has [text = ""], [shifted = None], [base = None], and [event = Press]. *)

val matches : t -> t -> bool
(** [matches k1 k2] is [true] when [k1] and [k2] name the same key and hold the same
    modifiers. [text], [shifted], [base], and [event] are ignored, so a [Press] and a
    [Release] of the same key with the same modifiers match. *)

val pp : t Fmt.t
(** [pp] prints [Fmt.string (to_string k)]. *)
