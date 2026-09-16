(** Terminal input events.

    [t] is one decoded unit of terminal input or a runtime-synthesized notification.
    {!Charm_tea.Input} produces every constructor except {!constructor:Profile} and
    {!constructor:Resize}, which the program runtime synthesizes itself:
    {!constructor:Profile} from {!Charm_colorprofile.detect} at startup (colors are never
    negotiated by decoding terminal bytes), and {!constructor:Resize} from [SIGWINCH] plus
    a window-size ioctl, not from an in-band terminal report. *)

type t =
  | Key of Key.t
  | Mouse of Mouse.t
  | Paste of string
      (** The complete content of one bracketed paste, or one 64 KiB segment of a paste
          longer than that, delivered as consecutive [Paste] events that concatenate back
          to the original bytes. Bytes are passed through exactly as received; a paste
          containing malformed UTF-8 is not repaired or filtered. *)
  | Focus
  | Blur
  | Resize of { rows : int; cols : int }
  | Cursor_position of { row : int; col : int }
      (** 0-based. Also delivered, alongside a synthetic {!constructor:Key} for [F 3],
          when a plain cursor-position report at row 1 is ambiguous with an unmodified-F3
          key report; see [Input] for the exact condition. *)
  | Background_color of Charm_ansi.Color.t
  | Foreground_color of Charm_ansi.Color.t
  | Cursor_color of Charm_ansi.Color.t
  | Terminal_version of string
  | Kitty_flags of int  (** The active Kitty keyboard protocol flags, as reported. *)
  | Mode_report of { mode : int; value : int }
      (** A DEC private or ANSI mode report (DECRPM); [mode] is the numeric mode, [value]
          is the reported setting (0 not recognized, 1 set, 2 reset, 3 permanently set, 4
          permanently reset). *)
  | Profile of Charm_colorprofile.t
  | Unknown of string
      (** The raw bytes of a sequence that was fully parsed but has no event of its own
          here (device attribute reports, XTGETTCAP replies, in-band window-size reports,
          the OSC 52 clipboard reply, Kitty graphics APC payloads, a cancelled/aborted
          control string, or any sequence this decoder does not recognize at all), or of a
          single byte that could not be decoded as a key, control code, or UTF-8 scalar.
          No byte is dropped. Every byte {!Charm_tea.Input} consumes without producing a
          more specific event is preserved here. *)
