(** Terminal input events.

    [t] is one decoded unit of terminal input or a runtime-synthesized notification.
    {!Charamel_tea.Input} produces every constructor except {!constructor:Profile},
    {!constructor:Resize} and {!constructor:Resume}, which the program runtime synthesizes
    itself: {!constructor:Profile} from {!Charamel_colorprofile.detect} at startup (colors
    are never negotiated by decoding terminal bytes), {!constructor:Resize} from
    [SIGWINCH] plus a window-size ioctl, not from an in-band terminal report, and
    {!constructor:Resume} when the program comes back to the foreground. *)

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
  | Background_color of Charamel_ansi.Color.t
  | Foreground_color of Charamel_ansi.Color.t
  | Cursor_color of Charamel_ansi.Color.t
  | Terminal_version of string
  | Kitty_flags of int  (** The active Kitty keyboard protocol flags, as reported. *)
  | Mode_report of { mode : int; value : int }
      (** A DEC private or ANSI mode report (DECRPM); [mode] is the numeric mode, [value]
          is the reported setting (0 not recognized, 1 set, 2 reset, 3 permanently set, 4
          permanently reset). *)
  | Profile of Charamel_colorprofile.t
  | Resume
      (** Synthesized by the runtime when the program resumes after a
          {!Charamel_tea.Cmd.suspend} or {!Charamel_tea.Cmd.exec} pause, or after an
          external [SIGCONT] stopped the process. Delivered through
          {!Charamel_tea.Sub.terminal}. Never synthesized on Windows, which has no job
          control, nor on a transport that is not the local terminal. *)
  | Clipboard of { selection : [ `System | `Primary ]; content : string }
      (** The reply to a {!Charamel_tea.Cmd.read_clipboard} query: OSC 52 with
          [ESC \] 52 ; c ; <base64>] for the clipboard and [ESC \] 52 ; p ; <base64>] for
          the primary selection, each terminated by [ST] or [BEL], and with the base64
          payload decoded. An empty [content] is a terminal that answered that nothing is
          set. A reply whose selection letter or base64 payload does not decode stays
          {!constructor:Unknown}. *)
  | Capability of string option
      (** The reply to a ~Capability:"name" {!Charamel_tea.Cmd.query} request, an
          XTGETTCAP DCS sequence: [Some value] decodes
          [ESC P 1 + r <hex name> = <hex value> ST] and [None] decodes the negative
          [ESC P 0 + r <hex name> ST] that reports the terminal lacks the capability. Only
          the value is carried, so a program that needs several capabilities queries them
          one at a time. *)
  | Unknown of string
      (** The raw bytes of a sequence that was fully parsed but has no event of its own
          here (device attribute reports, in-band window-size reports, Kitty graphics APC
          payloads, a cancelled/aborted control string, or any sequence this decoder does
          not recognize at all), or of a single byte that could not be decoded as a key,
          control code, or UTF-8 scalar. No byte is dropped. Every byte
          {!Charamel_tea.Input} consumes without producing a more specific event is
          preserved here. *)
