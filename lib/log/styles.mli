(** Styled fields for the [charamel.log] text reporter.

    A [Styles.t] names the {!Charamel_lipgloss.Style.t} used for each part of a rendered
    log line. [Logfmt] and [Json] output never consults these styles: only [Text] output
    does. *)

type t = {
  timestamp : Charamel_lipgloss.Style.t;  (** Styles a rendered timestamp. *)
  caller : Charamel_lipgloss.Style.t;  (** Styles a rendered [<file:line>] caller. *)
  prefix : Charamel_lipgloss.Style.t;  (** Styles a rendered [name:] source prefix. *)
  message : Charamel_lipgloss.Style.t;  (** Styles the log message. *)
  key : Charamel_lipgloss.Style.t;  (** Styles a structured field's key. *)
  value : Charamel_lipgloss.Style.t;  (** Styles a structured field's value. *)
  separator : Charamel_lipgloss.Style.t;
      (** Styles the ["="] between a key and its value. *)
  levels : Logs.level -> Charamel_lipgloss.Style.t;
      (** [levels level] is the style for [level]'s label. The reporter chooses the label
          text itself ([levels] only supplies color, weight, and padding); it never asks
          for [App]'s style, since [App] renders without a label. *)
}
(** The type for text-format styles. *)

val default : t
(** [default] reproduces the charmbracelet/log palette: bold, five columns wide, with
    xterm-256 colors 63 ([Debug]), 86 ([Info], also used for [App]), 192 ([Warning]), and
    204 ([Error]). Every other field is unstyled except [caller], [prefix], [key], and
    [separator], which are faint (and [prefix] is also bold). *)
