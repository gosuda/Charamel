(** Terminal background query and profile-keyed colour selection.

    {!val:background_color} asks the terminal what its default background is, the way
    upstream lipgloss does before it decides whether a theme should be light or dark. The
    reply is an OSC 11 sequence, so a terminal that does not answer is indistinguishable
    from one that answers too late: both leave the caller with no colour. The query is
    therefore best-effort by contract, never an error.

    The channels are injectable so a test can drive the exchange over a buffer instead of
    a real terminal. When they are not injected, the query reads the process's controlling
    terminal and writes standard output, putting standard input into raw mode for the
    duration and restoring it afterwards. *)

val background_color :
  ?timeout:float ->
  ?input:Lwt_io.input_channel ->
  ?output:Lwt_io.output_channel ->
  unit ->
  Charamel_ansi.Color.t option Lwt.t
(** [background_color ?timeout ?input ?output ()] writes the OSC 11 default-background
    query and the primary-device-attributes query to [output], flushes it, and reads
    [input] until an OSC 11 reply parses or [timeout] seconds elapse. [timeout] defaults
    to [2.0]. [input] defaults to the controlling terminal and [output] to standard
    output.

    [None] means no usable reply: the terminal stayed silent, the timeout expired, the
    reply could not be parsed, or the terminal could not be opened. A reply of the form
    [rgb:red/green/blue] is scaled to 8-bit components; any other spec is read as a color
    name or hexadecimal triple.

    When [input] is not injected, standard input is put into raw mode for the duration of
    the query and restored whatever the outcome. An injected channel is only read: its
    owner keeps its modes and its descriptor. *)

val has_dark_background :
  ?timeout:float ->
  ?input:Lwt_io.input_channel ->
  ?output:Lwt_io.output_channel ->
  unit ->
  bool Lwt.t
(** [has_dark_background ?timeout ?input ?output ()] is
    {!val:Charamel_lipgloss.Color_util.is_dark} of the reply, and [true] when there is no
    reply. Upstream treats an unanswered query as a dark background, so a caller that does
    not handle the failure gets the dark theme. *)

type triple = Charamel_lipgloss.Color_util.triple
(** The type for one color per output capability: [ansi], [ansi256] and [truecolor]. *)

val complete : Charamel_colorprofile.t -> triple -> Charamel_ansi.Color.t
(** [complete profile triple] is {!val:Charamel_lipgloss.Color_util.complete}: the slot of
    [triple] that [profile] can display. *)

val complete_adaptive :
  Charamel_colorprofile.t ->
  dark:bool ->
  light:triple ->
  night:triple ->
  Charamel_ansi.Color.t
(** [complete_adaptive profile ~dark ~light ~night] is
    {!val:Charamel_lipgloss.Color_util.complete_adaptive}: the [light] or [night] triple
    by [dark], resolved through [profile]. *)
