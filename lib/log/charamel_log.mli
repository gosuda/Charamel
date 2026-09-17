(** A styled {!Logs} reporter.

    [reporter] renders reported log messages using {!Charamel_lipgloss} styles and a
    {!Charamel_colorprofile} profile. A message's source ({!Logs.Src.name}) becomes its
    prefix; a message's tags ({!Logs.Tag.set}) become its structured fields. [App]-level
    messages render without a level label (they use [Info]'s color when [Text] output asks
    for one, but the reporter never renders the label for [App]). *)

module Styles = Styles

(** The type for output layouts. [Logfmt] and [Json] never emit ANSI escapes: embedding
    one in a logfmt or JSON value would corrupt it, so they ignore [~styles] entirely. *)
type format =
  | Text  (** Styled, human-facing lines. Uses [~styles]. *)
  | Logfmt  (** Unstyled [key=value] lines. Ignores [~styles]. *)
  | Json  (** Unstyled, one JSON object per line. Ignores [~styles]. *)

val reporter :
  ?format:format ->
  ?styles:Styles.t ->
  ?time_format:(Ptime.t -> string) ->
  ?report_timestamp:bool ->
  ?report_caller:bool ->
  clock:_ Eio.Time.clock ->
  profile:Charamel_colorprofile.t ->
  Format.formatter ->
  Logs.reporter
(** [reporter ?format ?styles ?time_format ?report_timestamp ?report_caller ~clock
     ~profile ppf] is a {!Logs.reporter} that prints one line per reported message on
    [ppf].

    [format] defaults to [Text]. [styles] defaults to {!Styles.default} and is used by
    [Text] only; it is adjusted for [profile] once, when [reporter] is called, so every
    subsequent line already respects what [profile] can show. Under [Ansi], [Ansi256], and
    [True_color], only color slots are reduced, with {!Charamel_colorprofile.convert};
    every other attribute (bold, faint, a style's width, ...) renders as [styles]
    configures it. Under [Ascii] and [No_tty], every appearance attribute is cleared (no
    bold, no color, no underline, ...), matching {!Charamel_colorprofile.Writer}'s
    "removes all SGR sequences" contract for those two profiles; a style's geometry (its
    width, for instance a level label padded to a fixed width) is untouched, since
    geometry does not itself emit SGR.

    [time_format] renders a message's timestamp and defaults to a fixed
    ["%Y/%m/%d %H:%M:%S"]-shaped layout in UTC. [report_timestamp] and [report_caller]
    both default to [false]. When [report_timestamp] is [true], [clock] is read once per
    message to produce its timestamp. When [report_caller] is [true], a message's
    [?header] argument, if given, renders as its caller; {!Logs} never computes a caller
    location itself, so a call site that wants one must pass its own, for example
    [~header:(Fmt.str "%s:%d" __FILE__ __LINE__)].

    A message's [?tags] become one structured field per tag, ordered by tag name (a
    {!Logs.Tag.set} does not itself have an order). In [Text] and [Logfmt] output, a
    field's value is quoted and escaped when it is empty or contains whitespace, ["="],
    ["\""], or a control character; [Json] always quotes and escapes through {!Jsont}.

    [reporter]'s report function calls a report's [over] and its continuation exactly
    once, synchronously, after the line has been written and [ppf] flushed. *)
