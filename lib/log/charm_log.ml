module Styles = Styles

type format = Text | Logfmt | Json

(* [Charm_colorprofile.convert] only reduces color slots. Under [No_tty]/[Ascii],
   {!Charm_colorprofile.Writer} documents removing every SGR sequence, not just color
   parameters, so every appearance attribute is cleared for those two profiles; geometry
   ([width], [padding], [border], ...) is untouched either way, since it never produces
   SGR bytes by itself. *)
let clear_appearance style =
  style |> Charm_lipgloss.Style.unset_bold |> Charm_lipgloss.Style.unset_italic
  |> Charm_lipgloss.Style.unset_underline |> Charm_lipgloss.Style.unset_underline_style
  |> Charm_lipgloss.Style.unset_underline_color
  |> Charm_lipgloss.Style.unset_strikethrough |> Charm_lipgloss.Style.unset_reverse
  |> Charm_lipgloss.Style.unset_blink |> Charm_lipgloss.Style.unset_faint
  |> Charm_lipgloss.Style.unset_underline_spaces
  |> Charm_lipgloss.Style.unset_strikethrough_spaces
  |> Charm_lipgloss.Style.unset_color_whitespace |> Charm_lipgloss.Style.unset_foreground
  |> Charm_lipgloss.Style.unset_background |> Charm_lipgloss.Style.unset_margin_background
  |> Charm_lipgloss.Style.unset_border_foreground
  |> Charm_lipgloss.Style.unset_border_background

let adjust_color profile c = Charm_colorprofile.convert profile c

let apply_adjusted profile ~get ~set style =
  match get style with Some c -> set (adjust_color profile c) style | None -> style

let adjust_style profile style =
  match (profile : Charm_colorprofile.t) with
  | No_tty | Ascii -> clear_appearance style
  | Ansi | Ansi256 | True_color ->
      style
      |> apply_adjusted profile ~get:Charm_lipgloss.Style.get_foreground
           ~set:Charm_lipgloss.Style.foreground
      |> apply_adjusted profile ~get:Charm_lipgloss.Style.get_background
           ~set:Charm_lipgloss.Style.background
      |> apply_adjusted profile ~get:Charm_lipgloss.Style.get_underline_color
           ~set:Charm_lipgloss.Style.underline_color

let adjust_styles profile (s : Styles.t) : Styles.t =
  {
    Styles.timestamp = adjust_style profile s.Styles.timestamp;
    caller = adjust_style profile s.Styles.caller;
    prefix = adjust_style profile s.Styles.prefix;
    message = adjust_style profile s.Styles.message;
    key = adjust_style profile s.Styles.key;
    value = adjust_style profile s.Styles.value;
    separator = adjust_style profile s.Styles.separator;
    levels = (fun level -> adjust_style profile (s.Styles.levels level));
  }

let default_time_format t =
  let (y, m, d), ((hh, mm, ss), _) = Ptime.to_date_time t in
  Fmt.str "%04d/%02d/%02d %02d:%02d:%02d" y m d hh mm ss

(* Lowercase word for [Logfmt]/[Json], matching the charmbracelet/log level names. [App]
   never reaches this: every renderer skips the level field for it before calling it. *)
let level_word = function
  | Logs.Debug -> "debug"
  | Logs.Info -> "info"
  | Logs.Warning -> "warn"
  | Logs.Error -> "error"
  | Logs.App -> "app"

let level_label level = String.uppercase_ascii (level_word level)

(* logfmt-style quoting: a value is quoted when empty or when it carries whitespace, an
   ["="], a ["\""], or a control character that would otherwise break a bare token. *)
let needs_quoting s =
  s = ""
  || String.exists
       (fun c ->
         c = ' ' || c = '"' || c = '=' || Char.code c < 0x20 || Char.code c = 0x7f)
       s

let escape_into buf s =
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          Buffer.add_string buf (Fmt.str "\\x%02x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s

let quote_value s =
  if not (needs_quoting s) then s
  else begin
    let buf = Buffer.create (String.length s + 2) in
    Buffer.add_char buf '"';
    escape_into buf s;
    Buffer.add_char buf '"';
    Buffer.contents buf
  end

let resolved_tags tags =
  Logs.Tag.fold
    (fun (Logs.Tag.V (d, v)) acc ->
      (Logs.Tag.name d, Fmt.str "%a" (Logs.Tag.printer d) v) :: acc)
    (Option.value ~default:Logs.Tag.empty tags)
    []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let render_text (styles : Styles.t) ~ts ~level ~caller ~prefix ~message ~tags =
  let buf = Buffer.create 128 in
  let first = ref true in
  let emit rendered =
    if not !first then Buffer.add_char buf ' ';
    first := false;
    Buffer.add_string buf rendered
  in
  Option.iter (fun s -> emit (Charm_lipgloss.Style.render styles.Styles.timestamp s)) ts;
  (match level with
  | Logs.App -> ()
  | level ->
      emit (Charm_lipgloss.Style.render (styles.Styles.levels level) (level_label level)));
  Option.iter
    (fun c -> emit (Charm_lipgloss.Style.render styles.Styles.caller (Fmt.str "<%s>" c)))
    caller;
  emit (Charm_lipgloss.Style.render styles.Styles.prefix (prefix ^ ":"));
  emit (Charm_lipgloss.Style.render styles.Styles.message message);
  List.iter
    (fun (k, v) ->
      if k <> "" then begin
        let key = Charm_lipgloss.Style.render styles.Styles.key k in
        let sep = Charm_lipgloss.Style.render styles.Styles.separator "=" in
        let value = Charm_lipgloss.Style.render styles.Styles.value (quote_value v) in
        emit (key ^ sep ^ value)
      end)
    tags;
  Buffer.add_char buf '\n';
  Buffer.contents buf

let render_logfmt ~ts ~level ~caller ~prefix ~message ~tags =
  let buf = Buffer.create 128 in
  let first = ref true in
  let emit k v =
    if not !first then Buffer.add_char buf ' ';
    first := false;
    Buffer.add_string buf k;
    Buffer.add_char buf '=';
    Buffer.add_string buf (quote_value v)
  in
  Option.iter (emit "time") ts;
  (match level with Logs.App -> () | level -> emit "level" (level_word level));
  Option.iter (emit "caller") caller;
  emit "prefix" prefix;
  emit "msg" message;
  List.iter (fun (k, v) -> if k <> "" then emit k v) tags;
  Buffer.add_char buf '\n';
  Buffer.contents buf

let render_json ~ts ~level ~caller ~prefix ~message ~tags =
  let member k v = (Jsont.Json.name k, Jsont.Json.string v) in
  let leading =
    List.filter_map Fun.id
      [
        Option.map (member "time") ts;
        (match level with
        | Logs.App -> None
        | level -> Some (member "level" (level_word level)));
        Option.map (member "caller") caller;
      ]
  in
  let tag_members =
    List.filter_map (fun (k, v) -> if k = "" then None else Some (member k v)) tags
  in
  let members =
    leading @ [ member "prefix" prefix; member "msg" message ] @ tag_members
  in
  let obj = Jsont.Object (members, Jsont.Meta.none) in
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json obj with
  | Ok s -> s ^ "\n"
  | Error msg -> Fmt.failwith "charm.log: json encoding failed: %s" msg

let emit_line ~format ~styles ~time_format ~report_timestamp ~report_caller ~clock ppf
    ~level ~src ~header ~tags message =
  let ts =
    if report_timestamp then
      Option.map time_format (Ptime.of_float_s (Eio.Time.now clock))
    else None
  in
  let caller = if report_caller then header else None in
  let prefix = Logs.Src.name src in
  let tags = resolved_tags tags in
  let line =
    match format with
    | Text -> render_text styles ~ts ~level ~caller ~prefix ~message ~tags
    | Logfmt -> render_logfmt ~ts ~level ~caller ~prefix ~message ~tags
    | Json -> render_json ~ts ~level ~caller ~prefix ~message ~tags
  in
  Format.pp_print_string ppf line;
  Format.pp_print_flush ppf ()

let reporter ?(format = Text) ?(styles = Styles.default) ?time_format
    ?(report_timestamp = false) ?(report_caller = false) ~clock ~profile ppf =
  let styles = adjust_styles profile styles in
  let time_format = Option.value ~default:default_time_format time_format in
  {
    Logs.report =
      (fun src level ~over k msgf ->
        (* [k] runs before [over]: {!Logs.report} wraps this whole function in a
           try/with that calls [over] again on an exception, so calling [over] only
           after [k] has returned keeps it to exactly one call even if a caller's own
           [k] (reachable through {!Logs.kmsg}) raises. {!Logs.nop_reporter} calls
           [over] first since its own [k] can never raise; ours may. *)
        let k' () =
          let r = k () in
          over ();
          r
        in
        msgf (fun ?header ?tags fmt ->
            Format.kasprintf
              (fun message ->
                emit_line ~format ~styles ~time_format ~report_timestamp ~report_caller
                  ~clock ppf ~level ~src ~header ~tags message;
                k' ())
              fmt));
  }
