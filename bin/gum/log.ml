module Env = Charamel_cli.Env

type formatter = Text | Logfmt | Json
type level = None_ | Debug | Info | Warn | Error | Fatal

let format_message format arguments =
  let output = Buffer.create (String.length format + 16) in
  let next = ref 0 in
  let argument () =
    if !next >= List.length arguments then None
    else
      let value = List.nth arguments !next in
      incr next;
      Some value
  in
  let add_argument verb =
    match argument () with
    | None ->
        Buffer.add_char output '%';
        Buffer.add_char output verb
    | Some value -> (
        match verb with
        | 'q' ->
            Buffer.add_char output '"';
            Buffer.add_string output (String.escaped value);
            Buffer.add_char output '"'
        | _ -> Buffer.add_string output value)
  in
  let rec loop index =
    if index >= String.length format then Buffer.contents output
    else if format.[index] <> '%' then (
      Buffer.add_char output format.[index];
      loop (index + 1))
    else if index + 1 >= String.length format then (
      Buffer.add_char output '%';
      loop (index + 1))
    else
      match format.[index + 1] with
      | '%' ->
          Buffer.add_char output '%';
          loop (index + 2)
      | ('s' | 'd' | 'v' | 'q') as verb ->
          add_argument verb;
          loop (index + 2)
      | verb ->
          Buffer.add_char output '%';
          Buffer.add_char output verb;
          loop (index + 2)
  in
  loop 0

let weekday = function
  | `Sun -> "Sun"
  | `Mon -> "Mon"
  | `Tue -> "Tue"
  | `Wed -> "Wed"
  | `Thu -> "Thu"
  | `Fri -> "Fri"
  | `Sat -> "Sat"

let month = function
  | 1 -> "Jan"
  | 2 -> "Feb"
  | 3 -> "Mar"
  | 4 -> "Apr"
  | 5 -> "May"
  | 6 -> "Jun"
  | 7 -> "Jul"
  | 8 -> "Aug"
  | 9 -> "Sep"
  | 10 -> "Oct"
  | 11 -> "Nov"
  | 12 -> "Dec"
  | _ -> "???"

let long_month = function
  | 1 -> "January"
  | 2 -> "February"
  | 3 -> "March"
  | 4 -> "April"
  | 5 -> "May"
  | 6 -> "June"
  | 7 -> "July"
  | 8 -> "August"
  | 9 -> "September"
  | 10 -> "October"
  | 11 -> "November"
  | 12 -> "December"
  | _ -> "Unknown"

let pad2 value = Fmt.str "%02d" value

let layout_name name =
  match String.lowercase_ascii name with
  | "layout" -> "01/02 03:04:05PM '06 -0700"
  | "ansic" -> "Mon Jan _2 15:04:05 2006"
  | "unixdate" -> "Mon Jan _2 15:04:05 MST 2006"
  | "rubydate" -> "Mon Jan 02 15:04:05 -0700 2006"
  | "rfc822" -> "02 Jan 06 15:04 MST"
  | "rfc822z" -> "02 Jan 06 15:04 -0700"
  | "rfc850" -> "Monday, 02-Jan-06 15:04:05 MST"
  | "rfc1123" -> "Mon, 02 Jan 2006 15:04:05 MST"
  | "rfc1123z" -> "Mon, 02 Jan 2006 15:04:05 -0700"
  | "rfc3339" -> "2006-01-02T15:04:05Z07:00"
  | "rfc3339nano" -> "2006-01-02T15:04:05.999999999Z07:00"
  | "kitchen" -> "3:04PM"
  | "stamp" -> "Jan _2 15:04:05"
  | "stampmilli" -> "Jan _2 15:04:05.000"
  | "stampmicro" -> "Jan _2 15:04:05.000000"
  | "stampnano" -> "Jan _2 15:04:05.000000000"
  | "datetime" -> "2006-01-02 15:04:05"
  | "dateonly" -> "2006-01-02"
  | "timeonly" -> "15:04:05"
  | value -> value

let fractional_digits time =
  let rfc3339 = Ptime.to_rfc3339 ~frac_s:9 ~tz_offset_s:0 time in
  match String.index_opt rfc3339 '.' with
  | None -> "000000000"
  | Some dot ->
      let available = min 9 (String.length rfc3339 - dot - 1) in
      let digits = String.sub rfc3339 (dot + 1) available in
      if available = 9 then digits else digits ^ String.make (9 - available) '0'

let render_layout layout time =
  let (year, month_number, day), ((hour, minute, second), _tz_offset) =
    Ptime.to_date_time time
  in
  let hour12 =
    let hour = hour mod 12 in
    if hour = 0 then 12 else hour
  in
  let am_pm = if hour < 12 then "AM" else "PM" in
  let am_pm_lower = String.lowercase_ascii am_pm in
  let month_name = month month_number in
  let long_month_name = long_month month_number in
  let long_weekday =
    match Ptime.weekday time with
    | `Sun -> "Sunday"
    | `Mon -> "Monday"
    | `Tue -> "Tuesday"
    | `Wed -> "Wednesday"
    | `Thu -> "Thursday"
    | `Fri -> "Friday"
    | `Sat -> "Saturday"
  in
  let nanosecond_text = fractional_digits time in
  let nanosecond_trimmed =
    let index = ref (String.length nanosecond_text) in
    while !index > 0 && nanosecond_text.[!index - 1] = '0' do
      decr index
    done;
    String.sub nanosecond_text 0 !index
  in
  let token_at index token =
    let length = String.length token in
    index + length <= String.length layout && String.sub layout index length = token
  in
  let output = Buffer.create (String.length layout + 16) in
  let rec loop index =
    if index >= String.length layout then Buffer.contents output
    else
      let emit text count =
        Buffer.add_string output text;
        loop (index + count)
      in
      if token_at index ".999999999" then
        if nanosecond_trimmed = "" then emit "" 10 else emit ("." ^ nanosecond_trimmed) 10
      else if token_at index ".000000000" then emit ("." ^ nanosecond_text) 10
      else if token_at index ".000000" then emit ("." ^ String.sub nanosecond_text 0 6) 7
      else if token_at index ".000" then emit ("." ^ String.sub nanosecond_text 0 3) 4
      else if token_at index "2006" then emit (Fmt.str "%04d" year) 4
      else if token_at index "Monday" then emit long_weekday 6
      else if token_at index "Mon" then emit (weekday (Ptime.weekday time)) 3
      else if token_at index "January" then emit long_month_name 7
      else if token_at index "Jan" then emit month_name 3
      else if token_at index "Z07:00" then emit "Z" 6
      else if token_at index "-0700" then emit "+0000" 5
      else if token_at index "-07:00" then emit "+00:00" 6
      else if token_at index "01" then emit (pad2 month_number) 2
      else if token_at index "02" then emit (pad2 day) 2
      else if token_at index "_2" then emit (Fmt.str "%2d" day) 2
      else if token_at index "15" then emit (pad2 hour) 2
      else if token_at index "03" then emit (pad2 hour12) 2
      else if token_at index "04" then emit (pad2 minute) 2
      else if token_at index "05" then emit (pad2 second) 2
      else if token_at index "06" then emit (Fmt.str "%02d" (year mod 100)) 2
      else if token_at index "PM" then emit am_pm 2
      else if token_at index "pm" then emit am_pm_lower 2
      else if token_at index "1" then emit (string_of_int month_number) 1
      else if token_at index "2" then emit (string_of_int day) 1
      else if token_at index "3" then emit (string_of_int hour12) 1
      else if token_at index "4" then emit (string_of_int minute) 1
      else if token_at index "5" then emit (string_of_int second) 1
      else if token_at index "6" then emit (Fmt.str "%02d" (year mod 100)) 1
      else (
        Buffer.add_char output layout.[index];
        loop (index + 1))
  in
  loop 0

let time_formatter layout time = render_layout (layout_name layout) time

let logs_level = function
  | None_ -> Logs.App
  | Debug -> Logs.Debug
  | Info -> Logs.Info
  | Warn -> Logs.Warning
  | Error | Fatal -> Logs.Error

let level_rank = function
  | None_ -> 0
  | Debug -> 0
  | Info -> 1
  | Warn -> 2
  | Error -> 3
  | Fatal -> 4

let parse_minimum text =
  match String.lowercase_ascii text with
  | "" -> Ok 0
  | "debug" -> Ok 0
  | "info" -> Ok 1
  | "warn" | "warning" -> Ok 2
  | "error" -> Ok 3
  | "fatal" -> Ok 4
  | value -> Result.Error (`Msg (Fmt.str "invalid log level: %s" value))

let make_tags fields =
  let rec pairs acc = function
    | key :: value :: rest -> pairs ((key, value) :: acc) rest
    | [ key ] -> List.rev ((key, "") :: acc)
    | [] -> List.rev acc
  in
  pairs [] fields
  |> List.fold_left
       (fun tags (key, value) ->
         let definition = Logs.Tag.def key Stdlib.Format.pp_print_string in
         Logs.Tag.add definition value tags)
       Logs.Tag.empty

let make_styles ~level_style ~time_style ~prefix_style ~message_style ~key_style
    ~value_style ~separator_style ~emitted =
  let defaults = Charamel_log.Styles.default in
  let level_style = Gum_style.inline level_style in
  let levels level =
    if level = emitted then
      Charamel_lipgloss.Style.inherit_
        ~parent:(defaults.Charamel_log.Styles.levels level)
        level_style
    else defaults.Charamel_log.Styles.levels level
  in
  {
    Charamel_log.Styles.timestamp = Gum_style.inline time_style;
    caller = defaults.Charamel_log.Styles.caller;
    prefix = Gum_style.inline prefix_style;
    message = Gum_style.inline message_style;
    key = Gum_style.inline key_style;
    value = Gum_style.inline value_style;
    separator = Gum_style.inline separator_style;
    levels;
  }

let emit ?file ?(formatter = Text) ?(level = None_) ?(min_level = "") ?(prefix = "")
    ?(time = "") ?(format = false) ?(structured = false) ?styles
    (env : Charamel_cli.Env.t) texts =
  if format && structured then
    Charamel_cli.error ~code:2 "--format and --structured are mutually exclusive";
  let minimum =
    match parse_minimum min_level with
    | Ok rank -> rank
    | Error (`Msg message) -> Charamel_cli.error message
  in
  if level_rank level < minimum then Lwt.return_unit
  else
    let emitted = logs_level level in
    let message, tags =
      match texts with
      | [] -> ("", Logs.Tag.empty)
      | first :: rest when structured -> (first, make_tags rest)
      | first :: rest when format -> (format_message first rest, Logs.Tag.empty)
      | _ -> (String.concat " " texts, Logs.Tag.empty)
    in
    let profile =
      match file with
      | Some path when path <> "" -> Charamel_colorprofile.No_tty
      | _ ->
          Charamel_colorprofile.detect ~is_tty:(Gum_io.stderr_is_tty env)
            ~env:Sys.getenv_opt
    in
    let styles = Option.value ~default:Charamel_log.Styles.default styles in
    let report ppf =
      let output_format =
        match formatter with
        | Text -> Charamel_log.Text
        | Logfmt -> Charamel_log.Logfmt
        | Json -> Charamel_log.Json
      in
      let reporter =
        Charamel_log.reporter ~format:output_format ~styles ~report_timestamp:(time <> "")
          ?time_format:(if time = "" then None else Some (time_formatter time))
          ~clock:env.Env.clock ~profile ppf
      in
      let source = Logs.Src.create prefix in
      Logs.Src.set_level source (Some Logs.Debug);
      let previous = Logs.reporter () in
      Fun.protect
        ~finally:(fun () -> Logs.set_reporter previous)
        (fun () ->
          Logs.set_reporter reporter;
          Logs.msg ~src:source emitted (fun message_fn -> message_fn ~tags "%s" message))
    in
    let result =
      match file with
      | Some path when path <> "" -> (
          try
            let resolved =
              if Filename.is_relative path then Filename.concat env.Env.cwd path else path
            in
            let fd =
              Unix.openfile resolved [ Unix.O_WRONLY; Unix.O_APPEND; Unix.O_CREAT ] 0o644
            in
            let oc = Unix.out_channel_of_descr fd in
            Fun.protect
              ~finally:(fun () -> close_out_noerr oc)
              (fun () ->
                let ppf = Stdlib.Format.formatter_of_out_channel oc in
                report ppf;
                Stdlib.Format.pp_print_flush ppf ());
            Ok ()
          with Unix.Unix_error (error, function_name, argument) ->
            Result.Error
              (`Msg
                 (Fmt.str "error opening file: %s (%s %s)" (Unix.error_message error)
                    function_name argument)))
      | _ ->
          report Stdlib.Format.err_formatter;
          Ok ()
    in
    (match result with Error (`Msg message) -> Charamel_cli.error message | Ok () -> ());
    if level = Fatal then Charamel_cli.exit 1 else Lwt.return_unit

let string_opt ?short ~cmd name ~default ~doc =
  let names =
    match short with
    | None -> [ name ]
    | Some character -> [ String.make 1 character; name ]
  in
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt Cmdliner.Arg.string default
       (Cmdliner.Arg.info names ~doc ~env:(Gum_flag.env ~cmd name)))

let formatter_term =
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt
       (Gum_flag.enum ~docv:"FORMATTER"
          [ ("text", Text); ("logfmt", Logfmt); ("json", Json) ])
       Text
       (Cmdliner.Arg.info [ "formatter" ] ~doc:"Output formatter."
          ~env:(Gum_flag.env ~cmd:"log" "formatter")))

let level_term =
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt
       (Gum_flag.enum ~docv:"LEVEL"
          [
            ("none", None_);
            ("debug", Debug);
            ("info", Info);
            ("warn", Warn);
            ("error", Error);
            ("fatal", Fatal);
          ])
       None_
       (Cmdliner.Arg.info [ "level"; "l" ] ~doc:"Message level."
          ~env:(Gum_flag.env ~cmd:"log" "level-name")))

let minimum_conv =
  Cmdliner.Arg.Conv.make ~docv:"LEVEL"
    ~parser:(fun value ->
      match parse_minimum value with
      | Ok _ -> Ok (String.lowercase_ascii value)
      | Error (`Msg message) -> Error message)
    ~pp:(fun ppf value -> Fmt.string ppf value)
    ()

let min_level_term =
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt minimum_conv ""
       (Cmdliner.Arg.info [ "min-level" ] ~doc:"Minimum level to show."
          ~env:(Gum_flag.env ~cmd:"log" "level")))

let style_terms () =
  let level =
    Gum_style.term ~cmd:"log" ~prefix:"level."
      ~defaults:(Gum_style.defaults ~bold:true ())
      ()
  in
  let time = Gum_style.term ~cmd:"log" ~prefix:"time." ~defaults:Gum_style.empty () in
  let prefix =
    Gum_style.term ~cmd:"log" ~prefix:"prefix."
      ~defaults:(Gum_style.defaults ~bold:true ~faint:true ())
      ()
  in
  let message =
    Gum_style.term ~cmd:"log" ~prefix:"message." ~defaults:Gum_style.empty ()
  in
  let key =
    Gum_style.term ~cmd:"log" ~prefix:"key."
      ~defaults:(Gum_style.defaults ~faint:true ())
      ()
  in
  let value = Gum_style.term ~cmd:"log" ~prefix:"value." ~defaults:Gum_style.empty () in
  let separator =
    Gum_style.term ~cmd:"log" ~prefix:"separator."
      ~defaults:(Gum_style.defaults ~faint:true ())
      ()
  in
  (level, time, prefix, message, key, value, separator)

let cmd env =
  let open Cmdliner in
  let file =
    string_opt ~short:'o' ~cmd:"log" "file" ~default:"" ~doc:"Append logs to FILE."
  in
  let printf =
    Gum_flag.flag ~cmd:"log" ~env:false ~short:'f' ~doc:"Format the message." "format"
  in
  let structured =
    Gum_flag.flag ~cmd:"log" ~env:false ~short:'s' ~doc:"Use structured key/value fields."
      "structured"
  in
  let formatter = formatter_term in
  let level = level_term in
  let prefix = string_opt ~cmd:"log" "prefix" ~default:"" ~doc:"Log source prefix." in
  let time =
    string_opt ~short:'t' ~cmd:"log" "time" ~default:"" ~doc:"Timestamp layout or preset."
  in
  let texts =
    Arg.non_empty
      (Arg.pos_all Arg.string [] (Arg.info [] ~docv:"TEXT" ~doc:"Text to log."))
  in
  let ( level_style,
        time_style,
        prefix_style,
        message_style,
        key_style,
        value_style,
        separator_style ) =
    style_terms ()
  in
  let term =
    let open Term.Syntax in
    let+ file = file
    and+ printf = printf
    and+ structured = structured
    and+ formatter = formatter
    and+ level = level
    and+ prefix = prefix
    and+ time = time
    and+ min_level = min_level_term
    and+ level_style = level_style
    and+ time_style = time_style
    and+ prefix_style = prefix_style
    and+ message_style = message_style
    and+ key_style = key_style
    and+ value_style = value_style
    and+ separator_style = separator_style
    and+ texts = texts in
    let styles =
      make_styles ~level_style ~time_style ~prefix_style ~message_style ~key_style
        ~value_style ~separator_style ~emitted:(logs_level level)
    in
    emit ~file ~formatter ~level ~min_level ~prefix ~time ~format:printf ~structured
      ~styles env texts
  in
  Cmd.v (Cmd.info "log" ~doc:"Write a structured or styled log message.") term
