(* No test here ever writes to [Format.std_formatter]: every reporter is bound to a
   buffer-backed formatter and assertions read the buffer back. *)

let src = Logs.Src.create "charamel.log.test" ~doc:"charamel.log test source"
let () = Logs.Src.set_level src (Some Logs.Debug)

module Log = (val Logs.src_log src : Logs.LOG)

let user_tag = Logs.Tag.def "user" Fmt.string
let note_tag = Logs.Tag.def "note" Fmt.string
let clock = Eio_mock.Clock.make ()

let with_reporter ?format ?styles ?time_format ?report_timestamp ?report_caller
    ?(profile = Charamel_colorprofile.True_color) f =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let r =
    Charamel_log.reporter ?format ?styles ?time_format ?report_timestamp ?report_caller
      ~clock ~profile ppf
  in
  Logs.set_reporter r;
  f ();
  Buffer.contents buf

let contains ~needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec loop i = i + nl <= hl && (String.sub haystack i nl = needle || loop (i + 1)) in
  nl = 0 || loop 0

let count_char c s = String.fold_left (fun n ch -> if ch = c then n + 1 else n) 0 s
let strip out = Charamel_ansi.Text.strip out

(* {1 Text format} *)

let test_text_error_level () =
  let out =
    with_reporter ~format:Charamel_log.Text (fun () ->
        Log.err (fun m -> m ~tags:(Logs.Tag.add user_tag "alice" Logs.Tag.empty) "boom"))
  in
  Alcotest.(check string)
    "level, prefix, message, tag" "ERROR charamel.log.test: boom user=alice\n" (strip out)

let test_text_level_padding () =
  (* [Info]'s label "INFO" pads to width 5 as "INFO ", and the next field's own leading
     separator adds a second space: reproduces charmbracelet/log's own layout via
     {!Charamel_lipgloss.Style}'s [width], not an error. *)
  let out =
    with_reporter ~format:Charamel_log.Text (fun () -> Log.info (fun m -> m "ready"))
  in
  Alcotest.(check string)
    "label padded to width 5" "INFO  charamel.log.test: ready\n" (strip out)

let test_text_quoted_tag () =
  let out =
    with_reporter ~format:Charamel_log.Text (fun () ->
        Log.err (fun m ->
            m ~tags:(Logs.Tag.add note_tag "has space" Logs.Tag.empty) "boom"))
  in
  Alcotest.(check string)
    "quoted tag value" "ERROR charamel.log.test: boom note=\"has space\"\n" (strip out)

let test_text_app_no_label () =
  let out =
    with_reporter ~format:Charamel_log.Text (fun () -> Log.app (fun m -> m "launched"))
  in
  Alcotest.(check string)
    "app renders without a level label" "charamel.log.test: launched\n" (strip out)

let test_text_caller_default_off () =
  let out =
    with_reporter ~format:Charamel_log.Text (fun () ->
        Log.err (fun m -> m ~header:"foo.ml:10" "boom"))
  in
  Alcotest.(check bool)
    "caller hidden when report_caller is off" false
    (contains ~needle:"foo.ml:10" (strip out))

let test_text_caller_on () =
  let out =
    with_reporter ~format:Charamel_log.Text ~report_caller:true (fun () ->
        Log.err (fun m -> m ~header:"foo.ml:10" "boom"))
  in
  Alcotest.(check string)
    "caller shown between level and prefix" "ERROR <foo.ml:10> charamel.log.test: boom\n"
    (strip out)

let test_text_timestamp () =
  Eio_mock.Clock.set_time clock 0.0;
  let out =
    with_reporter ~format:Charamel_log.Text ~report_timestamp:true (fun () ->
        Log.err (fun m -> m "boom"))
  in
  Alcotest.(check string)
    "epoch renders as 1970/01/01 00:00:00"
    "1970/01/01 00:00:00 ERROR charamel.log.test: boom\n" (strip out)

let test_text_no_timestamp_by_default () =
  let out =
    with_reporter ~format:Charamel_log.Text (fun () -> Log.err (fun m -> m "boom"))
  in
  Alcotest.(check bool)
    "no time field by default" false
    (contains ~needle:"1970" (strip out))

(* {1 Logfmt format} *)

let test_logfmt_basic () =
  let out =
    with_reporter ~format:Charamel_log.Logfmt (fun () -> Log.info (fun m -> m "hello"))
  in
  Alcotest.(check string)
    "level, prefix, msg" "level=info prefix=charamel.log.test msg=hello\n" out

let test_logfmt_quoting () =
  let out =
    with_reporter ~format:Charamel_log.Logfmt (fun () ->
        Log.info (fun m ->
            m ~tags:(Logs.Tag.add note_tag "a \"quote\"" Logs.Tag.empty) "hi there"))
  in
  Alcotest.(check string)
    "space and quote both escaped"
    "level=info prefix=charamel.log.test msg=\"hi there\" note=\"a \\\"quote\\\"\"\n" out

let test_logfmt_app_no_level () =
  let out =
    with_reporter ~format:Charamel_log.Logfmt (fun () -> Log.app (fun m -> m "launched"))
  in
  Alcotest.(check bool) "no level key for App" false (contains ~needle:"level=" out)

(* {1 Json format} *)

let json_member members key =
  match Jsont.Json.find_mem key members with
  | Some (_, Jsont.String (s, _)) -> Some s
  | Some _ | None -> None

let decode_object line =
  match Jsont_bytesrw.decode_string Jsont.json line with
  | Ok (Jsont.Object (members, _)) -> members
  | Ok _ -> Alcotest.fail "expected a JSON object"
  | Error msg -> Alcotest.fail ("json decode failed: " ^ msg)

let test_json_basic () =
  let out =
    with_reporter ~format:Charamel_log.Json (fun () -> Log.info (fun m -> m "hello"))
  in
  Alcotest.(check int) "exactly one line" 1 (count_char '\n' out);
  let members = decode_object (String.trim out) in
  Alcotest.(check (option string)) "level" (Some "info") (json_member members "level");
  Alcotest.(check (option string))
    "prefix" (Some "charamel.log.test")
    (json_member members "prefix");
  Alcotest.(check (option string)) "msg" (Some "hello") (json_member members "msg")

let test_json_app_no_level () =
  let out =
    with_reporter ~format:Charamel_log.Json (fun () -> Log.app (fun m -> m "launched"))
  in
  let members = decode_object (String.trim out) in
  Alcotest.(check (option string)) "no level member" None (json_member members "level")

let test_json_tag_round_trip () =
  let value = "a \"quote\"\nand a newline" in
  let out =
    with_reporter ~format:Charamel_log.Json (fun () ->
        Log.info (fun m -> m ~tags:(Logs.Tag.add note_tag value Logs.Tag.empty) "hi"))
  in
  let members = decode_object (String.trim out) in
  Alcotest.(check (option string))
    "tag value round-trips through JSON escaping" (Some value)
    (json_member members "note")

(* {1 Profile} *)

let test_profile_true_color_keeps_index () =
  let out =
    with_reporter ~format:Charamel_log.Text ~profile:Charamel_colorprofile.True_color
      (fun () -> Log.info (fun m -> m "x"))
  in
  Alcotest.(check bool)
    "full-fidelity indexed color present" true
    (contains ~needle:"38;5;86" out)

let test_profile_ascii_drops_color () =
  let out =
    with_reporter ~format:Charamel_log.Text ~profile:Charamel_colorprofile.Ascii
      (fun () -> Log.info (fun m -> m "x"))
  in
  Alcotest.(check bool)
    "no escape byte at all under Ascii" false (contains ~needle:"\x1b" out);
  Alcotest.(check bool)
    "label text itself still present" true (contains ~needle:"INFO" out)

let test_profile_no_tty_drops_color () =
  let out =
    with_reporter ~format:Charamel_log.Text ~profile:Charamel_colorprofile.No_tty
      (fun () -> Log.info (fun m -> m "x"))
  in
  Alcotest.(check bool)
    "no escape byte at all under No_tty" false (contains ~needle:"\x1b" out)

(* {1 Reporter lifecycle} *)

let test_single_invocation () =
  let calls = ref 0 in
  let out =
    with_reporter ~format:Charamel_log.Text (fun () ->
        Log.info (fun m ->
            incr calls;
            m "once"))
  in
  Alcotest.(check int) "message callback invoked exactly once" 1 !calls;
  Alcotest.(check int) "exactly one rendered line" 1 (count_char '\n' out)

let text_suite =
  ( "Text format",
    [
      Alcotest.test_case "level, prefix, message, tag" `Quick test_text_error_level;
      Alcotest.test_case "level label padded to width 5" `Quick test_text_level_padding;
      Alcotest.test_case "structured value needing quoting" `Quick test_text_quoted_tag;
      Alcotest.test_case "App level has no label" `Quick test_text_app_no_label;
      Alcotest.test_case "caller hidden by default" `Quick test_text_caller_default_off;
      Alcotest.test_case "caller shown when report_caller is set" `Quick
        test_text_caller_on;
      Alcotest.test_case "timestamp shown when report_timestamp is set" `Quick
        test_text_timestamp;
      Alcotest.test_case "no timestamp by default" `Quick
        test_text_no_timestamp_by_default;
    ] )

let logfmt_suite =
  ( "Logfmt format",
    [
      Alcotest.test_case "level, prefix, msg" `Quick test_logfmt_basic;
      Alcotest.test_case "space and quote escaped" `Quick test_logfmt_quoting;
      Alcotest.test_case "App level omits level key" `Quick test_logfmt_app_no_level;
    ] )

let json_suite =
  ( "Json format",
    [
      Alcotest.test_case "level, prefix, msg fields" `Quick test_json_basic;
      Alcotest.test_case "App level omits level member" `Quick test_json_app_no_level;
      Alcotest.test_case "tag value round-trips" `Quick test_json_tag_round_trip;
    ] )

let profile_suite =
  ( "Colorprofile",
    [
      Alcotest.test_case "True_color keeps the indexed color" `Quick
        test_profile_true_color_keeps_index;
      Alcotest.test_case "Ascii drops all SGR" `Quick test_profile_ascii_drops_color;
      Alcotest.test_case "No_tty drops all SGR" `Quick test_profile_no_tty_drops_color;
    ] )

let reporter_suite =
  ( "Reporter lifecycle",
    [ Alcotest.test_case "over/k run exactly once" `Quick test_single_invocation ] )

let () =
  Alcotest.run "charamel_log"
    [ text_suite; logfmt_suite; json_suite; profile_suite; reporter_suite ]
