open Lwt.Infix

type compiled_hook = { config : Config.hook; matcher : Re.re option }
type t = { hooks : compiled_hook list; cwd : string }
type pre_tool_outcome = Allow of Jsont.json | Deny of string
type stream_output = { text : string; truncated : bool }

type process_result =
  | Done of int * stream_output * stream_output
  | Failed of string
  | Timeout

type hook_decision = Continue | Replace of Jsont.json | Refuse of string

module Log = struct
  let src = Logs.Src.create "crush.hooks"

  include (val Logs.src_log src : Logs.LOG)
end

let max_output_size = 1_048_576

exception Over_limit

let json_object fields =
  Jsont.Json.object'
    (List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value) fields)

let json_string value = Jsont.Json.string value
let json_bool value = Jsont.Json.bool value

let rec drain channel =
  Lwt_io.read ~count:4096 channel >>= fun chunk ->
  if String.is_empty chunk then Lwt.return_unit else drain channel

let collected buffer = { text = Buffer.contents buffer; truncated = false }

let read_channel channel =
  let buffer = Buffer.create 4096 in
  let rec pump () =
    Lwt_io.read ~count:4096 channel >>= fun chunk ->
    if String.is_empty chunk then Lwt.return_unit
    else if Buffer.length buffer + String.length chunk > max_output_size then
      Lwt.fail Over_limit
    else (
      Buffer.add_string buffer chunk;
      pump ())
  in
  Lwt.catch
    (fun () -> pump () >>= fun () -> Lwt.return (collected buffer))
    (function
      | Over_limit ->
          drain channel >>= fun () -> Lwt.return { text = ""; truncated = true }
      | Unix.Unix_error _ | Sys_error _ | Lwt_io.Channel_closed _ ->
          Lwt.return (collected buffer)
      | exn -> Lwt.fail exn)

let unix_message function_name argument error =
  Fmt.str "%s(%s): %s" function_name argument (Unix.error_message error)

let write_input channel payload =
  Lwt.finalize (fun () -> Lwt_io.write channel payload) (fun () -> Lwt_io.close channel)

let run_command t ~timeout ~command ~payload =
  let child = ref None in
  let kill () = Option.iter Charamel_os.Process.kill_tree !child in
  let body () =
    let process =
      Charamel_os.Process.spawn ~cwd:t.cwd ~env:(Unix.environment ()) ~stdin:`Pipe
        ~stdout:`Pipe ~stderr:`Pipe [ "/bin/sh"; "-c"; command ]
    in
    child := Some process;
    let input =
      Lwt.catch
        (fun () -> write_input (Charamel_os.Process.stdin_w process) payload)
        (fun _ -> Lwt.return_unit)
    in
    let stdout = read_channel (Charamel_os.Process.stdout_r process) in
    let stderr = read_channel (Charamel_os.Process.stderr_r process) in
    Lwt.both stdout stderr >>= fun (stdout, stderr) ->
    input >>= fun () ->
    Charamel_os.Process.await process >|= fun code -> Done (code, stdout, stderr)
  in
  Lwt.catch
    (fun () -> Lwt_unix.with_timeout timeout body)
    (function
      | Lwt_unix.Timeout ->
          kill ();
          Lwt.return Timeout
      | Unix.Unix_error (error, function_name, argument) ->
          Lwt.return (Failed (unix_message function_name argument error))
      | exn -> Lwt.fail exn)

let compile_matcher command =
  Option.map (fun pattern ->
      try Re.Perl.compile_pat pattern with
      | Re.Perl.Parse_error ->
          invalid_arg (Fmt.str "invalid hook matcher for %s: %s" command pattern)
      | Re.Perl.Not_supported ->
          invalid_arg (Fmt.str "unsupported hook matcher for %s: %s" command pattern))

let create ~config ~cwd =
  let hooks =
    List.map
      (fun (config : Config.hook) ->
        { config; matcher = compile_matcher config.Config.command config.Config.matcher })
      config
  in
  List.iter
    (fun hook ->
      if hook.config.Config.timeout_s < 1 || hook.config.Config.timeout_s > 3_600 then
        invalid_arg
          (Fmt.str "invalid hook timeout for %s: %d" hook.config.Config.command
             hook.config.Config.timeout_s))
    hooks;
  { hooks; cwd }

let event_name = function
  | Config.Pre_tool -> "pre_tool"
  | Config.Post_tool -> "post_tool"
  | Config.Session_start -> "session_start"
  | Config.Stop -> "stop"

let matcher_matches hook tool =
  match hook.matcher with None -> true | Some matcher -> Re.execp matcher tool

let payload_pre ~session ~cwd ~tool ~input =
  json_object
    [
      ("event", json_string "pre_tool");
      ("session", json_string session);
      ("cwd", json_string cwd);
      ("tool", json_string tool);
      ("input", input);
    ]
  |> Jsonx.string_of_json

let payload_post ~session ~cwd ~tool ~input ~output ~is_error =
  json_object
    [
      ("event", json_string "post_tool");
      ("session", json_string session);
      ("cwd", json_string cwd);
      ("tool", json_string tool);
      ("input", input);
      ("output", json_string output);
      ("is_error", json_bool is_error);
    ]
  |> Jsonx.string_of_json

let payload_session ~event ~session ~cwd ~reason ~message =
  let fields =
    [
      ("event", json_string event);
      ("session", json_string session);
      ("cwd", json_string cwd);
    ]
  in
  let fields =
    match reason with
    | None -> fields
    | Some value -> fields @ [ ("reason", json_string value) ]
  in
  let fields =
    match message with
    | None -> fields @ [ ("message", Jsont.Json.null ()) ]
    | Some value -> fields @ [ ("message", json_string value) ]
  in
  json_object fields |> Jsonx.string_of_json

let payload_session_start ~session ~cwd =
  json_object
    [
      ("event", json_string "session_start");
      ("session", json_string session);
      ("cwd", json_string cwd);
    ]
  |> Jsonx.string_of_json

let parse_pre_output output =
  if String.equal (String.trim output) "" then Continue
  else
    match Jsont_bytesrw.decode_string Jsont.json output with
    | Error _ -> Continue
    | Ok json -> (
        match Jsonx.string_member "decision" json with
        | Some "deny" ->
            Refuse
              (Option.value (Jsonx.string_member "reason" json) ~default:"denied by hook")
        | Some "allow" -> (
            match Jsonx.member "input" json with
            | Some input -> Replace input
            | None -> Continue)
        | _ -> Continue)

let timeout_for hook = Float.of_int hook.config.Config.timeout_s

let log_failure hook detail =
  Log.warn (fun log -> log "hook %s failed: %s" hook.config.Config.command detail)

let run_one t hook payload =
  run_command t ~timeout:(timeout_for hook) ~command:hook.config.Config.command ~payload

let report_truncation hook stream output =
  if output.truncated then
    Log.warn (fun log ->
        log "hook %s %s exceeded the %d-byte output limit" hook.config.Config.command
          stream max_output_size)

let apply_pre_hook t hook ~payload =
  run_one t hook payload >>= function
  | Timeout ->
      Lwt.return (Refuse (Fmt.str "hook %s timed out" hook.config.Config.command))
  | Failed detail ->
      log_failure hook detail;
      Lwt.return Continue
  | Done (0, stdout, _) ->
      report_truncation hook "stdout" stdout;
      Lwt.return (parse_pre_output stdout.text)
  | Done (2, _, stderr) ->
      report_truncation hook "stderr" stderr;
      let reason = String.trim stderr.text in
      Lwt.return (Refuse (if String.equal reason "" then "denied by hook" else reason))
  | Done (code, _, stderr) ->
      report_truncation hook "stderr" stderr;
      log_failure hook
        (Fmt.str "exited with status %d%s" code
           (if String.equal (String.trim stderr.text) "" then ""
            else ": " ^ String.trim stderr.text));
      Lwt.return Continue

let pre_tool t ~session ~tool ~input =
  let rec loop current = function
    | [] -> Lwt.return (Allow current)
    | hook :: rest -> (
        if hook.config.Config.event <> Config.Pre_tool || not (matcher_matches hook tool)
        then loop current rest
        else
          let payload = payload_pre ~session ~cwd:t.cwd ~tool ~input:current in
          apply_pre_hook t hook ~payload >>= function
          | Continue -> loop current rest
          | Replace next -> loop next rest
          | Refuse reason -> Lwt.return (Deny reason))
  in
  loop input t.hooks

let run_observational t hook payload =
  run_one t hook payload >>= function
  | Done (0, stdout, stderr) ->
      report_truncation hook "stdout" stdout;
      report_truncation hook "stderr" stderr;
      Lwt.return_unit
  | Done (code, _, stderr) ->
      report_truncation hook "stderr" stderr;
      let suffix = String.trim stderr.text in
      log_failure hook
        (Fmt.str "exited with status %d%s" code
           (if String.equal suffix "" then "" else ": " ^ suffix));
      Lwt.return_unit
  | Timeout ->
      log_failure hook (Fmt.str "timed out after %ds" hook.config.Config.timeout_s);
      Lwt.return_unit
  | Failed detail ->
      log_failure hook detail;
      Lwt.return_unit

let matching t event predicate =
  List.filter (fun hook -> hook.config.Config.event = event && predicate hook) t.hooks

let post_tool t ~session ~tool ~input ~output ~is_error =
  Lwt_list.iter_s
    (fun hook ->
      run_observational t hook
        (payload_post ~session ~cwd:t.cwd ~tool ~input ~output ~is_error))
    (matching t Config.Post_tool (fun hook -> matcher_matches hook tool))

let session_start t ~session =
  Lwt_list.iter_s
    (fun hook -> run_observational t hook (payload_session_start ~session ~cwd:t.cwd))
    (matching t Config.Session_start (fun _ -> true))

let stop t ~session ~reason =
  let reason_name, message =
    match reason with
    | `Stop -> ("stop", None)
    | `Interrupted -> ("interrupted", None)
    | `Error message -> ("error", Some message)
  in
  Lwt_list.iter_s
    (fun hook ->
      run_observational t hook
        (payload_session ~event:(event_name Config.Stop) ~session ~cwd:t.cwd
           ~reason:(Some reason_name) ~message))
    (matching t Config.Stop (fun _ -> true))

let has t event = List.exists (fun hook -> hook.config.Config.event = event) t.hooks
