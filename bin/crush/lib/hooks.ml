type compiled_hook = { config : Config.hook; matcher : Re.re option }

type t = {
  hooks : compiled_hook list;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  cwd : string;
}

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

let shell_quote value =
  "'" ^ String.concat "'\\''" (String.split_on_char '\'' value) ^ "'"

let json_object fields =
  Jsont.Json.object'
    (List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value) fields)

let json_string value = Jsont.Json.string value
let json_bool value = Jsont.Json.bool value

let read_flow flow =
  match Eio.Buf_read.parse ~max_size:max_output_size Eio.Buf_read.take_all flow with
  | Ok value -> { text = value; truncated = false }
  | Error (`Msg _) ->
      (try Eio.Flow.copy flow Eio.Flow.null with End_of_file -> ());
      { text = ""; truncated = true }

let status_code = function `Exited code -> code | `Signaled signal -> 128 + signal

let fd_of_flow flow =
  match Eio_unix.Resource.fd_opt flow with
  | Some fd -> fd
  | None -> invalid_arg "hook process pipe is not backed by a Unix file descriptor"

let signal_group process signal =
  let pid = Eio.Process.pid process in
  try Eio_unix.run_in_systhread (fun () -> Unix.kill (-pid) signal)
  with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let run_command t ~timeout ~command ~payload =
  let body () =
    Eio.Switch.run (fun sw ->
        let stdin_r, stdin_w = Eio.Process.pipe ~sw t.proc_mgr in
        let stdout_r, stdout_w = Eio.Process.pipe ~sw t.proc_mgr in
        let stderr_r, stderr_w = Eio.Process.pipe ~sw t.proc_mgr in
        let child_command = "cd -- " ^ shell_quote t.cwd ^ " && " ^ command in
        let process =
          Eio_unix.Process.spawn_unix ~sw t.proc_mgr ~pgid:0
            ~env:(Eio_unix.run_in_systhread Unix.environment)
            ~fds:
              [
                (0, fd_of_flow stdin_r, `Blocking);
                (1, fd_of_flow stdout_w, `Blocking);
                (2, fd_of_flow stderr_w, `Blocking);
              ]
            ~executable:"/bin/sh"
            [ "/bin/sh"; "-c"; child_command ]
        in
        let finished = ref false in
        let process_cancelled () =
          if not !finished then signal_group process Sys.sigkill
        in
        Eio.Switch.on_release sw process_cancelled |> ignore;
        Eio.Flow.close stdin_r;
        Eio.Flow.close stdout_w;
        Eio.Flow.close stderr_w;
        let input_result = ref None in
        let write_input () =
          let result =
            try
              Fun.protect
                (fun () ->
                  Eio.Flow.copy_string payload stdin_w;
                  Ok ())
                ~finally:(fun () -> Eio.Flow.close stdin_w)
            with
            | Eio.Io (error, _) -> Error (Fmt.str "%a" Eio.Exn.pp_err error)
            | Unix.Unix_error (error, function_name, argument) ->
                Error
                  (Fmt.str "%s(%s): %s" function_name argument (Unix.error_message error))
          in
          input_result := Some result
        in
        let output = ref None in
        let read_output () =
          output :=
            Some
              (Eio.Fiber.pair
                 (fun () -> read_flow stdout_r)
                 (fun () -> read_flow stderr_r))
        in
        (try Eio.Fiber.both write_input read_output
         with Eio.Cancel.Cancelled _ as ex ->
           signal_group process Sys.sigkill;
           raise ex);
        let stdout, stderr =
          match !output with
          | Some output -> output
          | None -> invalid_arg "hook output reader did not return"
        in
        let status = Eio.Process.await process in
        finished := true;
        ignore input_result;
        Done (status_code status, stdout, stderr))
  in
  try
    match Eio.Time.with_timeout t.clock timeout (fun () -> Ok (body ())) with
    | Ok result -> result
    | Error `Timeout -> Timeout
  with
  | Eio.Io (error, _) -> Failed (Fmt.str "%a" Eio.Exn.pp_err error)
  | Unix.Unix_error (error, function_name, argument) ->
      Failed (Fmt.str "%s(%s): %s" function_name argument (Unix.error_message error))

let compile_matcher command = function
  | None -> None
  | Some pattern -> (
      try Some (Re.Perl.compile_pat pattern) with
      | Re.Perl.Parse_error ->
          invalid_arg (Fmt.str "invalid hook matcher for %s: %s" command pattern)
      | Re.Perl.Not_supported ->
          invalid_arg (Fmt.str "unsupported hook matcher for %s: %s" command pattern))

let create ~config ~proc_mgr ~clock ~cwd =
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
  { hooks; proc_mgr; clock; cwd }

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
  match run_one t hook payload with
  | Timeout -> Refuse (Fmt.str "hook %s timed out" hook.config.Config.command)
  | Failed detail ->
      log_failure hook detail;
      Continue
  | Done (0, stdout, _) ->
      report_truncation hook "stdout" stdout;
      parse_pre_output stdout.text
  | Done (2, _, stderr) ->
      report_truncation hook "stderr" stderr;
      let reason = String.trim stderr.text in
      Refuse (if String.equal reason "" then "denied by hook" else reason)
  | Done (code, _, stderr) ->
      report_truncation hook "stderr" stderr;
      log_failure hook
        (Fmt.str "exited with status %d%s" code
           (if String.equal (String.trim stderr.text) "" then ""
            else ": " ^ String.trim stderr.text));
      Continue

let pre_tool t ~session ~tool ~input =
  let rec loop current = function
    | [] -> Allow current
    | hook :: rest -> (
        if hook.config.Config.event <> Config.Pre_tool || not (matcher_matches hook tool)
        then loop current rest
        else
          let payload = payload_pre ~session ~cwd:t.cwd ~tool ~input:current in
          match apply_pre_hook t hook ~payload with
          | Continue -> loop current rest
          | Replace next -> loop next rest
          | Refuse reason -> Deny reason)
  in
  loop input t.hooks

let run_observational t hook payload =
  match run_one t hook payload with
  | Done (0, stdout, stderr) ->
      report_truncation hook "stdout" stdout;
      report_truncation hook "stderr" stderr
  | Done (code, _, stderr) ->
      report_truncation hook "stderr" stderr;
      let suffix = String.trim stderr.text in
      log_failure hook
        (Fmt.str "exited with status %d%s" code
           (if String.equal suffix "" then "" else ": " ^ suffix))
  | Timeout ->
      log_failure hook (Fmt.str "timed out after %ds" hook.config.Config.timeout_s)
  | Failed detail -> log_failure hook detail

let post_tool t ~session ~tool ~input ~output ~is_error =
  List.iter
    (fun hook ->
      if hook.config.Config.event = Config.Post_tool && matcher_matches hook tool then
        run_observational t hook
          (payload_post ~session ~cwd:t.cwd ~tool ~input ~output ~is_error))
    t.hooks

let session_start t ~session =
  List.iter
    (fun hook ->
      if hook.config.Config.event = Config.Session_start then
        run_observational t hook (payload_session_start ~session ~cwd:t.cwd))
    t.hooks

let stop t ~session ~reason =
  let reason_name, message =
    match reason with
    | `Stop -> ("stop", None)
    | `Interrupted -> ("interrupted", None)
    | `Error message -> ("error", Some message)
  in
  List.iter
    (fun hook ->
      if hook.config.Config.event = Config.Stop then
        run_observational t hook
          (payload_session ~event:(event_name Config.Stop) ~session ~cwd:t.cwd
             ~reason:(Some reason_name) ~message))
    t.hooks

let has t event = List.exists (fun hook -> hook.config.Config.event = event) t.hooks
