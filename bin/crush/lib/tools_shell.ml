open Result.Syntax
open Lwt_direct

type bash_params = {
  command : string;
  description : string;
  working_dir : string option;
  timeout_s : int option;
  run_in_background : bool option;
}

type job_output_params = { job_id : string; wait : bool option }
type job_kill_params = { job_id : string }

let bash_params_jsont =
  Jsont.Object.map (fun command description working_dir timeout_s run_in_background ->
      { command; description; working_dir; timeout_s; run_in_background })
  |> Jsont.Object.mem "command" Jsont.string
  |> Jsont.Object.mem "description" Jsont.string
  |> Jsont.Object.opt_mem "working_dir" Jsont.string
  |> Jsont.Object.opt_mem "timeout_s" Jsont.int
  |> Jsont.Object.opt_mem "run_in_background" Jsont.bool
  |> Jsont.Object.finish

let job_output_params_jsont =
  Jsont.Object.map (fun job_id wait -> { job_id; wait })
  |> Jsont.Object.mem "job_id" Jsont.string
  |> Jsont.Object.opt_mem "wait" Jsont.bool
  |> Jsont.Object.finish

let job_kill_params_jsont =
  Jsont.Object.map (fun job_id -> { job_id })
  |> Jsont.Object.mem "job_id" Jsont.string
  |> Jsont.Object.finish

let is_ascii_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_ascii_digit c = c >= '0' && c <= '9'

let shell_word_char c =
  is_ascii_letter c || is_ascii_digit c
  || match c with '_' | '-' | '.' | '/' | ':' | '=' -> true | _ -> false

let contains_unsafe_char command =
  let rec loop index =
    if index = String.length command then false
    else
      match command.[index] with
      | '|' -> loop (index + 1)
      | ' ' | '\t' -> loop (index + 1)
      | c when shell_word_char c -> loop (index + 1)
      | _ -> true
  in
  loop 0

let plain_tokens segment =
  if String.trim segment = "" || contains_unsafe_char segment then None
  else
    let tokens =
      String.split_on_char ' ' (String.trim segment)
      |> List.concat_map (fun item -> String.split_on_char '\t' item)
      |> List.filter (fun item -> item <> "")
    in
    match tokens with
    (* A screened [fd] argument containing [=] is data, not a shell assignment.
       [safe_simple_command] still checks the execution options. *)
    | "fd" :: _ -> Some tokens
    | _ ->
        if
          List.for_all
            (fun token -> token <> "" && not (String.contains token '='))
            tokens
        then Some tokens
        else None

let has_option_prefix prefixes args =
  List.exists
    (fun arg -> List.exists (fun prefix -> String.starts_with ~prefix arg) prefixes)
    args

let safe_simple_command command args =
  let safe =
    [
      "ls";
      "cat";
      "head";
      "tail";
      "wc";
      "pwd";
      "echo";
      "grep";
      "rg";
      "fd";
      "du";
      "df";
      "stat";
      "file";
      "tree";
      "sort";
      "uniq";
      "cut";
      "jq";
      "printenv";
    ]
  in
  if command = "fd" then not (Shell_screen.fd_executes args)
  else if command = "rg" then not (has_option_prefix [ "--pre"; "--pre-glob" ] args)
  else List.mem command safe || (command = "env" && args = [])

let safe_git args =
  match args with
  | subcommand :: rest
    when List.mem subcommand [ "status"; "log"; "diff"; "show"; "branch"; "blame" ] ->
      let forbidden =
        [
          "--output";
          "--exec";
          "--ext-diff";
          "--textconv";
          "--delete";
          "-d";
          "-D";
          "-m";
          "-M";
          "--move";
          "--edit-description";
          "--unset-upstream";
          "--set-upstream-to";
        ]
      in
      not (has_option_prefix forbidden rest)
  | _ -> false

let safe_tokens = function
  | [] -> false
  | command :: args ->
      if command = "git" then safe_git args
      else if command = "dune" then args = [ "describe" ]
      else safe_simple_command command args

let read_only_pipeline command =
  String.split_on_char '|' command
  |> List.for_all (fun segment ->
      match plain_tokens segment with Some tokens -> safe_tokens tokens | None -> false)

let is_read_only command =
  match Shell_screen.screen Shell_screen.platform command with
  | Shell_screen.Clear -> read_only_pipeline command
  | Shell_screen.Blocked _ | Shell_screen.Review _ -> false

let output_with_status ctx ~status text =
  let output = Tool.truncate ctx text in
  match status with `Ok -> output | `Error -> { output with is_error = true }

let drain source ~mutex ~buffer ~captured ~truncated =
  let open Lwt.Infix in
  let rec loop () =
    Lwt_io.read ~count:65_536 source >>= fun chunk ->
    if chunk = "" then Lwt.return_unit
    else
      Lwt_mutex.with_lock mutex (fun () ->
          let available = 10_485_760 - !captured in
          let keep = min available (String.length chunk) in
          if keep > 0 then (
            Buffer.add_substring buffer chunk 0 keep;
            captured := !captured + keep);
          if keep < String.length chunk then truncated := true;
          Lwt.return_unit)
      >>= loop
  in
  loop ()

let collect_output process =
  let open Lwt.Infix in
  let buffer = Buffer.create 256 in
  let mutex = Lwt_mutex.create () in
  let captured = ref 0 in
  let truncated = ref false in
  Lwt.join
    [
      drain (Charamel_os.Process.stdout_r process) ~mutex ~buffer ~captured ~truncated;
      drain (Charamel_os.Process.stderr_r process) ~mutex ~buffer ~captured ~truncated;
    ]
  >>= fun () ->
  Charamel_os.Process.await process >|= fun code ->
  let text = Buffer.contents buffer in
  let output =
    if !truncated then text ^ "\n[output truncated after 10485760 bytes]\n" else text
  in
  if code = 0 then `Ok output
  else if code > 128 then `Error (output, Fmt.str "Terminated by signal %d" (code - 128))
  else `Error (output, Fmt.str "Exit code %d" code)

let run_foreground_with_timeout ctx ~cwd ~command ~timeout_s =
  let process =
    Charamel_os.Process.spawn ~cwd ~stdin:`Null ~stdout:`Pipe ~stderr:`Pipe
      (if Sys.win32 then [ "cmd"; "/c"; command ] else [ "/bin/sh"; "-c"; command ])
  in
  let work = collect_output process in
  match
    try Ok (await (Lwt_unix.with_timeout (float_of_int timeout_s) (fun () -> work)))
    with Lwt_unix.Timeout -> Error (`Timeout (float_of_int timeout_s))
  with
  | Error error ->
      Charamel_os.Process.kill_tree process;
      Error error
  | Ok (`Ok output) ->
      Ok
        (output_with_status ctx ~status:`Ok
           (if output = "" then "(no output)" else output))
  | Ok (`Error (output, reason)) ->
      let content = if output = "" then reason else output ^ "\n" ^ reason in
      Ok (output_with_status ctx ~status:`Error content)

let bash_schema =
  Tool.schema_object ~required:[ "command"; "description" ]
    [
      ("command", Tool.s_string ~desc:"Shell command to execute." ());
      ("description", Tool.s_string ~desc:"Short explanation of the command." ());
      ("working_dir", Tool.s_string ~desc:"Working directory." ());
      ("timeout_s", Tool.s_int ~default:120 ());
      ("run_in_background", Tool.s_bool ~default:false ());
    ]

let bash =
  {
    Tool.name = "bash";
    description =
      "Execute a shell command with bounded output and an optional background job.";
    schema = bash_schema;
    read_only = false;
    run =
      (fun ctx value ->
        let* params = Tool.decode bash_params_jsont value in
        let command = params.command in
        let timeout_s = Option.value params.timeout_s ~default:120 in
        let run_in_background = Option.value params.run_in_background ~default:false in
        let cwd =
          match params.working_dir with
          | Some path when String.trim path <> "" -> Tool.absolute ctx path
          | _ -> ctx.Tool.cwd
        in
        if String.trim command = "" then
          Error (`Invalid_input "command must not be empty")
        else if String.length params.description > 80 then
          Error (`Invalid_input "description must be at most 80 bytes")
        else if timeout_s < 1 || timeout_s > 600 then
          Error (`Invalid_input "timeout_s must be between 1 and 600")
        else
          let permission_path = "" in
          let* () =
            match Shell_screen.screen Shell_screen.platform command with
            | Shell_screen.Blocked _ ->
                Error (`Unavailable "command is blocked by the shell safety policy")
            | Shell_screen.Review review ->
                Tool.request ctx ~review ~read_only:false ~tool:"bash" ~action:command
                  ~path:permission_path ~description:params.description
            | Shell_screen.Clear ->
                let read_only = read_only_pipeline command && not run_in_background in
                Tool.request ctx ~read_only ~tool:"bash" ~action:command
                  ~path:permission_path ~description:params.description
          in
          if run_in_background then
            let job_id = Jobs.start ctx.Tool.jobs ~cwd ~command ~env:[] ~timeout_s in
            Ok (Tool.truncate ctx (Fmt.str "started %s" job_id))
          else
            try run_foreground_with_timeout ctx ~cwd ~command ~timeout_s with
            | Unix.Unix_error (error, operation, argument) ->
                Error
                  (`Io
                     ( cwd,
                       Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument
                     ))
            | Invalid_argument message -> Error (`Invalid_input message)
            | Failure message -> Error (`Unavailable message));
  }

let job_output_schema =
  Tool.schema_object ~required:[ "job_id" ]
    [
      ("job_id", Tool.s_string ~desc:"Background job identifier." ());
      ("wait", Tool.s_bool ~default:false ());
    ]

let job_output =
  {
    Tool.name = "job_output";
    description = "Read bounded output from a background shell job.";
    schema = job_output_schema;
    read_only = true;
    run =
      (fun ctx value ->
        let* (params : job_output_params) = Tool.decode job_output_params_jsont value in
        let job_id = String.trim params.job_id in
        if job_id = "" then Error (`Invalid_input "job_id must not be empty")
        else
          let* () =
            Tool.request ctx ~read_only:true ~tool:"job_output" ~action:job_id
              ~path:ctx.Tool.cwd
              ~description:(Fmt.str "Read output for %s" job_id)
          in
          match
            await
              (Jobs.output ctx.Tool.jobs ~id:job_id
                 ~wait:(Option.value params.wait ~default:false))
          with
          | Error (`Not_found id) -> Error (`Not_found id)
          | Ok (captured, status) ->
              let status_text =
                match status with
                | Jobs.Running -> "running"
                | Jobs.Exited code -> Fmt.str "exited %d" code
                | Jobs.Killed -> "killed"
              in
              let content = Fmt.str "status: %s\n%s" status_text captured in
              Ok (Tool.truncate ctx content));
  }

let job_kill_schema =
  Tool.schema_object ~required:[ "job_id" ]
    [ ("job_id", Tool.s_string ~desc:"Background job identifier." ()) ]

let job_kill =
  {
    Tool.name = "job_kill";
    description = "Terminate a running background shell job.";
    schema = job_kill_schema;
    read_only = false;
    run =
      (fun ctx value ->
        let* params = Tool.decode job_kill_params_jsont value in
        let job_id = String.trim params.job_id in
        if job_id = "" then Error (`Invalid_input "job_id must not be empty")
        else
          let* () =
            Tool.request ctx ~read_only:false ~tool:"job_kill" ~action:job_id ~path:""
              ~description:(Fmt.str "Terminate %s" job_id)
          in
          match await (Jobs.kill ctx.Tool.jobs ~id:job_id) with
          | Ok () -> Ok (Tool.truncate ctx (Fmt.str "killed %s" job_id))
          | Error (`Not_found id) -> Error (`Not_found id));
  }
