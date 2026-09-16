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

let shell_words command =
  let words = ref [] in
  let current = Buffer.create 16 in
  let flush () =
    if Buffer.length current > 0 then (
      words := Buffer.contents current :: !words;
      Buffer.clear current)
  in
  String.iter
    (fun c ->
      if shell_word_char c then Buffer.add_char current (Char.lowercase_ascii c)
      else flush ())
    command;
  flush ();
  List.rev !words

let rec has_sequence wanted words =
  match wanted with
  | [] -> true
  | first :: rest -> (
      match words with
      | [] -> false
      | word :: tail ->
          if word = first then has_sequence rest tail else has_sequence wanted tail)

let blocked_commands =
  [
    "alias";
    "aria2c";
    "axel";
    "chrome";
    "curl";
    "curlie";
    "firefox";
    "httpie";
    "http-prompt";
    "links";
    "lynx";
    "nc";
    "safari";
    "scp";
    "ssh";
    "telnet";
    "w3m";
    "wget";
    "xh";
    "doas";
    "su";
    "sudo";
    "apk";
    "apt";
    "apt-cache";
    "apt-get";
    "dnf";
    "dpkg";
    "emerge";
    "home-manager";
    "makepkg";
    "opkg";
    "pacman";
    "paru";
    "pkg";
    "pkg_add";
    "pkg_delete";
    "portage";
    "rpm";
    "yay";
    "yum";
    "zypper";
    "at";
    "batch";
    "chkconfig";
    "crontab";
    "fdisk";
    "mkfs";
    "mount";
    "parted";
    "service";
    "systemctl";
    "umount";
    "firewall-cmd";
    "ifconfig";
    "ip";
    "iptables";
    "netstat";
    "pfctl";
    "route";
    "ufw";
    "kill";
    "pkill";
    "killall";
    "shutdown";
    "reboot";
    "poweroff";
    "halt";
    "eval";
    "exec";
    "source";
  ]

let basename word =
  match String.rindex_opt word '/' with
  | None -> word
  | Some index -> String.sub word (index + 1) (String.length word - index - 1)

let blocked_command command =
  let words = shell_words command in
  let command_name word = basename word in
  let has_banned_name =
    List.exists (fun word -> List.mem (command_name word) blocked_commands) words
  in
  let package_install =
    [
      [ "apk"; "add" ];
      [ "apt"; "install" ];
      [ "apt-get"; "install" ];
      [ "dnf"; "install" ];
      [ "pacman"; "-s" ];
      [ "pkg"; "install" ];
      [ "yum"; "install" ];
      [ "zypper"; "install" ];
      [ "brew"; "install" ];
      [ "cargo"; "install" ];
      [ "gem"; "install" ];
      [ "go"; "install" ];
      [ "npm"; "install"; "--global" ];
      [ "npm"; "install"; "-g" ];
      [ "pip"; "install"; "--user" ];
      [ "pip3"; "install"; "--user" ];
      [ "pnpm"; "add"; "--global" ];
      [ "pnpm"; "add"; "-g" ];
      [ "yarn"; "global"; "add" ];
      [ "go"; "test"; "-exec" ];
    ]
    |> List.exists (fun pattern ->
        let normalized = List.map command_name pattern in
        has_sequence normalized words)
  in
  has_banned_name || package_install

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
    if List.for_all (fun token -> token <> "" && not (String.contains token '=')) tokens
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
  if command = "fd" then not (has_option_prefix [ "--exec"; "-x" ] args)
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

let is_read_only command =
  if blocked_command command then false
  else
    let segments = String.split_on_char '|' command in
    List.for_all
      (fun segment ->
        match plain_tokens segment with
        | Some tokens -> safe_tokens tokens
        | None -> false)
      segments

let output_with_artifact (ctx : Tool.ctx) text =
  let content, artifact =
    Artifact.truncate ctx.Tool.artifacts ~random:ctx.Tool.random text
  in
  Tool.ok ?artifact content

let output_with_status ctx ~status text =
  let output = output_with_artifact ctx text in
  match status with `Ok -> output | `Error -> { output with is_error = true }

let append_chunk ~mutex ~buffer ~captured ~truncated chunk =
  let data = Cstruct.to_string chunk in
  Eio.Mutex.use_rw ~protect:true mutex @@ fun () ->
  let available = 10_485_760 - !captured in
  let keep = min available (String.length data) in
  if keep > 0 then (
    Buffer.add_substring buffer data 0 keep;
    captured := !captured + keep);
  if keep < String.length data then truncated := true

let drain source ~mutex ~buffer ~captured ~truncated =
  let chunk = Cstruct.create 65_536 in
  let rec loop () =
    match Eio.Flow.single_read source chunk with
    | count when count > 0 ->
        append_chunk ~mutex ~buffer ~captured ~truncated (Cstruct.sub chunk 0 count);
        loop ()
    | _ -> loop ()
    | exception End_of_file -> ()
  in
  loop ()

let signal_group pid signal =
  try Unix.kill (-pid) signal with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let fd_of_sink sink =
  match Eio_unix.Resource.fd_opt sink with
  | Some fd -> fd
  | None -> invalid_arg "shell pipe is not backed by a Unix file descriptor"

let run_foreground (ctx : Tool.ctx) ~cwd ~command =
  Eio.Switch.run @@ fun process_sw ->
  let stdout_r, stdout_w = Eio.Process.pipe ~sw:process_sw ctx.Tool.proc_mgr in
  let stderr_r, stderr_w = Eio.Process.pipe ~sw:process_sw ctx.Tool.proc_mgr in
  let null_unix =
    Eio_unix.run_in_systhread (fun () -> Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0)
  in
  let null_stdin = Eio_unix.Fd.of_unix ~sw:process_sw ~close_unix:true null_unix in
  let process =
    Eio_unix.Process.spawn_unix ~sw:process_sw ctx.Tool.proc_mgr
      ~cwd:Eio.Path.(ctx.Tool.fs / cwd)
      ~pgid:0
      ~fds:
        [
          (0, null_stdin, `Blocking);
          (1, fd_of_sink stdout_w, `Blocking);
          (2, fd_of_sink stderr_w, `Blocking);
        ]
      ~executable:"/bin/sh" [ "sh"; "-c"; command ]
  in
  Eio.Flow.close stdout_w;
  Eio.Flow.close stderr_w;
  let alive = ref true in
  let cleanup () =
    if !alive then (
      Eio.Process.signal process Sys.sigterm;
      signal_group (Eio.Process.pid process) Sys.sigterm;
      Eio.Process.signal process Sys.sigkill;
      signal_group (Eio.Process.pid process) Sys.sigkill)
  in
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect cleanup)
    (fun () ->
      let buffer = Buffer.create 256 in
      let mutex = Eio.Mutex.create () in
      let captured = ref 0 in
      let truncated = ref false in
      Eio.Fiber.both
        (fun () -> drain stdout_r ~mutex ~buffer ~captured ~truncated)
        (fun () -> drain stderr_r ~mutex ~buffer ~captured ~truncated);
      let status = Eio.Process.await process in
      alive := false;
      let output =
        let text = Buffer.contents buffer in
        if !truncated then text ^ "\n[output truncated after 10485760 bytes]\n" else text
      in
      match status with
      | `Exited 0 -> `Ok output
      | `Exited code -> `Error (output, Fmt.str "Exit code %d" code)
      | `Signaled signal -> `Error (output, Fmt.str "Terminated by signal %d" signal))

let run_foreground_with_timeout ctx ~cwd ~command ~timeout_s =
  match
    Tool.with_timeout ctx (float_of_int timeout_s) (fun () ->
        run_foreground ctx ~cwd ~command)
  with
  | Error error -> Error error
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
        match Tool.decode bash_params_jsont value with
        | Error error -> Error error
        | Ok params -> (
            let command = params.command in
            let timeout_s = Option.value params.timeout_s ~default:120 in
            let run_in_background =
              Option.value params.run_in_background ~default:false
            in
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
              let read_only = is_read_only command && not run_in_background in
              let permission_path = "" in
              match
                Tool.request ctx ~read_only ~tool:"bash" ~action:command
                  ~path:permission_path ~description:params.description
              with
              | Error error -> Error error
              | Ok () -> (
                  if blocked_command command then
                    Error (`Unavailable "command is blocked by the shell safety policy")
                  else if run_in_background then
                    let job_id =
                      Jobs.start ctx.Tool.jobs ~cwd ~command ~env:[] ~timeout_s
                    in
                    Ok (output_with_artifact ctx (Fmt.str "started %s" job_id))
                  else
                    try
                      match run_foreground_with_timeout ctx ~cwd ~command ~timeout_s with
                      | Ok output -> Ok output
                      | Error error -> Error error
                    with
                    | Eio.Io _ as exception_ ->
                        Error (`Io (cwd, Fmt.str "%a" Eio.Exn.pp exception_))
                    | Unix.Unix_error (error, operation, argument) ->
                        Error
                          (`Io
                             ( cwd,
                               Fmt.str "%s: %s (%s)" operation (Unix.error_message error)
                                 argument ))
                    | Invalid_argument message -> Error (`Invalid_input message)
                    | Failure message -> Error (`Unavailable message))));
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
        match Tool.decode job_output_params_jsont value with
        | Error error -> Error error
        | Ok params -> (
            let job_id = String.trim params.job_id in
            if job_id = "" then Error (`Invalid_input "job_id must not be empty")
            else
              match
                Tool.request ctx ~read_only:true ~tool:"job_output" ~action:job_id
                  ~path:ctx.Tool.cwd
                  ~description:(Fmt.str "Read output for %s" job_id)
              with
              | Error error -> Error error
              | Ok () -> (
                  match
                    Jobs.output ctx.Tool.jobs ~id:job_id
                      ~wait:(Option.value params.wait ~default:false)
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
                      Ok (output_with_artifact ctx content))));
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
        match Tool.decode job_kill_params_jsont value with
        | Error error -> Error error
        | Ok params -> (
            let job_id = String.trim params.job_id in
            if job_id = "" then Error (`Invalid_input "job_id must not be empty")
            else
              match
                Tool.request ctx ~read_only:false ~tool:"job_kill" ~action:job_id ~path:""
                  ~description:(Fmt.str "Terminate %s" job_id)
              with
              | Error error -> Error error
              | Ok () -> (
                  match Jobs.kill ctx.Tool.jobs ~id:job_id with
                  | Ok () -> Ok (output_with_artifact ctx (Fmt.str "killed %s" job_id))
                  | Error (`Not_found id) -> Error (`Not_found id))));
  }
