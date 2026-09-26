module Tool = Crush_core.Tool
module Config = Crush_core.Config
module Permission = Crush_core.Permission
module Hooks = Crush_core.Hooks
module Artifact = Crush_core.Artifact
module Jobs = Crush_core.Jobs
module Todos = Crush_core.Todos
module Skills = Crush_core.Skills
module Mcp = Crush_core.Mcp

let temp_root prefix =
  let path = Filename.temp_file prefix ".dir" in
  Unix.unlink path;
  path

let make_context ?(allowed_tools = [ "read"; "write"; "edit" ]) sw root =
  let open Lwt.Syntax in
  let clock = Charamel_os.Time.lwt in
  let config = Config.default in
  let permissions : Config.permissions = { allowed_tools; deny = [] } in
  let permission =
    Permission.create ~config:permissions ~yolo:false
      ~asker:(fun _ -> Permission.Allow_once)
      ~cwd:root
      ~plans_dir:(Filename.concat root ".crush/plans")
      ()
  in
  let hooks = Hooks.create ~config:[] ~cwd:root in
  let artifacts = Artifact.create ~fs_root:root ~dir:".artifacts" in
  let jobs = Jobs.create ~sw ~artifacts in
  let* mcp = Mcp.create ~cwd:root ~config in
  let todos = Todos.create () in
  let+ skills = Skills.load ~fs_root:root ~config ~home:root in
  {
    Tool.clock;
    fs_root = root;
    random = (fun length -> String.make length '\001');
    env = Sys.getenv_opt;
    cwd = root;
    session = "test-session";
    call_id = "test-call";
    config;
    permission;
    hooks;
    lsp = None;
    mcp;
    artifacts;
    jobs;
    todos;
    skills;
    log_path = Filename.concat root "crush.log";
    interactive = false;
    is_subagent = false;
    ask = None;
    run_subagent = None;
    read_tracker = Hashtbl.create 16;
  }

let with_context ?allowed_tools f =
  let open Lwt.Syntax in
  Test_support.with_temp_dir @@ fun root ->
  Lwt_switch.with_switch (fun sw ->
      let* context = make_context ?allowed_tools sw root in
      Lwt_direct.spawn (fun () -> f root context))

let rec mkdir_p dir =
  if
    dir <> Filename.current_dir_name
    && dir <> Filename.parent_dir_name
    && dir <> ""
    && not (Sys.file_exists dir)
  then begin
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  end

let write_file path content =
  mkdir_p (Filename.dirname path);
  let channel =
    open_out_gen [ Open_wronly; Open_creat; Open_trunc; Open_text ] 0o644 path
  in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel content)

let load_file path =
  let channel = open_in_bin path in
  let length = in_channel_length channel in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () -> really_input_string channel length)

let json_object fields =
  Jsont.Json.object'
    (List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value) fields)

let run_tool tool context value =
  match tool.Tool.run context value with
  | Ok output -> output
  | Error error -> Alcotest.failf "%a" Tool.pp_error error

let case name (speed : Alcotest.speed_level) f =
  Alcotest_lwt.test_case name speed (fun _switch () -> Lwt_direct.spawn (fun () -> f ()))

let with_scratch f =
  let thunk =
    Test_support.with_temp_dir @@ fun root -> Lwt_direct.spawn (fun () -> f root)
  in
  Lwt_direct.await thunk

type http_fixture = { port : int; shutdown : unit -> unit Lwt.t }

let rec drain_body flow remaining =
  let open Lwt.Syntax in
  if remaining <= 0 then Lwt.return_unit
  else
    let* chunk = Lwt_io.read ~count:remaining flow in
    if String.length chunk = 0 then Lwt.return_unit
    else drain_body flow (remaining - String.length chunk)

let drain_request flow =
  let open Lwt.Syntax in
  let rec headers content_length =
    Lwt.catch
      (fun () ->
        let* line = Lwt_io.read_line flow in
        if line = "" then Lwt.return content_length
        else
          let content_length =
            match String.index_opt line ':' with
            | Some index
              when String.lowercase_ascii (String.trim (String.sub line 0 index))
                   = "content-length" ->
                let value =
                  String.sub line (index + 1) (String.length line - index - 1)
                in
                Option.value (int_of_string_opt (String.trim value)) ~default:0
            | _ -> content_length
          in
          headers content_length)
      (fun exn ->
        match exn with End_of_file -> Lwt.return content_length | exn -> Lwt.fail exn)
  in
  let* length = headers 0 in
  drain_body flow length

let close_fd_quietly fd =
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _exn -> Lwt.return_unit)

let start_http_server ~content_type ~body =
  let open Lwt.Syntax in
  let listener = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt listener Lwt_unix.SO_REUSEADDR true;
  Lwt_direct.await @@ Lwt_unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Lwt_unix.listen listener 16;
  let port =
    match Lwt_unix.getsockname listener with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX path -> Alcotest.failf "unexpected Unix listener %s" path
  in
  let running = ref true in
  let handle_client client_fd =
    let input = Lwt_io.of_fd ~mode:Lwt_io.input client_fd in
    let output =
      Lwt_io.of_fd ~close:(fun () -> Lwt.return_unit) ~mode:Lwt_io.output client_fd
    in
    Lwt.catch
      (fun () ->
        let* () = drain_request input in
        let contents = body () in
        let header =
          Fmt.str
            "HTTP/1.1 200 OK\r\n\
             content-type: %s\r\n\
             content-length: %d\r\n\
             connection: close\r\n\
             \r\n"
            content_type (String.length contents)
        in
        let* () = Lwt_io.write output header in
        let* () = Lwt_io.write output contents in
        let* () = Lwt_io.flush output in
        Lwt_io.close input)
      (fun _exn -> close_fd_quietly client_fd)
  in
  let rec accept_loop () =
    if not !running then Lwt.return_unit
    else
      Lwt.try_bind
        (fun () -> Lwt_unix.accept listener)
        (fun (client_fd, _addr) ->
          Lwt.async (fun () -> handle_client client_fd);
          accept_loop ())
        (fun exn ->
          match exn with
          | Lwt.Canceled | End_of_file | Unix.Unix_error _ -> Lwt.return_unit
          | exn -> Lwt.fail exn)
  in
  Lwt.async accept_loop;
  {
    port;
    shutdown =
      (fun () ->
        running := false;
        let wake_fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        let* () =
          Lwt_unix.connect wake_fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
        in
        let* () = close_fd_quietly wake_fd in
        close_fd_quietly listener);
  }

let start_http_fixture ~content_type body =
  start_http_server ~content_type ~body:(fun () -> body)

let with_http_server ~content_type ~body f =
  let fixture = start_http_server ~content_type ~body in
  Fun.protect
    (fun () -> f fixture.port)
    ~finally:(fun () -> Lwt_direct.await (fixture.shutdown ()))

let with_http_fixture ~content_type body f =
  with_http_server ~content_type ~body:(fun () -> body) f
