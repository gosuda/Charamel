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
  Sys.remove path;
  path

let make_context ?(allowed_tools = [ "read"; "write"; "edit" ]) env sw root =
  let config = Config.default in
  let permissions : Config.permissions = { allowed_tools; deny = [] } in
  let permission =
    Permission.create ~config:permissions ~yolo:false
      ~asker:(fun _ -> Permission.Allow_once)
      ~cwd:root
      ~plans_dir:(Filename.concat root ".crush/plans")
      ()
  in
  let hooks =
    Hooks.create ~config:[] ~proc_mgr:env#process_mgr ~clock:env#clock ~cwd:root
  in
  let artifacts = Artifact.create ~fs:env#fs ~dir:(Filename.concat root ".artifacts") in
  let jobs = Jobs.create ~sw ~proc_mgr:env#process_mgr ~clock:env#clock ~artifacts in
  let mcp =
    Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock ~cwd:root
      ~config
  in
  let todos = Todos.create () in
  let skills = Skills.load ~fs:env#fs ~config ~home:root in
  {
    Tool.sw;
    clock = env#clock;
    fs = env#fs;
    net = env#net;
    proc_mgr = env#process_mgr;
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

let with_context ~temp_prefix ?allowed_tools f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let root = temp_root temp_prefix in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(env#fs / root);
  let context = make_context ?allowed_tools env sw root in
  f env root context

let write_file env path content =
  let target = Eio.Path.(env#fs / path) in
  begin match Eio.Path.split target with
  | Some (parent, _) -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent
  | None -> ()
  end;
  Eio.Path.save ~create:(`Or_truncate 0o644) target content

let load_file env path = Eio.Path.load Eio.Path.(env#fs / path)

let json_object fields =
  Jsont.Json.object'
    (List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value) fields)

let run_tool tool context value =
  match tool.Tool.run context value with
  | Ok output -> output
  | Error error -> Alcotest.failf "%a" Tool.pp_error error
