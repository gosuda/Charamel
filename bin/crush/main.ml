open Crush_core

type cli = {
  yolo : bool;
  plan : bool;
  model : string option;
  session : string option;
  continue_ : bool;
  cwd : string option;
  data_dir : string option;
  debug : bool;
}

type common = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  cwd : string;
  config : Config.t;
  auth : Auth.t;
  store : Session.store;
  rules : Rules.t;
  skills : Skills.t;
  hooks : Hooks.t;
  mcp : Mcp.t;
  lsp : Lsp.t option;
  log_path : string;
  bridge : Crush_ui.Bridge.t option;
  interactive : bool;
}

type agent_runtime = { common : common; agent : Agent.t; permission : Permission.t ref }

let random_bytes length = Mirage_crypto_rng_unix.getrandom length
let path fs filename = Eio.Path.(fs / filename)
let write flow text = Eio.Flow.copy_string text flow
let print_stdout env text = write env#stdout text
let print_stderr env text = write env#stderr text

let with_data_dir (config : Config.t) (options : cli) =
  let data_dir =
    match options.data_dir with
    | None -> config.options.data_dir
    | Some value when String.trim value <> "" -> value
    | Some _ -> Charm_cli.error "--data-dir must not be empty"
  in
  let options =
    { config.options with data_dir; debug = config.options.debug || options.debug }
  in
  { config with options }

let cwd_for env (options : cli) =
  let current = Eio.Path.native_exn env#cwd in
  match options.cwd with
  | None -> current
  | Some path -> if Filename.is_relative path then Filename.concat current path else path

let ensure_directory fs directory =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (path fs directory)

let prepare_directories env config cwd =
  let xdg =
    [
      Charm_cli.Xdg.config_dir ~app:"crush";
      Charm_cli.Xdg.data_dir ~app:"crush";
      Charm_cli.Xdg.state_dir ~app:"crush";
      Charm_cli.Xdg.cache_dir ~app:"crush";
      Config.data_dir config ~cwd;
    ]
  in
  List.iter (ensure_directory env#fs) xdg

let load_config env ~cwd (options : cli) =
  match Config.load ~fs:env#fs ~env:Sys.getenv_opt ~cwd with
  | Error error -> Charm_cli.error (Fmt.str "%a" Config.pp_error error)
  | Ok (config, _) -> with_data_dir config options

let home_directory () = match Sys.getenv_opt "HOME" with Some home -> home | None -> ""
let log_path () = Filename.concat (Charm_cli.Xdg.state_dir ~app:"crush") "crush.log"
let permission_asker bridge request = Crush_ui.Bridge.ask_permission bridge request

let create_common env sw (options : cli) ~interactive ~bridge =
  let cwd = cwd_for env options in
  let config = load_config env ~cwd options in
  prepare_directories env config cwd;
  let auth =
    match Auth.create ~path:(path env#fs (Auth.path ())) ~clock:env#clock () with
    | Ok value -> value
    | Error error -> Charm_cli.error (Fmt.str "%a" Auth.pp_error error)
  in
  let store = Session.store ~fs:env#fs ~cwd in
  let rules = Rules.load ~fs:env#fs ~cwd ~config in
  let skills = Skills.load ~fs:env#fs ~config ~home:(home_directory ()) in
  let hooks =
    Hooks.create ~config:config.hooks ~proc_mgr:env#process_mgr ~clock:env#clock ~cwd
  in
  let mcp =
    Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock ~cwd ~config
  in
  let lsp =
    if config.options.auto_lsp then
      Some
        (Lsp.create ~sw ~proc_mgr:env#process_mgr ~clock:env#clock ~fs:env#fs ~cwd ~config)
    else None
  in
  let log_path = log_path () in
  ensure_directory env#fs (Filename.dirname log_path);
  {
    env;
    sw;
    cwd;
    config;
    auth;
    store;
    rules;
    skills;
    hooks;
    mcp;
    lsp;
    log_path;
    bridge;
    interactive;
  }

let model_ref (model : Models.resolved) =
  { Session.provider = model.provider_id; model = model.model.id }

let selected_model config target ~fs ~auth ~env =
  let split_target value =
    match String.index_opt value '/' with
    | Some slash when slash > 0 && slash < String.length value - 1 ->
        Some
          ( String.sub value 0 slash,
            String.sub value (slash + 1) (String.length value - slash - 1) )
    | _ -> None
  in
  match split_target target with
  | Some (provider, model) ->
      Ok { Config.provider; model; reasoning = None; max_tokens = None }
  | None -> (
      let rows = Models.list ~fs config ~auth ~env in
      let found =
        List.find_map
          (fun (provider, models, _) ->
            match
              List.find_opt
                (fun (model : Charm_fantasy.Model.t) -> String.equal model.id target)
                models
            with
            | Some _ -> Some (provider, target)
            | None -> None)
          rows
      in
      match found with
      | Some (provider, model) ->
          Ok { Config.provider; model; reasoning = None; max_tokens = None }
      | None ->
          Error (Fmt.str "model must be PROVIDER/MODEL or a known model id: %s" target))

let config_for_model common (options : cli) =
  match options.model with
  | None -> Ok common.config
  | Some target -> (
      match
        selected_model common.config target ~fs:common.env#fs ~auth:common.auth
          ~env:Sys.getenv_opt
      with
      | Error message -> Error message
      | Ok selected ->
          Ok
            {
              common.config with
              models = { common.config.models with large = Some selected };
            })

let session_for common (options : cli) large =
  if Option.is_some options.session && options.continue_ then
    Error "--session and --continue cannot be used together"
  else
    match (options.session, options.continue_) with
    | Some id, _ -> (
        match Session.open_ common.store ~id with
        | Ok session -> Ok session
        | Error (`Not_found path) -> Error (Fmt.str "cannot open session %s" path)
        | Error (`Io (path, message)) ->
            Error (Fmt.str "cannot open session %s: %s" path message)
        | Error (`Index message) -> Error ("session index is corrupt: " ^ message)
        | Error (`Session_corrupt (path, line)) ->
            Error (Fmt.str "session %s is corrupt at line %d" path line))
    | None, true -> (
        match Session.last common.store with
        | Ok session -> Ok session
        | Error (`Not_found _) -> (
            match
              Session.create common.store ~clock:common.env#clock ~random:random_bytes
                ~cwd:common.cwd ~model:(model_ref large) ()
            with
            | Ok session -> Ok session
            | Error error -> Error (Fmt.str "%a" Session.pp_error error))
        | Error error ->
            Error (Fmt.str "cannot read session index: %a" Session.pp_error error))
    | None, false -> (
        match
          Session.create common.store ~clock:common.env#clock ~random:random_bytes
            ~cwd:common.cwd ~model:(model_ref large) ()
        with
        | Ok session -> Ok session
        | Error error -> Error (Fmt.str "%a" Session.pp_error error))

let make_events common =
  match common.bridge with
  | Some bridge -> Crush_ui.Bridge.push bridge
  | None -> (
      fun event ->
        match event with
        | Agent.Text_delta text -> print_stdout common.env text
        | Agent.Tool_finished { output; name; _ } when output.is_error ->
            print_stderr common.env (Fmt.str "tool %s: %s@." name output.content)
        | Agent.Failed error ->
            print_stderr common.env (Fmt.str "ERROR: %a@." Agent.pp_error error)
        | Agent.Turn_done _ -> print_stdout common.env "\n"
        | _ -> ())

let make_ask common =
  match common.bridge with
  | None -> None
  | Some bridge -> Some (fun questions -> Crush_ui.Bridge.ask bridge questions)

let make_permission common (config : Config.t) (options : cli) =
  let asker =
    match common.bridge with
    | None -> None
    | Some bridge -> Some (permission_asker bridge)
  in
  let on_decision request outcome =
    match common.bridge with
    | None -> ()
    | Some bridge ->
        Crush_ui.Bridge.push bridge (Agent.Permission_resolved (request, outcome))
  in
  Permission.create ~config:config.permissions ~yolo:options.yolo ?asker ~cwd:common.cwd
    ~plans_dir:(Filename.concat (Config.data_dir config ~cwd:common.cwd) "plans")
    ~on_decision ()

let create_agent_for_session common (options : cli) config ~large ~small session =
  let permission = make_permission common config options in
  let deps : Agent.deps =
    {
      sw = common.sw;
      clock = common.env#clock;
      fs = common.env#fs;
      net = common.env#net;
      proc_mgr = common.env#process_mgr;
      random = random_bytes;
      env = Sys.getenv_opt;
      cwd = common.cwd;
      config;
      auth = common.auth;
      store = common.store;
      permission;
      hooks = common.hooks;
      lsp = common.lsp;
      mcp = common.mcp;
      skills = common.skills;
      rules = common.rules;
      log_path = common.log_path;
      interactive = common.interactive;
      ask = make_ask common;
      events = make_events common;
    }
  in
  match Agent.create deps ~session ~large ~small with
  | Error error -> Error (`Agent error)
  | Ok agent ->
      if options.plan then Agent.set_plan_mode agent true;
      Ok { common; agent; permission = ref permission }

let create_agent common (options : cli) =
  match config_for_model common options with
  | Error message -> Error (`Config message)
  | Ok config -> (
      match
        ( Models.resolve ~fs:common.env#fs config ~auth:common.auth ~env:Sys.getenv_opt
            ~role:`Large,
          Models.resolve ~fs:common.env#fs config ~auth:common.auth ~env:Sys.getenv_opt
            ~role:`Small )
      with
      | Ok large, Ok small -> (
          match session_for common options large with
          | Error error -> Error (`Session error)
          | Ok session ->
              create_agent_for_session common options config ~large ~small session)
      | Error error, _ -> Error (`Models error)
      | _, Error error -> Error (`Models error))

let pp_runtime_error = function
  | `Config message -> message
  | `Session error -> "session: " ^ error
  | `Models error -> Fmt.str "models: %a" Models.pp_error error
  | `Agent error -> Fmt.str "agent: %a" Agent.pp_error error

let with_agent env (options : cli) ~interactive f =
  Eio.Switch.run @@ fun sw ->
  let bridge =
    if interactive then Some (Crush_ui.Bridge.create ~capacity:256 ()) else None
  in
  let result =
    try
      let common = create_common env sw options ~interactive ~bridge in
      match create_agent common options with
      | Error error -> Error (pp_runtime_error error)
      | Ok runtime -> f runtime
    with
    | Eio.Io _ as exn -> Error (Fmt.str "%a" Eio.Exn.pp exn)
    | Unix.Unix_error (error, fn, arg) ->
        Error (Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg)
    | Invalid_argument message -> Error message
  in
  Option.iter Crush_ui.Bridge.close bridge;
  result

let prompt_from env prompts =
  match prompts with
  | first :: rest -> Ok (String.concat " " (first :: rest))
  | [] -> (
      try
        let value =
          Eio.Buf_read.parse_exn ~max_size:10_485_760 Eio.Buf_read.take_all env#stdin
        in
        if String.trim value = "" then Error "prompt is empty" else Ok value
      with End_of_file -> Error "prompt is empty")

let validate_agent_options options =
  if Option.is_some options.session && options.continue_ then
    Charm_cli.error ~code:2 "--session and --continue cannot be used together"

let run_agent env options prompts =
  validate_agent_options options;
  match prompt_from env prompts with
  | Error message -> Charm_cli.error message
  | Ok prompt -> (
      match
        with_agent env options ~interactive:false (fun runtime ->
            match Agent.prompt runtime.agent prompt with
            | Error error -> Error (Fmt.str "%a" Agent.pp_error error)
            | Ok (`Stop | `Length | `Content_filter) -> Ok ()
            | Ok `Interrupted -> Error "interrupted"
            | Ok `Loop_detected -> Error "agent loop detected"
            | Ok `Budget -> Error "subagent request budget exhausted"
            | Ok (`Halted reason) -> Error ("agent halted: " ^ reason))
      with
      | Ok () -> ()
      | Error "interrupted" -> Charm_cli.exit 130
      | Error message -> Charm_cli.error message)

let form_environment env =
  Charm_huh.Form.Env.v ~fs:env#fs ~temp_dir:env#cwd
    ~editor:(Charm_huh.Form.Env.editor_of_string (Sys.getenv_opt "EDITOR"))

let mime_type path =
  match String.lowercase_ascii (Filename.extension path) with
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | ".webp" -> "image/webp"
  | ".pdf" -> "application/pdf"
  | ".json" -> "application/json"
  | ".txt" | ".md" | ".ml" | ".mli" | ".rs" | ".py" | ".ts" | ".js" -> "text/plain"
  | _ -> "application/octet-stream"

let attachment env cwd filename =
  let target =
    if Filename.is_relative filename then path env#fs (Filename.concat cwd filename)
    else path env#fs filename
  in
  try Ok (mime_type filename, Eio.Path.load target, Some (Filename.basename filename))
  with Eio.Io _ -> Error (Fmt.str "cannot read attachment %s" filename)

let model_rows runtime =
  match
    Models.list ~fs:runtime.common.env#fs runtime.common.config ~auth:runtime.common.auth
      ~env:Sys.getenv_opt
  with
  | rows ->
      List.concat_map
        (fun (provider, models, _) ->
          List.map
            (fun (model : Charm_fantasy.Model.t) ->
              {
                Crush_ui.id = model.Charm_fantasy.Model.id;
                provider;
                context_window = model.context_window;
                max_tokens = model.default_max_tokens;
                can_reason = model.can_reason;
                supports_attachments = model.supports_attachments;
              })
            models)
        rows

let session_rows runtime =
  match Session.list runtime.common.store with
  | Error _ -> []
  | Ok rows ->
      List.map
        (fun (row : Session.index_entry) ->
          {
            Crush_ui.id = row.Session.id;
            title = row.title;
            (* The session index does not persist the model; the column stays
               empty until the index gains a model_ref field. *)
            model = "";
            created_ms = row.created_ms;
          })
        rows

let model_config runtime ~agent target =
  match
    selected_model runtime.common.config target ~fs:runtime.common.env#fs
      ~auth:runtime.common.auth ~env:Sys.getenv_opt
  with
  | Error message -> Error message
  | Ok selected -> (
      let config =
        {
          runtime.common.config with
          models = { runtime.common.config.models with large = Some selected };
        }
      in
      match
        ( Models.resolve ~fs:runtime.common.env#fs config ~auth:runtime.common.auth
            ~env:Sys.getenv_opt ~role:`Large,
          Models.resolve ~fs:runtime.common.env#fs config ~auth:runtime.common.auth
            ~env:Sys.getenv_opt ~role:`Small )
      with
      | Ok large, Ok small ->
          Agent.set_models agent ~large ~small;
          Ok ()
      | Error error, _ -> Error (Fmt.str "%a" Models.pp_error error)
      | _, Error error -> Error (Fmt.str "%a" Models.pp_error error))

let replace_agent runtime options ~agent_ref session =
  Agent.cancel !agent_ref;
  let previous_plan = Permission.plan_mode !(runtime.permission) in
  match config_for_model runtime.common options with
  | Error message -> Error message
  | Ok config -> (
      match
        ( Models.resolve ~fs:runtime.common.env#fs config ~auth:runtime.common.auth
            ~env:Sys.getenv_opt ~role:`Large,
          Models.resolve ~fs:runtime.common.env#fs config ~auth:runtime.common.auth
            ~env:Sys.getenv_opt ~role:`Small )
      with
      | Ok large, Ok small -> (
          match
            create_agent_for_session runtime.common options config ~large ~small session
          with
          | Error (`Agent _) -> Error "could not create agent"
          | Error (`Models error) -> Error (Fmt.str "%a" Models.pp_error error)
          | Error (`Session error) -> Error ("session: " ^ error)
          | Error (`Config message) -> Error message
          | Ok next ->
              if previous_plan then Agent.set_plan_mode next.agent true;
              agent_ref := next.agent;
              runtime.permission := !(next.permission);
              Ok next.agent)
      | Error error, _ -> Error (Fmt.str "%a" Models.pp_error error)
      | _, Error error -> Error (Fmt.str "%a" Models.pp_error error))

let new_session_agent runtime options ~agent_ref =
  match config_for_model runtime.common options with
  | Error message -> Error message
  | Ok config -> (
      match
        Models.resolve ~fs:runtime.common.env#fs config ~auth:runtime.common.auth
          ~env:Sys.getenv_opt ~role:`Large
      with
      | Error error -> Error (Fmt.str "%a" Models.pp_error error)
      | Ok large -> (
          match
            Session.create runtime.common.store ~clock:runtime.common.env#clock
              ~random:random_bytes ~cwd:runtime.common.cwd ~model:(model_ref large) ()
          with
          | Error error -> Error (Fmt.str "%a" Session.pp_error error)
          | Ok session -> replace_agent runtime options ~agent_ref session))

let resumed_agent runtime options ~agent_ref id =
  match Session.open_ runtime.common.store ~id with
  | Error error -> Error (Fmt.str "%a" Session.pp_error error)
  | Ok session -> replace_agent runtime options ~agent_ref session

let message_role = function
  | Charm_fantasy.Message.System -> "system"
  | Charm_fantasy.Message.User -> "user"
  | Charm_fantasy.Message.Assistant -> "assistant"
  | Charm_fantasy.Message.Tool -> "tool"

let message_part_text = function
  | Charm_fantasy.Message.Text text -> text
  | Charm_fantasy.Message.Reasoning { text; _ } -> text
  | Charm_fantasy.Message.File { mime; data; name } ->
      Fmt.str "<file mime=%s name=%s bytes=%d>" mime
        (Option.value ~default:"" name)
        (String.length data)
  | Charm_fantasy.Message.Tool_call { id; name; input } ->
      Fmt.str "call %s (%s): %s" id name (Jsonx.string_of_json input)
  | Charm_fantasy.Message.Tool_result { id; name; output } ->
      let output =
        match output with
        | `Text text -> text
        | `Error text -> "error: " ^ text
        | `Media (mime, data) -> Fmt.str "media <%s> (%d bytes)" mime (String.length data)
      in
      Fmt.str "result %s (%s): %s" id name output

let history_rows agent =
  List.map
    (fun ({ Charm_fantasy.Message.role; parts } : Charm_fantasy.Message.t) ->
      {
        Crush_ui.role = message_role role;
        text = String.concat "" (List.map message_part_text parts);
      })
    (Session.messages (Agent.session agent))

let login_provider env provider ~force =
  if not (String.equal provider "anthropic") then
    Charm_cli.error "only anthropic OAuth login is supported"
  else
    Eio.Switch.run @@ fun sw ->
    let auth =
      match Auth.create ~path:(path env#fs (Auth.path ())) ~clock:env#clock () with
      | Ok value -> value
      | Error error -> Charm_cli.error (Fmt.str "%a" Auth.pp_error error)
    in
    match Auth.find auth ~provider with
    | Some (Auth.Oauth _) when not force ->
        print_stdout env
          (Fmt.str "You are already logged in to %s.\nUse --force to re-authenticate.\n"
             provider)
    | _ -> (
        let reader = Eio.Buf_read.of_flow ~max_size:65_536 env#stdin in
        let prompt_paste () =
          print_stderr env "Paste the OAuth callback URL or code#state: ";
          try Some (Eio.Buf_read.line reader) with End_of_file -> None
        in
        let open_browser uri =
          let browser = Option.value (Sys.getenv_opt "BROWSER") ~default:"xdg-open" in
          try
            ignore
              (Eio_unix.run_in_systhread (fun () ->
                   Unix.create_process browser [| browser; uri |] Unix.stdin Unix.stdout
                     Unix.stderr))
          with Unix.Unix_error _ ->
            print_stderr env (Fmt.str "Open this URL in a browser: %s@." uri)
        in
        match Auth.Login.anthropic ~sw ~net:env#net ~open_browser ~prompt_paste auth with
        | Ok () -> print_stdout env "Logged in to anthropic.\n"
        | Error `Timeout -> Charm_cli.error ~code:124 "OAuth login timed out"
        | Error `Aborted -> Charm_cli.exit 130
        | Error (`Oauth message) -> Charm_cli.error message
        | Error (`Io (path, message)) -> Charm_cli.error (Fmt.str "%s: %s" path message)
        | Error (`Parse (path, message)) ->
            Charm_cli.error (Fmt.str "%s: %s" path message))

let logout_provider env provider ~force =
  Eio.Switch.run @@ fun _sw ->
  let auth =
    match Auth.create ~path:(path env#fs (Auth.path ())) ~clock:env#clock () with
    | Ok value -> value
    | Error error -> Charm_cli.error (Fmt.str "%a" Auth.pp_error error)
  in
  if (not force) && Option.is_none (Auth.find auth ~provider) then
    Charm_cli.error (Fmt.str "no credentials for provider %s" provider);
  match Auth.remove auth ~provider with
  | Ok () -> print_stdout env (Fmt.str "Logged out of %s.\n" provider)
  | Error error -> Charm_cli.error (Fmt.str "%a" Auth.pp_error error)

let run_tui env options =
  match
    with_agent env options ~interactive:true (fun runtime ->
        match runtime.common.bridge with
        | None -> Error "interactive bridge unavailable"
        | Some bridge -> (
            let agent_ref = ref runtime.agent in
            let open_browser uri =
              let browser = Option.value (Sys.getenv_opt "BROWSER") ~default:"xdg-open" in
              try
                ignore
                  (Eio_unix.run_in_systhread (fun () ->
                       Unix.create_process browser [| browser; uri |] Unix.stdin
                         Unix.stdout Unix.stderr))
              with Unix.Unix_error _ ->
                print_stderr env (Fmt.str "Open this URL in a browser: %s@." uri)
            in
            let login provider code =
              if not (String.equal provider "anthropic") then
                Error "only anthropic OAuth login is supported"
              else
                let pasted = ref false in
                let prompt_paste () =
                  if !pasted || String.trim code = "" then None
                  else (
                    pasted := true;
                    Some code)
                in
                match
                  Auth.Login.anthropic ~sw:runtime.common.sw ~net:env#net ~open_browser
                    ~prompt_paste runtime.common.auth
                with
                | Error `Timeout -> Error "OAuth login timed out"
                | Error `Aborted -> Error "OAuth login aborted"
                | Error (`Oauth message) -> Error message
                | Error (`Io (path, message)) -> Error (Fmt.str "%s: %s" path message)
                | Error (`Parse (path, message)) -> Error (Fmt.str "%s: %s" path message)
                | Ok () -> (
                    let session = Agent.session !agent_ref in
                    match replace_agent runtime options ~agent_ref session with
                    | Ok _ -> Ok ()
                    | Error message -> Error message)
            in
            let backend : Crush_ui.backend =
              {
                agent = agent_ref;
                events = bridge;
                clock = env#clock;
                env;
                form_env = form_environment env;
                project = runtime.common.cwd;
                session_id = (fun () -> Session.id (Agent.session !agent_ref));
                new_session = (fun () -> new_session_agent runtime options ~agent_ref);
                sessions = (fun () -> session_rows runtime);
                resume_session = (fun id -> resumed_agent runtime options ~agent_ref id);
                history = (fun () -> history_rows !agent_ref);
                models = (fun () -> model_rows runtime);
                select_model = (fun id -> model_config runtime ~agent:!agent_ref id);
                login;
                logout =
                  (fun provider ->
                    match Auth.remove runtime.common.auth ~provider with
                    | Error error -> Error (Fmt.str "%a" Auth.pp_error error)
                    | Ok () ->
                        Agent.cancel !agent_ref;
                        Ok ());
                load_attachment =
                  (fun attachment_path ->
                    attachment env runtime.common.cwd attachment_path);
                yolo = (fun () -> options.yolo);
                approve_session =
                  (fun () ->
                    Permission.set_plan_mode !(runtime.permission) false;
                    Agent.set_plan_mode !agent_ref false;
                    Ok ());
                set_plan_mode =
                  (fun enabled ->
                    Permission.set_plan_mode !(runtime.permission) enabled;
                    Agent.set_plan_mode !agent_ref enabled;
                    Ok ());
                plan_mode = (fun () -> Permission.plan_mode !(runtime.permission));
                lsp_status =
                  (fun () ->
                    match runtime.common.lsp with
                    | None -> "disabled"
                    | Some lsp ->
                        let state_text = function
                          | Lsp.Not_started -> "not started"
                          | Lsp.Starting -> "starting"
                          | Lsp.Ready -> "ready"
                          | Lsp.Failed message -> "failed: " ^ message
                          | Lsp.Disabled -> "disabled"
                        in
                        String.concat ", "
                          (List.map
                             (fun (name, state) -> name ^ ": " ^ state_text state)
                             (Lsp.servers lsp)));
                mcp_status =
                  (fun () ->
                    let state_text = function
                      | Mcp.Connecting -> "connecting"
                      | Mcp.Connected { tools; resources; prompts } ->
                          Fmt.str "connected (%d tools, %d resources, %d prompts)" tools
                            resources prompts
                      | Mcp.Failed message -> "failed: " ^ message
                      | Mcp.Disabled -> "disabled"
                    in
                    String.concat ", "
                      (List.map
                         (fun (name, state) -> name ^ ": " ^ state_text state)
                         (Mcp.states runtime.common.mcp)));
                dark = (fun () -> Charm_cli.is_dark ~env:Sys.getenv_opt);
                quit = (fun () -> Agent.cancel !agent_ref);
              }
            in
            match Crush_ui.run backend with
            | Ok _ -> Ok ()
            | Error `Interrupted -> Error "interrupted"
            | Error `Killed -> Error "TUI killed"
            | Error (`Exn (exn, _)) -> Error (Printexc.to_string exn)))
  with
  | Ok () -> ()
  | Error "interrupted" -> Charm_cli.exit 130
  | Error message -> Charm_cli.error message

let list_models env options =
  Eio.Switch.run @@ fun sw ->
  let common = create_common env sw options ~interactive:false ~bridge:None in
  let rows = Models.list ~fs:env#fs common.config ~auth:common.auth ~env:Sys.getenv_opt in
  List.iter
    (fun (provider, models, status) ->
      let status =
        match status with
        | `Ready -> "ready"
        | `No_credential -> "no credential"
        | `Disabled -> "disabled"
      in
      print_stdout env (Fmt.str "%s\t%s\n" provider status);
      List.iter
        (fun (model : Charm_fantasy.Model.t) ->
          print_stdout env
            (Fmt.str "  %s\t%d\t%d\n" model.Charm_fantasy.Model.id model.context_window
               model.default_max_tokens))
        models)
    rows

let list_sessions env options =
  Eio.Switch.run @@ fun sw ->
  let common = create_common env sw options ~interactive:false ~bridge:None in
  match Session.list common.store with
  | Error error -> Charm_cli.error (Fmt.str "%a" Session.pp_error error)
  | Ok rows ->
      List.iter
        (fun (row : Session.index_entry) ->
          print_stdout env (Fmt.str "%s\t%s\n" row.Session.id row.title))
        rows

let update_providers env source =
  if String.equal source "embedded" then
    Charm_cli.error ~code:2 "--source expects a catalog URL"
  else
    Eio.Switch.run @@ fun _sw ->
    match Models.update_catalog ~source ~fs:env#fs ~net:env#net ~clock:env#clock () with
    | Error (`Io (target, message)) -> Charm_cli.error (Fmt.str "%s: %s" target message)
    | Error (`Parse message) -> Charm_cli.error (Fmt.str "provider catalog: %s" message)
    | Error (`Fetch fetch_error) ->
        Charm_cli.error
          (Fmt.str "provider catalog: %a" Charm_fantasy.Error.pp fetch_error)
    | Ok (Models.Updated etag) ->
        print_stdout env
          (if etag = "" then "Updated provider catalog.\n"
           else Fmt.str "Updated provider catalog (etag %s).\n" etag)
    | Ok Models.Not_modified -> print_stdout env "Provider catalog is unchanged.\n"

let read_log env ~tail ~follow =
  if tail < 1 then Charm_cli.error ~code:2 "--tail must be at least 1";
  let filename = log_path () in
  let read () =
    try Eio.Path.load (path env#fs filename)
    with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ""
  in
  let take_tail text =
    let lines =
      match List.rev (String.split_on_char '\n' text) with
      | "" :: rest -> List.rev rest
      | reversed -> List.rev reversed
    in
    let count = List.length lines in
    let start = max 0 (count - tail) in
    String.concat "\n" (List.filteri (fun index _ -> index >= start) lines)
  in
  let previous = ref "" in
  let emit () =
    let current = take_tail (read ()) in
    if not (String.equal current !previous) then (
      previous := current;
      print_stdout env (if current = "" then "" else current ^ "\n"))
  in
  if follow then
    while true do
      emit ();
      Eio.Time.sleep env#clock 0.25
    done
  else emit ()

let dirs env options =
  let cwd = cwd_for env options in
  let config = load_config env ~cwd options in
  let lines =
    [
      "config: " ^ Charm_cli.Xdg.config_dir ~app:"crush";
      "data: " ^ Charm_cli.Xdg.data_dir ~app:"crush";
      "state: " ^ Charm_cli.Xdg.state_dir ~app:"crush";
      "cache: " ^ Charm_cli.Xdg.cache_dir ~app:"crush";
      "project: " ^ Config.data_dir config ~cwd;
      "project-key: " ^ Config.project_key ~cwd;
    ]
  in
  print_stdout env (String.concat "\n" lines ^ "\n")

let option_term =
  let open Cmdliner in
  let yolo =
    Arg.(value & flag & info [ "y"; "yolo" ] ~doc:"Skip ordinary permission prompts.")
  in
  let plan = Arg.(value & flag & info [ "plan" ] ~doc:"Start in plan mode.") in
  let model =
    Arg.(
      value
      & opt (some string) None
      & info [ "m"; "model" ] ~docv:"PROVIDER/MODEL" ~doc:"Select the large model.")
  in
  let session =
    Arg.(
      value
      & opt (some string) None
      & info [ "s"; "session" ] ~docv:"ID" ~doc:"Resume the named session.")
  in
  let continue_ =
    Arg.(value & flag & info [ "C"; "continue" ] ~doc:"Resume the newest session.")
  in
  let cwd =
    Arg.(
      value
      & opt (some string) None
      & info [ "c"; "cwd" ] ~docv:"DIR" ~doc:"Use DIR as the project directory.")
  in
  let data_dir =
    Arg.(
      value
      & opt (some string) None
      & info [ "D"; "data-dir" ] ~docv:"DIR" ~doc:"Store Crush data below DIR.")
  in
  let debug =
    Arg.(value & flag & info [ "d"; "debug" ] ~doc:"Enable debug configuration.")
  in
  Term.(
    const (fun yolo plan model session continue_ cwd data_dir debug ->
        { yolo; plan; model; session; continue_; cwd; data_dir; debug })
    $ yolo $ plan $ model $ session $ continue_ $ cwd $ data_dir $ debug)

let command_info name doc = Cmdliner.Cmd.info name ~doc

let command_run env =
  let prompts = Cmdliner.Arg.(value & pos_all string [] & info [] ~docv:"PROMPT") in
  let action options prompts = run_agent env options prompts in
  Cmdliner.Cmd.v
    (command_info "run" "Run one coding-agent prompt.")
    Cmdliner.Term.(const action $ option_term $ prompts)

let command_login env =
  let provider =
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"PROVIDER")
  in
  let force =
    Cmdliner.Arg.(
      value & flag
      & Cmdliner.Arg.info [ "f"; "force" ]
          ~doc:"Force re-authentication even if already logged in.")
  in
  let action provider force = login_provider env provider ~force in
  Cmdliner.Cmd.v
    (command_info "login" "Authenticate a provider.")
    Cmdliner.Term.(const action $ provider $ force)

let command_logout env =
  let provider =
    Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"PROVIDER")
  in
  let force =
    Cmdliner.Arg.(
      value & flag
      & Cmdliner.Arg.info [ "f"; "force" ] ~doc:"Treat a missing credential as success.")
  in
  let action provider force = logout_provider env provider ~force in
  Cmdliner.Cmd.v
    (command_info "logout" "Remove a provider credential.")
    Cmdliner.Term.(const action $ provider $ force)

let command_models env =
  let action = list_models env in
  Cmdliner.Cmd.v
    (command_info "models" "List configured and catalog models.")
    Cmdliner.Term.(const action $ option_term)

let command_sessions env =
  let action = list_sessions env in
  Cmdliner.Cmd.v
    (command_info "sessions" "List saved sessions.")
    Cmdliner.Term.(const action $ option_term)

let command_update_providers env =
  let source =
    Cmdliner.Arg.(
      value
      & opt string "https://catwalk.charm.sh/v2/providers"
      & info [ "source" ] ~docv:"URL" ~doc:"Provider catalog URL.")
  in
  let action source = update_providers env source in
  Cmdliner.Cmd.v
    (command_info "update-providers" "Refresh the provider catalog.")
    Cmdliner.Term.(const action $ source)

let command_logs env =
  let follow =
    Cmdliner.Arg.(value & flag & info [ "f"; "follow" ] ~doc:"Follow new log lines.")
  in
  let tail =
    Cmdliner.Arg.(
      value & opt int 1000 & info [ "t"; "tail" ] ~docv:"N" ~doc:"Print the last N lines.")
  in
  let action follow tail = read_log env ~tail ~follow in
  Cmdliner.Cmd.v
    (command_info "logs" "Read Crush logs.")
    Cmdliner.Term.(
      const (fun _options follow tail -> action follow tail) $ option_term $ follow $ tail)

let command_dirs env =
  let action = dirs env in
  Cmdliner.Cmd.v
    (command_info "dirs" "Show Crush directories.")
    Cmdliner.Term.(const action $ option_term)

let command_schema env =
  let action _env = print_stdout env (Jsonx.string_of_json Config.schema ^ "\n") in
  let env_term = Cmdliner.Term.const env in
  Cmdliner.Cmd.v
    (command_info "schema" "Print the Crush configuration schema.")
    Cmdliner.Term.(const action $ env_term)

let default env =
  let action = run_tui env in
  Cmdliner.Term.(const action $ option_term)

let commands =
  [
    command_run;
    command_login;
    command_logout;
    command_models;
    command_sessions;
    command_update_providers;
    command_logs;
    command_dirs;
    command_schema;
  ]

let run () =
  Mirage_crypto_rng_unix.use_default ();
  Charm_cli.run ~name:"crush" ~version:Charm_cli.Version.current
    ~doc:"Agentic coding harness with a terminal UI and a scriptable run mode." ~default
    commands

let () = run ()
