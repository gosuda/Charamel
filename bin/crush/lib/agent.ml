open Result.Syntax

exception User_cancel

type event =
  | Text_delta of string
  | Reasoning_delta of string
  | Tool_started of { id : string; name : string; input : Jsont.json }
  | Tool_finished of {
      id : string;
      name : string;
      output : Tool.output;
      elapsed_ms : int;
    }
  | Permission_asked of Permission.request
  | Permission_resolved of Permission.request * Permission.outcome
  | Usage of {
      usage : Charm_fantasy.Usage.t;
      cost_usd : float;
      total_cost_usd : float;
      context_tokens : int;
    }
  | Compacted of { summary_chars : int }
  | Title of string
  | Advisor_note of { severity : string; guidance : string }
  | Turn_done of finish
  | Failed of error

and finish =
  [ `Stop
  | `Length
  | `Content_filter
  | `Interrupted
  | `Loop_detected
  | `Budget
  | `Halted of string ]

and error =
  [ `Busy
  | `Provider of string
  | `Session of Session.error
  | `Models of Models.error
  | `Auth of string
  | `Tool of string ]

type deps = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  net : Eio_unix.Net.t;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  random : int -> string;
  env : string -> string option;
  cwd : string;
  config : Config.t;
  auth : Auth.t;
  store : Session.store;
  permission : Permission.t;
  hooks : Hooks.t;
  lsp : Lsp.t option;
  mcp : Mcp.t;
  skills : Skills.t;
  rules : Rules.t;
  log_path : string;
  interactive : bool;
  ask :
    (Tool.question list -> (Tool.answer list, [ `Aborted | `Not_interactive ]) result)
    option;
  events : event -> unit;
}

type budget = { limit : int; mutable used : int; mutex : Eio.Mutex.t }

type call = {
  id : string;
  name : string;
  raw : Buffer.t;
  mutable decoded : (Jsont.json, string) result option;
}

type stream_item =
  | Text_item of Buffer.t
  | Reasoning_item of Buffer.t
  | Call_item of call

type stream_result = {
  finish : [ `Stop | `Length | `Content_filter | `Tool_calls | `Error of string ];
  calls : call list;
  parts : Charm_fantasy.Message.part list;
  prompt_tokens : int;
  completion_tokens : int;
  typed_error : Charm_fantasy.Error.t option;
  emitted : bool;
}

type execution = {
  id : string;
  name : string;
  input : Jsont.json;
  output : Tool.output;
  elapsed_ms : int;
  stop_turn : bool;
}

type scheduled = {
  call : call;
  input : Jsont.json;
  tool : Tool.t option;
  invalid : bool;
  mutable promise : execution Eio.Promise.or_exn option;
}

type turn_state = {
  mutable assistant_appended : bool;
  mutable parts : Charm_fantasy.Message.part list;
  mutable last_turn : Charm_fantasy.Message.t list;
}

type t = {
  deps : deps;
  session : Session.t;
  large_model : Models.resolved ref;
  small_model : Models.resolved ref;
  budget : budget;
  is_subagent : bool;
  mutable tools : Tool.t list;
  artifacts : Artifact.t;
  jobs : Jobs.t;
  todos : Todos.t;
  read_tracker : (string, int) Hashtbl.t;
  run_child : prompt:string -> (string, string) result;
  busy_state : bool ref;
  turn_cancel : Eio.Cancel.t option ref;
  usage : Charm_fantasy.Usage.t ref;
  cost_usd : float ref;
  context_tokens : int ref;
  loop_window : string list ref;
  advisor : Advisor.t;
  turn_count : int ref;
}

let log_src = Logs.Src.create "crush.agent"

module Log = (val Logs.src_log log_src : Logs.LOG)

let pp_error ppf = function
  | `Busy -> Fmt.string ppf "agent is busy"
  | `Provider message -> Fmt.pf ppf "provider: %s" message
  | `Session error -> Fmt.pf ppf "session: %a" Session.pp_error error
  | `Models error -> Fmt.pf ppf "models: %a" Models.pp_error error
  | `Auth message -> Fmt.pf ppf "auth: %s" message
  | `Tool message -> Fmt.pf ppf "tool: %s" message

let now_ms t = int_of_float (Eio.Time.now t.deps.clock *. 1000.)

let append t event : (unit, error) result =
  match Session.append t.session ~clock:t.deps.clock event with
  | Ok () -> Ok ()
  | Error error -> Error (`Session error)

let append_logged t event =
  match append t event with
  | Ok () -> ()
  | Error error -> Log.err (fun m -> m "session append failed: %a" pp_error error)

let emit t event = t.deps.events event
let decode_json text = Jsont_bytesrw.decode_string Jsont.json text

let git_command t turn_sw args =
  let run () =
    let source, sink = Eio.Process.pipe ~sw:turn_sw t.deps.proc_mgr in
    Fun.protect
      ~finally:(fun () ->
        Eio.Resource.close source;
        Eio.Resource.close sink)
      (fun () ->
        let process =
          Eio.Process.spawn ~sw:turn_sw t.deps.proc_mgr ~stdout:sink ~stderr:Eio.Flow.null
            args
        in
        Eio.Resource.close sink;
        let reader = Eio.Buf_read.of_flow ~max_size:65_536 source in
        let output = Eio.Buf_read.take_all reader in
        ignore (Eio.Process.await process);
        String.trim output)
  in
  match
    Eio.Time.with_timeout t.deps.clock 5. (fun () ->
        try Ok (Some (run ()))
        with Eio.Io _ | Unix.Unix_error _ | Sys_error _ | Failure _ -> Ok None)
  with
  | Ok output -> output
  | Error `Timeout -> None

let environment_block t turn_sw =
  let context = Rules.context_text t.deps.rules in
  let skills = Skills.index_text t.deps.skills in
  let lsp =
    match t.deps.lsp with
    | None -> ""
    | Some lsp -> String.concat "," (List.map fst (Lsp.servers lsp))
  in
  let mcp = String.concat "," (List.map fst (Mcp.states t.deps.mcp)) in
  let todos = Todos.render (Todos.get t.todos) in
  let date =
    let tm = Unix.gmtime (Eio.Time.now t.deps.clock) in
    Fmt.str "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
  in
  let branch =
    Option.value ~default:"unknown"
      (git_command t turn_sw
         [ "git"; "-C"; t.deps.cwd; "rev-parse"; "--abbrev-ref"; "HEAD" ])
  in
  let status =
    git_command t turn_sw [ "git"; "-C"; t.deps.cwd; "status"; "--porcelain" ]
  in
  let changed =
    match status with
    | None -> "status unavailable"
    | Some status when status = "" -> "0"
    | Some status -> string_of_int (List.length (String.split_on_char '\n' status))
  in
  let recent =
    Option.value ~default:"unavailable"
      (git_command t turn_sw [ "git"; "-C"; t.deps.cwd; "log"; "--oneline"; "-5" ])
  in
  let context_files = if context = "" then "" else "loaded" in
  let fields =
    [
      "# Environment";
      "cwd: " ^ t.deps.cwd;
      "platform: " ^ Sys.os_type;
      "date: " ^ date;
      "git: branch " ^ branch ^ ", " ^ changed ^ " changed files";
      "recent commits:";
      recent;
      "context files: " ^ context_files;
      "lsp: " ^ lsp;
      "mcp: " ^ mcp;
      "todos:";
      todos;
    ]
  in
  let fields = String.concat "\n" fields in
  if skills = "" then fields ^ "\n" else fields ^ "\nSkills:\n" ^ skills ^ "\n"

let system_prompt t turn_sw =
  let pieces =
    [ Prompt_coder.text; environment_block t turn_sw; Rules.context_text t.deps.rules ]
  in
  let skills = Skills.index_text t.deps.skills in
  let pieces = if skills = "" then pieces else pieces @ [ "Skills:\n" ^ skills ] in
  String.concat "\n\n" (List.filter (fun value -> value <> "") pieces)

let add_delta items kind text =
  if text = "" then ()
  else
    match (kind, !items) with
    | `Text, Text_item buffer :: _ -> Buffer.add_string buffer text
    | `Reasoning, Reasoning_item buffer :: _ -> Buffer.add_string buffer text
    | `Text, _ ->
        let buffer = Buffer.create (String.length text) in
        Buffer.add_string buffer text;
        items := Text_item buffer :: !items
    | `Reasoning, _ ->
        let buffer = Buffer.create (String.length text) in
        Buffer.add_string buffer text;
        items := Reasoning_item buffer :: !items

let call_by_id (calls : call list ref) id =
  let rec find = function
    | [] -> None
    | (call : call) :: rest -> if String.equal call.id id then Some call else find rest
  in
  find !calls

let parts_of_items items =
  let part = function
    | Text_item buffer ->
        let text = Buffer.contents buffer in
        if text = "" then None else Some (Charm_fantasy.Message.Text text)
    | Reasoning_item buffer ->
        let text = Buffer.contents buffer in
        if text = "" then None
        else Some (Charm_fantasy.Message.Reasoning { text; signature = None })
    | Call_item call ->
        let input =
          match call.decoded with
          | Some (Ok json) -> json
          | Some (Error _) | None -> Jsont.Json.object' []
        in
        Some (Charm_fantasy.Message.Tool_call { id = call.id; name = call.name; input })
  in
  List.filter_map part (List.rev items)

let decode_call call =
  match call.decoded with
  | Some decoded -> decoded
  | None ->
      let decoded = decode_json (Buffer.contents call.raw) in
      call.decoded <- Some decoded;
      decoded

let record_usage t (model : Models.resolved) usage =
  let cost = Models.cost model.Models.model usage in
  t.usage := Charm_fantasy.Usage.add !(t.usage) usage;
  t.cost_usd := !(t.cost_usd) +. cost;
  emit t
    (Usage
       {
         usage;
         cost_usd = cost;
         total_cost_usd = !(t.cost_usd);
         context_tokens = !(t.context_tokens);
       });
  append t
    (Session.Usage
       {
         ms = now_ms t;
         usage;
         cost_usd = cost;
         model =
           {
             Session.provider = model.Models.provider_id;
             model = model.Models.model.Charm_fantasy.Model.id;
           };
       })

let refresh_model ?(force = false) ?rejected t turn_sw role :
    (Models.resolved * Charm_fantasy.Provider.auth, error) result =
  let model = match role with `Large -> !(t.large_model) | `Small -> !(t.small_model) in
  let refresh : (Charm_fantasy.Provider.auth, [ `Auth of string ]) result =
    if force then
      match rejected with
      | None -> Error (`Auth "missing rejected credential for forced refresh")
      | Some rejected -> (
          match
            Auth.refresh ~sw:turn_sw ~net:t.deps.net ~config:t.deps.config ~env:t.deps.env
              t.deps.auth ~provider:model.Models.provider_id ~rejected
          with
          | Error (`Disabled reason) -> Error (`Auth reason)
          | Error `No_credential ->
              Error (`Auth ("no credential for " ^ model.Models.provider_id))
          | Error (`Refresh provider_error) ->
              Error (`Auth (Charm_fantasy.Error.message provider_error))
          | Error error -> Error (`Auth (Fmt.str "%a" Auth.pp_refresh_error error))
          | Ok provider_auth -> Ok provider_auth)
    else
      match
        Auth.ensure_fresh ~sw:turn_sw ~net:t.deps.net ~config:t.deps.config
          ~env:t.deps.env t.deps.auth ~provider:model.Models.provider_id
      with
      | Error (`Disabled reason) -> Error (`Auth reason)
      | Error `No_credential ->
          Error (`Auth ("no credential for " ^ model.Models.provider_id))
      | Error (`Refresh provider_error) ->
          Error (`Auth (Charm_fantasy.Error.message provider_error))
      | Error error -> Error (`Auth (Fmt.str "%a" Auth.pp_refresh_error error))
      | Ok provider_auth -> Ok provider_auth
  in
  match refresh with
  | Error (`Auth message) -> Error (`Auth message)
  | Ok provider_auth -> (
      match
        Models.with_auth ~fs:t.deps.fs t.deps.config ~env:t.deps.env model provider_auth
      with
      | Error error -> Error (`Models error)
      | Ok resolved ->
          (match role with
          | `Large -> t.large_model := resolved
          | `Small -> t.small_model := resolved);
          Ok (resolved, provider_auth))

let take_child_budget t =
  if not t.is_subagent then true
  else
    Eio.Mutex.use_rw ~protect:true t.budget.mutex (fun () ->
        if t.budget.used >= t.budget.limit then false
        else (
          t.budget.used <- t.budget.used + 1;
          true))

let tool_context t ~sw ~call_id : Tool.ctx =
  {
    sw;
    clock = t.deps.clock;
    fs = t.deps.fs;
    net = t.deps.net;
    proc_mgr = t.deps.proc_mgr;
    random = t.deps.random;
    env = t.deps.env;
    cwd = t.deps.cwd;
    session = Session.id t.session;
    call_id;
    config = t.deps.config;
    permission = t.deps.permission;
    hooks = t.deps.hooks;
    lsp = t.deps.lsp;
    mcp = t.deps.mcp;
    artifacts = t.artifacts;
    jobs = t.jobs;
    todos = t.todos;
    skills = t.deps.skills;
    log_path = t.deps.log_path;
    interactive = t.deps.interactive;
    is_subagent = t.is_subagent;
    ask = t.deps.ask;
    run_subagent = (if t.is_subagent then None else Some t.run_child);
    read_tracker = t.read_tracker;
  }

let rebuild_tools t =
  let ctx_template = tool_context t ~sw:t.deps.sw ~call_id:"" in
  t.tools <-
    Toolset.build ~ctx_template ~mcp_tools:(Mcp.tools t.deps.mcp) ~subagent:t.is_subagent

let consume_stream t turn_sw (model : Models.resolved) provider_auth messages state =
  rebuild_tools t;
  if not (take_child_budget t) then Result.Error (`Tool "subagent budget exhausted")
  else
    let typed_error = ref None in
    let emitted = ref false in
    let stream =
      Charm_fantasy.Provider.stream model.Models.provider ~sw:turn_sw ~clock:t.deps.clock
        ~net:t.deps.net ~model:model.Models.model
        ~system:[ system_prompt t turn_sw ]
        ~tools:(Toolset.fantasy t.tools) ~max_tokens:model.Models.max_tokens
        ~reasoning:model.Models.reasoning
        ~on_error:(fun error -> typed_error := Some error)
        messages
    in
    let items = ref [] in
    let calls = ref [] in
    let latest_usage = ref Charm_fantasy.Usage.zero in
    let rec consume () =
      match Eio.Stream.take stream with
      | Charm_fantasy.Stream_part.Text_delta text ->
          emitted := true;
          add_delta items `Text text;
          state.parts <- parts_of_items !items;
          emit t (Text_delta text);
          consume ()
      | Reasoning_delta text ->
          emitted := true;
          add_delta items `Reasoning text;
          state.parts <- parts_of_items !items;
          emit t (Reasoning_delta text);
          consume ()
      | Tool_call_start { id; name } ->
          emitted := true;
          let call = { id; name; raw = Buffer.create 128; decoded = None } in
          calls := call :: !calls;
          items := Call_item call :: !items;
          state.parts <- parts_of_items !items;
          consume ()
      | Tool_input_delta { id; delta } ->
          emitted := true;
          (match call_by_id calls id with
          | Some call -> Buffer.add_string call.raw delta
          | None -> ());
          state.parts <- parts_of_items !items;
          consume ()
      | Tool_call_end _ ->
          emitted := true;
          consume ()
      | Usage usage -> (
          emitted := true;
          latest_usage := Charm_fantasy.Usage.add !latest_usage usage;
          match record_usage t model usage with
          | Ok () -> consume ()
          | Error error -> Result.Error error)
      | Finish finish ->
          let calls = List.rev !calls in
          List.iter (fun call -> ignore (decode_call call)) calls;
          state.parts <- parts_of_items !items;
          let latest = !latest_usage in
          t.context_tokens := Charm_fantasy.Usage.total latest;
          Ok
            {
              finish;
              calls;
              parts = state.parts;
              prompt_tokens =
                latest.Charm_fantasy.Usage.input + latest.Charm_fantasy.Usage.cache_read
                + latest.Charm_fantasy.Usage.cache_write;
              completion_tokens = latest.Charm_fantasy.Usage.output;
              typed_error = !typed_error;
              emitted = !emitted;
            }
    in
    ignore provider_auth;
    consume ()

let result_output error = Tool.fail (Fmt.str "%a" Tool.pp_error error)

let run_tool t turn_sw (call : call) input (tool : Tool.t) =
  let started = Eio.Time.now t.deps.clock in
  let ctx = tool_context t ~sw:turn_sw ~call_id:call.id in
  let output, stop_turn, hook_input =
    match
      Hooks.pre_tool t.deps.hooks ~session:(Session.id t.session) ~tool:tool.Tool.name
        ~input
    with
    | Hooks.Deny reason -> (result_output (`Denied reason), true, input)
    | Hooks.Allow input' -> (
        match tool.Tool.run ctx input' with
        | Ok output -> (output, false, input')
        | Error (`Denied _ as error) -> (result_output error, true, input')
        | Error error -> (result_output error, false, input'))
  in
  let elapsed_ms = int_of_float ((Eio.Time.now t.deps.clock -. started) *. 1000.) in
  Hooks.post_tool t.deps.hooks ~session:(Session.id t.session) ~tool:tool.Tool.name
    ~input:hook_input ~output:output.Tool.content ~is_error:output.Tool.is_error;
  { id = call.id; name = call.name; input = hook_input; output; elapsed_ms; stop_turn }

let schedule_call ?(invalid = false) t _turn_sw (call : call) input =
  let tool = Toolset.find t.tools call.name in
  emit t (Tool_started { id = call.id; name = call.name; input });
  { call; input; tool; invalid; promise = None }

let read_only job = match job.tool with Some tool -> tool.Tool.read_only | None -> false

let execute_calls t turn_sw calls =
  let jobs =
    List.filter_map
      (fun call ->
        match decode_call call with
        | Error _ ->
            let input = Jsont.Json.object' [] in
            Some (schedule_call ~invalid:true t turn_sw call input)
        | Ok input -> Some (schedule_call t turn_sw call input))
      calls
  in
  let semaphore = Eio.Semaphore.make 8 in
  List.iter
    (fun job ->
      if (not job.invalid) && read_only job then
        match job.tool with
        | None -> ()
        | Some tool ->
            job.promise <-
              Some
                (Eio.Fiber.fork_promise ~sw:turn_sw (fun () ->
                     Eio.Semaphore.acquire semaphore;
                     Fun.protect
                       ~finally:(fun () -> Eio.Semaphore.release semaphore)
                       (fun () -> run_tool t turn_sw job.call job.input tool)))
      else ())
    jobs;
  let window_add name input output =
    let canonical = Jsonx.display_string input in
    let fingerprint =
      Digestif.SHA256.(
        digest_string (name ^ "\000" ^ canonical ^ "\000" ^ output ^ "\000") |> to_hex)
    in
    let next = !(t.loop_window) @ [ fingerprint ] in
    let length = List.length next in
    let next =
      if length > 10 then List.filteri (fun index _ -> index >= length - 10) next
      else next
    in
    t.loop_window := next;
    List.fold_left
      (fun count item -> if String.equal item fingerprint then count + 1 else count)
      0 next
    >= 5
  in
  let rec fold pending results stop_turn loop_detected =
    match pending with
    | [] -> (List.rev results, stop_turn, loop_detected)
    | job :: rest ->
        let execution =
          if job.invalid then
            {
              id = job.call.id;
              name = job.call.name;
              input = job.input;
              output = Tool.fail "invalid JSON input";
              elapsed_ms = 0;
              stop_turn = false;
            }
          else
            match job.promise with
            | Some promise -> Eio.Promise.await_exn promise
            | None -> (
                match job.tool with
                | Some tool -> run_tool t turn_sw job.call job.input tool
                | None ->
                    {
                      id = job.call.id;
                      name = job.call.name;
                      input = job.input;
                      output = Tool.fail (job.call.name ^ ": unknown tool");
                      elapsed_ms = 0;
                      stop_turn = false;
                    })
        in
        emit t
          (Tool_finished
             {
               id = execution.id;
               name = execution.name;
               output = execution.output;
               elapsed_ms = execution.elapsed_ms;
             });
        let output_text =
          match Tool.to_result execution.output with `Text text | `Error text -> text
        in
        let loop_detected =
          loop_detected || window_add execution.name execution.input output_text
        in
        let stop_turn = stop_turn || execution.stop_turn in
        if loop_detected || stop_turn then
          (List.rev (execution :: results), stop_turn, loop_detected)
        else fold rest (execution :: results) stop_turn false
  in
  fold jobs [] false false

let session_output output =
  match Tool.to_result output with
  | `Text text -> (`Text text : Session.tool_output)
  | `Error text -> (`Error text : Session.tool_output)

let persist_executions t executions =
  let rec loop = function
    | [] -> Ok ()
    | (execution : execution) :: rest -> (
        match
          append t
            (Session.Tool_call
               {
                 ms = now_ms t;
                 id = execution.id;
                 name = execution.name;
                 input = execution.input;
               })
        with
        | Error error -> Result.Error error
        | Ok () -> (
            match
              append t
                (Session.Tool_result
                   {
                     ms = now_ms t;
                     id = execution.id;
                     name = execution.name;
                     output = session_output execution.output;
                     elapsed_ms = execution.elapsed_ms;
                     artifact = execution.output.Tool.artifact;
                   })
            with
            | Error error -> Result.Error error
            | Ok () -> loop rest))
  in
  loop executions

let assistant_message parts =
  { Charm_fantasy.Message.role = Charm_fantasy.Message.Assistant; parts }

let finish_result = function
  | `Stop -> Ok `Stop
  | `Length -> Ok `Length
  | `Content_filter -> Ok `Content_filter
  | `Tool_calls -> Ok `Stop
  | `Error message -> Result.Error (`Provider message)

let add_attachments text attachments =
  let files =
    List.map
      (fun (mime, data, name) ->
        Charm_fantasy.Message.File
          { mime; data; name = (if name = "" then None else Some name) })
      attachments
  in
  {
    Charm_fantasy.Message.role = Charm_fantasy.Message.User;
    parts = Charm_fantasy.Message.Text text :: files;
  }

let post_metadata t turn_sw first_prompt state =
  if t.is_subagent then ()
  else (
    t.turn_count := !(t.turn_count) + 1;
    (if !(t.turn_count) = 1 && Session.title t.session = "" then
       match refresh_model t turn_sw `Small with
       | Error _ -> ()
       | Ok (small, _) -> (
           match
             Title.generate ~sw:turn_sw ~clock:t.deps.clock ~net:t.deps.net ~small
               ~first_prompt
           with
           | Error (`Provider message) ->
               Log.warn (fun m -> m "title generation failed: %s" message)
           | Ok title when title <> "" -> (
               match Session.set_title t.session ~title with
               | Ok () -> emit t (Title title)
               | Error error ->
                   Log.warn (fun m ->
                       m "title persistence failed: %a" Session.pp_error error))
           | Ok _ -> ()));
    if
      t.deps.config.Config.options.Config.advisor.Config.enabled
      && !(t.turn_count)
         mod max 1 t.deps.config.Config.options.Config.advisor.Config.every_n_turns
         = 0
    then
      let role =
        match t.deps.config.Config.options.Config.advisor.Config.model with
        | `Small -> `Small
        | `Large -> `Large
      in
      match refresh_model t turn_sw role with
      | Error _ -> ()
      | Ok (model, _) -> (
          match
            Advisor.review t.advisor ~sw:turn_sw ~clock:t.deps.clock ~net:t.deps.net model
              ~context:(Rules.context_text t.deps.rules)
              ~last_turn:state.last_turn
          with
          | Error (`Provider message) ->
              Log.warn (fun m -> m "advisor failed: %s" message)
          | Ok None -> ()
          | Ok (Some verdict) ->
              let severity =
                match verdict.Advisor.severity with
                | `Nit -> "nit"
                | `Concern -> "concern"
                | `Blocker -> "blocker"
              in
              emit t (Advisor_note { severity; guidance = verdict.Advisor.guidance });
              append_logged t
                (Session.Note
                   {
                     ms = now_ms t;
                     text = Fmt.str "advisor %s: %s" severity verdict.Advisor.guidance;
                   });
              if verdict.Advisor.severity = `Blocker then
                append_logged t
                  (Session.Message
                     { ms = now_ms t; message = Advisor.steering_message verdict })))

let provider_error_message = function
  | None -> "provider returned HTTP 401"
  | Some error -> Charm_fantasy.Error.message error

let is_http_401 = function
  | Some (`Http ({ status = 401; _ } : Charm_fantasy.Error.http_error)) -> true
  | _ -> false

let disable_oauth t provider_id rejected reason =
  Auth.disable t.deps.auth ~provider:provider_id ~rejected ~reason ~now_ms:(now_ms t)

let run_turn t turn_sw first_prompt user_message attachments state =
  let user_message = add_attachments user_message attachments in
  let* () = append t (Session.Message { ms = now_ms t; message = user_message }) in
  let force_retry_used = ref false in
  state.last_turn <- [ user_message ];
  let rec loop () =
    state.assistant_appended <- false;
    state.parts <- [];
    let messages = Session.messages t.session in
    let* model, provider_auth = refresh_model t turn_sw `Large in
    let rec stream_with_recovery (model : Models.resolved) provider_auth =
      let* stream_result = consume_stream t turn_sw model provider_auth messages state in
      let retryable =
        match (stream_result.finish, provider_auth) with
        | `Error _, Charm_fantasy.Provider.Oauth _ ->
            (not stream_result.emitted) && is_http_401 stream_result.typed_error
        | _ -> false
      in
      if retryable && not !force_retry_used then (
        force_retry_used := true;
        match refresh_model ~force:true ~rejected:provider_auth t turn_sw `Large with
        | Error error -> Result.Error error
        | Ok (fresh_model, fresh_auth) -> stream_with_recovery fresh_model fresh_auth)
      else if retryable then
        match
          disable_oauth t model.Models.provider_id provider_auth
            (provider_error_message stream_result.typed_error)
        with
        | Error error -> Result.Error (`Auth (Fmt.str "%a" Auth.pp_error error))
        | Ok () ->
            Result.Error
              (`Auth
                 ("credential rejected by provider; log in again for "
                ^ model.Models.provider_id))
      else Result.Ok stream_result
    in
    match stream_with_recovery model provider_auth with
    | Error error -> Error error
    | Ok stream_result -> (
        let empty_provider_error =
          match (stream_result.finish, stream_result.parts) with
          | `Error message, [] -> Some message
          | _ -> None
        in
        match empty_provider_error with
        | Some message -> Error (`Provider message)
        | None -> (
            List.iter (fun call -> ignore (decode_call call)) stream_result.calls;
            let assistant = assistant_message stream_result.parts in
            let* () = append t (Session.Message { ms = now_ms t; message = assistant }) in
            state.assistant_appended <- true;
            state.last_turn <- state.last_turn @ [ assistant ];
            match (stream_result.finish, stream_result.calls) with
            | `Tool_calls, calls when calls <> [] ->
                let executions, stop_turn, loop_detected =
                  execute_calls t turn_sw calls
                in
                let results =
                  List.map
                    (fun execution ->
                      (execution.id, execution.name, session_output execution.output))
                    executions
                in
                if results = [] then Ok `Stop
                else
                  let tool_message = Charm_fantasy.Message.tool_results results in
                  let* () =
                    append t (Session.Message { ms = now_ms t; message = tool_message })
                  in
                  state.last_turn <- state.last_turn @ [ tool_message ];
                  let* () = persist_executions t executions in
                  if loop_detected then Ok `Loop_detected
                  else if stop_turn then Ok `Stop
                  else if
                    Compaction.needed
                      ~context_window:
                        model.Models.model.Charm_fantasy.Model.context_window
                      ~prompt_tokens:stream_result.prompt_tokens
                      ~completion_tokens:stream_result.completion_tokens
                      ~disabled:
                        t.deps.config.Config.options.Config.disable_auto_compaction
                  then (
                    let* small, small_auth = refresh_model t turn_sw `Small in
                    match
                      Compaction.run ~sw:turn_sw ~clock:t.deps.clock ~net:t.deps.net
                        ~small ~auth:small_auth t.session
                    with
                    | Error (`Provider message) -> Error (`Provider message)
                    | Error (`Session error) -> Error (`Session error)
                    | Ok summary ->
                        emit t (Compacted { summary_chars = String.length summary });
                        loop ())
                  else loop ()
            | `Tool_calls, [] -> Ok `Stop
            | finish, _ -> finish_result finish))
  in
  let result = loop () in
  match result with
  | Ok finish ->
      post_metadata t turn_sw first_prompt state;
      Ok finish
  | Error _ -> result

let finalize t state result =
  let finish, error =
    match result with
    | Ok finish -> (finish, None)
    | Error error -> (`Halted (Fmt.str "%a" pp_error error), Some error)
  in
  if (not state.assistant_appended) && state.parts <> [] then (
    let partial = assistant_message state.parts in
    ignore (append t (Session.Message { ms = now_ms t; message = partial }));
    ignore (append t (Session.Note { ms = now_ms t; text = "interrupted" })));
  let reason =
    match (error, finish) with
    | Some error, _ -> `Error (Fmt.str "%a" pp_error error)
    | None, `Interrupted -> `Interrupted
    | None, _ -> `Stop
  in
  Hooks.stop t.deps.hooks ~session:(Session.id t.session) ~reason;
  (match error with Some error -> emit t (Failed error) | None -> ());
  emit t (Turn_done finish);
  result

let prompt t ?(attachments = []) text =
  if !(t.busy_state) then Error `Busy
  else (
    t.busy_state := true;
    let state = { assistant_appended = false; parts = []; last_turn = [] } in
    let external_cancel = ref None in
    let result =
      try
        Eio.Switch.run (fun turn_sw ->
            Eio.Cancel.sub (fun cancel_context ->
                t.turn_cancel := Some cancel_context;
                Fun.protect
                  ~finally:(fun () -> t.turn_cancel := None)
                  (fun () -> run_turn t turn_sw text text attachments state)))
      with
      | Eio.Cancel.Cancelled User_cancel -> Ok `Interrupted
      | Eio.Cancel.Cancelled exception_ ->
          external_cancel := Some exception_;
          Ok `Interrupted
    in
    let result = Eio.Cancel.protect (fun () -> finalize t state result) in
    t.busy_state := false;
    match !external_cancel with
    | Some exception_ -> raise (Eio.Cancel.Cancelled exception_)
    | None -> result)

let rec run_child_agent parent ~prompt:instruction =
  let model = !(parent.large_model) in
  let title =
    let prefix =
      if String.length instruction > 40 then String.sub instruction 0 40 else instruction
    in
    "subagent: " ^ prefix
  in
  match
    Session.create parent.deps.store ~clock:parent.deps.clock ~random:parent.deps.random
      ~parent:(Session.id parent.session) ~title ~cwd:parent.deps.cwd
      ~model:
        {
          Session.provider = model.Models.provider_id;
          model = model.Models.model.Charm_fantasy.Model.id;
        }
      ()
  with
  | Error error -> Error (Fmt.str "%a" Session.pp_error error)
  | Ok child_session -> (
      let child_deps = { parent.deps with interactive = false; ask = None } in
      let child =
        create_internal ~budget:parent.budget ~is_subagent:true child_deps
          ~session:child_session ~large:!(parent.large_model) ~small:!(parent.small_model)
      in
      match child with
      | Error error -> Error (Fmt.str "%a" pp_error error)
      | Ok child_agent -> (
          match prompt child_agent instruction with
          | Error (`Tool message) when String.equal message "subagent budget exhausted" ->
              ignore
                (Session.append parent.session ~clock:parent.deps.clock
                   (Session.Note { ms = now_ms parent; text = message }));
              Error message
          | Error error -> Error (Fmt.str "%a" pp_error error)
          | Ok _ ->
              let rec last = function
                | [] -> ""
                | { Charm_fantasy.Message.role = Charm_fantasy.Message.Assistant; parts }
                  :: rest ->
                    let text =
                      List.filter_map
                        (function
                          | Charm_fantasy.Message.Text text -> Some text | _ -> None)
                        parts
                      |> String.concat ""
                    in
                    if text = "" then last rest else text
                | _ :: rest -> last rest
              in
              Ok (last (List.rev (Session.messages child_session)))))

and create_internal ~budget ~is_subagent deps ~session ~large ~small =
  let artifacts =
    Artifact.create ~fs:deps.fs
      ~dir:(Session.artifacts_dir deps.store ~id:(Session.id session))
  in
  let jobs =
    Jobs.create ~sw:deps.sw ~proc_mgr:deps.proc_mgr ~clock:deps.clock ~artifacts
  in
  let todos = Todos.create () in
  let read_tracker = Hashtbl.create 32 in
  let run_child_ref = ref (fun ~prompt:_ -> Error "nested subagents are disabled") in
  let ctx_template : Tool.ctx =
    {
      sw = deps.sw;
      clock = deps.clock;
      fs = deps.fs;
      net = deps.net;
      proc_mgr = deps.proc_mgr;
      random = deps.random;
      env = deps.env;
      cwd = deps.cwd;
      session = Session.id session;
      call_id = "";
      config = deps.config;
      permission = deps.permission;
      hooks = deps.hooks;
      lsp = deps.lsp;
      mcp = deps.mcp;
      artifacts;
      jobs;
      todos;
      skills = deps.skills;
      log_path = deps.log_path;
      interactive = deps.interactive;
      is_subagent;
      ask = deps.ask;
      run_subagent =
        (if is_subagent then None else Some (fun ~prompt -> !run_child_ref ~prompt));
      read_tracker;
    }
  in
  let tools =
    Toolset.build ~ctx_template ~mcp_tools:(Mcp.tools deps.mcp) ~subagent:is_subagent
  in
  let usage, cost = Session.usage_total session in
  let agent =
    {
      deps;
      session;
      large_model = ref large;
      small_model = ref small;
      budget;
      is_subagent;
      tools;
      artifacts;
      jobs;
      todos;
      read_tracker;
      run_child = (fun ~prompt -> !run_child_ref ~prompt);
      busy_state = ref false;
      turn_cancel = ref None;
      usage = ref usage;
      cost_usd = ref cost;
      context_tokens = ref 0;
      loop_window = ref [];
      advisor = Advisor.create deps.config.Config.options.Config.advisor;
      turn_count = ref 0;
    }
  in
  (if is_subagent then
     run_child_ref := fun ~prompt:_ -> Error "nested subagents are disabled"
   else run_child_ref := fun ~prompt -> run_child_agent agent ~prompt);
  Hooks.session_start deps.hooks ~session:(Session.id session);
  Ok agent

let create deps ~session ~large ~small =
  let budget =
    {
      limit = max 0 deps.config.Config.options.Config.budgets.Config.subagent_requests;
      used = 0;
      mutex = Eio.Mutex.create ();
    }
  in
  create_internal ~budget ~is_subagent:false deps ~session ~large ~small

let session t = t.session
let large t = !(t.large_model)

let set_models t ~large ~small =
  t.large_model := large;
  t.small_model := small

let busy t = !(t.busy_state)

let cancel t =
  match !(t.turn_cancel) with
  | None -> ()
  | Some context -> Eio.Cancel.cancel context User_cancel

let compact t =
  if !(t.busy_state) then Error `Busy
  else
    Eio.Switch.run (fun turn_sw ->
        let* small, auth = refresh_model t turn_sw `Small in
        match
          Compaction.run ~sw:turn_sw ~clock:t.deps.clock ~net:t.deps.net ~small ~auth
            t.session
        with
        | Error (`Provider message) -> Error (`Provider message)
        | Error (`Session error) -> Error (`Session error)
        | Ok summary ->
            emit t (Compacted { summary_chars = String.length summary });
            Ok ())

let set_plan_mode t enabled =
  Permission.set_plan_mode t.deps.permission enabled;
  let text = if enabled then "plan mode on" else "plan mode off" in
  ignore (append t (Session.Note { ms = now_ms t; text }))

let stats t = (!(t.usage), !(t.cost_usd), !(t.context_tokens))
