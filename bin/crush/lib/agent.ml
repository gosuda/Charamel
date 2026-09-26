open Lwt.Infix
open Lwt_result.Syntax

type interrupt = Idle | User_cancel | External_cancel of exn

exception Interrupted of interrupt

type turn = { switch : Lwt_switch.t; mutable interrupt : interrupt }

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
      usage : Charamel_fantasy.Usage.t;
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
  sw : Lwt_switch.t;
  clock : Charamel_os.Time.clock;
  fs_root : string;
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
  events : event -> unit Lwt.t;
}

type budget = { limit : int; mutable used : int }

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
  parts : Charamel_fantasy.Message.part list;
  prompt_tokens : int;
  completion_tokens : int;
  typed_error : Charamel_fantasy.Error.t option;
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
  mutable promise : execution Lwt.t option;
}

type turn_state = {
  mutable assistant_appended : bool;
  mutable parts : Charamel_fantasy.Message.part list;
  mutable last_turn : Charamel_fantasy.Message.t list;
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
  turn : turn option ref;
  usage : Charamel_fantasy.Usage.t ref;
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

let now_ms t = int_of_float (Charamel_os.Time.now t.deps.clock *. 1000.)

let checkpoint t =
  match !(t.turn) with
  | Some { interrupt = User_cancel; _ } -> Lwt.fail (Interrupted User_cancel)
  | Some { interrupt = External_cancel error; _ } ->
      Lwt.fail (Interrupted (External_cancel error))
  | Some { interrupt = Idle; _ } | None -> Lwt.return_unit

let interrupt_turn turn reason =
  turn.interrupt <- reason;
  Lwt.async (fun () ->
      Lwt.catch (fun () -> Lwt_switch.turn_off turn.switch) (fun _ -> Lwt.return_unit))

let link_child_cancel parent child =
  match !(parent.turn) with
  | None -> ()
  | Some { switch; _ } ->
      Lwt_direct.await
        (Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
             (match !(child.turn) with
             | Some child_turn -> interrupt_turn child_turn User_cancel
             | None -> ());
             Lwt.return_unit))

let append t event : (unit, error) result Lwt.t =
  Session.append t.session ~clock:t.deps.clock event
  >|= Result.map_error (fun error -> `Session error)

let append_logged t event =
  append t event >|= function
  | Ok () -> ()
  | Error error -> Log.err (fun m -> m "session append failed: %a" pp_error error)

let emit t event = t.deps.events event
let decode_json text = Jsont_bytesrw.decode_string Jsont.json text
let git_output_limit = 65_536
let git_timeout = 5.

let read_bounded channel limit =
  let buffer = Buffer.create 4096 in
  let rec pump total =
    Lwt_io.read ~count:4096 channel >>= function
    | "" -> Lwt.return (Some (Buffer.contents buffer))
    | text when total + String.length text > limit -> Lwt.return_none
    | text ->
        Buffer.add_string buffer text;
        pump (total + String.length text)
  in
  pump 0

let git_command args =
  let run () =
    let process = Charamel_os.Process.spawn ~stdout:`Pipe ~stderr:`Null args in
    let stdout = Charamel_os.Process.stdout_r process in
    Lwt.finalize
      (fun () ->
        read_bounded stdout git_output_limit >>= function
        | None -> Lwt.return_none
        | Some output ->
            Charamel_os.Process.await process >|= fun _ -> Some (String.trim output))
      (fun () ->
        if Charamel_os.Process.alive process then Charamel_os.Process.kill_tree process;
        Lwt.catch (fun () -> Lwt_io.close stdout) (fun _ -> Lwt.return_unit))
  in
  Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout git_timeout (fun () ->
          Lwt.catch run (function
            | Unix.Unix_error _ | Sys_error _ | Failure _ -> Lwt.return_none
            | exn -> Lwt.fail exn)))
    (function Lwt_unix.Timeout -> Lwt.return_none | exn -> Lwt.fail exn)

let environment_block t =
  let context = Rules.context_text t.deps.rules in
  let skills = Skills.index_text t.deps.skills in
  (match t.deps.lsp with None -> Lwt.return [] | Some server -> Lsp.servers server)
  >>= fun servers ->
  let lsp = String.concat "," (List.map fst servers) in
  let mcp = String.concat "," (List.map fst (Mcp.states t.deps.mcp)) in
  let todos = Todos.render (Todos.get t.todos) in
  let date =
    let tm = Unix.gmtime (Charamel_os.Time.wall t.deps.clock) in
    Fmt.str "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
  in
  git_command [ "git"; "-C"; t.deps.cwd; "rev-parse"; "--abbrev-ref"; "HEAD" ]
  >>= fun branch ->
  git_command [ "git"; "-C"; t.deps.cwd; "status"; "--porcelain" ] >>= fun status ->
  git_command [ "git"; "-C"; t.deps.cwd; "log"; "--oneline"; "-5" ] >>= fun recent ->
  let branch = Option.value ~default:"unknown" branch in
  let changed =
    match status with
    | None -> "status unavailable"
    | Some status when status = "" -> "0"
    | Some status -> string_of_int (List.length (String.split_on_char '\n' status))
  in
  let recent = Option.value ~default:"unavailable" recent in
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
  Lwt.return
    (if skills = "" then fields ^ "\n" else fields ^ "\nSkills:\n" ^ skills ^ "\n")

let system_prompt t =
  environment_block t >>= fun environment ->
  let pieces = [ Prompt_coder.text; environment; Rules.context_text t.deps.rules ] in
  let skills = Skills.index_text t.deps.skills in
  let pieces = if skills = "" then pieces else pieces @ [ "Skills:\n" ^ skills ] in
  Lwt.return (String.concat "\n\n" (List.filter (fun value -> value <> "") pieces))

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
        if text = "" then None else Some (Charamel_fantasy.Message.Text text)
    | Reasoning_item buffer ->
        let text = Buffer.contents buffer in
        if text = "" then None
        else Some (Charamel_fantasy.Message.Reasoning { text; signature = None })
    | Call_item call ->
        let input =
          match call.decoded with
          | Some (Ok json) -> json
          | Some (Error _) | None -> Jsont.Json.object' []
        in
        Some
          (Charamel_fantasy.Message.Tool_call { id = call.id; name = call.name; input })
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
  t.usage := Charamel_fantasy.Usage.add !(t.usage) usage;
  t.cost_usd := !(t.cost_usd) +. cost;
  emit t
    (Usage
       {
         usage;
         cost_usd = cost;
         total_cost_usd = !(t.cost_usd);
         context_tokens = !(t.context_tokens);
       })
  >>= fun () ->
  append t
    (Session.Usage
       {
         ms = now_ms t;
         usage;
         cost_usd = cost;
         model =
           {
             Session.provider = model.Models.provider_id;
             model = model.Models.model.Charamel_fantasy.Model.id;
           };
       })

let refresh_model ?(force = false) ?rejected t role :
    (Models.resolved * Charamel_fantasy.Provider.auth, error) result Lwt.t =
  let model = match role with `Large -> !(t.large_model) | `Small -> !(t.small_model) in
  let to_auth_result = function
    | Error (`Disabled reason) -> Error (`Auth reason)
    | Error `No_credential ->
        Error (`Auth ("no credential for " ^ model.Models.provider_id))
    | Error (`Refresh provider_error) ->
        Error (`Auth (Charamel_fantasy.Error.message provider_error))
    | Error error -> Error (`Auth (Fmt.str "%a" Auth.pp_refresh_error error))
    | Ok provider_auth -> Ok provider_auth
  in
  let refresh : (Charamel_fantasy.Provider.auth, [ `Auth of string ]) result Lwt.t =
    if force then
      match rejected with
      | None ->
          Lwt.return (Error (`Auth "missing rejected credential for forced refresh"))
      | Some rejected ->
          Auth.refresh ~config:t.deps.config ~env:t.deps.env t.deps.auth
            ~provider:model.Models.provider_id ~rejected
          >|= to_auth_result
    else
      Auth.ensure_fresh ~config:t.deps.config ~env:t.deps.env t.deps.auth
        ~provider:model.Models.provider_id
      >|= to_auth_result
  in
  refresh >>= function
  | Error (`Auth message) -> Lwt.return (Error (`Auth message))
  | Ok provider_auth -> (
      Models.with_auth ~fs_root:t.deps.fs_root t.deps.config ~env:t.deps.env model
        provider_auth
      >>= function
      | Error error -> Lwt.return (Error (`Models error))
      | Ok resolved ->
          (match role with
          | `Large -> t.large_model := resolved
          | `Small -> t.small_model := resolved);
          Lwt.return (Ok (resolved, provider_auth)))

let take_child_budget t =
  if not t.is_subagent then true
  else if t.budget.used >= t.budget.limit then false
  else (
    t.budget.used <- t.budget.used + 1;
    true)

let tool_context t ~call_id : Tool.ctx =
  {
    clock = t.deps.clock;
    fs_root = t.deps.fs_root;
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
  let ctx_template = tool_context t ~call_id:"" in
  t.tools <-
    Toolset.build ~ctx_template ~mcp_tools:(Mcp.tools t.deps.mcp) ~subagent:t.is_subagent

let consume_stream t turn_sw (model : Models.resolved) provider_auth messages state =
  rebuild_tools t;
  if not (take_child_budget t) then
    Lwt.return (Result.Error (`Tool "subagent budget exhausted"))
  else
    system_prompt t >>= fun system ->
    checkpoint t >>= fun () ->
    let typed_error = ref None in
    let emitted = ref false in
    let stream =
      Charamel_fantasy.Provider.stream model.Models.provider ~stop:turn_sw
        ~clock:t.deps.clock ~model:model.Models.model ~system:[ system ]
        ~tools:(Toolset.fantasy t.tools) ~max_tokens:model.Models.max_tokens
        ~reasoning:model.Models.reasoning
        ~on_error:(fun error -> typed_error := Some error)
        messages
    in
    let items = ref [] in
    let calls = ref [] in
    let latest_usage = ref Charamel_fantasy.Usage.zero in
    let result finish =
      let calls = List.rev !calls in
      List.iter (fun call -> ignore (decode_call call)) calls;
      state.parts <- parts_of_items !items;
      let latest = !latest_usage in
      t.context_tokens := Charamel_fantasy.Usage.total latest;
      Ok
        {
          finish;
          calls;
          parts = state.parts;
          prompt_tokens =
            latest.Charamel_fantasy.Usage.input + latest.Charamel_fantasy.Usage.cache_read
            + latest.Charamel_fantasy.Usage.cache_write;
          completion_tokens = latest.Charamel_fantasy.Usage.output;
          typed_error = !typed_error;
          emitted = !emitted;
        }
    in
    let rec consume () =
      checkpoint t >>= fun () ->
      Lwt_stream.get stream >>= fun (item : Charamel_fantasy.Stream_part.t option) ->
      match item with
      | None -> checkpoint t >|= fun () -> result `Stop
      | Some (Text_delta text) ->
          emitted := true;
          add_delta items `Text text;
          state.parts <- parts_of_items !items;
          emit t (Text_delta text) >>= consume
      | Some (Reasoning_delta text) ->
          emitted := true;
          add_delta items `Reasoning text;
          state.parts <- parts_of_items !items;
          emit t (Reasoning_delta text) >>= consume
      | Some (Tool_call_start { id; name }) ->
          emitted := true;
          let call = { id; name; raw = Buffer.create 128; decoded = None } in
          calls := call :: !calls;
          items := Call_item call :: !items;
          state.parts <- parts_of_items !items;
          consume ()
      | Some (Tool_input_delta { id; delta }) ->
          emitted := true;
          (match call_by_id calls id with
          | Some call -> Buffer.add_string call.raw delta
          | None -> ());
          state.parts <- parts_of_items !items;
          consume ()
      | Some (Tool_call_end _) ->
          emitted := true;
          consume ()
      | Some (Usage usage) -> (
          emitted := true;
          latest_usage := Charamel_fantasy.Usage.add !latest_usage usage;
          record_usage t model usage >>= function
          | Ok () -> consume ()
          | Error error -> Lwt.return (Result.Error error))
      | Some (Finish finish) -> Lwt.return (result finish)
    in
    ignore provider_auth;
    consume ()

let result_output error = Tool.fail (Fmt.str "%a" Tool.pp_error error)

let run_tool t (call : call) input (tool : Tool.t) : execution Lwt.t =
  let started = Charamel_os.Time.now t.deps.clock in
  let ctx = tool_context t ~call_id:call.id in
  ( Hooks.pre_tool t.deps.hooks ~session:(Session.id t.session) ~tool:tool.Tool.name ~input
  >>= function
    | Hooks.Deny reason -> Lwt.return (result_output (`Denied reason), true, input)
    | Hooks.Allow input' -> (
        Lwt_direct.spawn (fun () -> tool.Tool.run ctx input') >>= function
        | Ok output -> Lwt.return (output, false, input')
        | Error (`Denied _ as error) -> Lwt.return (result_output error, true, input')
        | Error error -> Lwt.return (result_output error, false, input')) )
  >>= fun (output, stop_turn, hook_input) ->
  let elapsed_ms =
    int_of_float ((Charamel_os.Time.now t.deps.clock -. started) *. 1000.)
  in
  Hooks.post_tool t.deps.hooks ~session:(Session.id t.session) ~tool:tool.Tool.name
    ~input:hook_input ~output:output.Tool.content ~is_error:output.Tool.is_error
  >|= fun () ->
  { id = call.id; name = call.name; input = hook_input; output; elapsed_ms; stop_turn }

let schedule_call ?(invalid = false) t (call : call) input =
  let tool = Toolset.find t.tools call.name in
  { call; input; tool; invalid; promise = None }

let read_only job = match job.tool with Some tool -> tool.Tool.read_only | None -> false

let start_job t pool job tool =
  job.promise <-
    Some
      (Lwt.catch
         (fun () -> Lwt_pool.use pool (fun () -> run_tool t job.call job.input tool))
         Lwt.fail)

let execute_calls t turn_sw calls =
  let jobs =
    List.filter_map
      (fun call ->
        match decode_call call with
        | Error _ ->
            let input = Jsont.Json.object' [] in
            Some (schedule_call ~invalid:true t call input)
        | Ok input -> Some (schedule_call t call input))
      calls
  in
  let pool = Lwt_pool.create 8 (fun () -> Lwt.return_unit) in
  checkpoint t >>= fun () ->
  if Lwt_switch.is_on turn_sw then
    Lwt_switch.add_hook (Some turn_sw) (fun () ->
        List.iter
          (fun job ->
            match job.promise with Some promise -> Lwt.cancel promise | None -> ())
          jobs;
        Lwt.return_unit);
  Lwt_list.iter_s
    (fun job ->
      emit t (Tool_started { id = job.call.id; name = job.call.name; input = job.input }))
    jobs
  >>= fun () ->
  List.iter
    (fun job ->
      if (not job.invalid) && read_only job then
        match job.tool with Some tool -> start_job t pool job tool | None -> ()
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
  let invalid_execution job =
    {
      id = job.call.id;
      name = job.call.name;
      input = job.input;
      output = Tool.fail "invalid JSON input";
      elapsed_ms = 0;
      stop_turn = false;
    }
  in
  let unknown_execution job =
    {
      id = job.call.id;
      name = job.call.name;
      input = job.input;
      output = Tool.fail (job.call.name ^ ": unknown tool");
      elapsed_ms = 0;
      stop_turn = false;
    }
  in
  let rec fold pending results stop_turn loop_detected =
    match pending with
    | [] -> Lwt.return (List.rev results, stop_turn, loop_detected)
    | job :: rest ->
        checkpoint t >>= fun () ->
        (if job.invalid then Lwt.return (invalid_execution job)
         else
           match job.promise with
           | Some promise -> promise
           | None -> (
               match job.tool with
               | Some tool -> run_tool t job.call job.input tool
               | None -> Lwt.return (unknown_execution job)))
        >>= fun (execution : execution) ->
        emit t
          (Tool_finished
             {
               id = execution.id;
               name = execution.name;
               output = execution.output;
               elapsed_ms = execution.elapsed_ms;
             })
        >>= fun () ->
        let output_text =
          match Tool.to_result execution.output with `Text text | `Error text -> text
        in
        let loop_detected =
          loop_detected || window_add execution.name execution.input output_text
        in
        let stop_turn = stop_turn || execution.stop_turn in
        if loop_detected || stop_turn then
          Lwt.return (List.rev (execution :: results), stop_turn, loop_detected)
        else fold rest (execution :: results) stop_turn false
  in
  fold jobs [] false false

let session_output output =
  match Tool.to_result output with
  | `Text text -> (`Text text : Session.tool_output)
  | `Error text -> (`Error text : Session.tool_output)

let persist_executions t executions =
  let rec loop = function
    | [] -> Lwt.return (Ok ())
    | (execution : execution) :: rest -> (
        append t
          (Session.Tool_call
             {
               ms = now_ms t;
               id = execution.id;
               name = execution.name;
               input = execution.input;
             })
        >>= function
        | Error error -> Lwt.return (Result.Error error)
        | Ok () -> (
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
            >>= function
            | Error error -> Lwt.return (Result.Error error)
            | Ok () -> loop rest))
  in
  loop executions

let assistant_message parts =
  { Charamel_fantasy.Message.role = Charamel_fantasy.Message.Assistant; parts }

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
        Charamel_fantasy.Message.File
          { mime; data; name = (if name = "" then None else Some name) })
      attachments
  in
  {
    Charamel_fantasy.Message.role = Charamel_fantasy.Message.User;
    parts = Charamel_fantasy.Message.Text text :: files;
  }

let post_metadata t sw first_prompt state =
  if t.is_subagent then Lwt.return_unit
  else (
    t.turn_count := !(t.turn_count) + 1;
    let title () =
      if !(t.turn_count) = 1 && Session.title t.session = "" then
        refresh_model t `Small >>= function
        | Error _ -> Lwt.return_unit
        | Ok (small, _) -> (
            Title.generate ~sw ~clock:t.deps.clock ~small ~first_prompt >>= function
            | Error (`Provider message) ->
                Log.warn (fun m -> m "title generation failed: %s" message);
                Lwt.return_unit
            | Ok title when title <> "" -> (
                Session.set_title t.session ~title >>= function
                | Ok () -> emit t (Title title)
                | Error error ->
                    Log.warn (fun m ->
                        m "title persistence failed: %a" Session.pp_error error);
                    Lwt.return_unit)
            | Ok _ -> Lwt.return_unit)
      else Lwt.return_unit
    in
    let advisor () =
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
        refresh_model t role >>= function
        | Error _ -> Lwt.return_unit
        | Ok (model, _) -> (
            Advisor.review t.advisor ~sw ~clock:t.deps.clock model
              ~context:(Rules.context_text t.deps.rules)
              ~last_turn:state.last_turn
            >>= function
            | Error (`Provider message) ->
                Log.warn (fun m -> m "advisor failed: %s" message);
                Lwt.return_unit
            | Ok None -> Lwt.return_unit
            | Ok (Some verdict) ->
                let severity =
                  match verdict.Advisor.severity with
                  | `Nit -> "nit"
                  | `Concern -> "concern"
                  | `Blocker -> "blocker"
                in
                emit t (Advisor_note { severity; guidance = verdict.Advisor.guidance })
                >>= fun () ->
                append_logged t
                  (Session.Note
                     {
                       ms = now_ms t;
                       text = Fmt.str "advisor %s: %s" severity verdict.Advisor.guidance;
                     })
                >>= fun () ->
                if verdict.Advisor.severity = `Blocker then
                  append_logged t
                    (Session.Message
                       { ms = now_ms t; message = Advisor.steering_message verdict })
                else Lwt.return_unit)
      else Lwt.return_unit
    in
    title () >>= fun () -> advisor ())

let provider_error_message = function
  | None -> "provider returned HTTP 401"
  | Some error -> Charamel_fantasy.Error.message error

let is_http_401 = function
  | Some (`Http ({ status = 401; _ } : Charamel_fantasy.Error.http_error)) -> true
  | _ -> false

let disable_oauth t provider_id rejected reason =
  Auth.disable t.deps.auth ~provider:provider_id ~rejected ~reason ~now_ms:(now_ms t)

let run_turn t turn_sw first_prompt user_message attachments state =
  let user_message = add_attachments user_message attachments in
  let* () = append t (Session.Message { ms = now_ms t; message = user_message }) in
  let force_retry_used = ref false in
  state.last_turn <- [ user_message ];
  let rec loop () =
    checkpoint t >>= fun () ->
    state.assistant_appended <- false;
    state.parts <- [];
    let messages = Session.messages t.session in
    let* model, provider_auth = refresh_model t `Large in
    let rec stream_with_recovery (model : Models.resolved) provider_auth =
      let* stream_result = consume_stream t turn_sw model provider_auth messages state in
      let retryable =
        match (stream_result.finish, provider_auth) with
        | `Error _, Charamel_fantasy.Provider.Oauth _ ->
            (not stream_result.emitted) && is_http_401 stream_result.typed_error
        | _ -> false
      in
      if retryable && not !force_retry_used then (
        force_retry_used := true;
        refresh_model ~force:true ~rejected:provider_auth t `Large >>= function
        | Error error -> Lwt.return (Result.Error error)
        | Ok (fresh_model, fresh_auth) -> stream_with_recovery fresh_model fresh_auth)
      else if retryable then
        disable_oauth t model.Models.provider_id provider_auth
          (provider_error_message stream_result.typed_error)
        >>= function
        | Error error ->
            Lwt.return (Result.Error (`Auth (Fmt.str "%a" Auth.pp_error error)))
        | Ok () ->
            Lwt.return
              (Result.Error
                 (`Auth
                    ("credential rejected by provider; log in again for "
                   ^ model.Models.provider_id)))
      else Lwt.return (Result.Ok stream_result)
    in
    let* (stream_result : stream_result) = stream_with_recovery model provider_auth in
    let empty_provider_error =
      match (stream_result.finish, stream_result.parts) with
      | `Error message, [] -> Some message
      | _ -> None
    in
    match empty_provider_error with
    | Some message -> Lwt.return (Error (`Provider message))
    | None -> (
        List.iter (fun call -> ignore (decode_call call)) stream_result.calls;
        let assistant = assistant_message stream_result.parts in
        let* () = append t (Session.Message { ms = now_ms t; message = assistant }) in
        state.assistant_appended <- true;
        state.last_turn <- state.last_turn @ [ assistant ];
        match (stream_result.finish, stream_result.calls) with
        | `Tool_calls, calls when calls <> [] ->
            execute_calls t turn_sw calls
            >>= fun (executions, stop_turn, loop_detected) ->
            let results =
              List.map
                (fun execution ->
                  (execution.id, execution.name, session_output execution.output))
                executions
            in
            if results = [] then Lwt.return (Ok `Stop)
            else
              let tool_message = Charamel_fantasy.Message.tool_results results in
              let* () =
                append t (Session.Message { ms = now_ms t; message = tool_message })
              in
              state.last_turn <- state.last_turn @ [ tool_message ];
              let* () = persist_executions t executions in
              if loop_detected then Lwt.return (Ok `Loop_detected)
              else if stop_turn then Lwt.return (Ok `Stop)
              else if
                Compaction.needed
                  ~context_window:model.Models.model.Charamel_fantasy.Model.context_window
                  ~prompt_tokens:stream_result.prompt_tokens
                  ~completion_tokens:stream_result.completion_tokens
                  ~disabled:t.deps.config.Config.options.Config.disable_auto_compaction
              then
                let* small, small_auth = refresh_model t `Small in
                Compaction.run ~sw:turn_sw ~clock:t.deps.clock ~small ~auth:small_auth
                  t.session
                >>= function
                | Error (`Provider message) -> Lwt.return (Error (`Provider message))
                | Error (`Session error) -> Lwt.return (Error (`Session error))
                | Ok summary ->
                    emit t (Compacted { summary_chars = String.length summary })
                    >>= fun () -> loop ()
              else loop ()
        | `Tool_calls, [] -> Lwt.return (Ok `Stop)
        | finish, _ -> Lwt.return (finish_result finish))
  in
  loop () >>= function
  | Ok finish -> post_metadata t turn_sw first_prompt state >|= fun () -> Ok finish
  | Error _ as result -> Lwt.return result

let finalize t state result =
  let finish, error =
    match result with
    | Ok finish -> (finish, None)
    | Error error -> (`Halted (Fmt.str "%a" pp_error error), Some error)
  in
  let persist_partial () =
    if (not state.assistant_appended) && state.parts <> [] then
      let partial = assistant_message state.parts in
      append t (Session.Message { ms = now_ms t; message = partial }) >>= fun _ ->
      append t (Session.Note { ms = now_ms t; text = "interrupted" }) >>= fun _ ->
      Lwt.return_unit
    else Lwt.return_unit
  in
  let reason =
    match (error, finish) with
    | Some error, _ -> `Error (Fmt.str "%a" pp_error error)
    | None, `Interrupted -> `Interrupted
    | None, _ -> `Stop
  in
  persist_partial () >>= fun () ->
  Hooks.stop t.deps.hooks ~session:(Session.id t.session) ~reason >>= fun () ->
  (match error with Some error -> emit t (Failed error) | None -> Lwt.return_unit)
  >>= fun () ->
  emit t (Turn_done finish) >|= fun () -> result

let prompt t ?(attachments = []) text =
  if !(t.busy_state) then Lwt.return (Error `Busy)
  else (
    t.busy_state := true;
    let state = { assistant_appended = false; parts = []; last_turn = [] } in
    let switch = Lwt_switch.create () in
    let turn = { switch; interrupt = Idle } in
    let external_cancel = ref None in
    t.turn := Some turn;
    let run () =
      Lwt.finalize
        (fun () ->
          Lwt.catch
            (fun () -> run_turn t switch text text attachments state)
            (function
              | Interrupted User_cancel -> Lwt.return (Ok `Interrupted)
              | Interrupted (External_cancel error) ->
                  external_cancel := Some error;
                  Lwt.return (Ok `Interrupted)
              | Lwt.Canceled ->
                  external_cancel := Some Lwt.Canceled;
                  Lwt.return (Ok `Interrupted)
              | Lwt_switch.Off -> Lwt.return (Ok `Interrupted)
              | exception_ -> Lwt.fail exception_))
        (fun () ->
          t.turn := None;
          Lwt.catch (fun () -> Lwt_switch.turn_off switch) (fun _ -> Lwt.return_unit))
      >>= fun result ->
      Lwt.protected (finalize t state result) >>= fun result ->
      match !external_cancel with
      | Some error -> Lwt.fail error
      | None -> Lwt.return result
    in
    Lwt.finalize run (fun () ->
        t.busy_state := false;
        Lwt.return_unit))

let rec run_child_agent parent ~prompt:instruction =
  let model = !(parent.large_model) in
  let title =
    let prefix =
      if String.length instruction > 40 then String.sub instruction 0 40 else instruction
    in
    "subagent: " ^ prefix
  in
  match
    Lwt_direct.await
      (Session.create parent.deps.store ~clock:parent.deps.clock
         ~random:parent.deps.random ~parent:(Session.id parent.session) ~title
         ~cwd:parent.deps.cwd
         ~model:
           {
             Session.provider = model.Models.provider_id;
             model = model.Models.model.Charamel_fantasy.Model.id;
           }
         ())
  with
  | Error error -> Error (Fmt.str "%a" Session.pp_error error)
  | Ok child_session -> (
      let child_deps = { parent.deps with interactive = false; ask = None } in
      match
        Lwt_direct.await
          (create_internal ~budget:parent.budget ~is_subagent:true child_deps
             ~session:child_session ~large:!(parent.large_model)
             ~small:!(parent.small_model))
      with
      | Error error -> Error (Fmt.str "%a" pp_error error)
      | Ok child_agent -> (
          link_child_cancel parent child_agent;
          match Lwt_direct.await (prompt child_agent instruction) with
          | Error (`Tool message) when String.equal message "subagent budget exhausted" ->
              ignore
                (Lwt_direct.await
                   (Session.append parent.session ~clock:parent.deps.clock
                      (Session.Note { ms = now_ms parent; text = message })));
              Error message
          | Error error -> Error (Fmt.str "%a" pp_error error)
          | Ok _ ->
              let rec last = function
                | [] -> ""
                | {
                    Charamel_fantasy.Message.role = Charamel_fantasy.Message.Assistant;
                    parts;
                  }
                  :: rest ->
                    let text =
                      List.filter_map
                        (function
                          | Charamel_fantasy.Message.Text text -> Some text | _ -> None)
                        parts
                      |> String.concat ""
                    in
                    if text = "" then last rest else text
                | _ :: rest -> last rest
              in
              Ok (last (List.rev (Session.messages child_session)))))

and create_internal ~budget ~is_subagent deps ~session ~large ~small =
  let artifacts =
    Artifact.create ~fs_root:deps.fs_root
      ~dir:(Session.artifacts_dir deps.store ~id:(Session.id session))
  in
  let jobs = Jobs.create ~sw:deps.sw ~artifacts in
  let todos = Todos.create () in
  let read_tracker = Hashtbl.create 32 in
  let run_child_ref = ref (fun ~prompt:_ -> Error "nested subagents are disabled") in
  let ctx_template : Tool.ctx =
    {
      clock = deps.clock;
      fs_root = deps.fs_root;
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
      turn = ref None;
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
  Hooks.session_start deps.hooks ~session:(Session.id session) >|= fun () -> Ok agent

let create deps ~session ~large ~small =
  let budget =
    {
      limit = max 0 deps.config.Config.options.Config.budgets.Config.subagent_requests;
      used = 0;
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
  match !(t.turn) with None -> () | Some turn -> interrupt_turn turn User_cancel

let compact t =
  if !(t.busy_state) then Lwt.return (Error `Busy)
  else
    let switch = Lwt_switch.create () in
    let body () =
      let* small, auth = refresh_model t `Small in
      Compaction.run ~sw:switch ~clock:t.deps.clock ~small ~auth t.session >>= function
      | Error (`Provider message) -> Lwt.return (Error (`Provider message))
      | Error (`Session error) -> Lwt.return (Error (`Session error))
      | Ok summary ->
          emit t (Compacted { summary_chars = String.length summary }) >|= fun () -> Ok ()
    in
    Lwt.finalize body (fun () -> Lwt_switch.turn_off switch)

let set_plan_mode t enabled =
  Permission.set_plan_mode t.deps.permission enabled;
  let text = if enabled then "plan mode on" else "plan mode off" in
  append t (Session.Note { ms = now_ms t; text }) >|= fun _ -> ()

let stats t = (!(t.usage), !(t.cost_usd), !(t.context_tokens))
