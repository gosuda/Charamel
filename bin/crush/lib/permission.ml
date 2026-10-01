open Lwt_direct

type request = {
  session : string;
  tool : string;
  action : string;
  path : string;
  description : string;
  read_only : bool;
}

type decision = Allow_once | Allow_session | Deny
type outcome = Allowed | Denied of string
type asker = request -> decision
type hook = request -> [ `Allow | `Deny of string | `Pass ]
type grant = string * string * string * string

type t = {
  config : Config.permissions;
  yolo : bool;
  cwd : string;
  plans_dir : string;
  asker : asker;
  hook : hook;
  on_decision : request -> outcome -> unit;
  ask_mutex : Lwt_mutex.t;
  mutable plan_mode : bool;
  mutable grants : grant list;
  mutable auto_approved_sessions : string list;
}

let default_asker _ = Deny
let default_hook _ = `Pass
let default_on_decision _ _ = ()

let with_ask_lock t operation =
  await (Lwt_mutex.with_lock t.ask_mutex (fun () -> Lwt.return (operation ())))

let create ~config ~yolo ?(asker = default_asker) ?(hook = default_hook)
    ?(on_decision = default_on_decision) ~cwd ~plans_dir () =
  let cwd = Path.normalize cwd in
  let plans_dir = Path.normalize ~cwd plans_dir in
  {
    config;
    yolo;
    cwd;
    plans_dir;
    asker;
    hook;
    on_decision;
    ask_mutex = Lwt_mutex.create ();
    plan_mode = false;
    grants = [];
    auto_approved_sessions = [];
  }

let matches ~entry request =
  match String.index_opt entry ':' with
  | None -> String.equal entry request.tool
  | Some separator ->
      let tool = String.sub entry 0 separator in
      let action =
        String.sub entry (separator + 1) (String.length entry - separator - 1)
      in
      String.equal tool request.tool && String.equal action request.action

let plan_mode t = t.plan_mode
let set_plan_mode t enabled = t.plan_mode <- enabled
let plans_dir t = t.plans_dir

let normalized_request request =
  let path =
    if Path.is_absolute request.path then Path.normalize request.path else request.path
  in
  { request with path }

let request_key request = (request.session, request.tool, request.action, request.path)

let key_equal (session, tool, action, path)
    (other_session, other_tool, other_action, other_path) =
  String.equal session other_session
  && String.equal tool other_tool
  && String.equal action other_action
  && String.equal path other_path

let grant_session_locked t request =
  let key = request_key request in
  if not (List.exists (key_equal key) t.grants) then t.grants <- key :: t.grants

let grant_session t request = grant_session_locked t (normalized_request request)

let auto_approve_session t ~session =
  if not (List.exists (String.equal session) t.auto_approved_sessions) then
    t.auto_approved_sessions <- session :: t.auto_approved_sessions

let session_granted t request =
  List.exists (String.equal request.session) t.auto_approved_sessions
  || List.exists (fun key -> key_equal key (request_key request)) t.grants

let plan_denial t request =
  if String.equal request.tool "bash" then "plan mode: only read-only commands run"
  else
    "plan mode: writes are limited to " ^ t.plans_dir
    ^ "; run /propose to leave plan mode"

let known_plan_target t request =
  (String.equal request.tool "edit" || String.equal request.tool "write")
  && Path.within ~root:t.plans_dir request.path

let plan_denial_locked t request =
  if t.plan_mode && (not request.read_only) && not (known_plan_target t request) then
    Some (Denied (plan_denial t request))
  else None

let configured_outcome_locked t request =
  match List.find_opt (fun entry -> matches ~entry request) t.config.Config.deny with
  | Some entry -> Some (Denied ("denied by permissions.deny: " ^ entry))
  | None -> (
      match
        List.find_opt (fun entry -> matches ~entry request) t.config.Config.allowed_tools
      with
      | Some _ -> Some Allowed
      | None -> if session_granted t request then Some Allowed else None)

let policy_outcome_locked t request =
  match plan_denial_locked t request with
  | Some outcome -> Some outcome
  | None -> if t.yolo then Some Allowed else configured_outcome_locked t request

let outcome_after_answer_locked t request decision =
  match policy_outcome_locked t request with
  | Some outcome -> outcome
  | None -> (
      match decision with
      | Allow_once -> Allowed
      | Allow_session ->
          grant_session_locked t request;
          Allowed
      | Deny -> Denied "denied by user")

type after_hook = Hook_resolved of outcome | Hook_continue

let outcome_after_hook_locked t request hook_outcome =
  match policy_outcome_locked t request with
  | Some outcome -> Hook_resolved outcome
  | None -> (
      match hook_outcome with
      | `Allow -> Hook_resolved Allowed
      | `Deny message -> Hook_resolved (Denied message)
      | `Pass -> Hook_continue)

let resolve t request =
  let request = normalized_request request in
  let result =
    match policy_outcome_locked t request with
    | Some outcome -> outcome
    | None -> (
        let hook_outcome = t.hook request in
        match outcome_after_hook_locked t request hook_outcome with
        | Hook_resolved outcome -> outcome
        | Hook_continue ->
            if request.read_only && Path.within ~root:t.cwd request.path then Allowed
            else
              let decision = with_ask_lock t (fun () -> t.asker request) in
              outcome_after_answer_locked t request decision)
  in
  t.on_decision request result;
  result

let pp_request ppf request =
  Fmt.pf ppf "{session=%S; tool=%S; action=%S; path=%S; description=%S; read_only=%b}"
    request.session request.tool request.action request.path request.description
    request.read_only
