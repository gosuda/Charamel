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
  policy_mutex : Eio.Mutex.t;
  ask_mutex : Eio.Mutex.t;
  mutable plan_mode : bool;
  mutable grants : grant list;
  mutable auto_approved_sessions : string list;
}

let has_leading_slash path = String.length path > 0 && Char.equal path.[0] '/'

let canonical_components path =
  let rec visit acc = function
    | [] -> List.rev acc
    | part :: rest ->
        if String.equal part "" || String.equal part "." then visit acc rest
        else if String.equal part ".." then
          match acc with [] -> visit [] rest | _ :: acc -> visit acc rest
        else visit (part :: acc) rest
  in
  visit [] (String.split_on_char '/' path)

let canonical path =
  if String.equal path "" then ""
  else
    let components = canonical_components path in
    let body = String.concat "/" components in
    if has_leading_slash path then if String.equal body "" then "/" else "/" ^ body
    else body

let append_to_base ~base path =
  if String.equal path "" || has_leading_slash path then path
  else if String.equal base "" then path
  else base ^ "/" ^ path

let absolute_components path =
  if has_leading_slash path then Some (canonical_components path) else None

let component_prefix prefix value =
  let rec go prefix value =
    match (prefix, value) with
    | [], _ -> true
    | _ :: _, [] -> false
    | p :: prefix, v :: value -> String.equal p v && go prefix value
  in
  go prefix value

let within ~root path =
  match (absolute_components root, absolute_components path) with
  | Some root, Some path -> component_prefix root path
  | None, _ | _, None -> false

let default_asker _ = Deny
let default_hook _ = `Pass
let default_on_decision _ _ = ()

let with_policy_lock t operation =
  Eio.Mutex.lock t.policy_mutex;
  Fun.protect operation ~finally:(fun () -> Eio.Mutex.unlock t.policy_mutex)

let with_ask_lock t operation =
  Eio.Mutex.lock t.ask_mutex;
  Fun.protect operation ~finally:(fun () -> Eio.Mutex.unlock t.ask_mutex)

let create ~config ~yolo ?(asker = default_asker) ?(hook = default_hook)
    ?(on_decision = default_on_decision) ~cwd ~plans_dir () =
  let cwd = canonical cwd in
  let plans_dir = canonical (append_to_base ~base:cwd plans_dir) in
  {
    config;
    yolo;
    cwd;
    plans_dir;
    asker;
    hook;
    on_decision;
    policy_mutex = Eio.Mutex.create ();
    ask_mutex = Eio.Mutex.create ();
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

let plan_mode t = with_policy_lock t (fun () -> t.plan_mode)
let set_plan_mode t enabled = with_policy_lock t (fun () -> t.plan_mode <- enabled)
let plans_dir t = t.plans_dir

let normalized_request request =
  let path =
    if has_leading_slash request.path then canonical request.path else request.path
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

let grant_session t request =
  let request = normalized_request request in
  with_policy_lock t (fun () -> grant_session_locked t request)

let auto_approve_session t ~session =
  with_policy_lock t (fun () ->
      if not (List.exists (String.equal session) t.auto_approved_sessions) then
        t.auto_approved_sessions <- session :: t.auto_approved_sessions)

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
  && (not (String.equal request.path ""))
  && within ~root:t.plans_dir request.path

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
    match with_policy_lock t (fun () -> policy_outcome_locked t request) with
    | Some outcome -> outcome
    | None -> (
        let hook_outcome = t.hook request in
        match
          with_policy_lock t (fun () -> outcome_after_hook_locked t request hook_outcome)
        with
        | Hook_resolved outcome -> outcome
        | Hook_continue ->
            if
              request.read_only
              && (not (String.equal request.path ""))
              && within ~root:t.cwd request.path
            then Allowed
            else
              let decision = with_ask_lock t (fun () -> t.asker request) in
              with_policy_lock t (fun () ->
                  outcome_after_answer_locked t request decision))
  in
  t.on_decision request result;
  result

let pp_request ppf request =
  Fmt.pf ppf "{session=%S; tool=%S; action=%S; path=%S; description=%S; read_only=%b}"
    request.session request.tool request.action request.path request.description
    request.read_only
