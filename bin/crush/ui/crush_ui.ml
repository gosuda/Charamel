module Core = Crush_core
module Agent = Core.Agent
module Tool = Core.Tool
module Permission = Core.Permission
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module Key = Charamel_tea.Key
module View = Charamel_tea.View
module Style = Charamel_lipgloss.Style
module Layout = Charamel_lipgloss.Layout
module Color = Charamel_ansi.Color
module Textarea = Charamel_bubbles.Textarea
module Viewport = Charamel_bubbles.Viewport
module Bubble_list = Charamel_bubbles.List
module Huh = Charamel_huh
open Result.Syntax
open Lwt.Infix

type session = { id : string; title : string; model : string; created_ms : int }

type model = {
  id : string;
  provider : string;
  context_window : int;
  max_tokens : int;
  can_reason : bool;
  supports_attachments : bool;
}

module Bridge = struct
  type 'a queue = {
    capacity : int;
    values : 'a Queue.t;
    mutex : Lwt_mutex.t;
    not_empty : unit Lwt_condition.t;
    not_full : unit Lwt_condition.t;
    mutable closed : bool;
  }

  type answer_result = (Tool.answer list, [ `Aborted | `Not_interactive ]) result

  type ask_request = {
    questions : Tool.question list;
    resolver : answer_result Lwt.u;
    mutable answered : bool;
  }

  type permission_request = {
    request : Permission.request;
    resolver : Permission.decision Lwt.u;
    mutable answered : bool;
  }

  type t = {
    events : Agent.event queue;
    questions : ask_request queue;
    permissions : permission_request queue;
    mutable pending_questions : ask_request list;
    mutable pending_permissions : permission_request list;
  }

  let queue capacity =
    if capacity < 1 then invalid_arg "Crush_ui.Bridge.create: capacity must be positive";
    {
      capacity;
      values = Queue.create ();
      mutex = Lwt_mutex.create ();
      not_empty = Lwt_condition.create ();
      not_full = Lwt_condition.create ();
      closed = false;
    }

  let create ?(capacity = 256) () =
    {
      events = queue capacity;
      questions = queue capacity;
      permissions = queue capacity;
      pending_questions = [];
      pending_permissions = [];
    }

  (* [closed] and the pending-request lists are mutated only by plain, non-yielding
     statements (no [Lwt.bind] between a check and its mutation), so under Lwt's
     cooperative scheduler no other task can interleave mid-mutation; unlike [mutex]
     below, which guards genuinely yielding critical sections, these need no lock, the
     same reasoning [Permission] already applies to drop its [policy_mutex]. *)
  let close_queue q =
    q.closed <- true;
    Lwt_condition.broadcast q.not_empty ();
    Lwt_condition.broadcast q.not_full ()

  let resolve_question (request : ask_request) (answer : answer_result) =
    if not request.answered then begin
      request.answered <- true;
      Lwt.wakeup_later request.resolver answer
    end

  let resolve_permission (request : permission_request) (decision : Permission.decision) =
    if not request.answered then begin
      request.answered <- true;
      Lwt.wakeup_later request.resolver decision
    end

  let close t =
    close_queue t.events;
    close_queue t.questions;
    close_queue t.permissions;
    List.iter
      (fun request -> resolve_question request (Error `Aborted))
      t.pending_questions;
    List.iter
      (fun request -> resolve_permission request Permission.Deny)
      t.pending_permissions;
    t.pending_questions <- [];
    t.pending_permissions <- []

  let push_queue q value =
    Lwt_mutex.lock q.mutex >>= fun () ->
    let rec wait_for_space () =
      if (not q.closed) && Queue.length q.values >= q.capacity then
        Lwt_condition.wait ~mutex:q.mutex q.not_full >>= wait_for_space
      else Lwt.return_unit
    in
    wait_for_space () >>= fun () ->
    let accepted =
      if q.closed then false
      else begin
        Queue.add value q.values;
        Lwt_condition.broadcast q.not_empty ();
        true
      end
    in
    Lwt_mutex.unlock q.mutex;
    Lwt.return accepted

  let take_queue q =
    Lwt_mutex.lock q.mutex >>= fun () ->
    let rec wait_for_value () =
      if (not q.closed) && Queue.is_empty q.values then
        Lwt_condition.wait ~mutex:q.mutex q.not_empty >>= wait_for_value
      else Lwt.return_unit
    in
    wait_for_value () >>= fun () ->
    let result =
      if Queue.is_empty q.values then None
      else begin
        let value = Queue.take q.values in
        Lwt_condition.broadcast q.not_full ();
        Some value
      end
    in
    Lwt_mutex.unlock q.mutex;
    Lwt.return result

  let push t event = push_queue t.events event >>= fun _accepted -> Lwt.return_unit
  let take_event t = take_queue t.events
  let queue_closed q = q.closed

  let remove_question t request =
    t.pending_questions <-
      List.filter (fun value -> not (value == request)) t.pending_questions

  let remove_permission t request =
    t.pending_permissions <-
      List.filter (fun value -> not (value == request)) t.pending_permissions

  let ask t questions =
    if queue_closed t.questions then Lwt.return (Error `Aborted)
    else begin
      let promise, resolver = Lwt.wait () in
      let request = { questions; resolver; answered = false } in
      let accepted =
        if queue_closed t.questions then false
        else begin
          t.pending_questions <- request :: t.pending_questions;
          true
        end
      in
      if not accepted then Lwt.return (Error `Aborted)
      else
        push_queue t.questions request >>= function
        | false ->
            remove_question t request;
            Lwt.return (Error `Aborted)
        | true ->
            promise >>= fun result ->
            remove_question t request;
            Lwt.return result
    end

  let ask_permission t request =
    if queue_closed t.permissions then Lwt.return Permission.Deny
    else begin
      let promise, resolver = Lwt.wait () in
      let item = { request; resolver; answered = false } in
      let accepted =
        if queue_closed t.permissions then false
        else begin
          t.pending_permissions <- item :: t.pending_permissions;
          true
        end
      in
      if not accepted then Lwt.return Permission.Deny
      else
        push_queue t.permissions item >>= function
        | false ->
            remove_permission t item;
            Lwt.return Permission.Deny
        | true ->
            promise >>= fun result ->
            remove_permission t item;
            Lwt.return result
    end

  let take_question t = take_queue t.questions
  let take_permission t = take_queue t.permissions
  let questions (request : ask_request) = request.questions

  let answer t request value =
    resolve_question request value;
    remove_question t request

  let permission (request : permission_request) = request.request

  let answer_permission t request value =
    resolve_permission request value;
    remove_permission t request
end

type history_item = { role : string; text : string }

type backend = {
  agent : Agent.t ref;
  events : Bridge.t;
  clock : Charamel_os.Time.clock;
  form_env : Huh.Form.Env.t;
  project : string;
  session_id : unit -> string;
  new_session : unit -> (Agent.t, string) result;
  sessions : unit -> session list;
  resume_session : string -> (Agent.t, string) result;
  history : unit -> history_item list;
  models : unit -> model list;
  select_model : string -> (unit, string) result;
  login : string -> string -> (unit, string) result;
  logout : string -> (unit, string) result;
  load_attachment : string -> (string * string * string option, string) result;
  yolo : unit -> bool;
  approve_session : unit -> (unit, string) result;
  set_plan_mode : bool -> (unit, string) result;
  plan_mode : unit -> bool;
  lsp_status : unit -> string;
  mcp_status : unit -> string;
  dark : unit -> bool;
  quit : unit -> unit;
}

type chat_kind = User | Assistant | Reasoning | Tool_start | Tool_result | Status

type chat_item = {
  kind : chat_kind;
  text : string;
  id : string option;
  name : string option;
  input : string option;
  elapsed_ms : int option;
  mutable expanded : bool;
}

type dialog_field =
  | Input_field of string * string Huh.Key.t
  | Select_field of string * string Huh.Key.t
  | Multi_field of string * string list Huh.Key.t

type dialog_kind =
  | Session_dialog of session list
  | Model_dialog of model list
  | Questions_dialog of Bridge.ask_request
  | Permission_dialog of Bridge.permission_request
  | Proposal_dialog
  | OAuth_dialog of string

type dialog = { kind : dialog_kind; form : Huh.Form.t; fields : dialog_field list }

type dialog_request =
  | Question_dialog_request of Bridge.ask_request
  | Permission_dialog_request of Bridge.permission_request

type ui_model = {
  backend : backend;
  mutable viewport : Viewport.t;
  mutable editor : Textarea.t;
  mutable session_rows : session list;
  mutable sessions : session Bubble_list.t;
  mutable model_rows : model list;
  mutable chat : chat_item list;
  mutable status : string;
  mutable dialog : dialog option;
  dialog_queue : dialog_request Queue.t;
  mutable title : string;
  mutable rows : int;
  mutable cols : int;
  mutable dark : bool;
  mutable closed_events : bool;
  mutable closed_questions : bool;
  mutable closed_permissions : bool;
  mutable pending_prompt : string option;
  mutable last_ctrl_c : bool;
}

type ui_msg =
  | Agent_event of Agent.event
  | Agent_stream_closed
  | Question_request of Bridge.ask_request
  | Question_stream_closed
  | Permission_request of Bridge.permission_request
  | Permission_stream_closed
  | Editor_msg of Textarea.msg
  | Dialog_msg of Huh.Form.msg
  | Submit_prompt
  | Prompt_result of (Agent.finish, Agent.error) result
  | Command_result of string
  | New_session_result of (Agent.t, string) result
  | Resume_session_result of string * (Agent.t, string) result
  | Sessions_refreshed of session list
  | Models_refreshed of model list
  | Noop
  | Tick
  | Resize of int * int
  | Key of Key.t

let style_for (kind : chat_kind) =
  match kind with
  | User -> Style.bold true Style.empty
  | Assistant -> Style.empty
  | Reasoning -> Style.faint true Style.empty
  | Tool_start -> Style.foreground (Color.Indexed 244) Style.empty
  | Tool_result -> Style.foreground (Color.Indexed 245) Style.empty
  | Status -> Style.foreground (Color.Indexed 244) Style.empty

let render_item (item : chat_item) =
  let name = Option.value ~default:"tool" item.name in
  let prefix =
    match item.kind with
    | User -> "You: "
    | Assistant -> ""
    | Reasoning -> "  reasoning: "
    | Tool_start -> Fmt.str "  tool %s: " name
    | Tool_result -> Fmt.str "  result %s: " name
    | Status -> "  "
  in
  let body =
    match (item.kind, item.expanded) with
    | (Tool_start | Tool_result), true ->
        let details =
          List.filter_map Fun.id
            [
              Option.map (fun input -> "input: " ^ input) item.input;
              Option.map (fun elapsed -> Fmt.str "elapsed: %d ms" elapsed) item.elapsed_ms;
              (if item.text = "" then None else Some item.text);
            ]
        in
        let lines =
          match item.id with Some id -> Fmt.str "[%s]" id :: details | None -> details
        in
        String.concat "\n" lines
    | Tool_start, false -> Option.value ~default:"(collapsed)" item.input
    | Tool_result, false ->
        let line =
          match String.index_opt item.text '\n' with
          | Some index -> String.sub item.text 0 index
          | None -> item.text
        in
        if String.length line > 160 then String.sub line 0 160 ^ "…" else line
    | _ -> item.text
  in
  Style.render (style_for item.kind) (prefix ^ body)

let chat_text (m : ui_model) =
  match m.chat with
  | [] -> "No messages yet. Type a prompt and press Enter."
  | items -> String.concat "\n" (List.map render_item items)

let refresh_viewport m =
  m.viewport <-
    m.viewport
    |> Viewport.set_width (max 1 (m.cols - 28))
    |> Viewport.set_height (max 3 (m.rows - 7))
    |> Viewport.set_content (chat_text m)

let status_text m =
  let usage, cost, context = Agent.stats !(m.backend.agent) in
  let plan = if m.backend.plan_mode () then " PLAN" else "" in
  let yolo = if m.backend.yolo () then " YOLO" else "" in
  let busy = if Agent.busy !(m.backend.agent) then " working" else " ready" in
  Fmt.str "%s%s%s | %a | $%.4f | ctx %d | LSP %s | MCP %s" busy plan yolo
    Charamel_fantasy.Usage.pp usage cost context (m.backend.lsp_status ())
    (m.backend.mcp_status ())

let sessions_widget (backend : backend) (rows : session list) =
  let delegate =
    Bubble_list.default_delegate ~is_dark:(backend.dark ())
      ~title:(fun (value : session) -> value.title)
      ~description:(fun (value : session) -> value.id)
      ()
  in
  Bubble_list.v ~title:"Sessions" ~width:28 ~height:7 ~is_dark:(backend.dark ()) ~delegate
    ~filter_value:(fun (value : session) -> value.title ^ " " ^ value.id)
    rows

let sidebar m =
  let model_line =
    match m.model_rows with
    | [] -> "model: unavailable"
    | model :: _ -> Fmt.str "model: %s/%s" model.provider model.id
  in
  let theme = if m.dark then "dark" else "light" in
  String.concat "\n"
    [
      "Crush";
      "project: " ^ m.backend.project;
      model_line;
      "theme: " ^ theme;
      "";
      Bubble_list.view m.sessions;
      "";
      "ctrl+p sessions  ctrl+o models";
      "ctrl+g plan  ctrl+c quit";
    ]

let view (m : ui_model) =
  let body = Layout.join_horizontal [ sidebar m; Viewport.view m.viewport ] in
  let dialog =
    match m.dialog with
    | None -> ""
    | Some value ->
        let heading =
          match value.kind with
          | Permission_dialog request ->
              Fmt.str "\n\nPermission: %a" Permission.pp_request
                (Bridge.permission request)
          | _ -> "\n\nDialog"
        in
        heading ^ "\n"
        ^ Style.render (Style.bold true Style.empty) (Huh.Form.view value.form)
  in
  let footer = status_text m ^ if m.status = "" then "" else " | " ^ m.status in
  View.v ~alt_screen:true ~mouse:View.Mouse_click ~title:m.title
    (body ^ "\n\n" ^ Textarea.view m.editor ^ "\n" ^ footer ^ dialog)

let make_field (question : Tool.question) =
  let title =
    if question.Tool.header = "" then question.Tool.text else question.Tool.header
  in
  let key_name = if question.Tool.header = "" then "answer" else question.Tool.header in
  if question.Tool.free_text || question.Tool.options = [] then begin
    let key = Huh.Key.v key_name in
    let field = Huh.Field.input ~title:(Huh.Dyn.Const title) key in
    (Input_field (key_name, key), Huh.Group.v [ field ])
  end
  else if question.Tool.multi then begin
    let key = Huh.Key.v key_name in
    let options =
      List.map
        (fun (value, label) -> Huh.Field.option_ ~key:label value)
        question.Tool.options
    in
    let field =
      Huh.Field.multi_select ~title:(Huh.Dyn.Const title) ~options:(Huh.Dyn.Const options)
        key
    in
    (Multi_field (key_name, key), Huh.Group.v [ field ])
  end
  else begin
    let key = Huh.Key.v key_name in
    let options =
      List.map
        (fun (value, label) -> Huh.Field.option_ ~key:label value)
        question.Tool.options
    in
    let field =
      Huh.Field.select ~title:(Huh.Dyn.Const title) ~options:(Huh.Dyn.Const options) key
    in
    (Select_field (key_name, key), Huh.Group.v [ field ])
  end

let make_dialog backend (kind : dialog_kind) questions =
  let fields, groups =
    List.fold_left
      (fun (fields, groups) question ->
        let field, group = make_field question in
        (fields @ [ field ], groups @ [ group ]))
      ([], []) questions
  in
  let form = Huh.Form.v ~width:70 ~height:(max 6 (List.length questions * 4)) groups in
  let form, init_cmd = Huh.Form.init backend.form_env form in
  ({ kind; form; fields }, Cmd.map (fun value -> Dialog_msg value) init_cmd)

let session_dialog (backend : backend) (rows : session list) =
  let options =
    List.map (fun (value : session) -> (value.id, value.title ^ " — " ^ value.model)) rows
  in
  make_dialog backend (Session_dialog rows)
    [
      {
        Tool.header = "session";
        text = "Resume session";
        options;
        multi = false;
        free_text = false;
      };
    ]

let model_dialog (backend : backend) (rows : model list) =
  let options =
    List.map (fun (value : model) -> (value.id, value.provider ^ " — " ^ value.id)) rows
  in
  make_dialog backend (Model_dialog rows)
    [
      {
        Tool.header = "model";
        text = "Select model";
        options;
        multi = false;
        free_text = false;
      };
    ]

let proposal_dialog backend =
  make_dialog backend Proposal_dialog
    [
      {
        Tool.header = "approve";
        text = "Approve plan and leave plan mode?";
        options = [ ("yes", "Approve and execute"); ("no", "Keep planning") ];
        multi = false;
        free_text = false;
      };
    ]

let permission_dialog backend request =
  let decoded = Bridge.permission request in
  make_dialog backend (Permission_dialog request)
    [
      {
        Tool.header = "decision";
        text =
          Fmt.str "%a\n\n%s" Permission.pp_request decoded decoded.Permission.description;
        options =
          [ ("once", "Allow once"); ("session", "Allow for session"); ("deny", "Deny") ];
        multi = false;
        free_text = false;
      };
    ]

let oauth_dialog backend provider =
  make_dialog backend (OAuth_dialog provider)
    [
      {
        Tool.header = "code";
        text = "Paste OAuth code, code#state, or redirect URL";
        options = [];
        multi = false;
        free_text = true;
      };
    ]

let start_dialog m = function
  | Question_dialog_request request ->
      let dialog, command =
        make_dialog m.backend (Questions_dialog request) request.Bridge.questions
      in
      m.dialog <- Some dialog;
      command
  | Permission_dialog_request request ->
      let dialog, command = permission_dialog m.backend request in
      m.dialog <- Some dialog;
      command

let activate_next_dialog m =
  match m.dialog with
  | Some _ -> Cmd.none
  | None ->
      if Queue.is_empty m.dialog_queue then Cmd.none
      else start_dialog m (Queue.take m.dialog_queue)

let enqueue_dialog m request =
  Queue.add request m.dialog_queue;
  activate_next_dialog m

let finish_dialog m command =
  m.dialog <- None;
  Cmd.batch [ command; activate_next_dialog m ]

let field_answers (dialog : dialog) =
  let results = Huh.Form.results dialog.form in
  let answer = function
    | Input_field (header, key) ->
        let text = Option.value ~default:"" (Huh.Results.get key results) in
        { Tool.header; selected = []; text = Some text }
    | Select_field (header, key) ->
        let value = Option.value ~default:"" (Huh.Results.get key results) in
        { Tool.header; selected = (if value = "" then [] else [ value ]); text = None }
    | Multi_field (header, key) ->
        let selected = Option.value ~default:[] (Huh.Results.get key results) in
        { Tool.header; selected; text = None }
  in
  List.map answer dialog.fields

let append (m : ui_model) (item : chat_item) =
  m.chat <- m.chat @ [ item ];
  refresh_viewport m

let append_agent_event (m : ui_model) = function
  | Agent.Text_delta text ->
      (match List.rev m.chat with
      | item :: rest when item.kind = Assistant ->
          m.chat <- List.rev ({ item with text = item.text ^ text } :: rest)
      | _ ->
          append m
            {
              kind = Assistant;
              text;
              id = None;
              name = None;
              input = None;
              elapsed_ms = None;
              expanded = true;
            });
      refresh_viewport m
  | Agent.Reasoning_delta text ->
      (match List.rev m.chat with
      | item :: rest when item.kind = Reasoning ->
          m.chat <- List.rev ({ item with text = item.text ^ text } :: rest)
      | _ ->
          append m
            {
              kind = Reasoning;
              text;
              id = None;
              name = None;
              input = None;
              elapsed_ms = None;
              expanded = true;
            });
      refresh_viewport m
  | Agent.Tool_started { id; name; input } ->
      let input =
        match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json input with
        | Ok value -> value
        | Error _ -> "<invalid-json>"
      in
      append m
        {
          kind = Tool_start;
          text = "";
          id = Some id;
          name = Some name;
          input = Some input;
          elapsed_ms = None;
          expanded = false;
        }
  | Agent.Tool_finished { id; name; output; elapsed_ms } ->
      let text =
        match Tool.to_result output with
        | `Text value -> value
        | `Error value -> "error: " ^ value
      in
      append m
        {
          kind = Tool_result;
          text;
          id = Some id;
          name = Some name;
          input = None;
          elapsed_ms = Some elapsed_ms;
          expanded = false;
        }
  | Agent.Permission_asked request ->
      append m
        {
          kind = Status;
          text = Fmt.str "permission requested: %a" Permission.pp_request request;
          id = None;
          name = None;
          input = None;
          elapsed_ms = None;
          expanded = true;
        }
  | Agent.Permission_resolved (request, outcome) ->
      let decision =
        match outcome with
        | Permission.Allowed -> "allowed"
        | Permission.Denied reason -> "denied: " ^ reason
      in
      append m
        {
          kind = Status;
          text = Fmt.str "permission %s %s" decision request.Permission.tool;
          id = None;
          name = None;
          input = None;
          elapsed_ms = None;
          expanded = true;
        }
  | Agent.Usage { usage; _ } ->
      append m
        {
          kind = Status;
          text = Fmt.str "usage %a" Charamel_fantasy.Usage.pp usage;
          id = None;
          name = None;
          input = None;
          elapsed_ms = None;
          expanded = true;
        }
  | Agent.Compacted { summary_chars } ->
      append m
        {
          kind = Status;
          text = Fmt.str "compacted (%d chars)" summary_chars;
          id = None;
          name = None;
          input = None;
          elapsed_ms = None;
          expanded = true;
        }
  | Agent.Title title -> m.title <- title
  | Agent.Advisor_note { severity; guidance } ->
      append m
        {
          kind = Status;
          text = Fmt.str "advisor %s: %s" severity guidance;
          id = None;
          name = None;
          input = None;
          elapsed_ms = None;
          expanded = true;
        }
  | Agent.Turn_done finish ->
      let text =
        match finish with
        | `Stop -> "done"
        | `Length -> "max tokens"
        | `Content_filter -> "content filtered"
        | `Interrupted -> "interrupted"
        | `Loop_detected -> "loop detected"
        | `Budget -> "budget exhausted"
        | `Halted reason -> "halted: " ^ reason
      in
      append m
        {
          kind = Status;
          text;
          id = None;
          name = None;
          input = None;
          elapsed_ms = None;
          expanded = true;
        }
  | Agent.Failed error ->
      append m
        {
          kind = Status;
          text = Fmt.str "error: %a" Agent.pp_error error;
          id = None;
          name = None;
          input = None;
          elapsed_ms = None;
          expanded = true;
        }

(* The pollers await the bridge queues directly. Wrapping the take in
   Lwt_direct.spawn would park the message on a queue drained only by
   Lwt_main's iteration hooks, which never run under the synchronous
   Charamel_tea.Test pump; Cmd.await runs the promise on the dispatching
   task, so an already-queued item delivers immediately and a later push
   wakes the parked take through the queue's condition. At most one take
   is armed per queue at a time: each re-arm happens inside update after
   the previous take delivered. *)
let next_event backend =
  Bridge.take_event backend.events >|= function
  | Some event -> Agent_event event
  | None -> Agent_stream_closed

let next_question backend =
  Bridge.take_question backend.events >|= function
  | Some request -> Question_request request
  | None -> Question_stream_closed

let next_permission backend =
  Bridge.take_permission backend.events >|= function
  | Some request -> Permission_request request
  | None -> Permission_stream_closed

let poll_events m = if m.closed_events then Cmd.none else Cmd.await (next_event m.backend)

let poll_questions m =
  if m.closed_questions then Cmd.none else Cmd.await (next_question m.backend)

let poll_permissions m =
  if m.closed_permissions then Cmd.none else Cmd.await (next_permission m.backend)

let prompt_attachments backend text =
  let tokens = String.split_on_char ' ' text in
  let rec collect prompt attachments = function
    | [] -> Ok (String.concat " " (List.rev prompt), List.rev attachments)
    | token :: rest when String.length token > 1 && token.[0] = '@' ->
        let path = String.sub token 1 (String.length token - 1) in
        let* mime, raw, name = backend.load_attachment path in
        let name = Option.value ~default:(Filename.basename path) name in
        collect prompt ((mime, Base64.encode_string raw, name) :: attachments) rest
    | token :: rest -> collect (token :: prompt) attachments rest
  in
  collect [] [] tokens

let prompt_command m text =
  if Agent.busy !(m.backend.agent) then begin
    m.status <- "agent busy; prompt kept in editor";
    Cmd.none
  end
  else
    Cmd.await
      (Lwt_direct.spawn (fun () ->
           match prompt_attachments m.backend text with
           | Error error -> Command_result error
           | Ok (prompt, attachments) ->
               if String.trim prompt = "" then Noop
               else begin
                 append m
                   {
                     kind = User;
                     text = prompt;
                     id = None;
                     name = None;
                     input = None;
                     elapsed_ms = None;
                     expanded = true;
                   };
                 m.pending_prompt <- Some text;
                 let result =
                   Lwt_direct.await (Agent.prompt !(m.backend.agent) ~attachments prompt)
                 in
                 Prompt_result result
               end))

let command m text =
  let words =
    String.split_on_char ' ' (String.trim text) |> List.filter (fun value -> value <> "")
  in
  match words with
  | [ "/quit" ] | [ "/q" ] ->
      m.backend.quit ();
      Cmd.quit
  | [ "/help" ] ->
      m.status <-
        "/model /sessions /new /compact /plan /propose /yolo /login /logout /quit";
      Cmd.none
  | [ "/sessions" ] ->
      let dialog, cmd = session_dialog m.backend m.session_rows in
      m.dialog <- Some dialog;
      cmd
  | [ "/models" ] | [ "/model" ] ->
      let dialog, cmd = model_dialog m.backend m.model_rows in
      m.dialog <- Some dialog;
      cmd
  | [ "/new" ] ->
      Cmd.await
        (Lwt_direct.spawn (fun () -> New_session_result (m.backend.new_session ())))
  | [ "/compact" ] ->
      Cmd.await
        (Lwt_direct.spawn (fun () ->
             match Lwt_direct.await (Agent.compact !(m.backend.agent)) with
             | Ok () -> Command_result "compacted"
             | Error error -> Command_result (Fmt.str "%a" Agent.pp_error error)))
  | [ "/plan" ] ->
      Cmd.await
        (Lwt_direct.spawn (fun () ->
             match m.backend.set_plan_mode true with
             | Ok () -> Command_result "plan mode enabled"
             | Error error -> Command_result error))
  | [ "/propose" ] ->
      if m.backend.plan_mode () then (
        let dialog, cmd = proposal_dialog m.backend in
        m.dialog <- Some dialog;
        cmd)
      else (
        m.status <- "not in plan mode";
        Cmd.none)
  | [ "/yolo" ] ->
      Cmd.await
        (Lwt_direct.spawn (fun () ->
             match m.backend.approve_session () with
             | Ok () -> Command_result "session permissions approved"
             | Error error -> Command_result error))
  | [ "/login"; provider ] ->
      let dialog, cmd = oauth_dialog m.backend provider in
      m.dialog <- Some dialog;
      cmd
  | [ "/logout"; provider ] ->
      Cmd.await
        (Lwt_direct.spawn (fun () ->
             match m.backend.logout provider with
             | Ok () -> Command_result ("logged out " ^ provider)
             | Error error -> Command_result error))
  | _ ->
      m.status <- "unknown command; /help for commands";
      Cmd.none

let dialog_key key = Option.map (fun value -> Dialog_msg value) (Huh.Form.key key)

let selected_value answers header =
  Option.bind
    (List.find_opt (fun answer -> answer.Tool.header = header) answers)
    (fun (answer : Tool.answer) ->
      match answer.Tool.selected with [] -> None | first :: _ -> Some first)

let history_chat backend =
  let kind (role : string) : chat_kind =
    match String.lowercase_ascii role with
    | "user" -> User
    | "assistant" -> Assistant
    | "reasoning" -> Reasoning
    | "tool" -> Tool_result
    | _ -> Status
  in
  List.map
    (fun (value : history_item) : chat_item ->
      {
        kind = kind value.role;
        text = value.text;
        id = None;
        name = None;
        input = None;
        elapsed_ms = None;
        expanded = true;
      })
    (backend.history ())

let complete_dialog (m : ui_model) (dialog : dialog) =
  let values = field_answers dialog in
  match dialog.kind with
  | Session_dialog _ ->
      let command =
        match selected_value values "session" with
        | Some id ->
            Cmd.await
              (Lwt_direct.spawn (fun () ->
                   Resume_session_result (id, m.backend.resume_session id)))
        | None -> Cmd.none
      in
      finish_dialog m command
  | Model_dialog _ ->
      let command =
        match selected_value values "model" with
        | Some id ->
            Cmd.await
              (Lwt_direct.spawn (fun () ->
                   match m.backend.select_model id with
                   | Ok () -> Command_result ("model " ^ id)
                   | Error error -> Command_result error))
        | None -> Cmd.none
      in
      finish_dialog m command
  | Proposal_dialog ->
      let command =
        if selected_value values "approve" = Some "yes" then
          Cmd.await
            (Lwt_direct.spawn (fun () ->
                 match m.backend.set_plan_mode false with
                 | Ok () -> Command_result "plan approved; execution enabled"
                 | Error error -> Command_result error))
        else begin
          m.status <- "kept planning";
          Cmd.none
        end
      in
      finish_dialog m command
  | OAuth_dialog provider ->
      let code =
        match List.find_opt (fun answer -> answer.Tool.header = "code") values with
        | Some { text = Some value; _ } -> value
        | _ -> ""
      in
      let command =
        Cmd.await
          (Lwt_direct.spawn (fun () ->
               match m.backend.login provider code with
               | Ok () -> Command_result ("logged in " ^ provider)
               | Error error -> Command_result error))
      in
      finish_dialog m command
  | Questions_dialog request ->
      Bridge.answer m.backend.events request (Ok values);
      finish_dialog m (poll_questions m)
  | Permission_dialog request ->
      let decision =
        match selected_value values "decision" with
        | Some "once" -> Permission.Allow_once
        | Some "session" -> Permission.Allow_session
        | _ -> Permission.Deny
      in
      Bridge.answer_permission m.backend.events request decision;
      finish_dialog m (poll_permissions m)

let update_dialog (m : ui_model) (dialog : dialog) message =
  let form, command = Huh.Form.update message dialog.form in
  let dialog = { dialog with form } in
  let command = Cmd.map (fun value -> Dialog_msg value) command in
  match Huh.Form.state form with
  | `Completed _ -> Cmd.batch [ command; complete_dialog m dialog ]
  | `Aborted ->
      let continuation =
        match dialog.kind with
        | Questions_dialog request ->
            Bridge.answer m.backend.events request (Error `Aborted);
            finish_dialog m (poll_questions m)
        | Permission_dialog request ->
            Bridge.answer_permission m.backend.events request Permission.Deny;
            finish_dialog m (poll_permissions m)
        | _ -> finish_dialog m Cmd.none
      in
      Cmd.batch [ command; continuation ]
  | `Normal ->
      m.dialog <- Some dialog;
      command

let rec update (m : ui_model) = function
  | Agent_event event ->
      append_agent_event m event;
      poll_events m
  | Agent_stream_closed ->
      m.closed_events <- true;
      Cmd.none
  | Question_request request -> enqueue_dialog m (Question_dialog_request request)
  | Question_stream_closed ->
      m.closed_questions <- true;
      Cmd.none
  | Permission_stream_closed ->
      m.closed_permissions <- true;
      Cmd.none
  | Permission_request request -> enqueue_dialog m (Permission_dialog_request request)
  | Editor_msg message ->
      let editor, command = Textarea.update message m.editor in
      m.editor <- editor;
      Cmd.map (fun value -> Editor_msg value) command
  | Dialog_msg message -> (
      match m.dialog with
      | None -> Cmd.none
      | Some dialog -> update_dialog m dialog message)
  | Submit_prompt ->
      let text = Textarea.value m.editor in
      if String.length text > 0 && text.[0] = '/' then begin
        m.editor <- Textarea.set_value "" m.editor;
        command m text
      end
      else prompt_command m text
  | Prompt_result result ->
      (match (result, m.pending_prompt) with
      | Ok _, Some submitted when String.equal submitted (Textarea.value m.editor) ->
          m.editor <- Textarea.set_value "" m.editor
      | _ -> ());
      m.pending_prompt <- None;
      (match result with
      | Ok _ -> m.status <- ""
      | Error error -> m.status <- Fmt.str "%a" Agent.pp_error error);
      Cmd.none
  | Command_result text ->
      m.status <- text;
      Cmd.none
  | New_session_result result ->
      (match result with
      | Ok agent ->
          m.backend.agent := agent;
          m.chat <- [];
          refresh_viewport m;
          m.status <- "new conversation"
      | Error error -> m.status <- error);
      Cmd.none
  | Resume_session_result (id, result) ->
      (match result with
      | Ok agent ->
          m.backend.agent := agent;
          m.chat <- history_chat m.backend;
          refresh_viewport m;
          m.status <- "resumed " ^ id
      | Error error -> m.status <- error);
      Cmd.none
  | Sessions_refreshed rows ->
      m.session_rows <- rows;
      m.sessions <- sessions_widget m.backend rows;
      Cmd.none
  | Models_refreshed rows ->
      m.model_rows <- rows;
      Cmd.none
  | Noop -> Cmd.none
  | Tick ->
      let dark = m.backend.dark () in
      if dark <> m.dark then begin
        m.dark <- dark;
        m.editor <- Textarea.set_styles (Textarea.default_styles ~is_dark:dark) m.editor
      end;
      refresh_viewport m;
      (* Same substitution as the pollers: the refresh functions are pure and
         synchronous, so delivering their results as an already-resolved promise
         keeps the command shape while removing the Lwt_main-only spawn queue. *)
      Cmd.batch
        [
          Cmd.await (Lwt.return (Sessions_refreshed (m.backend.sessions ())));
          Cmd.await (Lwt.return (Models_refreshed (m.backend.models ())));
        ]
  | Resize (rows, cols) ->
      m.rows <- max 1 rows;
      m.cols <- max 1 cols;
      m.editor <- Textarea.set_width (max 20 (m.cols - 28)) m.editor;
      m.editor <- Textarea.set_height (max 2 (min 8 (m.rows / 4))) m.editor;
      refresh_viewport m;
      Cmd.none
  | Key key -> (
      let name = Key.to_string key in
      match m.dialog with
      | Some dialog when name = "escape" -> (
          match dialog.kind with
          | Questions_dialog request ->
              Bridge.answer m.backend.events request (Error `Aborted);
              finish_dialog m (poll_questions m)
          | Permission_dialog request ->
              Bridge.answer_permission m.backend.events request Permission.Deny;
              finish_dialog m (poll_permissions m)
          | _ -> finish_dialog m Cmd.none)
      | Some _ -> (
          match dialog_key key with None -> Cmd.none | Some msg -> update m msg)
      | None when name = "enter" -> update m Submit_prompt
      | None when name = "ctrl+c" ->
          if m.last_ctrl_c then (
            m.backend.quit ();
            Cmd.quit)
          else (
            m.last_ctrl_c <- true;
            m.status <- "press ctrl+c again to quit";
            Cmd.none)
      | None when name = "ctrl+p" ->
          let dialog, cmd = session_dialog m.backend m.session_rows in
          m.dialog <- Some dialog;
          cmd
      | None when name = "ctrl+o" ->
          let dialog, cmd = model_dialog m.backend m.model_rows in
          m.dialog <- Some dialog;
          cmd
      | None when name = "ctrl+g" ->
          if m.backend.plan_mode () then (
            let dialog, cmd = proposal_dialog m.backend in
            m.dialog <- Some dialog;
            cmd)
          else
            Cmd.await
              (Lwt_direct.spawn (fun () ->
                   match m.backend.set_plan_mode true with
                   | Ok () -> Command_result "plan mode enabled"
                   | Error error -> Command_result error))
      | None when name = "ctrl+x" ->
          (match List.rev m.chat with
          | item :: rest when item.kind = Tool_start || item.kind = Tool_result ->
              item.expanded <- not item.expanded;
              m.chat <- List.rev (item :: rest);
              refresh_viewport m
          | _ -> ());
          Cmd.none
      | None -> (
          match Textarea.key m.editor key with
          | Some message -> update m (Editor_msg message)
          | None -> (
              match Viewport.key m.viewport key with
              | Some message ->
                  let viewport, command = Viewport.update message m.viewport in
                  m.viewport <- viewport;
                  Cmd.map (fun _ -> Tick) command
              | None -> Cmd.none)))

let init (backend : backend) =
  let rows, cols = (24, 80) in
  let editor =
    Textarea.v ~prompt:"› " ~placeholder:"Ask Crush..." ~width:(cols - 28) ~height:3
      ~is_dark:(backend.dark ()) ~value:"" ()
  in
  let editor, editor_cmd = Textarea.focus editor in
  let model =
    {
      backend;
      viewport = Viewport.v ~width:(cols - 28) ~height:(rows - 7) ~soft_wrap:true ();
      editor;
      session_rows = [];
      sessions = sessions_widget backend [];
      model_rows = [];
      chat = history_chat backend;
      status = "";
      dialog = None;
      dialog_queue = Queue.create ();
      title = "Crush";
      rows;
      cols;
      dark = backend.dark ();
      closed_events = false;
      closed_questions = false;
      closed_permissions = false;
      pending_prompt = None;
      last_ctrl_c = false;
    }
  in
  refresh_viewport model;
  ( model,
    Cmd.batch
      [
        poll_events model;
        poll_questions model;
        poll_permissions model;
        Cmd.map (fun value -> Editor_msg value) editor_cmd;
        Cmd.await (Lwt.return (Sessions_refreshed (backend.sessions ())));
        Cmd.await (Lwt.return (Models_refreshed (backend.models ())));
        Cmd.msg Tick;
      ] )

let subscriptions m =
  Sub.batch
    [
      Sub.key (fun key -> Key key);
      Sub.resize (fun ~rows ~cols -> Resize (rows, cols));
      Sub.map (fun value -> Editor_msg value) (Textarea.subscriptions m.editor);
      Sub.map (fun _ -> Tick) (Bubble_list.subscriptions m.sessions);
    ]

let app backend : (ui_model, ui_msg) Charamel_tea.app =
  {
    init = (fun () -> init backend);
    update = (fun message model -> (model, update model message));
    view;
    subscriptions;
  }

let run backend =
  Lwt.finalize
    (fun () -> Charamel_tea.run ~clock:backend.clock (app backend))
    (fun () ->
      Agent.cancel !(backend.agent);
      Bridge.close backend.events;
      Lwt.return_unit)

let run_with backend ~events ~size =
  let events = `Wait 0. :: events in
  Fun.protect
    ~finally:(fun () -> Bridge.close backend.events)
    (fun () -> Charamel_tea.Test.run (app backend) ~events ~size)
