module Agent = Crush_core.Agent
module Config = Crush_core.Config
module Auth = Crush_core.Auth
module Session = Crush_core.Session
module Permission = Crush_core.Permission
module Hooks = Crush_core.Hooks
module Rules = Crush_core.Rules
module Skills = Crush_core.Skills
module Mcp = Crush_core.Mcp
module Models = Crush_core.Models

let response_body queue fallback =
  match Queue.take_opt queue with Some body -> body | None -> fallback

type fixture = { port : int; queue : string Queue.t; fallback : string }

let reason_phrase = function
  | 200 -> "OK"
  | 401 -> "Unauthorized"
  | 500 -> "Internal Server Error"
  | status -> string_of_int status

let drain_request _flow reader =
  let rec headers content_length =
    match Eio.Buf_read.line reader with
    | "" -> content_length
    | line ->
        let content_length =
          match String.index_opt line ':' with
          | Some index
            when String.equal
                   (String.lowercase_ascii (String.trim (String.sub line 0 index)))
                   "content-length" -> (
              let value = String.sub line (index + 1) (String.length line - index - 1) in
              match int_of_string_opt (String.trim value) with
              | Some value -> value
              | None -> 0)
          | _ -> content_length
        in
        headers content_length
  in
  let length = headers 0 in
  if length > 0 then ignore (Eio.Buf_read.take length reader)

let start_fixture ~sw ~net ?(queue = []) ~fallback () =
  let queue = Queue.of_seq (List.to_seq queue) in
  let socket =
    Eio.Net.listen net ~backlog:16 ~sw (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | _ -> 0 in
  let fixture = { port; queue; fallback } in
  let handle flow _addr =
    let reader = Eio.Buf_read.of_flow ~max_size:65_536 flow in
    drain_request flow reader;
    let body = response_body fixture.queue fixture.fallback in
    let head =
      Fmt.str
        "HTTP/1.1 200 %s\r\ncontent-type: text/event-stream\r\ncontent-length: %d\r\n\r\n"
        (reason_phrase 200) (String.length body)
    in
    Eio.Flow.copy_string head flow;
    Eio.Flow.copy_string body flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error:raise handle
      done;
      `Stop_daemon);
  fixture

let base_url fixture = Fmt.str "http://127.0.0.1:%d" fixture.port

let message_start model_id =
  Fmt.str
    "event: message_start\n\
     data: \
     {\"type\":\"message_start\",\"message\":{\"id\":\"m\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"%s\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":2,\"output_tokens\":0}}}\n\n"
    model_id

let text_response model_id text =
  message_start model_id
  ^ "event: content_block_start\n\
     data: \
     {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
  ^ Fmt.str
      "event: content_block_delta\n\
       data: \
       {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":%S}}\n\n"
      text
  ^ "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  ^ "event: message_delta\n\
     data: \
     {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}\n\n"
  ^ "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"

let tool_response model_id name input =
  message_start model_id
  ^ Fmt.str
      "event: content_block_start\n\
       data: \
       {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"call-1\",\"name\":%S,\"input\":{}}}\n\n"
      name
  ^ Fmt.str
      "event: content_block_delta\n\
       data: \
       {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":%S}}\n\n"
      input
  ^ "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  ^ "event: message_delta\n\
     data: \
     {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":1}}\n\n"
  ^ "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"

let model ~base_url:_ =
  {
    Charm_fantasy.Model.id = "fixture-model";
    name = "Fixture";
    provider = "anthropic";
    context_window = 200_000;
    default_max_tokens = 1024;
    can_reason = false;
    supports_attachments = false;
    cost_in = 0.;
    cost_out = 0.;
    cost_cache_read = 0.;
    cost_cache_write = 0.;
  }

let config ~base_url (model : Charm_fantasy.Model.t) =
  let provider : Config.provider =
    {
      kind = Config.Anthropic;
      base_url = Some base_url;
      api_key = Some "fixture-key";
      headers = [];
      models = [ model ];
    }
  in
  let selected : Config.selected_model =
    {
      provider = "anthropic";
      model = model.Charm_fantasy.Model.id;
      reasoning = None;
      max_tokens = None;
    }
  in
  {
    Config.default with
    providers = [ ("anthropic", provider) ];
    models = { large = Some selected; small = Some selected };
  }

let with_agent ~queue ~fallback f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fixture = start_fixture ~sw ~net:env#net ~queue ~fallback () in
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("crush-agent-" ^ string_of_int (Unix.getpid ()))
  in
  (try Unix.mkdir root 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let old_data = Sys.getenv_opt "XDG_DATA_HOME" in
  Unix.putenv "XDG_DATA_HOME" root;
  Fun.protect
    (fun () ->
      let selected_model = model ~base_url:(base_url fixture) in
      let config = config ~base_url:(base_url fixture) selected_model in
      let auth_path = Filename.concat root "auth.json" in
      let auth =
        match Auth.create ~path:Eio.Path.(env#fs / auth_path) ~clock:env#clock () with
        | Error error -> Alcotest.failf "auth setup failed: %a" Auth.pp_error error
        | Ok auth -> auth
      in
      (match Auth.set auth ~provider:"anthropic" (Auth.Api_key "fixture-key") with
      | Ok () -> ()
      | Error error ->
          Alcotest.failf "auth credential setup failed: %a" Auth.pp_error error);
      let store = Session.store ~fs:env#fs ~cwd:root in
      let session =
        match
          Session.create store ~clock:env#clock
            ~random:(fun n -> String.make n '\000')
            ~title:"fixture" ~cwd:root
            ~model:
              {
                Session.provider = "anthropic";
                model = selected_model.Charm_fantasy.Model.id;
              }
            ()
        with
        | Ok session -> session
        | Error error -> Alcotest.failf "session setup failed: %a" Session.pp_error error
      in
      let permission =
        Permission.create ~config:config.Config.permissions ~yolo:true ~cwd:root
          ~plans_dir:(Filename.concat root ".crush/plans")
          ()
      in
      let hooks =
        Hooks.create ~config:[] ~proc_mgr:env#process_mgr ~clock:env#clock ~cwd:root
      in
      let rules = Rules.load ~fs:env#fs ~cwd:root ~config in
      let skills = Skills.load ~fs:env#fs ~config ~home:root in
      let mcp =
        Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock ~cwd:root
          ~config
      in
      let events = ref [] in
      let deps : Agent.deps =
        {
          sw;
          clock = env#clock;
          fs = env#fs;
          net = env#net;
          proc_mgr = env#process_mgr;
          random = (fun n -> String.make n '\000');
          env = Sys.getenv_opt;
          cwd = root;
          config;
          auth;
          store;
          permission;
          hooks;
          lsp = None;
          mcp;
          skills;
          rules;
          log_path = Filename.concat root "crush.log";
          interactive = false;
          ask = None;
          events = (fun event -> events := event :: !events);
        }
      in
      let large =
        match Models.resolve ~fs:env#fs config ~auth ~env:Sys.getenv_opt ~role:`Large with
        | Ok model -> model
        | Error error ->
            Alcotest.failf "large model setup failed: %a" Models.pp_error error
      in
      let small =
        match Models.resolve ~fs:env#fs config ~auth ~env:Sys.getenv_opt ~role:`Small with
        | Ok model -> model
        | Error error ->
            Alcotest.failf "small model setup failed: %a" Models.pp_error error
      in
      let agent =
        match Agent.create deps ~session ~large ~small with
        | Ok agent -> agent
        | Error error -> Alcotest.failf "agent setup failed: %a" Agent.pp_error error
      in
      f agent session events)
    ~finally:(fun () ->
      match old_data with
      | Some value -> Unix.putenv "XDG_DATA_HOME" value
      | None -> Unix.putenv "XDG_DATA_HOME" "")

let assistant_count events =
  List.fold_left
    (fun count event ->
      match event with
      | Session.Message
          {
            message = { Charm_fantasy.Message.role = Charm_fantasy.Message.Assistant; _ };
            _;
          } ->
          count + 1
      | _ -> count)
    0 events

let tool_call_and_stop () =
  let model_id = "fixture-model" in
  let read_input = "{\"path\":\"note.txt\"}" in
  with_agent
    ~queue:[ tool_response model_id "read" read_input; text_response model_id "done" ]
    ~fallback:(text_response model_id "done")
    (fun agent session _events ->
      let path = Filename.concat (Filename.get_temp_dir_name ()) "note.txt" in
      ignore path;
      match Agent.prompt agent "read note.txt" with
      | Error error -> Alcotest.failf "agent prompt failed: %a" Agent.pp_error error
      | Ok finish ->
          Alcotest.(check bool) "finished" true (finish = `Stop);
          let events = Array.to_list (Session.events session) in
          Alcotest.(check bool)
            "tool call persisted" true
            (List.exists (function Session.Tool_call _ -> true | _ -> false) events);
          Alcotest.(check bool)
            "tool result persisted" true
            (List.exists (function Session.Tool_result _ -> true | _ -> false) events);
          Alcotest.(check int) "assistant messages" 2 (assistant_count events))

let loop_guard () =
  let model_id = "fixture-model" in
  let repeated =
    tool_response model_id "bash" "{\"command\":\"printf same\",\"description\":\"same\"}"
  in
  with_agent ~queue:[] ~fallback:repeated (fun agent _session _events ->
      match Agent.prompt agent "repeat" with
      | Error error -> Alcotest.failf "loop prompt failed: %a" Agent.pp_error error
      | Ok finish ->
          Alcotest.(check bool)
            "loop detected at fifth triple" true (finish = `Loop_detected))

let cases =
  [
    Alcotest.test_case "tool call and stop use HTTP fixture" `Quick tool_call_and_stop;
    Alcotest.test_case "last ten triples stop repeated calls" `Quick loop_guard;
  ]
