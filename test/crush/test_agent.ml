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
open Lwt_direct

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
    Charamel_fantasy.Model.id = "fixture-model";
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

let config ~base_url (model : Charamel_fantasy.Model.t) =
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
      model = model.Charamel_fantasy.Model.id;
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
  Test_tools_test_support.with_scratch (fun root ->
      let old_data = Sys.getenv_opt "XDG_DATA_HOME" in
      Unix.putenv "XDG_DATA_HOME" root;
      let pending = Queue.of_seq (List.to_seq queue) in
      let body () =
        match Queue.take_opt pending with Some response -> response | None -> fallback
      in
      Fun.protect
        (fun () ->
          Test_tools_test_support.with_http_server ~content_type:"text/event-stream" ~body
            (fun port ->
              let sw = Lwt_switch.create () in
              let clock = Charamel_os.Time.lwt in
              let base_url = Fmt.str "http://127.0.0.1:%d" port in
              let selected_model = model ~base_url in
              let config = config ~base_url selected_model in
              let auth_path = Filename.concat root "auth.json" in
              let auth =
                match await (Auth.create ~path:auth_path ~clock ()) with
                | Error error ->
                    Alcotest.failf "auth setup failed: %a" Auth.pp_error error
                | Ok auth -> auth
              in
              (match
                 await (Auth.set auth ~provider:"anthropic" (Auth.Api_key "fixture-key"))
               with
              | Ok () -> ()
              | Error error ->
                  Alcotest.failf "auth credential setup failed: %a" Auth.pp_error error);
              let store = Session.store ~fs_root:root ~cwd:root in
              let session =
                match
                  await
                  @@ Session.create store ~clock
                       ~random:(fun n -> String.make n '\000')
                       ~title:"fixture" ~cwd:root
                       ~model:
                         {
                           Session.provider = "anthropic";
                           model = selected_model.Charamel_fantasy.Model.id;
                         }
                       ()
                with
                | Ok session -> session
                | Error error ->
                    Alcotest.failf "session setup failed: %a" Session.pp_error error
              in
              let permission =
                Permission.create ~config:config.Config.permissions ~yolo:true ~cwd:root
                  ~plans_dir:(Filename.concat root ".crush/plans")
                  ()
              in
              let hooks = Hooks.create ~config:[] ~cwd:root in
              let rules = await (Rules.load ~fs_root:root ~cwd:root ~config) in
              let skills = await (Skills.load ~fs_root:root ~config ~home:root) in
              let mcp = await (Mcp.create ~cwd:root ~config) in
              let events = ref [] in
              let deps : Agent.deps =
                {
                  sw;
                  clock;
                  fs_root = root;
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
                  events =
                    (fun event ->
                      events := event :: !events;
                      Lwt.return_unit);
                }
              in
              let large =
                match
                  await
                  @@ Models.resolve ~fs_root:root config ~auth ~env:Sys.getenv_opt
                       ~role:`Large
                with
                | Ok model -> model
                | Error error ->
                    Alcotest.failf "large model setup failed: %a" Models.pp_error error
              in
              let small =
                match
                  await
                  @@ Models.resolve ~fs_root:root config ~auth ~env:Sys.getenv_opt
                       ~role:`Small
                with
                | Ok model -> model
                | Error error ->
                    Alcotest.failf "small model setup failed: %a" Models.pp_error error
              in
              let agent =
                match await (Agent.create deps ~session ~large ~small) with
                | Ok agent -> agent
                | Error error ->
                    Alcotest.failf "agent setup failed: %a" Agent.pp_error error
              in
              f agent session events))
        ~finally:(fun () ->
          match old_data with
          | Some value -> Unix.putenv "XDG_DATA_HOME" value
          | None -> Unix.putenv "XDG_DATA_HOME" ""))

let assistant_count events =
  List.fold_left
    (fun count event ->
      match event with
      | Session.Message
          {
            message =
              { Charamel_fantasy.Message.role = Charamel_fantasy.Message.Assistant; _ };
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
      match await (Agent.prompt agent "read note.txt") with
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
    tool_response model_id "bash" "{\"command\":\"echo same\",\"description\":\"same\"}"
  in
  with_agent ~queue:[] ~fallback:repeated (fun agent _session _events ->
      match await (Agent.prompt agent "repeat") with
      | Error error -> Alcotest.failf "loop prompt failed: %a" Agent.pp_error error
      | Ok finish ->
          Alcotest.(check bool)
            "loop detected at fifth triple" true (finish = `Loop_detected))

let cases =
  [
    Test_tools_test_support.case "tool call and stop use HTTP fixture" `Quick
      tool_call_and_stop;
    Test_tools_test_support.case "last ten triples stop repeated calls" `Quick loop_guard;
  ]
