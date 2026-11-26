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

let with_agent ?asker ?(hooks_config : Config.hook list = [])
    ?(clock = Charamel_os.Time.lwt) ~queue ~fallback f =
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
                Permission.create ~config:config.Config.permissions
                  ~yolo:(Option.is_none asker) ?asker ~cwd:root
                  ~plans_dir:(Filename.concat root ".crush/plans")
                  ()
              in
              let hooks = Hooks.create ~config:hooks_config ~cwd:root in
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
              f agent session events root))
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
    (fun agent session _events _root ->
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
  with_agent ~queue:[] ~fallback:repeated (fun agent _session _events _root ->
      match await (Agent.prompt agent "repeat") with
      | Error error -> Alcotest.failf "loop prompt failed: %a" Agent.pp_error error
      | Ok finish ->
          Alcotest.(check bool)
            "loop detected at fifth triple" true (finish = `Loop_detected))

let two_tool_response model_id name1 input1 name2 input2 =
  message_start model_id
  ^ Fmt.str
      "event: content_block_start\n\
       data: \
       {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"call-1\",\"name\":%S,\"input\":{}}}\n\n"
      name1
  ^ Fmt.str
      "event: content_block_delta\n\
       data: \
       {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":%S}}\n\n"
      input1
  ^ "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
  ^ Fmt.str
      "event: content_block_start\n\
       data: \
       {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"call-2\",\"name\":%S,\"input\":{}}}\n\n"
      name2
  ^ Fmt.str
      "event: content_block_delta\n\
       data: \
       {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":%S}}\n\n"
      input2
  ^ "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":1}\n\n"
  ^ "event: message_delta\n\
     data: \
     {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":1}}\n\n"
  ^ "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"

(* The allowed call signals that its body finished before the denial can stop the
   turn. Its post hook stays blocked until cancellation. *)
let body_outcome_survives_stop_turn () =
  if Sys.win32 then
    (* The hook scripts below are POSIX shell; the output cap is the same code path on
       Windows. *)
    Alcotest.skip ()
  else
    let model_id = "fixture-model" in
    let pre =
      "if grep -q denied.txt; then read ready < .body-ready; echo denied-by-hook 1>&2; \
       exit 2; else exit 0; fi"
    in
    let post =
      "if grep -q allowed.txt; then printf '%s\\n' \"$$\" > .post-pid; printf 'ready\\n' \
       > .body-ready; read release < .post-release; fi; exit 0"
    in
    let hooks_config : Config.hook list =
      [
        { event = Config.Pre_tool; matcher = None; command = pre; timeout_s = 30 };
        { event = Config.Post_tool; matcher = None; command = post; timeout_s = 30 };
      ]
    in
    with_agent ~hooks_config
      ~queue:
        [
          two_tool_response model_id "read" "{\"path\":\"denied.txt\"}" "read"
            "{\"path\":\"allowed.txt\"}";
          text_response model_id "done";
        ]
      ~fallback:(text_response model_id "done")
      (fun agent _session events root ->
        Unix.mkfifo (Filename.concat root ".body-ready") 0o600;
        Unix.mkfifo (Filename.concat root ".post-release") 0o600;
        Test_tools_test_support.write_file
          (Filename.concat root "denied.txt")
          "denied-body";
        Test_tools_test_support.write_file
          (Filename.concat root "allowed.txt")
          "allowed-body";
        (match await (Agent.prompt agent "read both files") with
        | Error error -> Alcotest.failf "agent prompt failed: %a" Agent.pp_error error
        | Ok finish -> Alcotest.(check bool) "finished" true (finish = `Stop));
        let post_pid =
          Test_tools_test_support.load_file (Filename.concat root ".post-pid")
          |> String.trim |> int_of_string
        in
        (match Unix.kill post_pid 0 with
        | () ->
            Unix.kill (0 - post_pid) Sys.sigkill;
            Alcotest.fail "cancelling the post hook left its process alive"
        | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ());
        let finished =
          List.filter_map
            (function
              | Agent.Tool_finished { id; output; _ } -> Some (id, output) | _ -> None)
            !events
        in
        let content id =
          match List.assoc_opt id finished with
          | Some output -> output.Crush_core.Tool.content
          | None -> Alcotest.failf "no Tool_finished event for %s" id
        in
        let denied = content "call-1" in
        let allowed = content "call-2" in
        Alcotest.(check bool)
          "denied call stops the turn" true
          (Test_support.contains ~needle:"denied-by-hook" ~haystack:denied);
        Alcotest.(check bool)
          "recorded body is kept" true
          (Test_support.contains ~needle:"allowed-body" ~haystack:allowed);
        Alcotest.(check bool)
          "kept body is not marked skipped" false
          (Test_support.contains ~needle:"skipped" ~haystack:allowed))

let post_hook_time_is_not_tool_time () =
  if Sys.win32 then Alcotest.skip ()
  else
    let virtual_clock, advance = Charamel_os.Time.create_virtual () in
    let clock = Charamel_os.Time.of_virtual virtual_clock in
    let hooks_config : Config.hook list =
      [
        {
          event = Config.Post_tool;
          matcher = None;
          command =
            "cat >/dev/null; printf 'ready\\n' > .timing-ready; read release < \
             .timing-release";
          timeout_s = 30;
        };
      ]
    in
    with_agent ~hooks_config ~clock
      ~queue:
        [
          tool_response "fixture-model" "read" "{\"path\":\"allowed.txt\"}";
          text_response "fixture-model" "done";
        ]
      ~fallback:(text_response "fixture-model" "done")
      (fun agent _session events root ->
        let ready_path = Filename.concat root ".timing-ready" in
        let release_path = Filename.concat root ".timing-release" in
        Unix.mkfifo ready_path 0o600;
        Unix.mkfifo release_path 0o600;
        let open_fifo path =
          Unix.openfile path [ Unix.O_RDWR; Unix.O_NONBLOCK ] 0
          |> Lwt_unix.of_unix_file_descr
        in
        let ready = Lwt_io.of_fd ~mode:Lwt_io.input (open_fifo ready_path) in
        let release = Lwt_io.of_fd ~mode:Lwt_io.output (open_fifo release_path) in
        Fun.protect
          ~finally:(fun () ->
            await (Lwt_io.close ready);
            await (Lwt_io.close release))
          (fun () ->
            Test_tools_test_support.write_file
              (Filename.concat root "allowed.txt")
              "allowed-body";
            let pending = Agent.prompt agent "read the file" in
            ignore (await (Lwt_unix.with_timeout 5. (fun () -> Lwt_io.read_line ready)));
            advance 10.;
            await (Lwt_io.write_line release "resume");
            await (Lwt_io.flush release);
            (match await pending with
            | Error error -> Alcotest.failf "agent prompt failed: %a" Agent.pp_error error
            | Ok _ -> ());
            let timings =
              List.filter_map
                (function
                  | Agent.Tool_finished { elapsed_ms; _ } -> Some elapsed_ms | _ -> None)
                !events
            in
            Alcotest.(check (list int)) "post-hook time excluded" [ 0 ] timings))

let stopped_turn_leaves_pending_approval () =
  if Sys.win32 then Alcotest.skip ()
  else
    let approval, answer = Lwt.wait () in
    let asked, announce = Lwt.wait () in
    let asker _ =
      if Lwt.is_sleeping asked then Lwt.wakeup_later announce ();
      await approval
    in
    let hooks_config : Config.hook list =
      [
        {
          event = Config.Pre_tool;
          matcher = None;
          command =
            "if grep -q denied.txt; then read ready < .approval-open; echo denied >&2; \
             exit 2; fi";
          timeout_s = 30;
        };
      ]
    in
    with_agent ~asker ~hooks_config
      ~queue:
        [
          two_tool_response "fixture-model" "read" "{\"path\":\"denied.txt\"}" "read"
            "{\"path\":\"../pending-approval.txt\"}";
        ]
      ~fallback:(text_response "fixture-model" "done")
      (fun agent _session _events root ->
        let ready_path = Filename.concat root ".approval-open" in
        Unix.mkfifo ready_path 0o600;
        let fd =
          Unix.openfile ready_path [ Unix.O_RDWR; Unix.O_NONBLOCK ] 0
          |> Lwt_unix.of_unix_file_descr
        in
        let ready = Lwt_io.of_fd ~mode:Lwt_io.output fd in
        let pending = Agent.prompt agent "read both files" in
        Fun.protect
          ~finally:(fun () ->
            if Lwt.is_sleeping approval then Lwt.wakeup_later answer Permission.Deny;
            ignore (await (Lwt_unix.with_timeout 5. (fun () -> Lwt.protected pending)));
            await (Lwt_io.close ready))
          (fun () ->
            await (Lwt_unix.with_timeout 5. (fun () -> Lwt.protected asked));
            await (Lwt_io.write_line ready "deny");
            await (Lwt_io.flush ready);
            (match await (Lwt_unix.with_timeout 5. (fun () -> Lwt.protected pending)) with
            | Ok `Stop -> ()
            | Ok _ -> Alcotest.fail "denied call did not stop the turn"
            | Error error -> Alcotest.failf "agent prompt failed: %a" Agent.pp_error error);
            Alcotest.(check bool)
              "approval remains unanswered" true (Lwt.is_sleeping approval);
            Alcotest.(check bool) "stopped agent is idle" false (Agent.busy agent)))

let explicit_cancel_joins_hook_cleanup () =
  if Sys.win32 then Alcotest.skip ()
  else
    let hooks_config : Config.hook list =
      [
        {
          event = Config.Post_tool;
          matcher = None;
          command =
            "cat >/dev/null; printf '%s\\n' \"$$\" > .cancel-pid; printf 'ready\\n' > \
             .cancel-ready; read release < .cancel-release";
          timeout_s = 30;
        };
      ]
    in
    with_agent ~hooks_config
      ~queue:[ tool_response "fixture-model" "read" "{\"path\":\"allowed.txt\"}" ]
      ~fallback:(text_response "fixture-model" "done")
      (fun agent _session _events root ->
        let ready_path = Filename.concat root ".cancel-ready" in
        Unix.mkfifo ready_path 0o600;
        Unix.mkfifo (Filename.concat root ".cancel-release") 0o600;
        let fd =
          Unix.openfile ready_path [ Unix.O_RDWR; Unix.O_NONBLOCK ] 0
          |> Lwt_unix.of_unix_file_descr
        in
        let ready = Lwt_io.of_fd ~mode:Lwt_io.input fd in
        Test_tools_test_support.write_file (Filename.concat root "allowed.txt") "ready";
        let pending = Agent.prompt agent "read the file" in
        Fun.protect
          ~finally:(fun () ->
            Agent.cancel agent;
            await (Lwt_io.close ready))
          (fun () ->
            ignore (await (Lwt_unix.with_timeout 5. (fun () -> Lwt_io.read_line ready)));
            let pid =
              Test_tools_test_support.load_file (Filename.concat root ".cancel-pid")
              |> String.trim |> int_of_string
            in
            Agent.cancel agent;
            (match await (Lwt_unix.with_timeout 5. (fun () -> Lwt.protected pending)) with
            | Ok `Interrupted -> ()
            | Ok _ -> Alcotest.fail "cancel did not interrupt the turn"
            | Error error -> Alcotest.failf "agent prompt failed: %a" Agent.pp_error error);
            (match Unix.kill pid 0 with
            | () ->
                Unix.kill (0 - pid) Sys.sigkill;
                Alcotest.fail "cancel completed before hook process cleanup"
            | exception Unix.Unix_error (Unix.ESRCH, _, _) -> ());
            Alcotest.(check bool) "cancelled agent is idle" false (Agent.busy agent)))

let cases =
  [
    Test_tools_test_support.case "tool call and stop use HTTP fixture" `Quick
      tool_call_and_stop;
    Test_tools_test_support.case "last ten triples stop repeated calls" `Quick loop_guard;
    Test_tools_test_support.case "finished body survives an earlier stop" `Quick
      body_outcome_survives_stop_turn;
    Test_tools_test_support.case "post-hook time is not tool time" `Quick
      post_hook_time_is_not_tool_time;
    Test_tools_test_support.case "stopped turn does not await sibling approval" `Quick
      stopped_turn_leaves_pending_approval;
    Test_tools_test_support.case "explicit cancel joins hook cleanup" `Quick
      explicit_cancel_joins_hook_cleanup;
  ]
