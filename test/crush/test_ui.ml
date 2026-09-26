module Ui = Crush_ui
module Agent = Crush_core.Agent
module Permission = Crush_core.Permission
module Config = Crush_core.Config
module Auth = Crush_core.Auth
module Session = Crush_core.Session
module Hooks = Crush_core.Hooks
module Rules = Crush_core.Rules
module Skills = Crush_core.Skills
module Mcp = Crush_core.Mcp
module Models = Crush_core.Models
open Lwt_direct

let clock = Charamel_os.Time.lwt

let fifo_is_lossless () =
  let bridge = Ui.Bridge.create ~capacity:1 () in
  let first = Agent.Text_delta "one" in
  let second = Agent.Reasoning_delta "two" in
  let pusher =
    Lwt.bind (Ui.Bridge.push bridge first) (fun () -> Ui.Bridge.push bridge second)
  in
  let got_first = await (Ui.Bridge.take_event bridge) in
  let got_second = await (Ui.Bridge.take_event bridge) in
  await pusher;
  Alcotest.(check bool)
    "first event preserved" true
    (match got_first with Some (Agent.Text_delta "one") -> true | _ -> false);
  Alcotest.(check bool)
    "second event preserved" true
    (match got_second with Some (Agent.Reasoning_delta "two") -> true | _ -> false);
  Ui.Bridge.close bridge

let close_wakes_question () =
  let bridge = Ui.Bridge.create () in
  let request =
    {
      Crush_core.Tool.header = "question";
      text = "continue?";
      options = [];
      multi = false;
      free_text = true;
    }
  in
  let asker = Ui.Bridge.ask bridge [ request ] in
  Lwt_direct.yield ();
  Ui.Bridge.close bridge;
  match await asker with
  | Error `Aborted -> ()
  | _ -> Alcotest.fail "close did not abort question"

let permission_round_trip () =
  let bridge = Ui.Bridge.create () in
  let request =
    {
      Permission.session = "s";
      tool = "edit";
      action = "edit";
      path = "/tmp/x";
      description = "PUT 1.=1";
      read_only = false;
    }
  in
  let asker = Ui.Bridge.ask_permission bridge request in
  let pending = await (Ui.Bridge.take_permission bridge) in
  (match pending with
  | None -> Alcotest.fail "permission request was not delivered"
  | Some pending -> Ui.Bridge.answer_permission bridge pending Permission.Allow_once);
  Alcotest.(check bool) "allow once delivered" true (await asker = Permission.Allow_once);
  Ui.Bridge.close bridge

let make_model () =
  {
    Charamel_fantasy.Model.id = "ui-fixture";
    name = "UI fixture";
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

let with_ui_backend f =
  Test_tools_test_support.with_scratch (fun root ->
      let previous_data_home = Sys.getenv_opt "XDG_DATA_HOME" in
      Unix.putenv "XDG_DATA_HOME" root;
      Fun.protect
        (fun () ->
          await
          @@ Lwt_switch.with_switch (fun sw ->
              Lwt_direct.spawn (fun () ->
                  let model = make_model () in
                  let config =
                    let provider : Config.provider =
                      {
                        kind = Config.Anthropic;
                        base_url = Some "http://127.0.0.1:1";
                        api_key = Some "fixture";
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
                  in
                  let auth_path = Filename.concat root "auth.json" in
                  let auth =
                    match await (Auth.create ~path:auth_path ~clock ()) with
                    | Error error -> Alcotest.failf "auth setup: %a" Auth.pp_error error
                    | Ok resource -> (
                        match
                          await
                          @@ Auth.set resource ~provider:"anthropic"
                               (Auth.Api_key "fixture")
                        with
                        | Ok () -> resource
                        | Error error ->
                            Alcotest.failf "auth credential setup: %a" Auth.pp_error error
                        )
                  in
                  let store = Session.store ~fs_root:root ~cwd:root in
                  let session =
                    match
                      await
                      @@ Session.create store ~clock
                           ~random:(fun n -> String.make n '\000')
                           ~title:"UI fixture" ~cwd:root
                           ~model:
                             {
                               Session.provider = "anthropic";
                               model = model.Charamel_fantasy.Model.id;
                             }
                           ()
                    with
                    | Ok value -> value
                    | Error error ->
                        Alcotest.failf "session setup: %a" Session.pp_error error
                  in
                  let permission =
                    Permission.create ~config:config.Config.permissions ~yolo:true
                      ~cwd:root
                      ~plans_dir:(Filename.concat root ".crush/plans")
                      ()
                  in
                  let hooks = Hooks.create ~config:[] ~cwd:root in
                  let rules = await (Rules.load ~fs_root:root ~cwd:root ~config) in
                  let skills = await (Skills.load ~fs_root:root ~config ~home:root) in
                  let mcp = await (Mcp.create ~cwd:root ~config) in
                  let bridge = Ui.Bridge.create () in
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
                      interactive = true;
                      ask = Some (fun questions -> await (Ui.Bridge.ask bridge questions));
                      events = Ui.Bridge.push bridge;
                    }
                  in
                  let resolved role =
                    match
                      await
                      @@ Models.resolve ~fs_root:root config ~auth ~env:Sys.getenv_opt
                           ~role
                    with
                    | Ok value -> value
                    | Error error ->
                        Alcotest.failf "model setup: %a" Models.pp_error error
                  in
                  let agent =
                    match
                      await
                      @@ Agent.create deps ~session ~large:(resolved `Large)
                           ~small:(resolved `Small)
                    with
                    | Ok value -> value
                    | Error error -> Alcotest.failf "agent setup: %a" Agent.pp_error error
                  in
                  let plan = ref false in
                  let backend : Ui.backend =
                    {
                      agent = ref agent;
                      events = bridge;
                      clock;
                      form_env =
                        Charamel_huh.Form.Env.v ~fs_root:root ~temp_dir:root
                          ~editor:(Some [ "true" ]) ~clock;
                      project = root;
                      session_id = (fun () -> Session.id session);
                      new_session = (fun () -> Ok agent);
                      sessions = (fun () -> []);
                      resume_session = (fun _ -> Ok agent);
                      history = (fun () -> []);
                      models =
                        (fun () ->
                          [
                            {
                              Ui.id = model.Charamel_fantasy.Model.id;
                              provider = model.Charamel_fantasy.Model.provider;
                              context_window = model.Charamel_fantasy.Model.context_window;
                              max_tokens = model.Charamel_fantasy.Model.default_max_tokens;
                              can_reason = model.Charamel_fantasy.Model.can_reason;
                              supports_attachments =
                                model.Charamel_fantasy.Model.supports_attachments;
                            };
                          ]);
                      select_model = (fun _ -> Ok ());
                      login = (fun _ _ -> Ok ());
                      logout = (fun _ -> Ok ());
                      load_attachment = (fun _ -> Error "no attachment");
                      yolo = (fun () -> false);
                      approve_session = (fun () -> Ok ());
                      set_plan_mode =
                        (fun value ->
                          plan := value;
                          (* The policy flip is synchronous inside [set_plan_mode]; only
                             the session note is a promise. [Ui.run_with] callbacks must
                             not suspend the step loop. *)
                          Lwt.async (fun () -> Agent.set_plan_mode agent value);
                          Ok ());
                      plan_mode = (fun () -> !plan);
                      lsp_status = (fun () -> "off");
                      mcp_status = (fun () -> "off");
                      dark = (fun () -> true);
                      quit = (fun () -> ());
                    }
                  in
                  Fun.protect
                    (fun () -> f backend bridge)
                    ~finally:(fun () -> Ui.Bridge.close bridge))))
        ~finally:(fun () ->
          match previous_data_home with
          | Some value -> Unix.putenv "XDG_DATA_HOME" value
          | None -> Unix.putenv "XDG_DATA_HOME" ""))

let ui_key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message

let serialized_dialogs () =
  with_ui_backend (fun backend bridge ->
      let question =
        {
          Crush_core.Tool.header = "question";
          text = "continue?";
          options = [];
          multi = false;
          free_text = true;
        }
      in
      let permission =
        {
          Permission.session = "s";
          tool = "edit";
          action = "edit";
          path = "/tmp/x";
          description = "PUT 1.=1";
          read_only = false;
        }
      in
      let question_asker = Ui.Bridge.ask bridge [ question ] in
      let permission_asker = Ui.Bridge.ask_permission bridge permission in
      Lwt_direct.yield ();
      ignore
        (Ui.run_with backend
           ~events:
             [
               `Wait 0.;
               `Key (ui_key "enter");
               `Wait 0.;
               `Key (ui_key "enter");
               `Wait 0.;
               `Key (ui_key "ctrl+c");
               `Wait 0.;
               `Key (ui_key "ctrl+c");
             ]
           ~size:(24, 80));
      (match await question_asker with
      | Ok [ (answer : Crush_core.Tool.answer) ] ->
          Alcotest.(check (option string))
            "question answer" (Some "") answer.Crush_core.Tool.text
      | Ok _ -> Alcotest.fail "question dialog returned the wrong answer"
      | Error `Aborted -> Alcotest.fail "question dialog was overwritten or aborted"
      | Error `Not_interactive -> Alcotest.fail "question dialog was not interactive");
      Alcotest.(check bool)
        "permission dialog is resolved after question" true
        (await permission_asker = Permission.Allow_once))

let scripted_runtime_event () =
  with_ui_backend (fun backend bridge ->
      let calls = ref 0 in
      let emitted = ref false in
      let backend =
        {
          backend with
          sessions =
            (fun () ->
              incr calls;
              if !calls = 2 && not !emitted then begin
                emitted := true;
                (* [Ui.run_with] drives the reactor synchronously; never await inside
                   its callbacks. The bridge has headroom, so the push completes. *)
                Lwt.async (fun () ->
                    Ui.Bridge.push bridge (Agent.Text_delta "runtime-produced reply"))
              end;
              []);
        }
      in
      let _model, frame =
        Ui.run_with backend
          ~events:
            [
              `Wait 0.; `Wait 0.; `Key (ui_key "ctrl+c"); `Wait 0.; `Key (ui_key "ctrl+c");
            ]
          ~size:(24, 80)
      in
      Alcotest.(check bool)
        "runtime event reaches chat frame" true
        (Test_support.contains ~needle:"runtime-produced reply" ~haystack:frame))

let scripted_ui_stream () =
  with_ui_backend (fun backend bridge ->
      await (Ui.Bridge.push bridge (Agent.Text_delta "streamed reply"));
      await (Ui.Bridge.push bridge (Agent.Turn_done `Stop));
      let ctrl_c = ui_key "ctrl+c" in
      let _model, frame =
        Ui.run_with backend ~events:[ `Key ctrl_c; `Key ctrl_c ] ~size:(24, 80)
      in
      Alcotest.(check bool)
        "streamed text reaches chat frame" true
        (Test_support.contains ~needle:"streamed reply" ~haystack:frame))

let scripted_counter () =
  let model, _frame =
    Charamel_tea.Test.run
      {
        init = (fun () -> (0, Charamel_tea.Cmd.none));
        update =
          (fun message value ->
            if message = `Tick then (value + 1, Charamel_tea.Cmd.quit)
            else (value, Charamel_tea.Cmd.none));
        view = (fun value -> Charamel_tea.View.v (string_of_int value));
        subscriptions = (fun _ -> Charamel_tea.Sub.none);
      }
      ~events:[ `Msg `Tick ]
      ~size:(24, 80)
  in
  Alcotest.(check int) "scripted model update" 1 model

let chat_lines count =
  String.concat "\n" (List.init count (fun index -> Fmt.str "alpha-%02d" (index + 1)))

let scripted_wheel_scroll () =
  let wheel_up = "\027[<64;40;5M" in
  let wheel_down = "\027[<65;40;5M" in
  let ctrl_c = ui_key "ctrl+c" in
  let quit = [ `Wait 0.; `Key ctrl_c; `Key ctrl_c ] in
  let downs = List.init 20 (fun _ -> `Text wheel_down) in
  let ups = List.init 40 (fun _ -> `Text wheel_up) in
  with_ui_backend (fun backend bridge ->
      await (Ui.Bridge.push bridge (Agent.Text_delta (chat_lines 60)));
      let _model, frame = Ui.run_with backend ~events:(downs @ quit) ~size:(24, 80) in
      Alcotest.(check bool)
        "wheel down reveals the chat tail" true
        (Test_support.contains ~needle:"alpha-60" ~haystack:frame);
      Alcotest.(check bool)
        "wheel down hides the chat head" false
        (Test_support.contains ~needle:"alpha-01" ~haystack:frame));
  with_ui_backend (fun backend bridge ->
      await (Ui.Bridge.push bridge (Agent.Text_delta (chat_lines 60)));
      let _model, frame =
        Ui.run_with backend ~events:(downs @ ups @ quit) ~size:(24, 80)
      in
      Alcotest.(check bool)
        "wheel up returns to the chat head" true
        (Test_support.contains ~needle:"alpha-01" ~haystack:frame);
      Alcotest.(check bool)
        "wheel up hides the chat tail" false
        (Test_support.contains ~needle:"alpha-60" ~haystack:frame))

(* The application cursor is the last cursor-position request of a paint, so the final
   CSI row ; col H sequence in the captured bytes is where the terminal parks. *)
let last_cursor_position bytes =
  let length = String.length bytes in
  let digit value = Char.code value >= 48 && Char.code value <= 57 in
  let rec number index value =
    if index < length && digit bytes.[index] then
      number (index + 1) ((value * 10) + (Char.code bytes.[index] - 48))
    else (index, value)
  in
  let rec csi index result =
    match String.index_from_opt bytes index '\027' with
    | None -> result
    | Some start ->
        let next = start + 1 in
        let found =
          if next + 1 < length && bytes.[next] = '[' && digit bytes.[next + 1] then begin
            let after_row, row = number (next + 1) 0 in
            if after_row < length && bytes.[after_row] = ';' then begin
              let after_col, col = number (after_row + 1) 0 in
              if after_col < length && bytes.[after_col] = 'H' then Some (row, col)
              else None
            end
            else None
          end
          else None
        in
        csi next (if Option.is_some found then found else result)
  in
  csi 0 None

let editor_cursor_reaches_the_terminal () =
  with_ui_backend (fun backend _bridge ->
      let buffer = Buffer.create 4096 in
      let channel =
        Lwt_io.make ~mode:Lwt_io.output (fun source offset length ->
            Buffer.add_subbytes buffer (Lwt_bytes.to_bytes source) offset length;
            Lwt.return length)
      in
      let _model, frame =
        Charamel_tea.Test.run (Ui.app backend) ~output:channel
          ~events:[ `Wait 0.; `Key (ui_key "ctrl+c"); `Key (ui_key "ctrl+c") ]
          ~size:(24, 80)
      in
      (match Lwt.poll (Lwt_io.flush channel) with
      | Some () -> ()
      | None -> Alcotest.fail "the captured output did not flush synchronously");
      let editor_line =
        List.length
          (List.take_while
             (fun line -> not (String.starts_with ~prefix:"\xe2\x80\xba " line))
             (String.split_on_char '\n' frame))
      in
      Alcotest.(check (option int))
        "the cursor sits on the editor line"
        (Some (editor_line + 1))
        (Option.map fst (last_cursor_position (Buffer.contents buffer))))

let cases =
  [
    Test_tools_test_support.case "bounded bridge preserves order" `Quick fifo_is_lossless;
    Test_tools_test_support.case "bridge close wakes question" `Quick close_wakes_question;
    Test_tools_test_support.case "permission dialog bridge round trip" `Quick
      permission_round_trip;
    Test_tools_test_support.case "serialized question and permission dialogs" `Quick
      serialized_dialogs;
    Test_tools_test_support.case "scripted UI stream" `Quick scripted_ui_stream;
    Test_tools_test_support.case "scripted runtime event" `Quick scripted_runtime_event;
    Test_tools_test_support.case "scripted wheel scroll reaches the viewport" `Quick
      scripted_wheel_scroll;
    Test_tools_test_support.case "editor cursor reaches the terminal" `Quick
      editor_cursor_reaches_the_terminal;
    Test_tools_test_support.case "scripted Tea model update" `Quick scripted_counter;
  ]
