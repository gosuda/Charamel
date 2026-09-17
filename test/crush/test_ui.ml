let contains ~needle haystack =
  let n = String.length needle in
  let limit = String.length haystack - n in
  let rec loop index =
    if index > limit then false
    else if String.sub haystack index n = needle then true
    else loop (index + 1)
  in
  n = 0 || (limit >= 0 && loop 0)

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

let fifo_is_lossless () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let bridge = Ui.Bridge.create ~capacity:1 () in
  let first = Agent.Text_delta "one" in
  let second = Agent.Reasoning_delta "two" in
  let done_, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Ui.Bridge.push bridge first;
      Ui.Bridge.push bridge second;
      Eio.Promise.resolve resolver ())
  |> ignore;
  let got_first = Ui.Bridge.take_event bridge in
  let got_second = Ui.Bridge.take_event bridge in
  Eio.Promise.await done_;
  Alcotest.(check bool)
    "first event preserved" true
    (match got_first with Some (Agent.Text_delta "one") -> true | _ -> false);
  Alcotest.(check bool)
    "second event preserved" true
    (match got_second with Some (Agent.Reasoning_delta "two") -> true | _ -> false);
  Ui.Bridge.close bridge;
  ignore env

let close_wakes_question () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let bridge = Ui.Bridge.create () in
  let finished, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let request =
        {
          Crush_core.Tool.header = "question";
          text = "continue?";
          options = [];
          multi = false;
          free_text = true;
        }
      in
      let result = Ui.Bridge.ask bridge [ request ] in
      Eio.Promise.resolve resolver result)
  |> ignore;
  Eio.Time.sleep env#clock 0.;
  Ui.Bridge.close bridge;
  match Eio.Promise.await finished with
  | Error `Aborted -> ()
  | _ -> Alcotest.fail "close did not abort question"

let permission_round_trip () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
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
  let finished, resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve resolver (Ui.Bridge.ask_permission bridge request))
  |> ignore;
  let pending = Ui.Bridge.take_permission bridge in
  (match pending with
  | None -> Alcotest.fail "permission request was not delivered"
  | Some pending -> Ui.Bridge.answer_permission bridge pending Permission.Allow_once);
  Alcotest.(check bool)
    "allow once delivered" true
    (Eio.Promise.await finished = Permission.Allow_once);
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
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("crush-ui-" ^ string_of_int (Unix.getpid ()))
  in
  (try Unix.mkdir root 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let previous_data_home = Sys.getenv_opt "XDG_DATA_HOME" in
  Unix.putenv "XDG_DATA_HOME" root;
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
  let auth_path = Eio.Path.(env#fs / root / "auth.json") in
  let auth =
    match Auth.create ~path:auth_path ~clock:env#clock () with
    | Error error -> Alcotest.failf "auth setup: %a" Auth.pp_error error
    | Ok resource -> (
        match Auth.set resource ~provider:"anthropic" (Auth.Api_key "fixture") with
        | Ok () -> resource
        | Error error -> Alcotest.failf "auth credential setup: %a" Auth.pp_error error)
  in
  let store = Session.store ~fs:env#fs ~cwd:root in
  let session =
    match
      Session.create store ~clock:env#clock
        ~random:(fun n -> String.make n '\000')
        ~title:"UI fixture" ~cwd:root
        ~model:{ Session.provider = "anthropic"; model = model.Charamel_fantasy.Model.id }
        ()
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "session setup: %a" Session.pp_error error
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
  let bridge = Ui.Bridge.create () in
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
      interactive = true;
      ask = Some (Ui.Bridge.ask bridge);
      events = Ui.Bridge.push bridge;
    }
  in
  let resolved role =
    match Models.resolve ~fs:env#fs config ~auth ~env:Sys.getenv_opt ~role with
    | Ok value -> value
    | Error error -> Alcotest.failf "model setup: %a" Models.pp_error error
  in
  let agent =
    match
      Agent.create deps ~session ~large:(resolved `Large) ~small:(resolved `Small)
    with
    | Ok value -> value
    | Error error -> Alcotest.failf "agent setup: %a" Agent.pp_error error
  in
  let plan = ref false in
  let backend : Ui.backend =
    {
      agent = ref agent;
      events = bridge;
      clock = env#clock;
      env;
      form_env = Charamel_huh.Form.Env.v ~fs:env#fs ~temp_dir:env#fs ~editor:[ "true" ];
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
              supports_attachments = model.Charamel_fantasy.Model.supports_attachments;
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
          Agent.set_plan_mode agent value;
          Ok ());
      plan_mode = (fun () -> !plan);
      lsp_status = (fun () -> "off");
      mcp_status = (fun () -> "off");
      dark = (fun () -> true);
      quit = (fun () -> ());
    }
  in
  Fun.protect
    (fun () -> f env sw backend bridge)
    ~finally:(fun () ->
      Ui.Bridge.close bridge;
      (match previous_data_home with
      | Some value -> Unix.putenv "XDG_DATA_HOME" value
      | None -> Unix.putenv "XDG_DATA_HOME" "");
      Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / root))

let ui_key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> key
  | Error (`Msg message) -> Alcotest.fail message

let serialized_dialogs () =
  with_ui_backend (fun env sw backend bridge ->
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
      let question_done, question_resolver = Eio.Promise.create () in
      let permission_done, permission_resolver = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.resolve question_resolver (Ui.Bridge.ask bridge [ question ]))
      |> ignore;
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.resolve permission_resolver
            (Ui.Bridge.ask_permission bridge permission))
      |> ignore;
      Eio.Time.sleep env#clock 0.;
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
      (match Eio.Promise.await question_done with
      | Ok [ (answer : Crush_core.Tool.answer) ] ->
          Alcotest.(check (option string))
            "question answer" (Some "") answer.Crush_core.Tool.text
      | Ok _ -> Alcotest.fail "question dialog returned the wrong answer"
      | Error `Aborted -> Alcotest.fail "question dialog was overwritten or aborted"
      | Error `Not_interactive -> Alcotest.fail "question dialog was not interactive");
      Alcotest.(check bool)
        "permission dialog is resolved after question" true
        (Eio.Promise.await permission_done = Permission.Allow_once))

let scripted_runtime_event () =
  with_ui_backend (fun _env _sw backend bridge ->
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
                Ui.Bridge.push bridge (Agent.Text_delta "runtime-produced reply")
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
        (contains ~needle:"runtime-produced reply" frame))

let scripted_ui_stream () =
  with_ui_backend (fun _env _sw backend bridge ->
      Ui.Bridge.push bridge (Agent.Text_delta "streamed reply");
      Ui.Bridge.push bridge (Agent.Turn_done `Stop);
      let ctrl_c = ui_key "ctrl+c" in
      let _model, frame =
        Ui.run_with backend ~events:[ `Key ctrl_c; `Key ctrl_c ] ~size:(24, 80)
      in
      Alcotest.(check bool)
        "streamed text reaches chat frame" true
        (contains ~needle:"streamed reply" frame))

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

let cases =
  [
    Alcotest.test_case "bounded bridge preserves order" `Quick fifo_is_lossless;
    Alcotest.test_case "bridge close wakes question" `Quick close_wakes_question;
    Alcotest.test_case "permission dialog bridge round trip" `Quick permission_round_trip;
    Alcotest.test_case "serialized question and permission dialogs" `Quick
      serialized_dialogs;
    Alcotest.test_case "scripted UI stream" `Quick scripted_ui_stream;
    Alcotest.test_case "scripted runtime event" `Quick scripted_runtime_event;
    Alcotest.test_case "scripted Tea model update" `Quick scripted_counter;
  ]
