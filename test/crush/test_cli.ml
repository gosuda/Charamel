open Lwt_direct

let binary_path () =
  match Sys.getenv_opt "CRUSH_BIN" with
  | Some path when Sys.file_exists path -> Some path
  | _ ->
      let root =
        match Sys.getenv_opt "DUNE_SOURCEROOT" with
        | Some value -> value
        | None -> Sys.getcwd ()
      in
      [
        Filename.concat root "_build/default/bin/crush/main.exe";
        Filename.concat root "_build/default/bin/crush/crush.exe";
      ]
      |> List.find_opt Sys.file_exists

let write_private path content =
  Test_tools_test_support.mkdir_p (Filename.dirname path);
  let channel =
    open_out_gen [ Open_wronly; Open_creat; Open_excl; Open_binary ] 0o600 path
  in
  Fun.protect
    ~finally:(fun () -> close_out channel)
    (fun () -> output_string channel content)

let child_environment root =
  let home = Filename.concat root "home" in
  let config = Filename.concat root "config" in
  let data = Filename.concat root "data" in
  let state = Filename.concat root "state" in
  let cache = Filename.concat root "cache" in
  List.iter Test_tools_test_support.mkdir_p [ home; config; data; state; cache ];
  Array.of_list
    [
      "HOME=" ^ home;
      "XDG_CONFIG_HOME=" ^ config;
      "XDG_DATA_HOME=" ^ data;
      "XDG_STATE_HOME=" ^ state;
      "XDG_CACHE_HOME=" ^ cache;
      "PATH=" ^ Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin:/bin";
    ]

let run_child root args =
  match binary_path () with
  | None -> Alcotest.skip ()
  | Some executable ->
      let _, output, _ =
        await
        @@ Test_support.run_cli ~exe:executable ~env:(child_environment root) ~cwd:root
             ~timeout:15. args
      in
      output

let run_child_status root args =
  match binary_path () with
  | None -> Alcotest.skip ()
  | Some executable ->
      await
      @@ Test_support.run_cli ~exe:executable ~env:(child_environment root) ~cwd:root
           ~timeout:20. args

let dirs_command () =
  Test_tools_test_support.with_scratch (fun root ->
      let output = run_child root [ "dirs" ] in
      Alcotest.(check bool)
        "config directory" true
        (Test_support.contains ~needle:"config:" ~haystack:output);
      Alcotest.(check bool)
        "project key" true
        (Test_support.contains ~needle:"project-key:" ~haystack:output))

let schema_command () =
  Test_tools_test_support.with_scratch (fun root ->
      let output = run_child root [ "schema" ] in
      Alcotest.(check bool)
        "schema object" true
        (String.length output > 2 && output.[0] = '{');
      Alcotest.(check bool)
        "provider schema" true
        (Test_support.contains ~needle:"providers" ~haystack:output))

let fixture_body =
  "event: message_start\n"
  ^ "data: \
     {\"type\":\"message_start\",\"message\":{\"id\":\"m\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"fixture-model\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":2,\"output_tokens\":0}}}\n\n"
  ^ "event: content_block_start\n"
  ^ "data: \
     {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
  ^ "event: content_block_delta\n"
  ^ "data: \
     {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"pong\"}}\n\n"
  ^ "event: content_block_stop\n"
  ^ "data: {\"type\":\"content_block_stop\",\"index\":0}\n\n" ^ "event: message_delta\n"
  ^ "data: \
     {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":1}}\n\n"
  ^ "event: message_stop\n" ^ "data: {\"type\":\"message_stop\"}\n\n"

let fixture_config port =
  Fmt.str
    {|{"providers":{"anthropic":{"type":"anthropic","base_url":"http://127.0.0.1:%d","api_key":"fixture-key","models":[{"id":"fixture-model","name":"Fixture","cost_per_1m_in":0,"cost_per_1m_out":0,"cost_per_1m_in_cached":0,"cost_per_1m_out_cached":0,"context_window":200000,"default_max_tokens":1024,"can_reason":false,"supports_attachments":false}]}},"models":{"large":{"provider":"anthropic","model":"fixture-model"},"small":{"provider":"anthropic","model":"fixture-model"}}}|}
    port

let run_fixture () =
  Test_tools_test_support.with_scratch (fun root ->
      Test_tools_test_support.with_http_fixture ~content_type:"text/event-stream"
        fixture_body (fun port ->
          write_private (Filename.concat root "crush.json") (fixture_config port);
          let status, output, error =
            run_child_status root [ "run"; "-y"; "--cwd"; root; "say"; "hi" ]
          in
          Alcotest.(check int) "run exits successfully" 0 status;
          Alcotest.(check bool)
            "assistant text is streamed" true
            (Test_support.contains ~needle:"pong" ~haystack:output);
          Alcotest.(check string) "fixture diagnostics are empty" "" error))

let providers_catalog_body =
  {|[{"id":"acme","name":"Acme","api_endpoint":"","models":[{"id":"acme-model","name":"Acme Model","cost_per_1m_in":0,"cost_per_1m_out":0,"context_window":128000,"default_max_tokens":4096,"can_reason":false,"supports_attachments":false}]}]|}

let logout_missing_credential_without_force () =
  Test_tools_test_support.with_scratch (fun root ->
      let status, _output, error = run_child_status root [ "logout"; "anthropic" ] in
      Alcotest.(check int) "logout without a stored credential fails" 1 status;
      Alcotest.(check bool)
        "missing credential is reported" true
        (Test_support.contains ~needle:"no credentials for provider anthropic"
           ~haystack:error))

let logout_missing_credential_with_force () =
  Test_tools_test_support.with_scratch (fun root ->
      let status, output, error =
        run_child_status root [ "logout"; "--force"; "anthropic" ]
      in
      Alcotest.(check int) "--force treats a missing credential as success" 0 status;
      Alcotest.(check string) "no diagnostics on stderr" "" error;
      Alcotest.(check bool)
        "logout confirmation" true
        (Test_support.contains ~needle:"Logged out of anthropic" ~haystack:output))

let login_existing_oauth_without_force () =
  Test_tools_test_support.with_scratch (fun root ->
      let auth_path = Filename.concat (Filename.concat root "config/crush") "auth.json" in
      let auth_contents =
        {|{"anthropic":{"type":"oauth","access":"access-token","refresh":"refresh-token","expires_at_ms":9000000,"account":"account"}}|}
      in
      write_private auth_path auth_contents;
      let status, output, error = run_child_status root [ "login"; "anthropic" ] in
      Alcotest.(check int) "login without force succeeds" 0 status;
      Alcotest.(check string) "no diagnostics on stderr" "" error;
      Alcotest.(check bool)
        "already logged in notice" true
        (Test_support.contains ~needle:"You are already logged in to anthropic."
           ~haystack:output);
      Alcotest.(check bool)
        "force hint" true
        (Test_support.contains ~needle:"Use --force to re-authenticate." ~haystack:output);
      Alcotest.(check string)
        "stored OAuth credential is unchanged" auth_contents
        (Test_tools_test_support.load_file auth_path))

let sessions_lists_a_persisted_session () =
  Test_tools_test_support.with_scratch (fun root ->
      Test_tools_test_support.with_http_fixture ~content_type:"text/event-stream"
        fixture_body (fun port ->
          write_private (Filename.concat root "crush.json") (fixture_config port);
          let run_status, _run_output, run_error =
            run_child_status root [ "run"; "-y"; "--cwd"; root; "say"; "hi" ]
          in
          Alcotest.(check int) "seeding run exits successfully" 0 run_status;
          Alcotest.(check string) "seeding run has no diagnostics" "" run_error;
          let status, output, error =
            run_child_status root [ "sessions"; "--cwd"; root ]
          in
          Alcotest.(check int) "sessions exits successfully" 0 status;
          Alcotest.(check string) "no diagnostics" "" error;
          (* Session.index_entry stores id/title only; the model is not persisted in the index. *)
          let listed =
            List.exists
              (fun line ->
                match String.split_on_char '\t' line with
                | [ id; _title ] -> id <> ""
                | _ -> false)
              (String.split_on_char '\n' output)
          in
          Alcotest.(check bool)
            "the persisted session has exactly an id/title row" true listed))

let logs_are_empty_before_any_run () =
  Test_tools_test_support.with_scratch (fun root ->
      let status, output, error = run_child_status root [ "logs"; "--cwd"; root ] in
      Alcotest.(check int) "logs exits successfully" 0 status;
      Alcotest.(check string) "no diagnostics" "" error;
      Alcotest.(check string) "no log lines before anything ran" "" output)

let logs_rejects_a_zero_tail () =
  Test_tools_test_support.with_scratch (fun root ->
      let status, _output, error =
        run_child_status root [ "logs"; "--tail"; "0"; "--cwd"; root ]
      in
      Alcotest.(check int) "a zero tail is a usage error" 2 status;
      Alcotest.(check bool)
        "the tail bound is reported" true
        (Test_support.contains ~needle:"--tail must be at least 1" ~haystack:error))

let update_providers_rejects_the_embedded_source () =
  Test_tools_test_support.with_scratch (fun root ->
      let status, _output, error =
        run_child_status root [ "update-providers"; "--source"; "embedded" ]
      in
      Alcotest.(check int) "the embedded source is rejected" 2 status;
      Alcotest.(check bool)
        "the usage message names --source" true
        (Test_support.contains ~needle:"--source expects a catalog URL" ~haystack:error))

let update_providers_refreshes_from_a_fixture () =
  Test_tools_test_support.with_scratch (fun root ->
      Test_tools_test_support.with_http_fixture ~content_type:"application/json"
        providers_catalog_body (fun port ->
          let status, output, error =
            run_child_status root
              [ "update-providers"; "--source"; Fmt.str "http://127.0.0.1:%d" port ]
          in
          Alcotest.(check int) "update-providers exits successfully" 0 status;
          Alcotest.(check string) "no diagnostics" "" error;
          Alcotest.(check bool)
            "the refresh is confirmed" true
            (Test_support.contains ~needle:"Updated provider catalog" ~haystack:output)))

let cases =
  [
    Test_tools_test_support.case "dirs uses child environment" `Quick dirs_command;
    Test_tools_test_support.case "schema emits configuration schema" `Quick schema_command;
    Test_tools_test_support.case "run uses configured HTTP provider" `Quick run_fixture;
    Test_tools_test_support.case "logout without a credential fails" `Quick
      logout_missing_credential_without_force;
    Test_tools_test_support.case "logout --force treats a missing credential as success"
      `Quick logout_missing_credential_with_force;
    Test_tools_test_support.case "login without force preserves OAuth credentials" `Quick
      login_existing_oauth_without_force;
    Test_tools_test_support.case "sessions lists a persisted session" `Quick
      sessions_lists_a_persisted_session;
    Test_tools_test_support.case "logs are empty before any run" `Quick
      logs_are_empty_before_any_run;
    Test_tools_test_support.case "logs rejects a zero tail" `Quick
      logs_rejects_a_zero_tail;
    Test_tools_test_support.case "update-providers rejects the embedded source" `Quick
      update_providers_rejects_the_embedded_source;
    Test_tools_test_support.case "update-providers refreshes from a fixture" `Quick
      update_providers_refreshes_from_a_fixture;
  ]
