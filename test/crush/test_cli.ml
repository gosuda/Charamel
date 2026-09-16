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

let with_scratch f =
  Eio_main.run @@ fun env ->
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Fmt.str "crush-cli-%d-%d" (Unix.getpid ()) (Random.bits ()))
  in
  Eio.Path.mkdirs ~perm:0o700 Eio.Path.(env#fs / root);
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / root))
    (fun () -> f env root)

let run_child env root args =
  let home = Filename.concat root "home" in
  let config = Filename.concat root "config" in
  let data = Filename.concat root "data" in
  let state = Filename.concat root "state" in
  let cache = Filename.concat root "cache" in
  List.iter
    (fun path -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / path))
    [ home; config; data; state; cache ];
  let child_env =
    [
      "HOME=" ^ home;
      "XDG_CONFIG_HOME=" ^ config;
      "XDG_DATA_HOME=" ^ data;
      "XDG_STATE_HOME=" ^ state;
      "XDG_CACHE_HOME=" ^ cache;
      "PATH=" ^ Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin:/bin";
    ]
  in
  match binary_path () with
  | None -> Alcotest.skip ()
  | Some executable ->
      Eio.Time.with_timeout_exn env#clock 15. (fun () ->
          Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
            ~cwd:Eio.Path.(env#fs / root)
            ~env:(Array.of_list child_env) (executable :: args))

let contains ~needle text =
  let n = String.length needle in
  let length = String.length text in
  let rec find index =
    if index + n > length then false
    else if String.sub text index n = needle then true
    else find (index + 1)
  in
  n = 0 || find 0

let dirs_command () =
  with_scratch (fun env root ->
      let output = run_child env root [ "dirs" ] in
      Alcotest.(check bool) "config directory" true (contains ~needle:"config:" output);
      Alcotest.(check bool) "project key" true (contains ~needle:"project-key:" output))

let schema_command () =
  with_scratch (fun env root ->
      let output = run_child env root [ "schema" ] in
      Alcotest.(check bool)
        "schema object" true
        (String.length output > 2 && output.[0] = '{');
      Alcotest.(check bool) "provider schema" true (contains ~needle:"providers" output))

type fixture = { port : int; body : string }

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

let drain_request reader =
  let rec headers content_length =
    match Eio.Buf_read.line reader with
    | "" -> content_length
    | line ->
        let content_length =
          match String.index_opt line ':' with
          | Some index
            when String.lowercase_ascii (String.trim (String.sub line 0 index))
                 = "content-length" ->
              let value = String.sub line (index + 1) (String.length line - index - 1) in
              Option.value (int_of_string_opt (String.trim value)) ~default:0
          | _ -> content_length
        in
        headers content_length
  in
  let length = headers 0 in
  if length > 0 then ignore (Eio.Buf_read.take length reader)

let start_fixture ~sw ~net () =
  let socket =
    Eio.Net.listen net ~backlog:16 ~sw (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | _ -> 0 in
  let fixture = { port; body = fixture_body } in
  let handle flow _ =
    let reader = Eio.Buf_read.of_flow ~max_size:65_536 flow in
    drain_request reader;
    let header =
      Fmt.str
        "HTTP/1.1 200 OK\r\ncontent-type: text/event-stream\r\ncontent-length: %d\r\n\r\n"
        (String.length fixture.body)
    in
    Eio.Flow.copy_string header flow;
    Eio.Flow.copy_string fixture.body flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error:raise handle
      done;
      `Stop_daemon);
  fixture

let fixture_config port =
  Fmt.str
    {|{"providers":{"anthropic":{"type":"anthropic","base_url":"http://127.0.0.1:%d","api_key":"fixture-key","models":[{"id":"fixture-model","name":"Fixture","cost_per_1m_in":0,"cost_per_1m_out":0,"cost_per_1m_in_cached":0,"cost_per_1m_out_cached":0,"context_window":200000,"default_max_tokens":1024,"can_reason":false,"supports_attachments":false}]}},"models":{"large":{"provider":"anthropic","model":"fixture-model"},"small":{"provider":"anthropic","model":"fixture-model"}}}|}
    port

let run_child_status env root args =
  let home = Filename.concat root "home" in
  let config = Filename.concat root "config" in
  let data = Filename.concat root "data" in
  let state = Filename.concat root "state" in
  let cache = Filename.concat root "cache" in
  List.iter
    (fun path -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / path))
    [ home; config; data; state; cache ];
  let child_env =
    [
      "HOME=" ^ home;
      "XDG_CONFIG_HOME=" ^ config;
      "XDG_DATA_HOME=" ^ data;
      "XDG_STATE_HOME=" ^ state;
      "XDG_CACHE_HOME=" ^ cache;
      "PATH=" ^ Option.value (Sys.getenv_opt "PATH") ~default:"/usr/bin:/bin";
    ]
  in
  match binary_path () with
  | None -> Alcotest.skip ()
  | Some executable ->
      let status = ref None in
      let stderr = Buffer.create 256 in
      let output =
        Eio.Time.with_timeout_exn env#clock 20. (fun () ->
            Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
              ~cwd:Eio.Path.(env#fs / root)
              ~stderr:(Eio.Flow.buffer_sink stderr)
              ~is_success:(fun code ->
                status := Some code;
                true)
              ~env:(Array.of_list child_env) (executable :: args))
      in
      (Option.value !status ~default:127, output, Buffer.contents stderr)

let run_fixture () =
  with_scratch (fun env root ->
      Eio.Switch.run @@ fun sw ->
      let fixture = start_fixture ~sw ~net:env#net () in
      Eio.Path.save ~create:(`Exclusive 0o600)
        Eio.Path.(env#fs / root / "crush.json")
        (fixture_config fixture.port);
      let status, output, error =
        run_child_status env root [ "run"; "-y"; "--cwd"; root; "say"; "hi" ]
      in
      Alcotest.(check int) "run exits successfully" 0 status;
      Alcotest.(check bool)
        "assistant text is streamed" true
        (contains ~needle:"pong" output);
      Alcotest.(check string) "fixture diagnostics are empty" "" error)

let providers_catalog_body =
  {|[{"id":"acme","name":"Acme","api_endpoint":"","models":[{"id":"acme-model","name":"Acme Model","cost_per_1m_in":0,"cost_per_1m_out":0,"context_window":128000,"default_max_tokens":4096,"can_reason":false,"supports_attachments":false}]}]|}

let start_json_fixture ~sw ~net body =
  let socket =
    Eio.Net.listen net ~backlog:16 ~sw (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | _ -> 0 in
  let handle flow _ =
    let reader = Eio.Buf_read.of_flow ~max_size:65_536 flow in
    drain_request reader;
    let header =
      Fmt.str
        "HTTP/1.1 200 OK\r\ncontent-type: application/json\r\ncontent-length: %d\r\n\r\n"
        (String.length body)
    in
    Eio.Flow.copy_string header flow;
    Eio.Flow.copy_string body flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error:raise handle
      done;
      `Stop_daemon);
  port

let logout_missing_credential_without_force () =
  with_scratch (fun env root ->
      let status, _output, error = run_child_status env root [ "logout"; "anthropic" ] in
      Alcotest.(check int) "logout without a stored credential fails" 1 status;
      Alcotest.(check bool)
        "missing credential is reported" true
        (contains ~needle:"no credentials for provider anthropic" error))

let logout_missing_credential_with_force () =
  with_scratch (fun env root ->
      let status, output, error =
        run_child_status env root [ "logout"; "--force"; "anthropic" ]
      in
      Alcotest.(check int) "--force treats a missing credential as success" 0 status;
      Alcotest.(check string) "no diagnostics on stderr" "" error;
      Alcotest.(check bool)
        "logout confirmation" true
        (contains ~needle:"Logged out of anthropic" output))

let login_existing_oauth_without_force () =
  with_scratch (fun env root ->
      let auth_dir = Filename.concat root "config/crush" in
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / auth_dir);
      let auth_path = Eio.Path.(env#fs / auth_dir / "auth.json") in
      let auth_contents =
        {|{"anthropic":{"type":"oauth","access":"access-token","refresh":"refresh-token","expires_at_ms":9000000,"account":"account"}}|}
      in
      Eio.Path.save ~create:(`Exclusive 0o600) auth_path auth_contents;
      let status, output, error = run_child_status env root [ "login"; "anthropic" ] in
      Alcotest.(check int) "login without force succeeds" 0 status;
      Alcotest.(check string) "no diagnostics on stderr" "" error;
      Alcotest.(check bool)
        "already logged in notice" true
        (contains ~needle:"You are already logged in to anthropic." output);
      Alcotest.(check bool)
        "force hint" true
        (contains ~needle:"Use --force to re-authenticate." output);
      Alcotest.(check string)
        "stored OAuth credential is unchanged" auth_contents (Eio.Path.load auth_path))

let sessions_lists_a_persisted_session () =
  with_scratch (fun env root ->
      Eio.Switch.run @@ fun sw ->
      let fixture = start_fixture ~sw ~net:env#net () in
      Eio.Path.save ~create:(`Exclusive 0o600)
        Eio.Path.(env#fs / root / "crush.json")
        (fixture_config fixture.port);
      let run_status, _run_output, run_error =
        run_child_status env root [ "run"; "-y"; "--cwd"; root; "say"; "hi" ]
      in
      Alcotest.(check int) "seeding run exits successfully" 0 run_status;
      Alcotest.(check string) "seeding run has no diagnostics" "" run_error;
      let status, output, error =
        run_child_status env root [ "sessions"; "--cwd"; root ]
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
        "the persisted session has exactly an id/title row" true listed)

let logs_are_empty_before_any_run () =
  with_scratch (fun env root ->
      let status, output, error = run_child_status env root [ "logs"; "--cwd"; root ] in
      Alcotest.(check int) "logs exits successfully" 0 status;
      Alcotest.(check string) "no diagnostics" "" error;
      Alcotest.(check string) "no log lines before anything ran" "" output)

let logs_rejects_a_zero_tail () =
  with_scratch (fun env root ->
      let status, _output, error =
        run_child_status env root [ "logs"; "--tail"; "0"; "--cwd"; root ]
      in
      Alcotest.(check int) "a zero tail is a usage error" 2 status;
      Alcotest.(check bool)
        "the tail bound is reported" true
        (contains ~needle:"--tail must be at least 1" error))

let update_providers_rejects_the_embedded_source () =
  with_scratch (fun env root ->
      let status, _output, error =
        run_child_status env root [ "update-providers"; "--source"; "embedded" ]
      in
      Alcotest.(check int) "the embedded source is rejected" 2 status;
      Alcotest.(check bool)
        "the usage message names --source" true
        (contains ~needle:"--source expects a catalog URL" error))

let update_providers_refreshes_from_a_fixture () =
  with_scratch (fun env root ->
      Eio.Switch.run @@ fun sw ->
      let port = start_json_fixture ~sw ~net:env#net providers_catalog_body in
      let status, output, error =
        run_child_status env root
          [ "update-providers"; "--source"; Fmt.str "http://127.0.0.1:%d" port ]
      in
      Alcotest.(check int) "update-providers exits successfully" 0 status;
      Alcotest.(check string) "no diagnostics" "" error;
      Alcotest.(check bool)
        "the refresh is confirmed" true
        (contains ~needle:"Updated provider catalog" output))

let cases =
  [
    Alcotest.test_case "dirs uses child environment" `Quick dirs_command;
    Alcotest.test_case "schema emits configuration schema" `Quick schema_command;
    Alcotest.test_case "run uses configured HTTP provider" `Quick run_fixture;
    Alcotest.test_case "logout without a credential fails" `Quick
      logout_missing_credential_without_force;
    Alcotest.test_case "logout --force treats a missing credential as success" `Quick
      logout_missing_credential_with_force;
    Alcotest.test_case "login without force preserves OAuth credentials" `Quick
      login_existing_oauth_without_force;
    Alcotest.test_case "sessions lists a persisted session" `Quick
      sessions_lists_a_persisted_session;
    Alcotest.test_case "logs are empty before any run" `Quick
      logs_are_empty_before_any_run;
    Alcotest.test_case "logs rejects a zero tail" `Quick logs_rejects_a_zero_tail;
    Alcotest.test_case "update-providers rejects the embedded source" `Quick
      update_providers_rejects_the_embedded_source;
    Alcotest.test_case "update-providers refreshes from a fixture" `Quick
      update_providers_refreshes_from_a_fixture;
  ]
