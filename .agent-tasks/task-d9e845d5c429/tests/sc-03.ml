let ( / ) = Eio.Path.( / )

(* The OpenAI-compatible decoder defers its terminal [Finish] until the
   stream ends, so the [finish_reason] chunk must arrive before [DONE]
   (lib/fantasy/openai_compat_codec.ml complete/release/feed). *)
let fixture_body =
  "data: {\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"\"},\"finish_reason\":null}]}\n\n"
  ^ "data: {\"choices\":[{\"index\":0,\"delta\":{\"content\":\"pong from fixture\"},\"finish_reason\":null}]}\n\n"
  ^ "data: {\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":42,\"completion_tokens\":3,\"total_tokens\":45}}\n\n"
  ^ "data: [DONE]\n\n"

type request = { request_line : string; headers : (string * string) list; body : string }

let read_request reader =
  let request_line = Eio.Buf_read.line reader in
  let rec collect_headers acc =
    match Eio.Buf_read.line reader with
    | "" -> List.rev acc
    | line -> (
        match String.index_opt line ':' with
        | None -> collect_headers acc
        | Some index ->
            let name = String.lowercase_ascii (String.trim (String.sub line 0 index)) in
            let value =
              String.trim (String.sub line (index + 1) (String.length line - index - 1))
            in
            collect_headers ((name, value) :: acc))
  in
  let headers = collect_headers [] in
  let content_length =
    match List.assoc_opt "content-length" headers with
    | Some value -> Option.value (int_of_string_opt value) ~default:0
    | None -> 0
  in
  let body = if content_length > 0 then Eio.Buf_read.take content_length reader else "" in
  { request_line; headers; body }

(* Only the first request the turn sends is asserted on; a later request
   (for example a title-generation call reusing the same model) is served
   the same scripted reply and otherwise ignored. *)
let start_fixture ~sw ~net =
  let socket = Eio.Net.listen net ~backlog:8 ~sw (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port = match Eio.Net.listening_addr socket with `Tcp (_, port) -> port | _ -> 0 in
  let first_request = ref None in
  let handle flow _addr =
    let reader = Eio.Buf_read.of_flow ~max_size:4_194_304 flow in
    let request = read_request reader in
    if Option.is_none !first_request then first_request := Some request;
    let header =
      Fmt.str
        "HTTP/1.1 200 OK\r\ncontent-type: text/event-stream\r\ncontent-length: %d\r\nconnection: close\r\n\r\n"
        (String.length fixture_body)
    in
    Eio.Flow.copy_string header flow;
    Eio.Flow.copy_string fixture_body flow;
    Eio.Flow.close flow
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork ~sw socket ~on_error:raise handle
      done;
      `Stop_daemon);
  (port, first_request)

let fixture_config port =
  Fmt.str
    {|{"providers":{"fixture":{"type":"openai_compatible","base_url":"http://127.0.0.1:%d","api_key":"goal-sc03-test-key","models":[{"id":"fixture-model","name":"Fixture Model","cost_per_1m_in":0,"cost_per_1m_out":0,"cost_per_1m_in_cached":0,"cost_per_1m_out_cached":0,"context_window":200000,"default_max_tokens":1024,"can_reason":false,"supports_attachments":false}]}},"models":{"large":{"provider":"fixture","model":"fixture-model"},"small":{"provider":"fixture","model":"fixture-model"}}}|}
    port

let run () =
  Eio_main.run @@ fun env ->
  let root = Goal_fixture.repo_root () in
  let binary = Filename.concat root "_build/default/bin/crush/main.exe" in
  if not (Sys.file_exists binary) then Alcotest.failf "crush binary is missing: %s" binary;
  let scratch = Goal_fixture.fresh_scratch env ~root ~name:"sc-03" in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true (env#fs / scratch))
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      let port, first_request = start_fixture ~sw ~net:env#net in
      Eio.Path.save ~create:(`Exclusive 0o600) (env#fs / scratch / "crush.json")
        (fixture_config port);
      let home = Filename.concat scratch "home" in
      let config = Filename.concat scratch "config" in
      let data = Filename.concat scratch "data" in
      let state = Filename.concat scratch "state" in
      let cache = Filename.concat scratch "cache" in
      List.iter
        (fun path -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (env#fs / path))
        [ home; config; data; state; cache ];
      let child_env =
        [| "HOME=" ^ home;
           "XDG_CONFIG_HOME=" ^ config;
           "XDG_DATA_HOME=" ^ data;
           "XDG_STATE_HOME=" ^ state;
           "XDG_CACHE_HOME=" ^ cache;
           "PATH=" ^ Goal_fixture.real_path () |]
      in
      let status = ref None in
      let stderr_buf = Buffer.create 256 in
      let stdout =
        Eio.Time.with_timeout_exn env#clock 30. (fun () ->
            Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all ~cwd:(env#fs / scratch)
              ~stderr:(Eio.Flow.buffer_sink stderr_buf)
              ~is_success:(fun code ->
                status := Some code;
                true)
              ~env:child_env [ binary; "run"; "-y"; "--cwd"; scratch; "say hi" ])
      in
      Alcotest.(check int) "crush run exits 0" 0 (Option.value !status ~default:127);
      Alcotest.(check string) "no diagnostics on stderr" "" (Buffer.contents stderr_buf);
      Alcotest.(check bool) "the fixture's scripted reply is printed" true
        (Goal_fixture.contains ~needle:"pong from fixture" stdout);
      match !first_request with
      | None -> Alcotest.fail "the fixture provider received no HTTP request"
      | Some first ->
          Alcotest.(check bool) "the request targets the chat completions endpoint" true
            (Goal_fixture.contains ~needle:"POST /chat/completions HTTP/1.1" first.request_line);
          Alcotest.(check (option string)) "the request carries the configured test API key"
            (Some "Bearer goal-sc03-test-key") (List.assoc_opt "authorization" first.headers);
          Alcotest.(check bool) "the request selects the configured model" true
            (Goal_fixture.contains ~needle:{|"model":"fixture-model"|} first.body);
          Alcotest.(check bool) "the request opts into streaming" true
            (Goal_fixture.contains ~needle:{|"stream":true|} first.body);
          Alcotest.(check bool) "the request carries the user turn" true
            (Goal_fixture.contains ~needle:{|"role":"user"|} first.body
            && Goal_fixture.contains ~needle:"say hi" first.body))
