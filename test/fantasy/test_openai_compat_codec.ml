open Charamel_fantasy
open Stream_test_support

let fixture_body path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let sse = fixture_body "data/openai_compat_codec.sse"

let model =
  {
    Model.id = "deepseek-chat";
    name = "DeepSeek Chat";
    provider = "deepseek";
    context_window = 128_000;
    default_max_tokens = 4096;
    can_reason = true;
    supports_attachments = true;
    cost_in = 0.;
    cost_out = 0.;
    cost_cache_read = 0.;
    cost_cache_write = 0.;
  }

let user s = Message.text Message.User s

let usage ~input ~output ~cache_read ~cache_write ~reasoning =
  Stream_part.Usage { Usage.input; output; cache_read; cache_write; reasoning }

let chunk ?(index = 0) ?finish delta =
  let finish =
    match finish with
    | Some f -> Fmt.str {|,"finish_reason":"%s"|} f
    | None -> {|,"finish_reason":null|}
  in
  Fmt.str
    {|{"id":"x","object":"chat.completion.chunk","created":1,"model":"m","choices":[{"index":%d,"delta":%s%s}]}|}
    index delta finish

let usage_chunk =
  {|{"id":"x","object":"chat.completion.chunk","created":1,"model":"m","choices":[],"usage":{"prompt_tokens":120,"completion_tokens":40,"total_tokens":160,"prompt_tokens_details":{"cached_tokens":20},"completion_tokens_details":{"reasoning_tokens":8}}}|}

let usage_zero =
  {|{"id":"x","object":"chat.completion.chunk","created":1,"model":"m","choices":[],"usage":{"prompt_tokens":0,"completion_tokens":0,"total_tokens":0}}|}

let event data = Fmt.str "event: chat.completion.chunk\ndata: %s\n\n" data
let named_event name data = Fmt.str "event: %s\ndata: %s\n\n" name data
let data_event data = Fmt.str "data: %s\n\n" data
let done_event = data_event "[DONE]"

let provider_call ?(body = sse) ?(reasoning = `Off) ?temperature ?max_tokens
    ?(system = []) ?(tools = []) ?(messages = [ user "hi" ]) ?truncate () =
  let stream, observed, path, body =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let server = Fixture_server.start ~sw ~net:env#net () in
    Fixture_server.respond server body;
    (match truncate with Some n -> Fixture_server.trunc server n | None -> ());
    let provider =
      Provider.openai_compatible
        ~base_url:(Fixture_server.base_url server)
        ~auth:(Provider.Api_key "test-key") ()
    in
    let stream =
      Provider.stream provider ~sw ~clock:env#clock ~net:env#net ~model ?max_tokens
        ?temperature ~reasoning ~system ~tools messages
    in
    let observed = drain stream in
    (stream, observed, Fixture_server.last_path server, Fixture_server.last_body server)
  in
  (observed @ Stream_test_support.drain_queued stream, path, body)

let json s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok j -> j
  | Error e -> Alcotest.failf "json %s: %s" s e

let mem (j : Jsont.json) k =
  match j with
  | Jsont.Object (ms, _) -> (
      match Jsont.Json.find_mem k ms with Some (_, v) -> Some v | None -> None)
  | _ -> None

let str_mem k j = match mem j k with Some (Jsont.String (s, _)) -> Some s | _ -> None

let int_mem k j =
  match mem j k with Some (Jsont.Number (f, _)) -> Some (int_of_float f) | _ -> None

let number_mem k j = match mem j k with Some (Jsont.Number (f, _)) -> Some f | _ -> None
let bool_mem k j = match mem j k with Some (Jsont.Bool (b, _)) -> Some b | _ -> None

let test_fixture_stream () =
  let ps, path, _ = provider_call () in
  Alcotest.(check (option string))
    "posts to /chat/completions" (Some "/chat/completions") path;
  Alcotest.(check parts)
    "fixture parts through the public provider"
    [
      Stream_part.Reasoning_delta "The user wants the date.";
      Stream_part.Reasoning_delta " I'll call get_date.";
      Stream_part.Text_delta "Let me check.";
      Stream_part.Tool_call_start { id = "call_00_abc"; name = "get_date" };
      Stream_part.Tool_call_start { id = "call_00_def"; name = "echo" };
      Stream_part.Tool_input_delta { id = "call_00_def"; delta = "{\"msg\":" };
      Stream_part.Tool_input_delta { id = "call_00_abc"; delta = "{}" };
      Stream_part.Tool_input_delta { id = "call_00_def"; delta = "\"hi\"}" };
      Stream_part.Tool_call_end "call_00_abc";
      Stream_part.Tool_call_end "call_00_def";
      usage ~input:100 ~output:40 ~cache_read:20 ~cache_write:0 ~reasoning:8;
      Stream_part.Finish `Tool_calls;
    ]
    ps

let test_usage_never_lost () =
  let body =
    String.concat ""
      [
        event (chunk {|{"content":"hi"}|});
        event (chunk ~finish:"stop" {|{}|});
        event usage_chunk;
        done_event;
      ]
  in
  let ps, _, _ = provider_call ~body () in
  Alcotest.(check parts)
    "finish follows trailing usage"
    [
      Stream_part.Text_delta "hi";
      usage ~input:100 ~output:40 ~cache_read:20 ~cache_write:0 ~reasoning:8;
      Stream_part.Finish `Stop;
    ]
    ps

let test_finish_reasons () =
  let finish_of reason =
    let body = event (chunk ~finish:reason {|{}|}) ^ done_event in
    provider_call ~body () |> fun (ps, _, _) -> finish_name (single_finish ps)
  in
  Alcotest.check Alcotest.string "stop" "Stop" (finish_of "stop");
  Alcotest.check Alcotest.string "length" "Length" (finish_of "length");
  Alcotest.check Alcotest.string "content filter" "Content_filter"
    (finish_of "content_filter");
  Alcotest.check Alcotest.string "tool_calls" "Tool_calls" (finish_of "tool_calls");
  Alcotest.check Alcotest.string "insufficient system resource"
    "Error provider ended with finish reason insufficient_system_resource"
    (finish_of "insufficient_system_resource");
  Alcotest.check Alcotest.bool "unknown reason errors" true
    (String.starts_with ~prefix:"Error unknown finish reason banana" (finish_of "banana"))

let test_interleaved_tool_calls () =
  let body =
    String.concat ""
      [
        event
          (chunk
             {|{"tool_calls":[{"index":0,"id":"c0","type":"function","function":{"name":"grep","arguments":"{\"pat"}}]}|});
        event
          (chunk
             {|{"tool_calls":[{"index":1,"id":"c1","type":"function","function":{"name":"glob","arguments":"{\"pa"}}]}|});
        event
          (chunk
             {|{"tool_calls":[{"index":0,"function":{"arguments":"tern\":\"foo\"}"}}]}|});
        event
          (chunk {|{"tool_calls":[{"index":1,"function":{"arguments":"th\":\"x\"}"}}]}|});
        event (chunk ~finish:"tool_calls" {|{}|});
        done_event;
      ]
  in
  let ps, _, _ = provider_call ~body () in
  let args id =
    List.filter_map
      (function
        | Stream_part.Tool_input_delta { id = i; delta } when i = id -> Some delta
        | _ -> None)
      ps
    |> String.concat ""
  in
  Alcotest.(check string) "call 0 arguments" {|{"pattern":"foo"}|} (args "c0");
  Alcotest.(check string) "call 1 arguments" {|{"path":"x"}|} (args "c1");
  Alcotest.(check string) "finish" "Tool_calls" (finish_name (single_finish ps))

let test_synthetic_tool_id () =
  let body =
    event
      (chunk
         {|{"tool_calls":[{"index":2,"type":"function","function":{"name":"f","arguments":"{}"}}]}|})
    ^ event (chunk ~finish:"tool_calls" {|{}|})
    ^ done_event
  in
  let ps, _, _ = provider_call ~body () in
  Alcotest.(check parts)
    "missing id gets a stable synthetic id"
    [
      Stream_part.Tool_call_start { id = "tool-call-2"; name = "f" };
      Stream_part.Tool_input_delta { id = "tool-call-2"; delta = "{}" };
      Stream_part.Tool_call_end "tool-call-2";
      Stream_part.Finish `Tool_calls;
    ]
    ps

let test_empty_tool_entry_skipped () =
  let body =
    event (chunk {|{"tool_calls":[{"index":0}]}|})
    ^ event (chunk ~finish:"stop" {|{}|})
    ^ done_event
  in
  let ps, _, _ = provider_call ~body () in
  Alcotest.(check parts)
    "empty tool entries have no tool effects" [ Stream_part.Finish `Stop ] ps

let test_unknown_tool_type_errors () =
  let body =
    event
      (chunk
         {|{"tool_calls":[{"index":0,"id":"c1","type":"custom_tool","function":{"name":"f","arguments":"{}"}}]}|})
    ^ done_event
  in
  let ps, _, _ = provider_call ~body () in
  match single_finish ps with
  | `Error m ->
      Alcotest.(check bool)
        "reports the type" true
        (Test_support.contains ~needle:"not function" ~haystack:m)
  | other -> Alcotest.failf "expected error finish, got %s" (finish_name other)

let test_malformed_event () =
  let body = named_event "chat.completion.chunk" "not json at all" in
  let ps, _, _ = provider_call ~body () in
  match ps with
  | [ Stream_part.Finish (`Error m) ] ->
      Alcotest.(check bool)
        "names malformed input" true
        (String.length m >= 16 && String.sub m 0 16 = "malformed event:")
  | other ->
      Alcotest.failf "expected one error finish, got %a" Fmt.(list Stream_part.pp) other

let test_truncated_arguments_error () =
  let body =
    event
      (chunk
         {|{"tool_calls":[{"index":0,"id":"c1","type":"function","function":{"name":"f","arguments":"{\"pa"}}]}|})
    ^ done_event
  in
  let ps, _, _ = provider_call ~body () in
  match single_finish ps with
  | `Error m ->
      Alcotest.(check bool)
        "reports truncated arguments" true
        (String.length m >= 9 && String.sub m 0 9 = "stream en")
  | other -> Alcotest.failf "expected error finish, got %s" (finish_name other)

let test_premature_eof () =
  let body = event (chunk {|{"content":"partial"}|}) in
  let ps, _, _ = provider_call ~body ~truncate:(String.length body - 1) () in
  match ps with
  | [ Stream_part.Text_delta "partial"; Stream_part.Finish (`Error _) ] -> ()
  | other ->
      Alcotest.failf "expected text then error finish, got %a"
        Fmt.(list Stream_part.pp)
        other

let test_silence_after_finish () =
  let body =
    String.concat ""
      [
        event (chunk ~finish:"stop" {|{}|});
        event usage_chunk;
        done_event;
        event (chunk {|{"content":"late"}|});
      ]
  in
  let ps, _, _ = provider_call ~body () in
  Alcotest.(check parts)
    "events after the terminal are silent"
    [
      usage ~input:100 ~output:40 ~cache_read:20 ~cache_write:0 ~reasoning:8;
      Stream_part.Finish `Stop;
    ]
    ps

let test_ignored_events () =
  let body =
    String.concat ""
      [
        named_event "ping" {|{"type":"ping"}|};
        named_event "custom"
          {|{"id":"x","object":"chat.completion.chunk","created":1,"model":"m","choices":[{"index":0,"delta":{"content":"z"},"finish_reason":null}]}|};
        event usage_zero;
        event usage_chunk;
        event (chunk ~finish:"stop" {|{}|});
        done_event;
      ]
  in
  let ps, _, _ = provider_call ~body () in
  Alcotest.(check parts)
    "ignored events do not alter the stream"
    [
      usage ~input:100 ~output:40 ~cache_read:20 ~cache_write:0 ~reasoning:8;
      Stream_part.Finish `Stop;
    ]
    ps

let test_encode_system_tools_and_config () =
  let tool =
    Tool.v ~name:"weather" ~description:"Get weather"
      ~schema:
        (json
           {|{"type":"object","properties":{"location":{"type":"string"}},"required":["location"]}|})
  in
  let response = data_event "[DONE]" in
  let _, path, body =
    provider_call ~body:response
      ~system:[ "You are a helpful assistant" ]
      ~tools:[ tool ] ~temperature:0.25 ()
  in
  Alcotest.(check (option string))
    "chat completions endpoint" (Some "/chat/completions") path;
  let j = parse_body body in
  Alcotest.(check (option string)) "model" (Some model.Model.id) (str_mem "model" j);
  Alcotest.(check (option bool)) "stream" (Some true) (bool_mem "stream" j);
  Alcotest.(check (option int)) "max_tokens" (Some 4096) (int_mem "max_tokens" j);
  Alcotest.(check (option (float 0.)))
    "temperature" (Some 0.25) (number_mem "temperature" j);
  (match mem j "stream_options" with
  | Some options ->
      Alcotest.(check (option bool))
        "include usage" (Some true)
        (bool_mem "include_usage" options)
  | None -> Alcotest.fail "stream_options is missing");
  (match mem j "messages" with
  | Some (Jsont.Array ([ system; user_message ], _)) ->
      Alcotest.(check (option string))
        "system role" (Some "system") (str_mem "role" system);
      Alcotest.(check (option string))
        "system content" (Some "You are a helpful assistant") (str_mem "content" system);
      Alcotest.(check (option string))
        "user role" (Some "user") (str_mem "role" user_message)
  | Some (Jsont.Array (_, _)) -> Alcotest.fail "unexpected message count"
  | _ -> Alcotest.fail "messages are missing");
  match mem j "tools" with
  | Some (Jsont.Array ([ tool_json ], _)) -> (
      Alcotest.(check (option string))
        "tool type" (Some "function") (str_mem "type" tool_json);
      match mem tool_json "function" with
      | Some function_json -> (
          Alcotest.(check (option string))
            "tool name" (Some "weather")
            (str_mem "name" function_json);
          Alcotest.(check (option string))
            "tool description" (Some "Get weather")
            (str_mem "description" function_json);
          match mem function_json "parameters" with
          | Some (Jsont.Object _) -> ()
          | _ -> Alcotest.fail "tool schema is missing")
      | None -> Alcotest.fail "tool function is missing")
  | _ -> Alcotest.fail "tools are missing"

let test_encode_reasoning_model () =
  let _, _, body = provider_call ~body:done_event ~reasoning:`High ~temperature:0.25 () in
  let j = parse_body body in
  Alcotest.(check (option string))
    "reasoning effort" (Some "high")
    (str_mem "reasoning_effort" j);
  Alcotest.(check (option int))
    "reasoning token cap" (Some 4096)
    (int_mem "max_completion_tokens" j);
  Alcotest.(check bool) "reasoning omits temperature" true (mem j "temperature" = None);
  Alcotest.(check bool) "reasoning omits max_tokens" true (mem j "max_tokens" = None)

let cases =
  [
    ("fixture stream", `Quick, test_fixture_stream);
    ("usage never lost", `Quick, test_usage_never_lost);
    ("finish reasons", `Quick, test_finish_reasons);
    ("interleaved tool calls", `Quick, test_interleaved_tool_calls);
    ("synthetic tool id", `Quick, test_synthetic_tool_id);
    ("empty tool entry skipped", `Quick, test_empty_tool_entry_skipped);
    ("unknown tool type errors", `Quick, test_unknown_tool_type_errors);
    ("malformed event", `Quick, test_malformed_event);
    ("truncated arguments error", `Quick, test_truncated_arguments_error);
    ("premature eof", `Quick, test_premature_eof);
    ("silence after finish", `Quick, test_silence_after_finish);
    ("ignored events", `Quick, test_ignored_events);
    ("encode system tools config", `Quick, test_encode_system_tools_and_config);
    ("encode reasoning model", `Quick, test_encode_reasoning_model);
  ]
