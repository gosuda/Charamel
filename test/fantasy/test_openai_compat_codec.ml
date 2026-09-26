open Charamel_fantasy
open Stream_test_support
open Lwt.Infix

let fixture_path = "data/openai_compat_codec.sse"
let read_fixture () = Stream_test_support.read_fixture ~fixture_path ()

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
let base_url server = Uri.to_string (Fixture_http.uri server "")

let provider_call ~body ?(reasoning = `Off) ?temperature ?max_tokens ?(system = [])
    ?(tools = []) ?(messages = [ user "hi" ]) ?truncate () =
  Stream_test_support.with_fixture (fun server ->
      Fixture_http.respond server ?truncate body;
      let provider =
        Provider.openai_compatible ~base_url:(base_url server)
          ~auth:(Provider.Api_key "test-key") ()
      in
      let stream =
        Provider.stream provider ~clock:Charamel_os.Time.lwt ~model ?max_tokens
          ?temperature ~reasoning ~system ~tools messages
      in
      Stream_test_support.drain stream >>= fun observed ->
      Stream_test_support.drain_queued stream >|= fun tail ->
      (observed @ tail, Fixture_http.last_path server, Fixture_http.last_body server))

let expect_parts name expected observed =
  Stream_test_support.expect_parts ~label:name expected observed

let test_fixture_stream () =
  provider_call ~body:(read_fixture ()) () >|= fun (ps, path, _) ->
  Alcotest.(check (option string))
    "posts to /chat/completions" (Some "/chat/completions") path;
  expect_parts "fixture parts through the public provider"
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
  provider_call ~body () >|= fun (ps, _, _) ->
  expect_parts "finish follows trailing usage"
    [
      Stream_part.Text_delta "hi";
      usage ~input:100 ~output:40 ~cache_read:20 ~cache_write:0 ~reasoning:8;
      Stream_part.Finish `Stop;
    ]
    ps

let test_finish_reasons () =
  let finish_of reason =
    let body = event (chunk ~finish:reason {|{}|}) ^ done_event in
    provider_call ~body () >|= fun (ps, _, _) -> finish_name (single_finish ps)
  in
  finish_of "stop" >>= fun stop ->
  Alcotest.check Alcotest.string "stop" "Stop" stop;
  finish_of "length" >>= fun length ->
  Alcotest.check Alcotest.string "length" "Length" length;
  finish_of "content_filter" >>= fun content_filter ->
  Alcotest.check Alcotest.string "content filter" "Content_filter" content_filter;
  finish_of "tool_calls" >>= fun tool_calls ->
  Alcotest.check Alcotest.string "tool_calls" "Tool_calls" tool_calls;
  finish_of "insufficient_system_resource" >>= fun insufficient_system_resource ->
  Alcotest.check Alcotest.string "insufficient system resource"
    "Error provider ended with finish reason insufficient_system_resource"
    insufficient_system_resource;
  finish_of "banana" >>= fun banana ->
  Alcotest.check Alcotest.bool "unknown reason errors" true
    (String.starts_with ~prefix:"Error unknown finish reason banana" banana);
  Lwt.return_unit

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
  provider_call ~body () >|= fun (ps, _, _) ->
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
  provider_call ~body () >|= fun (ps, _, _) ->
  expect_parts "missing id gets a stable synthetic id"
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
  provider_call ~body () >|= fun (ps, _, _) ->
  expect_parts "empty tool entries have no tool effects" [ Stream_part.Finish `Stop ] ps

let test_unknown_tool_type_errors () =
  let body =
    event
      (chunk
         {|{"tool_calls":[{"index":0,"id":"c1","type":"custom_tool","function":{"name":"f","arguments":"{}"}}]}|})
    ^ done_event
  in
  provider_call ~body () >|= fun (ps, _, _) ->
  match single_finish ps with
  | `Error m ->
      Alcotest.(check bool)
        "reports the type" true
        (Test_support.contains ~needle:"not function" ~haystack:m)
  | other -> Alcotest.failf "expected error finish, got %s" (finish_name other)

let test_malformed_event () =
  let body = named_event "chat.completion.chunk" "not json at all" in
  provider_call ~body () >|= fun (ps, _, _) ->
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
  provider_call ~body () >|= fun (ps, _, _) ->
  match single_finish ps with
  | `Error m ->
      Alcotest.(check bool)
        "reports truncated arguments" true
        (String.length m >= 9 && String.sub m 0 9 = "stream en")
  | other -> Alcotest.failf "expected error finish, got %s" (finish_name other)

let test_premature_eof () =
  let body = event (chunk {|{"content":"partial"}|}) in
  provider_call ~body ~truncate:(String.length body - 1) () >|= fun (ps, _, _) ->
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
  provider_call ~body () >|= fun (ps, _, _) ->
  expect_parts "events after the terminal are silent"
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
  provider_call ~body () >|= fun (ps, _, _) ->
  expect_parts "ignored events do not alter the stream"
    [
      usage ~input:100 ~output:40 ~cache_read:20 ~cache_write:0 ~reasoning:8;
      Stream_part.Finish `Stop;
    ]
    ps

let tool =
  Tool.v ~name:"weather" ~description:"Get weather"
    ~schema:
      (Jsont.Json.object'
         [
           Jsont.Json.mem (Jsont.Json.name "type") (Jsont.Json.string "object");
           Jsont.Json.mem
             (Jsont.Json.name "properties")
             (Jsont.Json.object'
                [
                  Jsont.Json.mem (Jsont.Json.name "location")
                    (Jsont.Json.object'
                       [
                         Jsont.Json.mem (Jsont.Json.name "type")
                           (Jsont.Json.string "string");
                       ]);
                ]);
           Jsont.Json.mem (Jsont.Json.name "required")
             (Jsont.Json.list [ Jsont.Json.string "location" ]);
         ])

let test_encode_system_tools_and_config () =
  provider_call ~body:(data_event "[DONE]")
    ~system:[ "You are a helpful assistant" ]
    ~tools:[ tool ] ~temperature:0.25 ()
  >|= fun (_, path, body) ->
  Alcotest.(check (option string))
    "chat completions endpoint" (Some "/chat/completions") path;
  let j = parse_body body in
  check_member_string "model" "model" model.Model.id j;
  check_bool_member "stream" "stream" true j;
  Alcotest.(check int)
    "max tokens" 4096
    (int_of_float (number_value "max tokens" (required "max_tokens" j)));
  check_member_number "temperature" "temperature" 0.25 j;
  check_bool_member "include usage" "include_usage" true (object_value "stream_options" j);
  let messages = array "messages" j in
  Alcotest.(check int) "message count" 2 (List.length messages);
  let system_message = List.hd messages in
  check_member_string "system role" "role" "system" system_message;
  check_member_string "system content" "content" "You are a helpful assistant"
    system_message;
  check_member_string "user role" "role" "user" (List.nth messages 1);
  let tools = array "tools" j in
  Alcotest.(check int) "tool count" 1 (List.length tools);
  let tool_json = List.hd tools in
  check_member_string "tool type" "type" "function" tool_json;
  let function_json = object_value "function" tool_json in
  check_member_string "tool name" "name" "weather" function_json;
  check_member_string "tool description" "description" "Get weather" function_json;
  let parameters = object_value "parameters" function_json in
  check_member_string "schema type" "type" "object" parameters

let test_encode_reasoning_model () =
  provider_call ~body:done_event ~reasoning:`High ~temperature:0.25 ()
  >|= fun (_, _, body) ->
  let j = parse_body body in
  check_member_string "reasoning effort" "reasoning_effort" "high" j;
  Alcotest.(check int)
    "reasoning token cap" 4096
    (int_of_float
       (number_value "reasoning token cap" (required "max_completion_tokens" j)));
  Alcotest.(check bool)
    "reasoning omits temperature" false
    (Option.is_some (member "temperature" j));
  Alcotest.(check bool)
    "reasoning omits max_tokens" false
    (Option.is_some (member "max_tokens" j))

let cases =
  [
    Alcotest_lwt.test_case "fixture stream" `Quick (fun _switch () ->
        test_fixture_stream ());
    Alcotest_lwt.test_case "usage never lost" `Quick (fun _switch () ->
        test_usage_never_lost ());
    Alcotest_lwt.test_case "finish reasons" `Quick (fun _switch () ->
        test_finish_reasons ());
    Alcotest_lwt.test_case "interleaved tool calls" `Quick (fun _switch () ->
        test_interleaved_tool_calls ());
    Alcotest_lwt.test_case "synthetic tool id" `Quick (fun _switch () ->
        test_synthetic_tool_id ());
    Alcotest_lwt.test_case "empty tool entry skipped" `Quick (fun _switch () ->
        test_empty_tool_entry_skipped ());
    Alcotest_lwt.test_case "unknown tool type errors" `Quick (fun _switch () ->
        test_unknown_tool_type_errors ());
    Alcotest_lwt.test_case "malformed event" `Quick (fun _switch () ->
        test_malformed_event ());
    Alcotest_lwt.test_case "truncated arguments error" `Quick (fun _switch () ->
        test_truncated_arguments_error ());
    Alcotest_lwt.test_case "premature eof" `Quick (fun _switch () ->
        test_premature_eof ());
    Alcotest_lwt.test_case "silence after finish" `Quick (fun _switch () ->
        test_silence_after_finish ());
    Alcotest_lwt.test_case "ignored events" `Quick (fun _switch () ->
        test_ignored_events ());
    Alcotest_lwt.test_case "encode system tools config" `Quick (fun _switch () ->
        test_encode_system_tools_and_config ());
    Alcotest_lwt.test_case "encode reasoning model" `Quick (fun _switch () ->
        test_encode_reasoning_model ());
  ]
