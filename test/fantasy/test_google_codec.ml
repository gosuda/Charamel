open Charamel_fantasy
open Stream_test_support

let sse_data data = Fmt.str "data: %s\n\n" data
let sse_event name data = Fmt.str "event: %s\ndata: %s\n\n" name data

let candidate content =
  sse_data (Fmt.str {|{"candidates":[{"content":{"parts":%s,"role":"model"}}]}|} content)

let finished body =
  sse_data
    (Fmt.str
       {|{"candidates":[{%s}],"usageMetadata":{"promptTokenCount":11,"candidatesTokenCount":5,"totalTokenCount":23,"thoughtsTokenCount":7,"cachedContentTokenCount":3}}|}
       body)

let test_usage =
  { Usage.input = 8; output = 12; cache_read = 3; cache_write = 0; reasoning = 7 }

let fixture_path = "data/google_codec.sse"
let read_fixture () = Stream_test_support.read_fixture ~fixture_path ()

let model =
  {
    Model.id = "gemini-2.5-pro";
    name = "Gemini 2.5 Pro";
    provider = "google";
    context_window = 1_048_576;
    default_max_tokens = 4096;
    can_reason = true;
    supports_attachments = true;
    cost_in = 0.;
    cost_out = 0.;
    cost_cache_read = 0.;
    cost_cache_write = 0.;
  }

let user s = Message.text Message.User s
let default_body = finished {|"finishReason":"STOP"|}

let call ?(body = default_body) ?(model = model) ?(reasoning = `Off) ?temperature
    ?max_tokens ?(system = []) ?(tools = []) ?trunc_bytes messages =
  let stream, observed, path, body =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let server = Fixture_server.start ~sw ~net:env#net () in
    Fixture_server.respond server body;
    Option.iter (Fixture_server.trunc server) trunc_bytes;
    let provider =
      Provider.google
        ~base_url:(Fixture_server.base_url server)
        ~auth:(Provider.Api_key "test-key") ()
    in
    let stream =
      Provider.stream provider ~sw ~clock:env#clock ~net:env#net ~model ?temperature
        ?max_tokens ~reasoning ~system ~tools messages
    in
    let observed = drain stream in
    (stream, observed, Fixture_server.last_path server, Fixture_server.last_body server)
  in
  (observed @ Stream_test_support.drain_queued stream, path, body)

let expect_parts expected observed =
  Stream_test_support.expect_parts ~label:"stream parts" expected observed

let test_fixture_stream () =
  let observed, path, _ = call ~body:(read_fixture ()) [ user "hi" ] in
  Alcotest.(check (option string))
    "posts to streamGenerateContent"
    (Some "/v1beta/models/gemini-2.5-pro:streamGenerateContent?alt=sse") path;
  expect_parts
    [
      Stream_part.Reasoning_delta "**Mapping the request**\n";
      Stream_part.Text_delta "The weather in Florence";
      Stream_part.Tool_call_start { id = "8hd01vsf"; name = "weather" };
      Stream_part.Tool_input_delta
        { id = "8hd01vsf"; delta = {|{"location":"Florence","unit":"C"}|} };
      Stream_part.Tool_call_end "8hd01vsf";
      Stream_part.Text_delta " is 40 degrees.";
      Stream_part.Usage
        { Usage.input = 54; output = 90; cache_read = 0; cache_write = 0; reasoning = 64 };
      Stream_part.Finish `Tool_calls;
    ]
    observed

let test_split_text_accumulation () =
  let observed, _, _ =
    call
      ~body:
        (candidate {|[{"text":"Hel"}]|}
        ^ candidate {|[{"text":"lo, world"}]|}
        ^ finished {|"finishReason":"STOP"|})
      [ user "hi" ]
  in
  expect_parts
    [
      Stream_part.Text_delta "Hel";
      Stream_part.Text_delta "lo, world";
      Stream_part.Usage test_usage;
      Stream_part.Finish `Stop;
    ]
    observed

let test_thought_marks_reasoning () =
  let observed, _, _ =
    call
      ~body:
        (candidate {|[{"text":"thinking","thought":true}]|}
        ^ candidate {|[{"text":"answer"}]|}
        ^ finished {|"finishReason":"STOP"|})
      [ user "hi" ]
  in
  expect_parts
    [
      Stream_part.Reasoning_delta "thinking";
      Stream_part.Text_delta "answer";
      Stream_part.Usage test_usage;
      Stream_part.Finish `Stop;
    ]
    observed

let test_tool_call_without_id () =
  let observed, _, _ =
    call
      ~body:
        (candidate {|[{"functionCall":{"name":"weather","args":{"a":1}}}]|}
        ^ finished {|"finishReason":"STOP"|})
      [ user "hi" ]
  in
  expect_parts
    [
      Stream_part.Tool_call_start { id = "call_1"; name = "weather" };
      Stream_part.Tool_input_delta { id = "call_1"; delta = {|{"a":1}|} };
      Stream_part.Tool_call_end "call_1";
      Stream_part.Usage test_usage;
      Stream_part.Finish `Tool_calls;
    ]
    observed

let test_multi_part_event () =
  let observed, _, _ =
    call
      ~body:
        (candidate
           {|[{"text":"a"},{"text":"b"},{"functionCall":{"name":"t","id":"x","args":{}}}]|}
        ^ finished {|"finishReason":"STOP"|})
      [ user "hi" ]
  in
  expect_parts
    [
      Stream_part.Text_delta "a";
      Stream_part.Text_delta "b";
      Stream_part.Tool_call_start { id = "x"; name = "t" };
      Stream_part.Tool_input_delta { id = "x"; delta = "{}" };
      Stream_part.Tool_call_end "x";
      Stream_part.Usage test_usage;
      Stream_part.Finish `Tool_calls;
    ]
    observed

let test_finish_reasons () =
  let finish reason =
    let observed, _, _ =
      call
        ~body:(sse_data (Fmt.str {|{"candidates":[{"finishReason":"%s"}]}|} reason))
        [ user "hi" ]
    in
    finish_name (single_finish observed)
  in
  Alcotest.check Alcotest.string "stop" "Stop" (finish "STOP");
  Alcotest.check Alcotest.string "length" "Length" (finish "MAX_TOKENS");
  Alcotest.check Alcotest.string "safety" "Content_filter" (finish "SAFETY");
  Alcotest.check Alcotest.string "prohibited content" "Content_filter"
    (finish "PROHIBITED_CONTENT");
  Alcotest.(check bool)
    "malformed function call is an error" true
    (String.starts_with ~prefix:"Error" (finish "MALFORMED_FUNCTION_CALL"))

let test_usage_kept_with_finish () =
  let observed, _, _ =
    call
      ~body:
        (finished {|"content":{"parts":[{"text":"x"}]}|}
        ^ finished {|"finishReason":"STOP"|})
      [ user "hi" ]
  in
  expect_parts
    [ Stream_part.Text_delta "x"; Stream_part.Usage test_usage; Stream_part.Finish `Stop ]
    observed

let test_malformed_event () =
  let observed, _, _ = call ~body:(sse_data "not json at all") [ user "hi" ] in
  match single_finish observed with
  | `Error m ->
      Alcotest.(check bool)
        "reports malformed data" true
        (String.starts_with ~prefix:"malformed event" m)
  | other -> Alcotest.failf "expected an error finish, got %s" (finish_name other)

let test_named_events_ignored () =
  let observed, _, _ =
    call
      ~body:
        (sse_event "ping" {|{"candidates":[]}|}
        ^ candidate {|[{"text":"kept"}]|}
        ^ finished {|"finishReason":"STOP"|})
      [ user "hi" ]
  in
  expect_parts
    [
      Stream_part.Text_delta "kept";
      Stream_part.Usage test_usage;
      Stream_part.Finish `Stop;
    ]
    observed

(* Gemini has no [DONE] sentinel, so an event carrying [finishReason] only
   records the terminal: the decoder releases it at end of stream, which lets
   late [usageMetadata] still attach to it. That deferral is pinned by
   [test_usage_kept_with_finish]. Mid-stream silence is therefore shown with a
   terminal the decoder raises itself, and an [error] member is the smallest
   one: its [message] becomes the error text. *)
let test_silence_after_finish () =
  let error =
    {|{"error":{"code":400,"message":"Cannot fetch content from the provided URL","status":"INVALID_ARGUMENT"}}|}
  in
  let observed, _, _ =
    call ~body:(sse_data error ^ candidate {|[{"text":"late"}]|}) [ user "hi" ]
  in
  expect_parts
    [ Stream_part.Finish (`Error "Cannot fetch content from the provided URL") ]
    observed

let test_premature_eof () =
  let observed, _, _ = call ~body:(candidate {|[{"text":"partial"}]|}) [ user "hi" ] in
  (match single_finish observed with
  | `Error m ->
      Alcotest.(check bool)
        "eof without a finish reason is an error" true
        (String.starts_with ~prefix:"stream ended" m)
  | other -> Alcotest.failf "expected an error finish, got %s" (finish_name other));
  Alcotest.(check bool)
    "the error is the last event" true
    (match last observed with Some p -> is_finish p | None -> false)

let test_prompt_blocked () =
  let blocked =
    {|{"promptFeedback":{"blockReason":"SAFETY","safetyRatings":[{"category":"HARM_CATEGORY_DANGEROUS_CONTENT","probability":"HIGH"}]},"usageMetadata":{"promptTokenCount":9}}|}
  in
  let observed, _, _ = call ~body:(sse_data blocked) [ user "hi" ] in
  expect_parts
    [
      Stream_part.Usage
        { Usage.input = 9; output = 0; cache_read = 0; cache_write = 0; reasoning = 0 };
      Stream_part.Finish `Content_filter;
    ]
    observed

let test_prompt_blocked_unknown_reason () =
  let observed, _, _ =
    call ~body:(sse_data {|{"promptFeedback":{"blockReason":"OTHER"}}|}) [ user "hi" ]
  in
  Alcotest.(check string)
    "an unknown block reason is still a filter" "Content_filter"
    (finish_name (single_finish observed))

let test_prompt_feedback_without_block () =
  let observed, _, _ =
    call
      ~body:
        (sse_data {|{"promptFeedback":{"blockReason":"BLOCK_REASON_UNSPECIFIED"}}|}
        ^ candidate {|[{"text":"ok"}]|}
        ^ finished {|"finishReason":"STOP"|})
      [ user "hi" ]
  in
  expect_parts
    [
      Stream_part.Text_delta "ok"; Stream_part.Usage test_usage; Stream_part.Finish `Stop;
    ]
    observed

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
                         Jsont.Json.mem
                           (Jsont.Json.name "description")
                           (Jsont.Json.string "the city");
                       ]);
                ]);
           Jsont.Json.mem (Jsont.Json.name "required")
             (Jsont.Json.list (List.map Jsont.Json.string [ "location" ]));
         ])

let test_encode_system_tools_and_config () =
  let _, _, body =
    call ~system:[ "be terse" ] ~tools:[ tool ] ~reasoning:`High ~temperature:0.25
      [ user "hi" ]
  in
  let json = parse_body body in
  let instruction = object_value "systemInstruction" json in
  check_member_string "system role" "role" "user" instruction;
  check_member_string "system text" "text" "be terse"
    (List.hd (array "parts" instruction));
  let content = List.hd (array "contents" json) in
  check_member_string "content role" "role" "user" content;
  let user_part = List.hd (array "parts" content) in
  check_member_string "content text" "text" "hi" user_part;
  let config = object_value "generationConfig" json in
  Alcotest.(check int)
    "max output tokens" 4096
    (int_of_float (number_value "max output tokens" (required "maxOutputTokens" config)));
  check_member_number "temperature" "temperature" 0.25 config;
  let thinking = object_value "thinkingConfig" config in
  Alcotest.(check bool)
    "include thoughts" true
    (match required "includeThoughts" thinking with
    | Jsont.Bool (value, _) -> value
    | _ -> false);
  Alcotest.(check int)
    "thinking budget" 32768
    (int_of_float (number_value "thinking budget" (required "thinkingBudget" thinking)));
  let declarations =
    List.hd (array "functionDeclarations" (List.hd (array "tools" json)))
  in
  check_member_string "tool name" "name" "weather" declarations;
  check_member_string "tool description" "description" "Get weather" declarations;
  let parameters = object_value "parameters" declarations in
  check_member_string "schema type" "type" "OBJECT" parameters;
  let location = object_value "location" (object_value "properties" parameters) in
  check_member_string "property type" "type" "STRING" location;
  check_member_string "property description" "description" "the city" location;
  check_string_array "required fields" [ "location" ] (required "required" parameters)

let test_encode_conversation () =
  let assistant =
    {
      Message.role = Message.Assistant;
      parts =
        [
          Message.Reasoning { text = "need a tool"; signature = Some "sig_1" };
          Message.Tool_call
            {
              id = "c1";
              name = "weather";
              input =
                Jsont.Json.object'
                  [
                    Jsont.Json.mem (Jsont.Json.name "city") (Jsont.Json.string "Florence");
                  ];
            };
        ];
    }
  in
  let tool_result =
    {
      Message.role = Message.Tool;
      parts =
        [ Message.Tool_result { id = "c1"; name = "weather"; output = `Text "40 C" } ];
    }
  in
  let _, _, body = call [ user "what is the weather"; assistant; tool_result ] in
  let contents = array "contents" (parse_body body) in
  let assistant_content = List.nth contents 1 in
  let function_call =
    object_value "functionCall" (List.hd (array "parts" assistant_content))
  in
  check_member_string "tool call name" "name" "weather" function_call;
  check_member_string "thought signature" "thoughtSignature" "sig_1" function_call;
  let args = object_value "args" function_call in
  check_member_string "tool call argument" "city" "Florence" args;
  let response_content = List.nth contents 2 in
  let function_response =
    object_value "functionResponse" (List.hd (array "parts" response_content))
  in
  check_member_string "tool result id" "id" "c1" function_response;
  check_member_string "tool result name" "name" "weather" function_response;
  check_member_string "tool result text" "result" "40 C"
    (object_value "response" function_response)

let test_encode_file_part () =
  let message =
    {
      Message.role = Message.User;
      parts =
        [ Message.File { mime = "image/png"; data = "aGVsbG8="; name = Some "shot.png" } ];
    }
  in
  let _, _, body = call [ message ] in
  let image =
    object_value "inlineData"
      (List.hd (array "parts" (List.hd (array "contents" (parse_body body)))))
  in
  check_member_string "file mime" "mimeType" "image/png" image;
  check_member_string "file data" "data" "aGVsbG8=" image

let test_encode_without_system_or_tools () =
  let _, _, body = call [ user "hi" ] in
  let json = parse_body body in
  Alcotest.(check bool)
    "no systemInstruction" false
    (Option.is_some (member "systemInstruction" json));
  Alcotest.(check bool) "no tools" false (Option.is_some (member "tools" json));
  let config = object_value "generationConfig" json in
  Alcotest.(check bool)
    "no thinkingConfig" false
    (Option.is_some (member "thinkingConfig" config))

let test_provider_posts_encoded_body () =
  let _, path, body = call ~system:[ "be terse" ] [ user "hi" ] in
  Alcotest.(check (option string))
    "the endpoint is streamGenerateContent"
    (Some "/v1beta/models/gemini-2.5-pro:streamGenerateContent?alt=sse") path;
  let json = parse_body body in
  check_member_string "the posted body carries the system instruction" "text" "be terse"
    (List.hd (array "parts" (object_value "systemInstruction" json)))

let test_provider_premature_eof () =
  let first = candidate {|[{"text":"partial"}]|} in
  let body = first ^ finished {|"finishReason":"STOP"|} in
  let observed, _, _ = call ~body ~trunc_bytes:(String.length first + 8) [ user "hi" ] in
  match single_finish observed with
  | `Error _ -> ()
  | other -> Alcotest.failf "expected an error finish, got %s" (finish_name other)

let cases =
  [
    ("fixture stream", `Quick, test_fixture_stream);
    ("split text accumulation", `Quick, test_split_text_accumulation);
    ("thought marks reasoning", `Quick, test_thought_marks_reasoning);
    ("tool call without id", `Quick, test_tool_call_without_id);
    ("multi part event", `Quick, test_multi_part_event);
    ("finish reasons", `Quick, test_finish_reasons);
    ("usage kept with finish", `Quick, test_usage_kept_with_finish);
    ("malformed event", `Quick, test_malformed_event);
    ("named events ignored", `Quick, test_named_events_ignored);
    ("silence after finish", `Quick, test_silence_after_finish);
    ("premature eof", `Quick, test_premature_eof);
    ("prompt blocked", `Quick, test_prompt_blocked);
    ("prompt blocked unknown reason", `Quick, test_prompt_blocked_unknown_reason);
    ("prompt feedback without block", `Quick, test_prompt_feedback_without_block);
    ("encode system tools config", `Quick, test_encode_system_tools_and_config);
    ("encode conversation", `Quick, test_encode_conversation);
    ("encode file part", `Quick, test_encode_file_part);
    ("encode minimal request", `Quick, test_encode_without_system_or_tools);
    ("provider posts encoded body", `Quick, test_provider_posts_encoded_body);
    ("provider premature eof", `Quick, test_provider_premature_eof);
  ]
