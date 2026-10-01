open Charamel_fantasy
open Stream_test_support
open Lwt.Infix

let sse ?event data =
  let prefix = match event with Some name -> Fmt.str "event: %s\n" name | None -> "" in
  Fmt.str "%sdata: %s\n\n" prefix data

let item_added ~index item =
  sse ~event:"response.output_item.added"
    (Fmt.str {|{"type":"response.output_item.added","item":%s,"output_index":%d}|} item
       index)

let item_done ~index item =
  sse ~event:"response.output_item.done"
    (Fmt.str {|{"type":"response.output_item.done","item":%s,"output_index":%d}|} item
       index)

let arguments_delta ~index delta =
  sse ~event:"response.function_call_arguments.delta"
    (Fmt.str
       {|{"type":"response.function_call_arguments.delta","delta":%S,"output_index":%d}|}
       delta index)

let text_delta ~index delta =
  sse ~event:"response.output_text.delta"
    (Fmt.str
       {|{"type":"response.output_text.delta","delta":%S,"item_id":"msg_x","output_index":%d}|}
       delta index)

let completed ?(reason = "") body =
  let incomplete = if reason = "" then "null" else Fmt.str {|{"reason":"%s"}|} reason in
  sse ~event:"response.completed"
    (Fmt.str
       {|{"type":"response.completed","response":{"id":"resp_x","status":"completed","incomplete_details":%s,%s}}|}
       incomplete body)

let usage_body =
  {|"usage":{"input_tokens":64,"input_tokens_details":{"cached_tokens":10},"output_tokens":26,"output_tokens_details":{"reasoning_tokens":7},"total_tokens":90}|}

let test_usage =
  { Usage.input = 54; output = 26; cache_read = 10; cache_write = 0; reasoning = 7 }

(* Derived from the terminal snapshot of [data/responses_codec.sse], which
   reports input_tokens 54 with input_tokens_details.cached_tokens 10,
   output_tokens 26 and output_tokens_details.reasoning_tokens 64, and a
   total_tokens of 80 = 54 + 26. Because input_tokens already includes the
   cached amount, responses_codec moves the 10 to cache_read and leaves 44 as
   the disjoint prompt count, so the two counters never charge the same
   token: 44 + 10 + 26 = 80 = total_tokens. [reasoning] is copied verbatim
   from output_tokens_details.reasoning_tokens and is never added to
   [output]. *)
let fixture_usage =
  { Usage.input = 44; output = 26; cache_read = 10; cache_write = 0; reasoning = 64 }

let fixture_path = "data/responses_codec.sse"
let read_fixture () = Stream_test_support.read_fixture ~fixture_path ()

let model =
  {
    Model.id = "gpt-4o";
    name = "GPT-4o";
    provider = "openai";
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
let default_body = completed usage_body
let base_url server = Uri.to_string (Fixture_http.uri server "")

let call ?(body = default_body) ?(model = model) ?(reasoning = `Off) ?temperature
    ?max_tokens ?(system = []) ?(tools = []) ?trunc_bytes messages =
  Stream_test_support.with_fixture (fun server ->
      Fixture_http.respond server ?truncate:trunc_bytes body;
      let provider =
        Provider.openai_responses ~base_url:(base_url server)
          ~auth:(Provider.Api_key "test-key") ()
      in
      let stream =
        Provider.stream provider ~clock:Charamel_os.Time.lwt ~model ?temperature
          ?max_tokens ~reasoning ~system ~tools messages
      in
      Stream_test_support.drain stream >>= fun observed ->
      Stream_test_support.drain_queued stream >|= fun tail ->
      (observed @ tail, Fixture_http.last_path server, Fixture_http.last_body server))

let expect_parts expected observed =
  Stream_test_support.expect_parts ~label:"stream parts" expected observed

let test_fixture_stream () =
  call ~body:(read_fixture ()) [ user "hi" ] >|= fun (observed, path, _) ->
  Alcotest.(check (option string)) "posts to /responses" (Some "/responses") path;
  expect_parts
    [
      Stream_part.Reasoning_delta "\n";
      Stream_part.Reasoning_delta "**Mapping the request**";
      Stream_part.Text_delta "The weather in Florence";
      Stream_part.Tool_call_start { id = "call_fixture01"; name = "weather" };
      Stream_part.Tool_input_delta { id = "call_fixture01"; delta = "{\"loc" };
      Stream_part.Tool_input_delta { id = "call_fixture01"; delta = "ation\":" };
      Stream_part.Tool_input_delta { id = "call_fixture01"; delta = "\"Florence\"}" };
      Stream_part.Tool_call_end "call_fixture01";
      Stream_part.Text_delta " is 40 degrees.";
      Stream_part.Usage fixture_usage;
      Stream_part.Finish `Tool_calls;
    ]
    observed

let test_split_argument_accumulation () =
  let opened =
    item_added ~index:0
      {|{"id":"fc_x","type":"function_call","status":"in_progress","arguments":"","call_id":"call_x","name":"weather"}|}
  in
  let closed =
    item_done ~index:0
      {|{"id":"fc_x","type":"function_call","status":"completed","arguments":"{\"location\":\"Florence\"}","call_id":"call_x","name":"weather"}|}
  in
  call
    ~body:
      (opened
      ^ arguments_delta ~index:0 {|{"loc|}
      ^ arguments_delta ~index:0 {|ation":"Flo|}
      ^ arguments_delta ~index:0 {|rence"}|}
      ^ closed ^ completed usage_body)
    [ user "hi" ]
  >|= fun (observed, _, _) ->
  expect_parts
    [
      Stream_part.Tool_call_start { id = "call_x"; name = "weather" };
      Stream_part.Tool_input_delta { id = "call_x"; delta = "{\"loc" };
      Stream_part.Tool_input_delta { id = "call_x"; delta = "ation\":\"Flo" };
      Stream_part.Tool_input_delta { id = "call_x"; delta = "rence\"}" };
      Stream_part.Tool_call_end "call_x";
      Stream_part.Usage test_usage;
      Stream_part.Finish `Tool_calls;
    ]
    observed

let test_interleaved_calls () =
  let open_a =
    item_added ~index:0
      {|{"id":"fc_a","type":"function_call","arguments":"","call_id":"call_a","name":"a"}|}
  in
  let open_b =
    item_added ~index:1
      {|{"id":"fc_b","type":"function_call","arguments":"","call_id":"call_b","name":"b"}|}
  in
  let done_a =
    item_done ~index:0
      {|{"id":"fc_a","type":"function_call","status":"completed","arguments":"A1","call_id":"call_a","name":"a"}|}
  in
  let done_b =
    item_done ~index:1
      {|{"id":"fc_b","type":"function_call","status":"completed","arguments":"B1B2","call_id":"call_b","name":"b"}|}
  in
  call
    ~body:
      (open_a ^ open_b ^ arguments_delta ~index:0 "A1" ^ arguments_delta ~index:1 "B1"
     ^ arguments_delta ~index:1 "B2" ^ done_a ^ done_b ^ completed usage_body)
    [ user "hi" ]
  >|= fun (observed, _, _) ->
  expect_parts
    [
      Stream_part.Tool_call_start { id = "call_a"; name = "a" };
      Stream_part.Tool_call_start { id = "call_b"; name = "b" };
      Stream_part.Tool_input_delta { id = "call_a"; delta = "A1" };
      Stream_part.Tool_input_delta { id = "call_b"; delta = "B1" };
      Stream_part.Tool_input_delta { id = "call_b"; delta = "B2" };
      Stream_part.Tool_call_end "call_a";
      Stream_part.Tool_call_end "call_b";
      Stream_part.Usage test_usage;
      Stream_part.Finish `Tool_calls;
    ]
    observed

let test_finish_reasons () =
  let finish reason =
    call ~body:(completed ~reason usage_body) [ user "hi" ] >|= fun (observed, _, _) ->
    finish_name (single_finish observed)
  in
  finish "" >>= fun reason ->
  Alcotest.check Alcotest.string "stop" "Stop" reason;
  finish "max_output_tokens" >>= fun reason ->
  Alcotest.check Alcotest.string "length" "Length" reason;
  finish "content_filter" >>= fun reason ->
  Alcotest.check Alcotest.string "content filter" "Content_filter" reason;
  Lwt.return_unit

let test_usage_precedes_finish () =
  call ~body:(text_delta ~index:0 "hi" ^ completed usage_body) [ user "hi" ]
  >|= fun (observed, _, _) ->
  Alcotest.(check bool)
    "usage present" true
    (List.exists (function Stream_part.Usage _ -> true | _ -> false) observed);
  Alcotest.check Alcotest.string "finish last" "Stop"
    (finish_name (single_finish observed))

let test_malformed_event () =
  call ~body:(sse ~event:"response.output_text.delta" "not json") [ user "hi" ]
  >>= fun (malformed, _, _) ->
  (match malformed with
  | [ Stream_part.Finish (`Error _) ] -> ()
  | other ->
      Alcotest.failf "expected one error finish, got %a" Fmt.(list Stream_part.pp) other);
  call
    ~body:(sse ~event:"response.output_text.delta" "not json" ^ text_delta ~index:0 "late")
    [ user "hi" ]
  >|= fun (later, _, _) ->
  match later with
  | [ Stream_part.Finish (`Error _) ] -> ()
  | other ->
      Alcotest.failf "malformed stream was not silent afterwards: %a"
        Fmt.(list Stream_part.pp)
        other

let test_premature_eof () =
  call ~body:(text_delta ~index:0 "hi") [ user "hi" ] >|= fun (observed, _, _) ->
  (match last observed with
  | Some (Stream_part.Finish (`Error _)) -> ()
  | _ ->
      Alcotest.failf "expected an error finish at eof, got %a"
        Fmt.(list Stream_part.pp)
        observed);
  Alcotest.(check bool)
    "no usage on incomplete stream" true
    (not (List.exists (function Stream_part.Usage _ -> true | _ -> false) observed))

let test_silence_after_finish () =
  call ~body:(completed usage_body ^ text_delta ~index:0 "late") [ user "hi" ]
  >|= fun (observed, _, _) ->
  expect_parts [ Stream_part.Usage test_usage; Stream_part.Finish `Stop ] observed

(* input1000/cached600/cache_write300/output100 must land as a disjoint
   {input=100; cache_read=600; cache_write=300; output=100}: input excludes
   both the cache read and the cache write buckets, and an absent
   [output_tokens_details] still defaults [reasoning] to [0]. *)
let cache_write_usage_body =
  {|"usage":{"input_tokens":1000,"input_tokens_details":{"cached_tokens":600,"cache_write_tokens":300},"output_tokens":100}|}

let test_cache_write_tokens () =
  call ~body:(completed cache_write_usage_body) [ user "hi" ] >|= fun (observed, _, _) ->
  expect_parts
    [
      Stream_part.Usage
        {
          Usage.input = 100;
          output = 100;
          cache_read = 600;
          cache_write = 300;
          reasoning = 0;
        };
      Stream_part.Finish `Stop;
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
                       ]);
                ]);
           Jsont.Json.mem (Jsont.Json.name "required")
             (Jsont.Json.list [ Jsont.Json.string "location" ]);
         ])

let test_encode_system_tools_and_config () =
  call
    ~system:[ "You are a helpful assistant" ]
    ~tools:[ tool ] ~temperature:0.25
    [ user "hi" ]
  >|= fun (_, _, body) ->
  let json = parse_body body in
  let input = array "input" json in
  let system = List.hd input in
  check_member_string "system role" "role" "system" system;
  check_member_string "system text" "content" "You are a helpful assistant" system;
  let user_item = List.nth input 1 in
  check_member_string "user role" "role" "user" user_item;
  let user_part = List.hd (array "content" user_item) in
  check_member_string "user text type" "type" "input_text" user_part;
  check_member_string "user text" "text" "hi" user_part;
  let tool_item = List.hd (array "tools" json) in
  check_member_string "tool type" "type" "function" tool_item;
  check_member_string "tool name" "name" "weather" tool_item;
  check_member_string "tool description" "description" "Get weather" tool_item;
  let parameters = object_value "parameters" tool_item in
  check_member_string "schema type" "type" "object" parameters;
  check_member_string "property type" "type" "string"
    (object_value "location" (object_value "properties" parameters));
  check_member_string "tool choice" "tool_choice" "auto" json;
  Alcotest.(check int)
    "max output tokens" 4096
    (int_of_float (number_value "max output tokens" (required "max_output_tokens" json)));
  check_member_number "temperature" "temperature" 0.25 json;
  check_bool_member "stream" "stream" true json;
  check_bool_member "store" "store" false json

let test_encode_reasoning_model () =
  let reasoning_model = { model with Model.id = "o3-mini" } in
  call ~model:reasoning_model ~reasoning:`Medium ~system:[ "be terse" ] ~temperature:0.25
    [ user "hi" ]
  >|= fun (_, _, body) ->
  let json = parse_body body in
  let input = List.hd (array "input" json) in
  check_member_string "reasoning system role" "role" "developer" input;
  let reasoning = object_value "reasoning" json in
  check_member_string "reasoning effort" "effort" "medium" reasoning;
  Alcotest.(check bool)
    "temperature dropped for a reasoning model" false
    (Option.is_some (member "temperature" json))

let test_encode_o1_mini_removes_system () =
  call ~model:{ model with Model.id = "o1-mini" } ~system:[ "dropped" ] [ user "hi" ]
  >|= fun (_, _, body) ->
  let json = parse_body body in
  let input = array "input" json in
  Alcotest.(check int) "only user input remains" 1 (List.length input);
  check_member_string "user input role" "role" "user" (List.hd input);
  Alcotest.(check bool)
    "system text is absent" false
    (List.exists
       (fun item ->
         match member "content" item with
         | Some (Jsont.String (value, _)) -> value = "dropped"
         | _ -> false)
       input)

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
  call [ user "what is the weather"; assistant; tool_result ] >|= fun (_, _, body) ->
  let input = array "input" (parse_body body) in
  let user_item = List.nth input 0 in
  let user_part = List.hd (array "content" user_item) in
  check_member_string "replayed user role" "role" "user" user_item;
  check_member_string "replayed user text type" "type" "input_text" user_part;
  check_member_string "replayed user text" "text" "what is the weather" user_part;
  let function_call = List.nth input 1 in
  check_member_string "tool call type" "type" "function_call" function_call;
  check_member_string "tool call id" "call_id" "c1" function_call;
  check_member_string "tool call name" "name" "weather" function_call;
  check_member_string "tool call arguments" "arguments" {|{"city":"Florence"}|}
    function_call;
  let function_output = List.nth input 2 in
  check_member_string "tool output type" "type" "function_call_output" function_output;
  check_member_string "tool output id" "call_id" "c1" function_output;
  check_member_string "tool output text" "output" "40 C" function_output;
  Alcotest.(check bool)
    "reasoning parts are skipped on replay" false
    (List.exists
       (fun item ->
         match member "content" item with
         | Some (Jsont.String (value, _)) -> value = "need a tool"
         | _ -> false)
       input)

let test_encode_file_part () =
  let message =
    {
      Message.role = Message.User;
      parts =
        [ Message.File { mime = "image/png"; data = "aGVsbG8="; name = Some "shot.png" } ];
    }
  in
  call [ message ] >|= fun (_, _, body) ->
  let user_item = List.hd (array "input" (parse_body body)) in
  let image = List.hd (array "content" user_item) in
  check_member_string "file part type" "type" "input_image" image;
  check_member_string "file data URL" "image_url" "data:image/png;base64,aGVsbG8=" image

let test_encode_media_tool_result () =
  let message =
    {
      Message.role = Message.Tool;
      parts =
        [
          Message.Tool_result
            { id = "c2"; name = "camera"; output = `Media ("image/png", "aGk=") };
        ];
    }
  in
  call [ message ] >|= fun (_, _, body) ->
  let input = array "input" (parse_body body) in
  let output_item = List.nth input 0 in
  check_member_string "media output type" "type" "function_call_output" output_item;
  check_member_string "media output id" "call_id" "c2" output_item;
  check_member_string "media output placeholder" "output"
    "The tool returned image/png content; see the following user message." output_item;
  let image_item = List.nth input 1 in
  check_member_string "media user role" "role" "user" image_item;
  check_member_string "media input type" "type" "input_image"
    (List.hd (array "content" image_item));
  check_member_string "media data URL" "image_url" "data:image/png;base64,aGk="
    (List.hd (array "content" image_item))

let test_provider_posts_encoded_body () =
  call ~system:[ "be terse" ] [ user "hi" ] >|= fun (_, path, body) ->
  Alcotest.(check (option string))
    "the endpoint is the Responses API" (Some "/responses") path;
  let json = parse_body body in
  check_bool_member "the posted body streams" "stream" true json;
  check_member_string "the posted body carries the system role" "role" "system"
    (List.hd (array "input" json))

let test_provider_premature_eof () =
  let first = text_delta ~index:0 "partial" in
  let body = first ^ completed usage_body in
  call ~body ~trunc_bytes:(String.length first + 8) [ user "hi" ]
  >|= fun (observed, _, _) ->
  match single_finish observed with
  | `Error _ -> ()
  | other -> Alcotest.failf "expected an error finish, got %s" (finish_name other)

let cases =
  [
    Alcotest_lwt.test_case "fixture stream" `Quick (fun _switch () ->
        test_fixture_stream ());
    Alcotest_lwt.test_case "split argument accumulation" `Quick (fun _switch () ->
        test_split_argument_accumulation ());
    Alcotest_lwt.test_case "interleaved calls" `Quick (fun _switch () ->
        test_interleaved_calls ());
    Alcotest_lwt.test_case "finish reasons" `Quick (fun _switch () ->
        test_finish_reasons ());
    Alcotest_lwt.test_case "usage precedes finish" `Quick (fun _switch () ->
        test_usage_precedes_finish ());
    Alcotest_lwt.test_case "malformed event" `Quick (fun _switch () ->
        test_malformed_event ());
    Alcotest_lwt.test_case "premature eof" `Quick (fun _switch () ->
        test_premature_eof ());
    Alcotest_lwt.test_case "silence after finish" `Quick (fun _switch () ->
        test_silence_after_finish ());
    Alcotest_lwt.test_case "cache write tokens" `Quick (fun _switch () ->
        test_cache_write_tokens ());
    Alcotest_lwt.test_case "encode system tools config" `Quick (fun _switch () ->
        test_encode_system_tools_and_config ());
    Alcotest_lwt.test_case "encode reasoning model" `Quick (fun _switch () ->
        test_encode_reasoning_model ());
    Alcotest_lwt.test_case "encode o1 mini removes system" `Quick (fun _switch () ->
        test_encode_o1_mini_removes_system ());
    Alcotest_lwt.test_case "encode conversation" `Quick (fun _switch () ->
        test_encode_conversation ());
    Alcotest_lwt.test_case "encode file part" `Quick (fun _switch () ->
        test_encode_file_part ());
    Alcotest_lwt.test_case "encode media tool result" `Quick (fun _switch () ->
        test_encode_media_tool_result ());
    Alcotest_lwt.test_case "provider posts encoded body" `Quick (fun _switch () ->
        test_provider_posts_encoded_body ());
    Alcotest_lwt.test_case "provider premature eof" `Quick (fun _switch () ->
        test_provider_premature_eof ());
  ]
