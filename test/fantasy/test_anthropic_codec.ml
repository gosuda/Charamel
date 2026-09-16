open Charm_fantasy
open Stream_test_support

let fixture_body path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let sse = fixture_body "data/anthropic_codec.sse"

let model =
  {
    Model.id = "claude-sonnet-4-20250514";
    name = "Claude Sonnet 4";
    provider = "anthropic";
    context_window = 200_000;
    default_max_tokens = 4096;
    can_reason = true;
    supports_attachments = false;
    cost_in = 0.;
    cost_out = 0.;
    cost_cache_read = 0.;
    cost_cache_write = 0.;
  }

let api_key = Provider.Api_key "test-key"
let text role s = { Message.role; parts = [ Message.Text s ] }

let usage ~input ~output ~cache_read ~cache_write =
  Stream_part.Usage { Usage.input; output; cache_read; cache_write; reasoning = 0 }

let call ?(body = sse) ?(auth = api_key) ?(reasoning = `Off) ?temperature ?max_tokens
    ?(system = []) ?(tools = []) ?truncate messages =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let server = Fixture_server.start ~sw ~net:env#net () in
  Fixture_server.respond server body;
  (match truncate with Some n -> Fixture_server.trunc server n | None -> ());
  let provider = Provider.anthropic ~base_url:(Fixture_server.base_url server) ~auth () in
  let parts =
    Provider.stream provider ~sw ~clock:env#clock ~net:env#net ~model ?temperature
      ?max_tokens ~reasoning ~system ~tools messages
  in
  let observed = drain parts in
  (observed, Fixture_server.last_path server, Fixture_server.last_body server)

let expect_parts name expected observed = Alcotest.(check parts) name expected observed

let parse_body = function
  | None -> Alcotest.fail "no request body was posted"
  | Some s -> (
      match Jsont_bytesrw.decode_string Jsont.json s with
      | Ok j -> j
      | Error e -> Alcotest.failf "posted body is not JSON: %s" e)

let mem (j : Jsont.json) k =
  match j with
  | Jsont.Object (ms, _) -> (
      match Jsont.Json.find_mem k ms with Some (_, v) -> Some v | None -> None)
  | _ -> None

let str_mem k j = match mem j k with Some (Jsont.String (s, _)) -> Some s | _ -> None
let number_mem k j = match mem j k with Some (Jsont.Number (f, _)) -> Some f | _ -> None
let has_cache (j : Jsont.json) = mem j "cache_control" <> None

let last_of = function
  | Jsont.Array (items, _) -> List.nth_opt items (List.length items - 1)
  | _ -> None

let test_fixture_stream () =
  let parts, path, _ = call [ text Message.User "hi" ] in
  Alcotest.(check (option string)) "posts to /v1/messages" (Some "/v1/messages") path;
  expect_parts "fixture decodes through the public provider"
    [
      Stream_part.Text_delta "Hello";
      Stream_part.Text_delta ", world";
      Stream_part.Reasoning_delta "first";
      Stream_part.Tool_call_start { id = "toolu_1"; name = "ping" };
      Stream_part.Tool_input_delta { id = "toolu_1"; delta = "{\"a\":" };
      Stream_part.Tool_input_delta { id = "toolu_1"; delta = "1}" };
      Stream_part.Tool_call_end "toolu_1";
      usage ~input:10 ~output:7 ~cache_read:3 ~cache_write:2;
      Stream_part.Finish `Tool_calls;
    ]
    parts

let split_tool_body =
  {|event: message_start
data: {"type":"message_start","message":{"usage":{"input_tokens":1}}}

event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"a","name":"t","input":{}}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"x\":1,\"y\":["}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"2]}"}}

event: content_block_stop
data: {"type":"content_block_stop","index":0}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":2}}

event: message_stop
data: {"type":"message_stop"}

|}

let test_split_tool_accumulation () =
  let parts, _, _ = call ~body:split_tool_body [ text Message.User "hi" ] in
  expect_parts "split deltas accumulate on one id"
    [
      Stream_part.Tool_call_start { id = "a"; name = "t" };
      Stream_part.Tool_input_delta { id = "a"; delta = "{\"x\":1,\"y\":[" };
      Stream_part.Tool_input_delta { id = "a"; delta = "2]}" };
      Stream_part.Tool_call_end "a";
      usage ~input:1 ~output:2 ~cache_read:0 ~cache_write:0;
      Stream_part.Finish `Tool_calls;
    ]
    parts;
  let json =
    List.filter_map
      (function Stream_part.Tool_input_delta { delta; _ } -> Some delta | _ -> None)
      parts
    |> String.concat ""
  in
  Alcotest.(check string) "arguments reassemble" {|{"x":1,"y":[2]}|} json

let interleaved_body =
  {|event: message_start
data: {"type":"message_start","message":{"usage":{"input_tokens":5}}}

event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":""}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"checking"}}

event: content_block_stop
data: {"type":"content_block_stop","index":0}

event: content_block_start
data: {"type":"content_block_start","index":1,"content_block":{"type":"text","text":""}}

event: content_block_delta
data: {"type":"content_block_delta","index":1,"delta":{"type":"text_delta","text":"I will call both tools."}}

event: content_block_stop
data: {"type":"content_block_stop","index":1}

event: content_block_start
data: {"type":"content_block_start","index":2,"content_block":{"type":"tool_use","id":"toolu_b","name":"t2","input":{}}}

event: content_block_delta
data: {"type":"content_block_delta","index":2,"delta":{"type":"input_json_delta","partial_json":"{}"}}

event: content_block_stop
data: {"type":"content_block_stop","index":2}

event: content_block_start
data: {"type":"content_block_start","index":3,"content_block":{"type":"tool_use","id":"toolu_c","name":"t3","input":{}}}

event: content_block_delta
data: {"type":"content_block_delta","index":3,"delta":{"type":"input_json_delta","partial_json":"{\"n\":1}"}}

event: content_block_stop
data: {"type":"content_block_stop","index":3}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"tool_use"},"usage":{"output_tokens":9}}

event: message_stop
data: {"type":"message_stop"}

|}

let test_interleaved_blocks () =
  let parts, _, _ = call ~body:interleaved_body [ text Message.User "hi" ] in
  expect_parts "thinking, text and two tool blocks arrive in stream order"
    [
      Stream_part.Reasoning_delta "checking";
      Stream_part.Text_delta "I will call both tools.";
      Stream_part.Tool_call_start { id = "toolu_b"; name = "t2" };
      Stream_part.Tool_input_delta { id = "toolu_b"; delta = "{}" };
      Stream_part.Tool_call_end "toolu_b";
      Stream_part.Tool_call_start { id = "toolu_c"; name = "t3" };
      Stream_part.Tool_input_delta { id = "toolu_c"; delta = "{\"n\":1}" };
      Stream_part.Tool_call_end "toolu_c";
      usage ~input:5 ~output:9 ~cache_read:0 ~cache_write:0;
      Stream_part.Finish `Tool_calls;
    ]
    parts

(* The Messages API closes a content block before opening the next one, so a
   stream whose events cross indices is not something the endpoint emits. A
   middlebox or a resumed stream can reorder them, and the decoder must then
   keep serving its documented contract: emit each delta as it arrives and
   resolve every index to the block that opened it. The body below closes the
   higher index first and resumes the lower one afterwards, which fails any
   decoder that tracks a single "current" block. *)
let out_of_order_body =
  {|event: content_block_start
data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}

event: content_block_start
data: {"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"toolu_a","name":"t1","input":{}}}

event: content_block_delta
data: {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\"x\":1"}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"mid"}}

event: content_block_start
data: {"type":"content_block_start","index":2,"content_block":{"type":"tool_use","id":"toolu_b","name":"t2","input":{}}}

event: content_block_delta
data: {"type":"content_block_delta","index":2,"delta":{"type":"input_json_delta","partial_json":"{\"y\":2}"}}

event: content_block_stop
data: {"type":"content_block_stop","index":2}

event: content_block_delta
data: {"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":",\"z\":3}"}}

event: content_block_stop
data: {"type":"content_block_stop","index":1}

event: content_block_stop
data: {"type":"content_block_stop","index":0}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":4}}

event: message_stop
data: {"type":"message_stop"}

|}

let test_out_of_order_blocks () =
  let parts, _, _ = call ~body:out_of_order_body [ text Message.User "hi" ] in
  expect_parts "each index keeps the id of the block that opened it"
    [
      Stream_part.Tool_call_start { id = "toolu_a"; name = "t1" };
      Stream_part.Tool_input_delta { id = "toolu_a"; delta = "{\"x\":1" };
      Stream_part.Text_delta "mid";
      Stream_part.Tool_call_start { id = "toolu_b"; name = "t2" };
      Stream_part.Tool_input_delta { id = "toolu_b"; delta = "{\"y\":2}" };
      Stream_part.Tool_call_end "toolu_b";
      Stream_part.Tool_input_delta { id = "toolu_a"; delta = ",\"z\":3}" };
      Stream_part.Tool_call_end "toolu_a";
      usage ~input:0 ~output:4 ~cache_read:0 ~cache_write:0;
      Stream_part.Finish `Stop;
    ]
    parts;
  let arguments =
    parts
    |> List.filter_map (function
      | Stream_part.Tool_input_delta { id; delta } when id = "toolu_a" -> Some delta
      | _ -> None)
    |> String.concat ""
  in
  Alcotest.(check string)
    "the resumed block reassembles alone" {|{"x":1,"z":3}|} arguments

let malformed_body =
  {|event: message_start
data: {"type":"message_start","message":{}}

event: content_block_delta
data: not json at all

|}

let test_malformed_event () =
  let parts, _, _ = call ~body:malformed_body [ text Message.User "hi" ] in
  match parts with
  | [ Stream_part.Finish (`Error m) ] ->
      Alcotest.(check bool)
        "names the failure" true
        (String.length m >= 22 && String.sub m 0 22 = "malformed stream event")
  | _ -> Alcotest.failf "expected one error finish, got %d parts" (List.length parts)

let premature_body =
  {|event: message_start
data: {"type":"message_start","message":{"usage":{"input_tokens":4}}}

event: content_block_delta
data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"partial"}}

|}

let test_premature_eof () =
  let parts, _, _ =
    call ~body:premature_body
      ~truncate:(String.length premature_body - 1)
      [ text Message.User "hi" ]
  in
  expect_parts "EOF before message_stop keeps usage and reports an error"
    [
      Stream_part.Text_delta "partial";
      usage ~input:4 ~output:0 ~cache_read:0 ~cache_write:0;
      Stream_part.Finish (`Error "stream closed before message_stop");
    ]
    parts

(* Anthropic reports [usage] on [message_start] and every [message_delta] as
   a cumulative snapshot, not an incremental delta: a present field replaces
   the previously reported value for that field, and a field absent from a
   later snapshot preserves whatever the last snapshot reported. Two
   [output_tokens] snapshots (60, then 100) must settle on 100, never on
   their sum, while the [input_tokens]/cache fields reported only at
   [message_start] must survive untouched. *)
let cumulative_usage_body =
  {|event: message_start
data: {"type":"message_start","message":{"usage":{"input_tokens":100,"output_tokens":1,"cache_read_input_tokens":600,"cache_creation_input_tokens":300}}}

event: message_delta
data: {"type":"message_delta","usage":{"output_tokens":60}}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":100}}

event: message_stop
data: {"type":"message_stop"}

|}

let test_cumulative_usage_replaces_present_fields () =
  let parts, _, _ = call ~body:cumulative_usage_body [ text Message.User "hi" ] in
  expect_parts "later message_delta snapshots replace output and preserve the rest"
    [
      usage ~input:100 ~output:100 ~cache_read:600 ~cache_write:300;
      Stream_part.Finish `Stop;
    ]
    parts

(* A field explicitly reported as [0] in a later snapshot replaces a
   previously nonzero value: it is a reported fact, not silence. A field
   never mentioned again ([input_tokens], [output_tokens] here) keeps the
   value [message_start] reported. *)
let zero_replaces_usage_body =
  {|event: message_start
data: {"type":"message_start","message":{"usage":{"input_tokens":50,"output_tokens":5,"cache_creation_input_tokens":20}}}

event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"cache_creation_input_tokens":0}}

event: message_stop
data: {"type":"message_stop"}

|}

let test_cumulative_usage_explicit_zero_replaces () =
  let parts, _, _ = call ~body:zero_replaces_usage_body [ text Message.User "hi" ] in
  expect_parts "an explicit zero replaces a previously reported cache-write count"
    [ usage ~input:50 ~output:5 ~cache_read:0 ~cache_write:0; Stream_part.Finish `Stop ]
    parts

let stop_reason_body reason =
  Fmt.str
    {|event: message_delta
data: {"type":"message_delta","delta":{"stop_reason":"%s"}}

event: message_stop
data: {"type":"message_stop"}

|}
    reason

let stop_of ~body () =
  let parts, _, _ = call ~body [ text Message.User "hi" ] in
  match parts with
  | [ Stream_part.Finish f ] -> finish_name f
  | _ -> Alcotest.failf "expected exactly one finish, got %d parts" (List.length parts)

let test_stop_reasons () =
  Alcotest.(check string)
    "end_turn stops" "Stop"
    (stop_of ~body:(stop_reason_body "end_turn") ());
  Alcotest.(check string)
    "max_tokens is a length stop" "Length"
    (stop_of ~body:(stop_reason_body "max_tokens") ());
  Alcotest.(check string)
    "refusal is a content filter stop" "Content_filter"
    (stop_of ~body:(stop_reason_body "refusal") ())

let tool =
  Tool.v ~name:"read" ~description:"read a file"
    ~schema:
      Jsont.Json.(
        object'
          [ mem (name "type") (string "object"); mem (name "properties") (object' []) ])

let test_request_encoding () =
  let _, path, body =
    call ~system:[ "be terse" ] ~tools:[ tool ]
      [ text Message.User "one"; text Message.User "two"; text Message.User "three" ]
  in
  Alcotest.(check (option string)) "messages endpoint" (Some "/v1/messages") path;
  let j = parse_body body in
  Alcotest.(check (option string)) "model id" (Some model.Model.id) (str_mem "model" j);
  Alcotest.(check bool)
    "streams" true
    (match mem j "stream" with Some (Jsont.Bool (true, _)) -> true | _ -> false);
  (match mem j "system" with
  | Some blocks -> (
      match last_of blocks with
      | Some b when has_cache b -> ()
      | _ -> Alcotest.fail "last system block has no cache_control")
  | None -> Alcotest.fail "system is missing");
  (match mem j "messages" with
  | Some (Jsont.Array ([ message ], _)) -> (
      match mem message "content" with
      | Some content -> (
          match last_of content with
          | Some item when has_cache item -> ()
          | _ -> Alcotest.fail "last merged user content has no cache_control")
      | None -> Alcotest.fail "merged message has no content")
  | Some (Jsont.Array (_, _)) -> Alcotest.fail "adjacent turns were not merged"
  | _ -> Alcotest.fail "messages is not an array");
  (match mem j "tools" with
  | Some (Jsont.Array ([ tool_json ], _)) -> (
      Alcotest.(check (option string))
        "tool name" (Some "read") (str_mem "name" tool_json);
      Alcotest.(check (option string))
        "tool description" (Some "read a file")
        (str_mem "description" tool_json);
      match mem tool_json "input_schema" with
      | Some (Jsont.Object _) -> ()
      | _ -> Alcotest.fail "tool schema is missing")
  | _ -> Alcotest.fail "tools are missing");
  let oauth =
    Provider.Oauth
      {
        Oauth.Credential.access = "access";
        refresh = "refresh";
        expires_at_ms = max_int;
        account = None;
      }
  in
  let _, _, oauth_body =
    call ~auth:oauth ~system:[ "be terse" ] [ text Message.User "hi" ]
  in
  let oauth_json = parse_body oauth_body in
  match mem oauth_json "system" with
  | Some (Jsont.Array ([ first; last ], _)) ->
      Alcotest.(check (option string))
        "OAuth system prefix"
        (Some "You are Claude Code, Anthropic's official CLI for Claude.")
        (str_mem "text" first);
      Alcotest.(check bool) "OAuth system suffix is cacheable" true (has_cache last)
  | Some (Jsont.Array (_, _)) -> Alcotest.fail "OAuth prefix changed system block count"
  | _ -> Alcotest.fail "OAuth system is missing"

(* The endpoint rejects a [budget_tokens] that reaches [max_tokens], so the
   encoder clamps the requested level to [max_tokens - 1]. The model fixture
   declares [default_max_tokens = 4096], so a request that does not override
   [max_tokens] cannot carry the full medium budget: asserting 8192 there
   would ask for the impossible. Both sides are pinned: the level survives
   when the window admits it and is capped when it does not. *)
let thinking_field ?max_tokens reasoning =
  let _, _, body = call ?max_tokens ~reasoning [ text Message.User "hi" ] in
  let j = parse_body body in
  match mem j "thinking" with
  | Some t ->
      Alcotest.(check (option string))
        "thinking is enabled" (Some "enabled") (str_mem "type" t);
      (number_mem "budget_tokens" t, number_mem "max_tokens" j)
  | None -> Alcotest.fail "no thinking block"

let test_thinking_budget () =
  Alcotest.(check (option (float 0.)))
    "a wide window keeps the medium budget" (Some 8192.)
    (fst (thinking_field ~max_tokens:16_000 `Medium));
  (match thinking_field `Medium with
  | Some budget, Some cap ->
      Alcotest.(check (float 0.)) "the default window caps the budget" 4095. budget;
      Alcotest.(check (float 0.)) "the cap is the declared default" 4096. cap;
      Alcotest.(check bool)
        "the budget stays strictly below max_tokens" true (budget < cap)
  | None, _ -> Alcotest.fail "the medium request carries no budget_tokens"
  | Some _, None -> Alcotest.fail "the request carries no max_tokens");
  Alcotest.(check (option (float 0.)))
    "a low level needs no clamp" (Some 1024.)
    (fst (thinking_field ~max_tokens:16_000 `Low));
  Alcotest.(check (option (float 0.)))
    "a high level is clamped too" (Some 2047.)
    (fst (thinking_field ~max_tokens:2048 `High));
  (* A window of one token leaves no positive budget, so the block is omitted
     rather than sent at zero, which the API rejects. *)
  let _, _, tiny = call ~max_tokens:1 ~reasoning:`Medium [ text Message.User "hi" ] in
  Alcotest.(check bool)
    "an unfillable window drops thinking" true
    (mem (parse_body tiny) "thinking" = None)

let test_temperature_and_thinking () =
  let _, _, body = call ~reasoning:`Low ~temperature:0.5 [ text Message.User "hi" ] in
  let j = parse_body body in
  Alcotest.(check bool)
    "temperature is omitted with thinking" true
    (mem j "temperature" = None);
  let _, _, body = call ~temperature:0.5 [ text Message.User "hi" ] in
  let j = parse_body body in
  Alcotest.(check (option (float 0.)))
    "temperature is sent without thinking" (Some 0.5) (number_mem "temperature" j)

let cases =
  [
    ("fixture stream", `Quick, test_fixture_stream);
    ("split tool accumulation", `Quick, test_split_tool_accumulation);
    ("interleaved blocks", `Quick, test_interleaved_blocks);
    ("out-of-order blocks", `Quick, test_out_of_order_blocks);
    ("malformed event", `Quick, test_malformed_event);
    ("premature eof", `Quick, test_premature_eof);
    ( "cumulative usage replaces present fields",
      `Quick,
      test_cumulative_usage_replaces_present_fields );
    ( "cumulative usage explicit zero replaces",
      `Quick,
      test_cumulative_usage_explicit_zero_replaces );
    ("stop reasons", `Quick, test_stop_reasons);
    ("request encoding", `Quick, test_request_encoding);
    ("thinking budget", `Quick, test_thinking_budget);
    ("temperature and thinking", `Quick, test_temperature_and_thinking);
  ]
