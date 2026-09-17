(* Google Gemini [streamGenerateContent] codec.

   Wire sources, all under [.references/fantasy]:
   - [providers/google/google.go] [prepareParams] and [toGooglePrompt] for the
     request body, [languageModel.Stream] for the event decoding order,
     [mapUsage] and [mapFinishReason] for the two mappings.
   - [providertests/testdata/TestGoogle*/] recorded responses: the ground
     truth for member names ([thoughtsTokenCount], [candidatesTokenCount]),
     for [thought: true] marking reasoning text, and for [thoughtSignature]
     arriving as a member of the part it signs.

   The [data] of a [File] part and the [signature] of a [Reasoning] part
   already hold the provider wire form ([lib/fantasy/message.mli]), so both
   are copied into the request verbatim; re-encoding would corrupt them. *)

let log_src = Logs.Src.create "charm.fantasy.google_codec"

module Log = (val Logs.src_log log_src : Logs.LOG)
open Json

(* Request: schema conversion *)

(* Gemini rejects most of the JSON Schema dialect tools ship with, so the
   properties subtree is transcribed into [genai.Schema] and unknown "type"
   values fall back to "STRING", as [mapJSONTypeToGoogle] does
   (google.go:1303-1320). *)
let schema_type s =
  match String.lowercase_ascii s with
  | "string" -> "STRING"
  | "number" -> "NUMBER"
  | "integer" -> "INTEGER"
  | "boolean" -> "BOOLEAN"
  | "array" -> "ARRAY"
  | "object" -> "OBJECT"
  | _ -> "STRING"

let object_members j =
  match j with
  | Jsont.Object (o, _) ->
      List.filter_map (fun (k, v) -> if is_object v then Some (k, v) else None) o
  | _ -> []

let members_of j = match j with Jsont.Object (o, _) -> o | _ -> []

let rec convert_schema j =
  let typ = match string_mem j "type" with Some s -> schema_type s | None -> "STRING" in
  let base () =
    let t = (n "type", str typ) in
    match string_mem j "description" with
    | Some d -> obj [ t; (n "description", str d) ]
    | None -> obj [ t ]
  in
  match typ with
  | "ARRAY" -> (
      match oopt j "items" with
      | Some v -> obj ((n "items", convert_schema v) :: members_of (base ()))
      | None -> base ())
  | "OBJECT" -> (
      match oopt j "properties" with
      | Some v when is_object v ->
          obj ((n "properties", obj (properties_of v)) :: members_of (base ()))
      | _ -> base ())
  | _ -> base ()

and properties_of j = List.map (fun (k, v) -> (k, convert_schema v)) (object_members j)

let google_schema (tool : Tool.t) =
  let properties =
    match oopt tool.Tool.schema "properties" with
    | Some v when is_object v -> properties_of v
    | _ -> []
  in
  let required =
    match oopt tool.Tool.schema "required" with
    | Some v -> ( match strings_of_json v with Some l -> l | None -> [])
    | None -> []
  in
  let parameters =
    (n "type", str "OBJECT")
    :: (n "properties", obj properties)
    :: (if required = [] then [] else [ (n "required", arr (List.map str required)) ])
  in
  obj
    [
      (n "name", str tool.Tool.name);
      (n "description", str tool.Tool.description);
      (n "parameters", obj parameters);
    ]

(* Request: contents *)

(* google.go:435-475 carries the signature of a reasoning block on the part
   that follows it: the signature is a member of the text or function-call
   part it signs, never a part of its own. *)
let signature_mem = function Some s -> [ (n "thoughtSignature", str s) ] | None -> []
let text_part ?signature s = obj ((n "text", str s) :: signature_mem signature)

let inline_data_part ~mime ~data =
  obj [ (n "inlineData", obj [ (n "mimeType", str mime); (n "data", str data) ]) ]

let function_call_part ?signature id name args =
  let idm = match id with Some i -> [ (n "id", str i) ] | None -> [] in
  let call =
    ((n "name", str name) :: (n "args", args) :: idm) @ signature_mem signature
  in
  obj [ (n "functionCall", obj call) ]

let function_response_part id name output =
  let response =
    match output with
    | `Text s | `Error s -> obj [ (n "result", str s) ]
    | `Media (mime, data) -> obj [ (n "result", str data); (n "media_type", str mime) ]
  in
  obj
    [
      ( n "functionResponse",
        obj [ (n "id", str id); (n "name", str name); (n "response", response) ] );
    ]

let rec assistant_parts ~signature acc = function
  | [] -> List.rev acc
  | Message.Text s :: tl when s <> "" ->
      assistant_parts ~signature:None (text_part ?signature s :: acc) tl
  | Message.Tool_call { id; name; input } :: tl ->
      let part = function_call_part ?signature (Some id) name input in
      assistant_parts ~signature:None (part :: acc) tl
  | Message.Reasoning { signature = Some s; _ } :: tl ->
      assistant_parts ~signature:(Some s) acc tl
  | _ :: tl -> assistant_parts ~signature acc tl

let user_parts (ps : Message.part list) =
  List.filter_map
    (fun p ->
      match p with
      | Message.Text s when s <> "" -> Some (text_part s)
      | Message.File { mime; data; name = _ } -> Some (inline_data_part ~mime ~data)
      | Message.Text _ | Message.Reasoning _ | Message.Tool_call _ | Message.Tool_result _
        ->
          None)
    ps

let tool_parts (ps : Message.part list) =
  List.filter_map
    (fun p ->
      match p with
      | Message.Tool_result { id; name; output } ->
          Some (function_response_part id name output)
      | Message.Text _ | Message.File _ | Message.Reasoning _ | Message.Tool_call _ ->
          None)
    ps

let content_of_message (m : Message.t) =
  let role = match m.Message.role with Message.Assistant -> "model" | _ -> "user" in
  let parts =
    match m.Message.role with
    | Message.System -> []
    | Message.User -> user_parts m.Message.parts
    | Message.Assistant -> assistant_parts ~signature:None [] m.Message.parts
    | Message.Tool -> tool_parts m.Message.parts
  in
  if parts = [] then None else Some (obj [ (n "role", str role); (n "parts", arr parts) ])

let contents msgs = arr (List.filter_map content_of_message msgs)

let system_instruction = function
  | [] -> []
  | blocks ->
      [
        ( n "systemInstruction",
          obj
            [
              (n "role", str "user");
              (n "parts", arr [ text_part (String.concat "\n" blocks) ]);
            ] );
      ]

(* Request: generation config *)

let thinking_config = function
  | Request.Off -> None
  | Request.Low ->
      Some [ (n "includeThoughts", bool true); (n "thinkingBudget", int 1024) ]
  | Request.Medium ->
      Some [ (n "includeThoughts", bool true); (n "thinkingBudget", int 8192) ]
  | Request.High ->
      Some [ (n "includeThoughts", bool true); (n "thinkingBudget", int 32768) ]

let generation_config (r : Request.t) =
  let cap =
    if r.Request.max_tokens > 0 then r.Request.max_tokens
    else r.Request.model.Model.default_max_tokens
  in
  let temperature =
    match r.Request.temperature with Some t -> [ (n "temperature", num t) ] | None -> []
  in
  let thinking =
    match thinking_config r.Request.reasoning with
    | Some c -> [ (n "thinkingConfig", obj c) ]
    | None -> []
  in
  obj (((n "maxOutputTokens", int cap) :: temperature) @ thinking)

let tools_member = function
  | [] -> []
  | ts ->
      [
        ( n "tools",
          arr [ obj [ (n "functionDeclarations", arr (List.map google_schema ts)) ] ] );
      ]

let encode (r : Request.t) =
  let message_system =
    List.concat_map
      (fun (m : Message.t) ->
        match m.Message.role with
        | Message.System ->
            List.filter_map
              (function Message.Text s -> Some s | _ -> None)
              m.Message.parts
        | Message.User | Message.Assistant | Message.Tool -> [])
      r.Request.messages
  in
  let system = r.Request.system @ message_system in
  obj
    (system_instruction system
    @ [
        (n "contents", contents r.Request.messages);
        (n "generationConfig", generation_config r);
      ]
    @ tools_member r.Request.tools)

(* Response decoding *)

type state = {
  mutable pending_finish : Stream_part.t option;
  mutable usage : Usage.t option;
  mutable finished : bool;
  mutable saw_tool_call : bool;
  mutable anonymous_calls : int;
}

type t = state

let create () =
  {
    pending_finish = None;
    usage = None;
    finished = false;
    saw_tool_call = false;
    anonymous_calls = 0;
  }

let usage_of_metadata j =
  let cache_read = int_mem j "cachedContentTokenCount" in
  let reasoning = int_mem j "thoughtsTokenCount" in
  {
    Usage.zero with
    input = max 0 (int_mem j "promptTokenCount" - cache_read);
    output = int_mem j "candidatesTokenCount" + reasoning;
    reasoning;
    cache_read;
  }

(* mapFinishReason (google.go:1456-1477) folds safety and content categories
   onto the content-filter terminal and the malformed-function-call and
   recitation categories onto an error. *)
let finish_of_string = function
  | "STOP" -> `Stop
  | "MAX_TOKENS" -> `Length
  | "SAFETY" | "BLOCKLIST" | "PROHIBITED_CONTENT" | "SPII" | "IMAGE_SAFETY" ->
      `Content_filter
  | ("RECITATION" | "LANGUAGE" | "MALFORMED_FUNCTION_CALL" | "OTHER") as reason ->
      `Error (Fmt.str "provider ended with finish reason %s" reason)
  | other -> `Error (Fmt.str "unknown finish reason %s" other)

(* Gemini's [usageMetadata] is a cumulative snapshot of the turn, not a delta:
   the second event of TestGoogleCommon/gemini-2.5-flash/tool_streaming.yaml
   reports prompt 127 / candidates 4 and the third prompt 127 / candidates 12
   for the same turn. Emitting one [Usage] per event and folding them with
   [Usage.add] double counts the prompt, so the last snapshot is kept and
   emitted once, immediately before the terminal event. *)
let usage_events (st : state) =
  match st.usage with Some u -> [ Stream_part.Usage u ] | None -> []

let terminal (st : state) msg =
  st.finished <- true;
  st.pending_finish <- None;
  let usage = usage_events st in
  st.usage <- None;
  usage @ [ Stream_part.Finish (`Error msg) ]

let release (st : state) =
  match st.pending_finish with
  | None -> []
  | Some f ->
      st.finished <- true;
      st.pending_finish <- None;
      let usage = usage_events st in
      st.usage <- None;
      usage @ [ f ]

let call_id (st : state) (fc : Jsont.json) =
  match string_mem fc "id" with
  | Some i when i <> "" -> i
  | _ ->
      st.anonymous_calls <- st.anonymous_calls + 1;
      Fmt.str "call_%d" st.anonymous_calls

let function_call_parts (st : state) (fc : Jsont.json) =
  match string_mem fc "name" with
  | None -> terminal st "function call without a name"
  | Some name ->
      st.saw_tool_call <- true;
      let id = call_id st fc in
      let args = match oopt fc "args" with Some a -> string_of_json a | None -> "{}" in
      [
        Stream_part.Tool_call_start { id; name };
        Stream_part.Tool_input_delta { id; delta = args };
        Stream_part.Tool_call_end id;
      ]

let decode_part (st : state) (p : Jsont.json) : Stream_part.t list =
  match (oopt p "text", oopt p "functionCall") with
  | Some (Jsont.String (s, _)), _ when s <> "" ->
      if bool_mem p "thought" then [ Stream_part.Reasoning_delta s ]
      else [ Stream_part.Text_delta s ]
  | Some (Jsont.String _), _ -> []
  | Some _, _ -> terminal st "candidate part has a non-string text field"
  | None, Some fc -> function_call_parts st fc
  | None, None -> []

(* Gemini reports [STOP] for a turn whose content is a function call and adds
   [finishMessage] "Model generated function call(s)." ; google.go:866-871
   upgrades such a turn to the tool-calls terminal. Only [STOP] is upgraded, so
   a length or safety stop that happens to carry a call keeps its reason. *)
let record_finish (st : state) (reason : string) =
  if not st.finished then begin
    let reason =
      if st.saw_tool_call && reason = "STOP" then `Tool_calls else finish_of_string reason
    in
    st.pending_finish <- Some (Stream_part.Finish reason)
  end

let rec decode_candidate (st : state) (c : Jsont.json) : Stream_part.t list =
  let parts =
    match oopt c "content" with
    | Some content -> (
        match oopt content "parts" with
        | Some ps -> (
            match objects_of_json ps with
            | Some parts -> parts_of st parts
            | None -> terminal st "candidate content parts is not an array of objects")
        | None -> [])
    | None -> []
  in
  match string_mem c "finishReason" with
  | None -> parts
  | Some reason ->
      record_finish st reason;
      parts

(* A part that fails to decode ends the stream, so decoding stops there: the
   [Finish] the contract demands is terminal and nothing may follow it. *)
and parts_of (st : state) (parts : Jsont.json list) : Stream_part.t list =
  let rec go acc = function
    | [] -> List.rev acc
    | p :: tl ->
        let acc = List.rev_append (decode_part st p) acc in
        if st.finished then List.rev acc else go acc tl
  in
  go [] parts

let error_message j =
  match oopt j "error" with
  | Some e -> (
      match string_mem e "message" with Some m -> m | None -> string_of_json e)
  | None -> "provider reported an error"

(* A blocked prompt arrives with [promptFeedback.blockReason] set and often
   with no candidates at all, so the block is the only signal the stream
   gives: it maps to the content-filter terminal rather than to silence.
   [blockReason] values are the harm categories (PROHIBITED_CONTENT, SAFETY,
   BLOCKLIST, SPII, OTHER); [OTHER] and any unknown value still mean blocked,
   so they are reported as a filter with the reason named. *)
let blocked_reason j =
  Option.bind (oopt j "promptFeedback") (fun fb ->
      match string_mem fb "blockReason" with
      | Some r when r <> "" && r <> "BLOCK_REASON_UNSPECIFIED" -> Some r
      | _ -> None)

let decode_event (st : state) (j : Jsont.json) : Stream_part.t list =
  if not (is_object j) then terminal st "event is not a JSON object"
  else if has_error j then terminal st (error_message j)
  else begin
    (match oopt j "usageMetadata" with
    | Some u when is_object u -> st.usage <- Some (usage_of_metadata u)
    | Some _ | None -> ());
    match blocked_reason j with
    | Some reason ->
        let reason =
          match finish_of_string reason with `Error _ -> `Content_filter | f -> f
        in
        st.pending_finish <- Some (Stream_part.Finish reason);
        release st
    | None -> (
        match oopt j "candidates" with
        | None -> []
        | Some cs -> (
            match objects_of_json cs with
            | Some (c :: _) -> decode_candidate st c
            | Some [] -> []
            | None -> terminal st "candidates is not an array of objects"))
  end

let feed (st : t) ~event ~(data : string) =
  if st.finished || (event <> "" && event <> "message") then []
  else
    match json_of_string data with
    | Error e ->
        Log.err (fun m -> m "malformed google event: %s" e);
        terminal st (Fmt.str "malformed event: %s" e)
    | Ok j ->
        let parts = decode_event st j in
        if st.finished then parts @ release st else parts

let finish (st : t) =
  if st.finished then []
  else if st.pending_finish = None && st.saw_tool_call then (
    st.pending_finish <- Some (Stream_part.Finish `Tool_calls);
    release st)
  else if st.pending_finish = None then
    terminal st "stream ended before the provider reported a finish reason"
  else release st
