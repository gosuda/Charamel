(* OpenAI Responses API codec.

   Wire sources, all under [.references/fantasy]:
   - [providers/openai/responses_language_model.go]: [toResponsesPrompt]
     (426-712) for the request body, [responsesUsage] (409-424) for the
     usage mapping, [mapResponsesFinishReason] (962-980), and
     [Stream] (982-1326) for the event decoding order.
   - [providers/openai/responses_options.go]: [getResponsesModelConfig]
     (90-153) for the reasoning-model and system-message classification.
   - [providertests/testdata/TestOpenAIResponsesCommon/] and
     [TestOpenAIResponsesWithSummaryThinking/]: recorded streams, the
     ground truth for member names ([call_id] vs [id], [output_index]
     correlation, easy messages carrying plain string [content],
     [instructions] and [parallel_tool_calls] left unset).

   The stream sets its tool-call flag when a call closes
   (responses_language_model.go:1083-1103), not when it opens, so a
   truncated stream still finishes as a stop. *)

let log_src = Logs.Src.create "charamel.fantasy.responses_codec"

module Log = (val Logs.src_log log_src : Logs.LOG)
open Json

(* Request: model classification.

   getResponsesModelConfig (responses_options.go:90-153) keys reasoning
   support and system handling off the model id. o1-mini and o1-preview
   reject system prompts outright; the other reasoning families take the
   developer role; everything else takes the system role and is not a
   reasoning model. *)

let contains needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  go 0

let classify_model id =
  let lower = String.lowercase_ascii id in
  let contains_lower needle = contains needle lower in
  let prefix p = String.starts_with ~prefix:p lower in
  if contains_lower "gpt-5-chat" then `Chat
  else if contains_lower "o1-mini" || contains_lower "o1-preview" then `Remove_system
  else if
    prefix "o1" || contains_lower "-o1" || prefix "o3" || contains_lower "-o3"
    || prefix "o4" || contains_lower "-o4" || prefix "oss" || contains_lower "-oss"
    || contains_lower "gpt-5" || prefix "codex-" || contains_lower "computer-use"
  then `Reasoning
  else `Chat

(* Request: input items. *)

let is_image mime = String.starts_with ~prefix:"image/" mime
let text_part s = obj [ (n "type", str "input_text"); (n "text", str s) ]

let image_part mime data =
  obj
    [
      (n "type", str "input_image");
      (n "image_url", str (Fmt.str "data:%s;base64,%s" mime data));
    ]

let file_part index mime data name =
  let filename = match name with Some f -> f | None -> Fmt.str "part-%d.pdf" index in
  obj
    [
      (n "type", str "input_file");
      (n "filename", str filename);
      (n "file_data", str (Fmt.str "data:%s;base64,%s" mime data));
    ]

(* Easy input messages carry plain string content (the recorded request
   bodies show {"content":"...","role":"system"}); user messages carry the
   typed part array. *)
let easy_message_item role content =
  obj [ (n "role", str role); (n "content", str content) ]

let message_item role parts = obj [ (n "role", str role); (n "content", arr parts) ]

(* responses_language_model.go:475-545: user text and image/pdf file parts
   are visible content; an unsupported media type or an empty message is
   dropped. *)
let user_items parts =
  let visible =
    List.filter_map
      (fun (index, p) ->
        match p with
        | Message.Text s when s <> "" -> Some (text_part s)
        | Message.File { mime; data; name = _ } when is_image mime ->
            Some (image_part mime data)
        | Message.File { mime; data; name } when mime = "application/pdf" ->
            Some (file_part index mime data name)
        | _ -> None)
      (List.mapi (fun index p -> (index, p)) parts)
  in
  if visible = [] then [] else [ message_item "user" visible ]

(* toResponsesPrompt (responses_language_model.go:547-610): each assistant
   text part and each tool call becomes its own input item; reasoning and
   file parts are skipped on replay. *)
let assistant_items parts =
  List.filter_map
    (fun p ->
      match p with
      | Message.Text s when s <> "" -> Some (easy_message_item "assistant" s)
      | Message.Tool_call { id; name; input } ->
          Some
            (obj
               [
                 (n "type", str "function_call");
                 (n "call_id", str id);
                 (n "name", str name);
                 (n "arguments", str (string_of_json input));
               ])
      | _ -> None)
    parts

(* responses_language_model.go:612-707: a tool result output is a string;
   image media rides along as a synthetic user image message after the call
   output, and other media degrade to the text placeholder. *)
let tool_items parts =
  List.concat_map
    (fun p ->
      match p with
      | Message.Tool_result { id; output; name = _ } -> (
          let output_item text =
            obj
              [
                (n "type", str "function_call_output");
                (n "call_id", str id);
                (n "output", str text);
              ]
          in
          match output with
          | `Text s | `Error s -> [ output_item s ]
          | `Media (mime, data) when is_image mime ->
              let placeholder =
                Fmt.str "The tool returned %s content; see the following user message."
                  mime
              in
              [ output_item placeholder; message_item "user" [ image_part mime data ] ]
          | `Media (mime, _) ->
              [
                output_item
                  (Fmt.str "The tool returned %s content; see the following user message."
                     mime);
              ])
      | _ -> [])
    parts

let system_items (r : Request.t) =
  let blocks = Request.system_blocks r in
  match blocks with
  | [] -> []
  | blocks -> (
      match classify_model r.Request.model.Model.id with
      | `Remove_system -> []
      | `Chat -> [ easy_message_item "system" (String.concat "" blocks) ]
      | `Reasoning -> [ easy_message_item "developer" (String.concat "" blocks) ])

let input_items (r : Request.t) =
  let message_items =
    List.concat_map
      (fun (m : Message.t) ->
        match m.Message.role with
        | Message.System -> []
        | Message.User -> user_items m.Message.parts
        | Message.Assistant -> assistant_items m.Message.parts
        | Message.Tool -> tool_items m.Message.parts)
      r.Request.messages
  in
  system_items r @ message_items

(* Request: reasoning and knobs. *)

let encode (r : Request.t) =
  let cap = Request.effective_max_tokens r in
  let is_reasoning = classify_model r.Request.model.Model.id = `Reasoning in
  (* Summaries are never requested: the provider only sets [summary] from
     explicit caller options, and without it no reasoning text streams. *)
  let reasoning_member =
    match (is_reasoning, Openai_wire.effort r.Request.reasoning) with
    | true, Some e -> [ (n "reasoning", obj [ (n "effort", str e) ]) ]
    | _ -> []
  in
  (* A reasoning model rejects temperature alongside reasoning. *)
  let temperature =
    match (r.Request.temperature, is_reasoning) with
    | Some t, false -> [ (n "temperature", num t) ]
    | _ -> []
  in
  let tools =
    match r.Request.tools with
    | [] -> []
    | ts ->
        [
          ( n "tools",
            arr
              (List.map
                 (fun (t : Tool.t) ->
                   obj
                     [
                       (n "type", str "function");
                       (n "name", str t.Tool.name);
                       (n "description", str t.Tool.description);
                       (n "parameters", t.Tool.schema);
                     ])
                 ts) );
          (n "tool_choice", str "auto");
        ]
  in
  obj
    ([
       (n "model", str r.Request.model.Model.id);
       (n "input", arr (input_items r));
       (n "max_output_tokens", int cap);
       (n "stream", bool true);
       (n "store", bool false);
     ]
    @ temperature @ reasoning_member @ tools)

type tool_state = { call_id : string; mutable arguments : string; mutable closed : bool }

type state = {
  tools : (int, tool_state) Hashtbl.t;
  term : Codec_state.t;
  mutable saw_tool_call : bool;
}

type t = state

let create () =
  { tools = Hashtbl.create 8; term = Codec_state.create (); saw_tool_call = false }

(* responsesUsage (responses_language_model.go:409-424): input_tokens
   includes both cached and cache-write tokens, so both are subtracted
   to keep the Usage counters disjoint. *)
let usage_of j =
  let input = int_mem j "input_tokens" in
  let cache_read, cache_write =
    match oopt j "input_tokens_details" with
    | Some d -> (int_mem d "cached_tokens", int_mem d "cache_write_tokens")
    | None -> (0, 0)
  in
  let reasoning =
    match oopt j "output_tokens_details" with
    | Some d -> int_mem d "reasoning_tokens"
    | None -> 0
  in
  {
    Usage.input = max 0 (input - cache_read - cache_write);
    output = int_mem j "output_tokens";
    cache_read;
    cache_write;
    reasoning;
  }

(* mapResponsesFinishReason (responses_language_model.go:962-980): a tool
   call overrides every reason except the two length reasons. *)
let finish_of_string saw_tool_call = function
  | "max_tokens" | "max_output_tokens" -> `Length
  | reason -> (
      if saw_tool_call then `Tool_calls
      else
        match reason with
        | "" -> `Stop
        | "content_filter" -> `Content_filter
        | other -> `Error (Fmt.str "provider ended with incomplete reason %s" other))

let append_done_arguments (st : state) (call : tool_state) (item : Jsont.json) =
  match string_mem item "arguments" with
  | None -> []
  | Some full when full = "" -> []
  | Some full when call.arguments = full -> []
  | Some full when call.arguments = "" ->
      call.arguments <- full;
      [ Stream_part.Tool_input_delta { id = call.call_id; delta = full } ]
  | Some full when String.starts_with ~prefix:call.arguments full ->
      let prefix_length = String.length call.arguments in
      let suffix = String.sub full prefix_length (String.length full - prefix_length) in
      call.arguments <- full;
      if suffix = "" then []
      else [ Stream_part.Tool_input_delta { id = call.call_id; delta = suffix } ]
  | Some _ ->
      Codec_state.terminal st.term "tool call arguments changed between delta and done"

(* response.output_item.added: a function_call item opens the call keyed by
   output_index (responses_language_model.go:1022-1036); message and
   reasoning items open Stream_part boundaries the contract does not model,
   so nothing is emitted for them. *)
let output_item_added (st : state) (item : Jsont.json) (output_index : int) =
  match string_mem item "type" with
  | Some "function_call" -> (
      match (string_mem item "call_id", string_mem item "name") with
      | Some call_id, Some name when call_id <> "" && name <> "" ->
          let arguments = Option.value (string_mem item "arguments") ~default:"" in
          Hashtbl.replace st.tools output_index { call_id; arguments; closed = false };
          let initial =
            if arguments = "" then []
            else [ Stream_part.Tool_input_delta { id = call_id; delta = arguments } ]
          in
          Stream_part.Tool_call_start { id = call_id; name } :: initial
      | _ -> Codec_state.terminal st.term "function call is missing call_id or name")
  | _ -> []

(* response.output_item.done closes the call opened at the same output index
   and records that the stream carried one. *)
let output_item_done (st : state) (item : Jsont.json) (output_index : int) =
  match string_mem item "type" with
  | Some "function_call" -> (
      match Hashtbl.find_opt st.tools output_index with
      | None ->
          Codec_state.terminal st.term "function call completed before it was opened"
      | Some call ->
          if call.closed then []
          else
            let arguments = append_done_arguments st call item in
            if st.term.finished then arguments
            else begin
              call.closed <- true;
              st.saw_tool_call <- true;
              arguments @ [ Stream_part.Tool_call_end call.call_id ]
            end)
  | _ -> []

let function_call_arguments_delta (st : state) (j : Jsont.json) =
  match int_option_mem j "output_index" with
  | None ->
      Codec_state.terminal st.term "function call argument delta is missing output_index"
  | Some output_index -> (
      match (Hashtbl.find_opt st.tools output_index, string_mem j "delta") with
      | Some call, Some delta when not call.closed ->
          call.arguments <- call.arguments ^ delta;
          if delta = "" then []
          else [ Stream_part.Tool_input_delta { id = call.call_id; delta } ]
      | Some _, Some _ ->
          Codec_state.terminal st.term "function call argument delta follows completion"
      | Some _, None -> []
      | None, _ ->
          Codec_state.terminal st.term "function call argument delta has no open call")

let function_call_arguments_done (st : state) (j : Jsont.json) =
  match int_option_mem j "output_index" with
  | None ->
      Codec_state.terminal st.term "function call arguments done is missing output_index"
  | Some output_index -> (
      match Hashtbl.find_opt st.tools output_index with
      | None ->
          Codec_state.terminal st.term "function call arguments done has no open call"
      | Some call -> append_done_arguments st call j)

let delta_part make j =
  match string_mem j "delta" with Some d -> [ make d ] | None -> []

let error_message ~fallback j =
  match string_mem j "message" with Some m when m <> "" -> m | _ -> fallback

(* response.completed and response.incomplete carry the finish reason in
   [incomplete_details.reason] and the usage snapshot on the response; the
   terminal part is deferred to [release] so usage cannot be lost.
   (responses_language_model.go:1250-1262). *)
let decode_terminal (st : state) (j : Jsont.json) =
  match oopt j "response" with
  | Some resp when is_object resp ->
      let reason =
        match oopt resp "incomplete_details" with
        | Some d when is_object d -> Option.value ~default:"" (string_mem d "reason")
        | Some _ | None -> ""
      in
      (match oopt resp "usage" with
      | Some u when is_object u -> st.term.usage <- Some (usage_of u)
      | Some _ | None -> ());
      st.term.pending_finish <-
        Some (Stream_part.Finish (finish_of_string st.saw_tool_call reason));
      Codec_state.release st.term
  | Some _ | None ->
      Codec_state.terminal st.term "terminal event carries no response object"

let decode_failed (st : state) (j : Jsont.json) =
  let fallback = "response failed" in
  let msg =
    match oopt j "response" with
    | Some resp when is_object resp -> (
        match oopt resp "error" with
        | Some e -> error_message ~fallback e
        | None -> fallback)
    | Some _ | None -> fallback
  in
  Codec_state.terminal st.term msg

let output_item_added_event (st : state) (j : Jsont.json) =
  match (oopt j "item", int_option_mem j "output_index") with
  | Some item, Some output_index when is_object item ->
      output_item_added st item output_index
  | Some _, Some _ -> Codec_state.terminal st.term "output item is not an object"
  | _ -> Codec_state.terminal st.term "output item event is missing item or output_index"

let output_item_done_event (st : state) (j : Jsont.json) =
  match (oopt j "item", int_option_mem j "output_index") with
  | Some item, Some output_index when is_object item ->
      output_item_done st item output_index
  | Some _, Some _ -> Codec_state.terminal st.term "output item is not an object"
  | _ -> Codec_state.terminal st.term "output item event is missing item or output_index"

let decode_event (st : state) (j : Jsont.json) : Stream_part.t list =
  if not (is_object j) then Codec_state.terminal st.term "event is not a JSON object"
  else
    match string_mem j "type" with
    | None -> Codec_state.terminal st.term "event has no type member"
    | Some "response.output_item.added" -> output_item_added_event st j
    | Some "response.output_item.done" -> output_item_done_event st j
    | Some "response.function_call_arguments.delta" -> function_call_arguments_delta st j
    | Some "response.function_call_arguments.done" -> function_call_arguments_done st j
    | Some "response.output_text.delta" ->
        delta_part (fun d -> Stream_part.Text_delta d) j
    | Some "response.reasoning_summary_part.added" -> [ Stream_part.Reasoning_delta "\n" ]
    | Some ("response.reasoning_summary_text.delta" | "response.reasoning_text.delta") ->
        delta_part (fun d -> Stream_part.Reasoning_delta d) j
    | Some ("response.completed" | "response.incomplete") -> decode_terminal st j
    | Some "response.failed" -> decode_failed st j
    | Some "error" ->
        Codec_state.terminal st.term
          (error_message ~fallback:"provider reported an error" j)
    | _ -> []

let feed (st : t) ~event ~(data : string) =
  if st.term.finished then []
  else
    match json_of_string data with
    | Error e ->
        Log.err (fun m -> m "malformed responses event: %s" e);
        Codec_state.terminal st.term (Fmt.str "malformed event: %s" e)
    | Ok j ->
        (* The [type] member is authoritative; it is present in every
           recorded stream and the SSE event name repeats it. *)
        ignore event;
        let parts = decode_event st j in
        if st.term.finished then parts @ Codec_state.release st.term else parts

let finish (st : t) =
  if st.term.finished then []
  else
    Codec_state.terminal st.term
      "stream ended before the provider reported a terminal event"
