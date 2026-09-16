(* OpenAI-compatible Chat Completions codec.

   Wire sources, all under [.references/fantasy]:
   - [providers/openai/language_model.go], [Stream] (lines 453-771) for the
     chunk loop and the terminal evaluation, [prepareParams] (259-363) for
     the request, [DefaultMapFinishReasonFunc]
     ([language_model_hooks.go:190-208]) for finish reason mapping and
     [DefaultStreamUsageFunc] ([language_model_hooks.go:244-286]) for the
     usage mapping.
   - [providers/openaicompat/language_model_hooks.go] [ToPromptFunc]
     (189-566) for message conversion, [StreamExtraFunc] (106-184) for the
     reasoning_content rules, and the openai provider's
     [ToolResultMediaMessages] (620-635) for media tool results.
   - [providers/openaicompat/replay_test.go] for the DeepSeek/Kimi chunk
     shapes this decoder must survive: interleaved parallel tool calls,
     batched boundary chunks, reasoning replay and cut-stream suppression.

   The [data] of a [File] part already holds the provider wire form
   ([lib/fantasy/message.mli]), so it is copied into the request verbatim;
   re-encoding would corrupt it. *)

let log_src = Logs.Src.create "charm.fantasy.openai_compat_codec"

module Log = (val Logs.src_log log_src : Logs.LOG)
open Json_util

let jtrue = Jsont.Json.bool true

(* Request encoding *)

(* One content part per text or file part; tool calls and reasoning belong
   to their own members of the assistant message, and tool results become
   one [role: "tool"] message per result. *)
let file_media_part ~index ~mime ~data ~name : Jsont.json option =
  if String.starts_with ~prefix:"image/" mime then
    let url = "data:" ^ mime ^ ";base64," ^ data in
    Some
      (obj [ (n "type", str "image_url"); (n "image_url", obj [ (n "url", str url) ]) ])
  else if mime = "audio/wav" || mime = "audio/mpeg" || mime = "audio/mp3" then
    let format = if mime = "audio/wav" then "wav" else "mp3" in
    Some
      (obj
         [
           (n "type", str "input_audio");
           (n "input_audio", obj [ (n "data", str data); (n "format", str format) ]);
         ])
  else if mime = "application/pdf" then
    let filename =
      match name with Some value -> value | None -> Fmt.str "part-%d.pdf" index
    in
    let file =
      obj
        [
          (n "filename", str filename);
          (n "file_data", str ("data:application/pdf;base64," ^ data));
        ]
    in
    Some (obj [ (n "type", str "file"); (n "file", file) ])
  else if String.starts_with ~prefix:"text/" mime then
    let fields =
      match name with
      | Some value -> [ (n "filename", str value); (n "file_data", str data) ]
      | None -> [ (n "file_data", str data) ]
    in
    Some (obj [ (n "type", str "file"); (n "file", obj fields) ])
  else None

let user_parts (ps : Message.part list) : Jsont.json list =
  List.filter_map
    (fun (index, p) ->
      match p with
      | Message.Text s -> Some (obj [ (n "type", str "text"); (n "text", str s) ])
      | Message.File { mime; data; name } -> file_media_part ~index ~mime ~data ~name
      | Message.Reasoning _ | Message.Tool_call _ | Message.Tool_result _ -> None)
    (List.mapi (fun index part -> (index, part)) ps)

let media_user_message ~mime ~data =
  match file_media_part ~index:0 ~mime ~data ~name:None with
  | Some part -> [ obj [ (n "role", str "user"); (n "content", arr [ part ]) ] ]
  | None -> []

let tool_result_parts (ps : Message.part list) : Jsont.json list =
  List.concat_map
    (fun p ->
      match p with
      | Message.Tool_result tr ->
          let text, media =
            match tr.output with
            | `Text s -> (s, [])
            | `Error e -> (e, [])
            | `Media (mime, data) ->
                ( Fmt.str "The tool returned %s content; see the following user message."
                    mime,
                  if
                    String.starts_with ~prefix:"image/" mime
                    || mime = "audio/wav" || mime = "audio/mpeg" || mime = "audio/mp3"
                  then media_user_message ~mime ~data
                  else [] )
          in
          let tool =
            obj
              [
                (n "role", str "tool");
                (n "tool_call_id", str tr.id);
                (n "content", str text);
              ]
          in
          tool :: media
      | _ -> [])
    ps

(* A turn may carry several reasoning or text segments; reasoning
   concatenates in order rather than last-write-wins so no segment of a
   reasoning/speaking/reasoning turn is dropped. *)
let assistant_parts (ps : Message.part list) :
    Jsont.json list * Jsont.json list * string option =
  let fold (content, calls, reasoning) (p : Message.part) =
    match p with
    | Message.Text s ->
        (obj [ (n "type", str "text"); (n "text", str s) ] :: content, calls, reasoning)
    | Message.Tool_call tc ->
        let args =
          match tc.input with
          | Jsont.Object (ms, _) when ms = [] -> "{}"
          | j -> string_of_json j
        in
        let call =
          obj
            [
              (n "id", str tc.id);
              (n "type", str "function");
              (n "function", obj [ (n "name", str tc.name); (n "arguments", str args) ]);
            ]
        in
        (content, call :: calls, reasoning)
    | Message.Reasoning r ->
        let joined =
          match reasoning with Some t -> t ^ "\n" ^ r.text | None -> r.text
        in
        (content, calls, Some joined)
    | Message.File _ | Message.Tool_result _ -> (content, calls, reasoning)
  in
  let content, calls, reasoning = List.fold_left fold ([], [], None) ps in
  (List.rev content, List.rev calls, reasoning)

(* A single text part collapses to the bare string form; any other single
   part, or several parts, stay an array. *)
let string_or_parts = function
  | [ part ] -> (
      match oopt part "text" with
      | Some (Jsont.String (s, _)) -> str s
      | Some _ | None -> arr [ part ])
  | parts -> arr parts

(* System-role conversation messages are hoisted by [system_parts], so a
   [System] message here contributes nothing in place. *)
let message_of (m : Message.t) : Jsont.json list =
  match m.Message.role with
  | Message.System -> []
  | Message.User -> (
      match user_parts m.Message.parts with
      | [] -> []
      | parts -> [ obj [ (n "role", str "user"); (n "content", string_or_parts parts) ] ])
  | Message.Assistant -> (
      let content, calls, reasoning = assistant_parts m.Message.parts in
      match (content, calls) with
      | [], [] -> (
          (* A reasoning-only turn serializes with neither content nor tool
             calls, which strict upstreams reject; it is dropped. *)
          match reasoning with
          | Some _ -> []
          | None -> [])
      | content, calls ->
          let ms = ref [ (n "role", str "assistant") ] in
          if content <> [] then ms := !ms @ [ (n "content", string_or_parts content) ];
          if calls <> [] then ms := !ms @ [ (n "tool_calls", arr calls) ];
          (match reasoning with
          | Some r -> ms := !ms @ [ (n "reasoning_content", str r) ]
          | None -> ());
          [ obj !ms ])
  | Message.Tool -> tool_result_parts m.Message.parts

let tools_json (tools : Tool.t list) =
  match tools with
  | [] -> []
  | tools ->
      [
        ( n "tools",
          arr
            (List.map
               (fun (t : Tool.t) ->
                 obj
                   [
                     (n "type", str "function");
                     ( n "function",
                       obj
                         [
                           (n "name", str t.Tool.name);
                           (n "description", str t.Tool.description);
                           (n "parameters", t.Tool.schema);
                         ] );
                   ])
               tools) );
      ]

(* Reasoning effort; reasoning models reject the sampling knobs, so
   [temperature] is dropped alongside [reasoning_effort]
   (language_model.go:285-328). *)
let reasoning_effort = function
  | Request.Off -> None
  | Request.Low -> Some "low"
  | Request.Medium -> Some "medium"
  | Request.High -> Some "high"

(* System blocks join into one [role: "system"] message ahead of the
   conversation, alongside the text of any System-role message; blank
   blocks are dropped, and a request with no system content posts no
   system message (ToPromptFunc:215-239). *)
let system_parts (r : Request.t) =
  let from_messages =
    List.concat_map
      (fun (m : Message.t) ->
        match m.Message.role with
        | Message.System ->
            List.filter_map
              (fun p -> match p with Message.Text s -> Some s | _ -> None)
              m.Message.parts
        | Message.User | Message.Assistant | Message.Tool -> [])
      r.Request.messages
  in
  match List.filter (fun s -> String.trim s <> "") (r.Request.system @ from_messages) with
  | [] -> []
  | blocks ->
      [ obj [ (n "role", str "system"); (n "content", str (String.concat "\n" blocks)) ] ]

let encode (r : Request.t) =
  let reasoning = reasoning_effort r.Request.reasoning in
  let ms = ref [ (n "model", str r.Request.model.Model.id); (n "stream", jtrue) ] in
  (match reasoning with
  | Some effort -> ms := !ms @ [ (n "reasoning_effort", str effort) ]
  | None -> ());
  (match (r.Request.temperature, reasoning) with
  | Some t, None -> ms := !ms @ [ (n "temperature", num t) ]
  | Some _, Some _ | None, _ -> ());
  (* Reasoning models take the cap as [max_completion_tokens]; the
     [max_tokens] member is rejected there (language_model.go:321-327). *)
  (match reasoning with
  | None -> ms := !ms @ [ (n "max_tokens", int r.Request.max_tokens) ]
  | Some _ -> ms := !ms @ [ (n "max_completion_tokens", int r.Request.max_tokens) ]);
  (* The usage chunk is only sent when the request asks for it. *)
  ms := !ms @ [ (n "stream_options", obj [ (n "include_usage", jtrue) ]) ];
  let messages = system_parts r @ List.concat_map message_of r.Request.messages in
  (match messages with
  | [] -> ()
  | messages -> ms := !ms @ [ (n "messages", arr messages) ]);
  ms := !ms @ tools_json r.Request.tools;
  obj !ms

(* Stream decoding *)

type tool_state = { id : string; mutable arguments : string }

type state = {
  tools : (int, tool_state) Hashtbl.t;
  closed : (string, unit) Hashtbl.t;
  mutable pending_finish : Stream_part.t option;
  mutable usage : Usage.t option;
  mutable finished : bool;
}

type t = state

let create () =
  {
    tools = Hashtbl.create 8;
    closed = Hashtbl.create 8;
    pending_finish = None;
    usage = None;
    finished = false;
  }

(* DefaultStreamUsageFunc (language_model_hooks.go:244-286). OpenAI reports
   prompt_tokens including cached tokens; the cached amount is moved to
   cache_read so a consumer folding with Usage.add never double counts. *)
let usage_of j =
  let prompt = int_mem j "prompt_tokens" in
  let cached =
    match oopt j "prompt_tokens_details" with
    | Some d -> int_mem d "cached_tokens"
    | None -> 0
  in
  let reasoning =
    match oopt j "completion_tokens_details" with
    | Some d -> int_mem d "reasoning_tokens"
    | None -> 0
  in
  {
    Usage.input = max 0 (prompt - cached);
    output = int_mem j "completion_tokens";
    cache_read = cached;
    cache_write = 0;
    reasoning;
  }

(* DefaultMapFinishReasonFunc (language_model_hooks.go:190-208). *)
let finish_of_string = function
  | "stop" -> `Stop
  | "length" -> `Length
  | "content_filter" -> `Content_filter
  | "function_call" | "tool_calls" -> `Tool_calls
  | "insufficient_system_resource" ->
      (* DeepSeek-family upstreams report this when the request could not
         be served; the partial output must never complete a turn. *)
      `Error "provider ended with finish reason insufficient_system_resource"
  | other -> `Error (Fmt.str "unknown finish reason %s" other)

let usage_events (st : state) =
  match st.usage with Some u -> [ Stream_part.Usage u ] | None -> []

let terminal (st : state) msg =
  st.finished <- true;
  st.pending_finish <- None;
  let usage = usage_events st in
  st.usage <- None;
  usage @ [ Stream_part.Finish (`Error msg) ]

let calls_in_index_order (st : state) : tool_state list =
  let items = Hashtbl.fold (fun k v acc -> (k, v) :: acc) st.tools [] in
  let items = List.sort (fun (a, _) (b, _) -> compare a b) items in
  List.map snd items

(* Tool calls close at their finish chunk, but a batching host may put the
   last argument bytes and the finish reason in one chunk, so any call not
   yet closed is closed here, before the terminal. *)
let close_open_calls (st : state) =
  List.concat_map
    (fun c ->
      if Hashtbl.mem st.closed c.id then []
      else begin
        Hashtbl.replace st.closed c.id ();
        [ Stream_part.Tool_call_end c.id ]
      end)
    (calls_in_index_order st)

(* Every event after the finish reason may carry the usage chunk, so the
   deferred Finish is released only once the stream ends. Terminal reasons
   that can cut output mid-call suppress the closing of truncated calls:
   emitting [Tool_call_end] would present fabricated complete input. *)
(* A stream cut without arguments, or with arguments that never parsed, must
   not present fabricated "{}" input: the calls are suppressed and the turn
   errors (CHARM-2020). *)
let valid_arguments args =
  args <> ""
  &&
  match Jsont_bytesrw.decode_string Jsont.json args with
  | Ok _ -> true
  | Error _ -> false

let release (st : state) =
  match st.pending_finish with
  | None -> []
  | Some f ->
      let suppress =
        match f with
        | Stream_part.Finish (`Length | `Content_filter | `Error _) -> true
        | _ -> false
      in
      let calls = calls_in_index_order st in
      let truncated = List.exists (fun c -> not (valid_arguments c.arguments)) calls in
      if truncated && not suppress then
        terminal st "stream ended before tool call arguments completed"
      else
        let ends = if suppress then [] else close_open_calls st in
        st.finished <- true;
        st.pending_finish <- None;
        let usage = usage_events st in
        st.usage <- None;
        ends @ usage @ [ f ]

let complete (st : state) : Stream_part.t list =
  if st.finished then []
  else
    match st.pending_finish with
    | Some _ -> release st
    | None -> (
        let calls = calls_in_index_order st in
        let truncated = List.exists (fun c -> not (valid_arguments c.arguments)) calls in
        if truncated then terminal st "stream ended before tool call arguments completed"
        else
          match calls with
          | [] -> terminal st "stream ended before the provider reported a finish reason"
          | _ :: _ ->
              (* No finish reason at all with complete arguments infers a
                 tool-call turn. *)
              st.pending_finish <- Some (Stream_part.Finish `Tool_calls);
              release st)

let error_message j =
  match oopt j "error" with
  | Some (Jsont.String (s, _)) -> s
  | Some e -> (
      match string_mem e "message" with Some m -> m | None -> string_of_json e)
  | None -> "provider reported an error"

let record_finish (st : state) (reason : string) =
  if not st.finished then
    st.pending_finish <- Some (Stream_part.Finish (finish_of_string reason))

(* Reasoning: StreamExtraFunc (openaicompat/language_model_hooks.go:106-184)
   reduced to the observable part. A null or empty reasoning_content never
   opens a block, so the Kimi finish chunk with "reasoning_content": null
   cannot rekindle one, and non-empty deltas are the only emitted parts: the
   Stream_part surface has no reasoning start or end events to model the
   empty-block shapes with. *)
let reasoning_parts (delta : Jsont.json) : Stream_part.t list =
  match oopt delta "reasoning_content" with
  | Some (Jsont.String (s, _)) when s <> "" -> [ Stream_part.Reasoning_delta s ]
  | Some _ | None -> []

let text_parts (delta : Jsont.json) : Stream_part.t list =
  match oopt delta "content" with
  | Some (Jsont.String (s, _)) when s <> "" -> [ Stream_part.Text_delta s ]
  | Some _ | None -> []

let tool_delta (call : tool_state) (tc : Jsont.json) : Stream_part.t list =
  match oopt tc "function" with
  | Some f -> (
      match string_mem f "arguments" with
      | Some s when s <> "" ->
          call.arguments <- call.arguments ^ s;
          [ Stream_part.Tool_input_delta { id = call.id; delta = s } ]
      | Some _ | None -> [])
  | None -> []

(* A delta for a call the index already opened appends argument bytes. An
   unopened index opens the call: some upstreams send empty entries or miss
   fields, so an entry with neither a name nor arguments is skipped, an
   unknown call type is a decoder error, and a missing id falls back to the
   upstream default [tool-call-<index>] (language_model.go:526-565). *)
let open_tool (st : state) (index : int) (tc : Jsont.json) : Stream_part.t list * bool =
  let function' = oopt tc "function" in
  let name = match function' with Some f -> string_mem f "name" | None -> None in
  let arguments =
    match function' with Some f -> string_mem f "arguments" | None -> None
  in
  let has_name = match name with Some s -> s <> "" | None -> false in
  let has_args = match arguments with Some s -> s <> "" | None -> false in
  if (not has_name) && not has_args then ([], true)
  else
    match string_mem tc "type" with
    | Some t when t <> "function" ->
        (terminal st (Fmt.str "tool call type %s is not function" t), false)
    | Some _ | None ->
        let name = match name with Some s -> s | None -> "" in
        let id =
          match string_mem tc "id" with
          | Some id when id <> "" -> id
          | Some _ | None -> Fmt.str "tool-call-%d" index
        in
        let arguments = match arguments with Some s -> s | None -> "" in
        let call = { id; arguments } in
        Hashtbl.replace st.tools index call;
        let initial =
          if arguments = "" then []
          else [ Stream_part.Tool_input_delta { id; delta = arguments } ]
        in
        ([ Stream_part.Tool_call_start { id; name } ] @ initial, true)

let tool_entry (st : state) (tc : Jsont.json) : Stream_part.t list * bool =
  match int_option_mem tc "index" with
  | None -> (terminal st "tool call is missing its index", false)
  | Some index -> (
      match Hashtbl.find_opt st.tools index with
      | Some call -> (tool_delta call tc, true)
      | None -> open_tool st index tc)

(* The [bool] is [false] when the decoder ended the stream inside this
   entry, which stops the fold. *)
let tool_parts (st : state) (tcs : Jsont.json) : Stream_part.t list =
  match array_of tcs with
  | None -> []
  | Some tcs ->
      let out = ref [] in
      let stop = ref false in
      List.iter
        (fun tc ->
          if (not !stop) && not st.finished then
            match tc with
            | Jsont.Object _ ->
                let parts, continue_ = tool_entry st tc in
                out := List.rev_append parts !out;
                if not continue_ then stop := true
            | _ -> ())
        tcs;
      List.rev !out

let choice_delta (st : state) (delta : Jsont.json) : Stream_part.t list =
  (* Reasoning first, then text, then tool calls; the three channels
     accumulate independently so the order only fixes presentation. *)
  let reasoning = reasoning_parts delta in
  let text = text_parts delta in
  let tools =
    match oopt delta "tool_calls" with Some tcs -> tool_parts st tcs | None -> []
  in
  reasoning @ text @ tools

let decode_choice (st : state) (c : Jsont.json) : Stream_part.t list =
  (match string_mem c "finish_reason" with
  | Some reason when reason <> "" -> record_finish st reason
  | Some _ | None -> ());
  match oopt c "delta" with None -> [] | Some delta -> choice_delta st delta

let decode_choices (st : state) (cs : Jsont.json list) : Stream_part.t list =
  let out = ref [] in
  let stop = ref false in
  List.iter
    (fun c ->
      if not !stop then begin
        out := List.rev_append (decode_choice st c) !out;
        if st.finished then stop := true
      end)
    cs;
  List.rev !out

(* Usage rides its own trailing chunk, so a chunk that carries no choices is
   normal and never an error. A zero-usage object is ignored, as upstream's
   accumulator does. *)
let decode_event (st : state) (j : Jsont.json) : Stream_part.t list =
  if not (is_object j) then terminal st "event is not a JSON object"
  else if has_error j then terminal st (error_message j)
  else begin
    (match oopt j "usage" with
    | Some u
      when is_object u
           && (int_mem u "prompt_tokens" <> 0
              || int_mem u "completion_tokens" <> 0
              || int_mem u "total_tokens" <> 0
              || (match oopt u "prompt_tokens_details" with
                | Some details -> int_mem details "cached_tokens" <> 0
                | None -> false)
              ||
              match oopt u "completion_tokens_details" with
              | Some details -> int_mem details "reasoning_tokens" <> 0
              | None -> false) ->
        st.usage <- Some (usage_of u)
    | Some _ | None -> ());
    match oopt j "choices" with
    | None -> []
    | Some cs -> (
        match array_of cs with
        | None -> terminal st "choices is not an array"
        | Some cs -> decode_choices st cs)
  end

(* Chat completions streams carry unnamed events; a named event other than
   a chunk is ignored. The [data: [DONE]] sentinel closes the stream and
   releases the deferred terminal exactly like a clean end of response. *)
let event_kind ~event j =
  if event = "" || event = "message" then Option.value ~default:"" (string_mem j "type")
  else event

let feed (st : t) ~event ~(data : string) =
  if st.finished then []
  else if data = "[DONE]" then complete st
  else
    match Jsont_bytesrw.decode_string Jsont.json data with
    | Error e ->
        Log.err (fun m -> m "malformed openai event: %s" e);
        terminal st (Fmt.str "malformed event: %s" e)
    | Ok j ->
        let kind = event_kind ~event j in
        if
          kind <> "" && kind <> "message"
          && kind <> "chat.completion.chunk"
          && kind <> "error"
        then []
        else decode_event st j

let finish (st : t) = complete st
