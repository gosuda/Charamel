type item = bool -> Jsont.json

let jstr s = Jsont.Json.string s
let jint i = Jsont.Json.int i
let jfloat f = Jsont.Json.number f
let jlist l = Jsont.Json.list l
let jobj ms = Jsont.Json.object' ms
let jm k v = Jsont.Json.(mem (name k) v)
let jtrue = Jsont.Json.bool true
let cache_control = jobj [ jm "type" (jstr "ephemeral") ]

let with_cache (ms : Jsont.mem list) cache =
  if cache then ms @ [ jm "cache_control" cache_control ] else ms

let base64_source mime data =
  jobj [ jm "type" (jstr "base64"); jm "media_type" (jstr mime); jm "data" (jstr data) ]

let text_source data = jobj [ jm "type" (jstr "text"); jm "data" (jstr data) ]

let text_item s cache =
  jobj (with_cache [ jm "type" (jstr "text"); jm "text" (jstr s) ] cache)

let image_item mime data cache =
  jobj
    (with_cache [ jm "type" (jstr "image"); jm "source" (base64_source mime data) ] cache)

let document_item ?name source cache =
  let ms = [ jm "type" (jstr "document"); jm "source" source ] in
  let ms = match name with Some n -> ms @ [ jm "title" (jstr n) ] | None -> ms in
  jobj (with_cache ms cache)

let file_item ~mime ~data ?name cache =
  if String.starts_with ~prefix:"image/" mime then image_item mime data cache
  else if mime = "application/pdf" then
    document_item ?name (base64_source mime data) cache
  else document_item ?name (text_source data) cache

let tool_result_item ~id output cache =
  let content, is_error =
    match output with
    | `Text s -> ([ text_item s false ], false)
    | `Error s -> ([ text_item s false ], true)
    | `Media (mime, data) -> ([ image_item mime data false ], false)
  in
  let ms =
    [
      jm "type" (jstr "tool_result");
      jm "tool_use_id" (jstr id);
      jm "content" (jlist content);
    ]
  in
  let ms = if is_error then ms @ [ jm "is_error" jtrue ] else ms in
  jobj (with_cache ms cache)

let tool_use_item ~id ~name input cache =
  jobj
    (with_cache
       [
         jm "type" (jstr "tool_use");
         jm "id" (jstr id);
         jm "name" (jstr name);
         jm "input" input;
       ]
       cache)

let thinking_item ~text ~signature cache =
  jobj
    (with_cache
       [
         jm "type" (jstr "thinking");
         jm "thinking" (jstr text);
         jm "signature" (jstr signature);
       ]
       cache)

let user_part (p : Message.part) : item list =
  match p with
  | Message.Text s -> [ text_item s ]
  | Message.File { mime; data; name } -> [ file_item ~mime ~data ?name ]
  | Message.Tool_result { id; name = _; output } -> [ tool_result_item ~id output ]
  | Message.Reasoning _ | Message.Tool_call _ -> []

let assistant_part (p : Message.part) : item list =
  match p with
  | Message.Text s -> [ text_item s ]
  | Message.Tool_call { id; name; input } -> [ tool_use_item ~id ~name input ]
  | Message.Reasoning { text; signature } -> (
      (* The API accepts a thinking block only together with the signature
         it issued, so unsigned reasoning cannot round-trip. *)
      match signature with
      | Some signature -> [ thinking_item ~text ~signature ]
      | None -> [])
  | Message.File _ | Message.Tool_result _ -> []

let oauth_system = "You are Claude Code, Anthropic's official CLI for Claude."

let system_texts (r : Request.t) =
  let prefix =
    match r.auth with Request.Oauth -> [ oauth_system ] | Request.Api_key -> []
  in
  let from_messages =
    List.concat_map
      (fun (m : Message.t) ->
        match m.role with
        | Message.System ->
            List.filter_map
              (fun p -> match p with Message.Text s -> Some s | _ -> None)
              m.parts
        | Message.User | Message.Assistant | Message.Tool -> [])
      r.messages
  in
  prefix @ r.system @ from_messages

let system_json (r : Request.t) =
  match system_texts r with
  | [] -> []
  | texts ->
      let last = List.length texts - 1 in
      [ jm "system" (jlist (List.mapi (fun i s -> text_item s (i = last)) texts)) ]

let wire_role (m : Message.t) =
  match m.role with
  | Message.System -> None
  | Message.User | Message.Tool -> Some true
  | Message.Assistant -> Some false

let rec split_same is_user acc = function
  | m :: rest when wire_role m = Some is_user -> split_same is_user (m :: acc) rest
  | rest -> (List.rev acc, rest)

let rec group acc = function
  | [] -> List.rev acc
  | m :: rest -> (
      match wire_role m with
      | None -> group acc rest
      | Some is_user ->
          let same, tail = split_same is_user [ m ] rest in
          group ((is_user, same) :: acc) tail)

let content_of is_user ms =
  let one (m : Message.t) =
    List.concat_map (fun p -> if is_user then user_part p else assistant_part p) m.parts
  in
  List.concat_map one ms

let message_json is_user items cache =
  let last = List.length items - 1 in
  let role = if is_user then "user" else "assistant" in
  jobj
    [
      jm "role" (jstr role);
      jm "content" (jlist (List.mapi (fun i f -> f (cache && i = last)) items));
    ]

let messages_json (r : Request.t) =
  let wire_messages = List.filter (fun m -> Option.is_some (wire_role m)) r.messages in
  let turns =
    group [] wire_messages
    |> List.filter_map (fun (is_user, ms) ->
        match content_of is_user ms with [] -> None | items -> Some (is_user, items))
  in
  let user_count = List.length (List.filter (fun (is_user, _) -> is_user) turns) in
  let seen = ref 0 in
  List.map
    (fun (is_user, items) ->
      let cache =
        if not is_user then false
        else begin
          incr seen;
          !seen > user_count - 2
        end
      in
      message_json is_user items cache)
    turns

let tools_json (tools : Tool.t list) =
  match tools with
  | [] -> []
  | _ ->
      let last = List.length tools - 1 in
      [
        jm "tools"
          (jlist
             (List.mapi
                (fun i (t : Tool.t) ->
                  jobj
                    (with_cache
                       [
                         jm "name" (jstr t.name);
                         jm "description" (jstr t.description);
                         jm "input_schema" t.schema;
                       ]
                       (i = last)))
                tools));
      ]

let budget = function
  | Request.Off -> None
  | Request.Low -> Some 1024
  | Request.Medium -> Some 8192
  | Request.High -> Some 32768

let effective_max_tokens (r : Request.t) =
  if r.max_tokens > 0 then r.max_tokens else r.model.default_max_tokens

let thinking_budget (r : Request.t) =
  match budget r.reasoning with
  | None -> None
  | Some requested ->
      let clamped = min requested (effective_max_tokens r - 1) in
      if clamped > 0 then Some clamped else None

let thinking_json (r : Request.t) =
  match thinking_budget r with
  | None -> []
  | Some b ->
      [ jm "thinking" (jobj [ jm "type" (jstr "enabled"); jm "budget_tokens" (jint b) ]) ]

let temperature_json (r : Request.t) =
  match (thinking_budget r, r.temperature) with
  | Some _, _ -> []
  | None, None -> []
  | None, Some t -> [ jm "temperature" (jfloat t) ]

let encode (r : Request.t) =
  jobj
    ([
       jm "model" (jstr r.model.id);
       jm "max_tokens" (jint (effective_max_tokens r));
       jm "stream" jtrue;
       jm "messages" (jlist (messages_json r));
     ]
    @ system_json r @ tools_json r.tools @ thinking_json r @ temperature_json r)

type tool_block = { id : string; mutable arguments : string; mutable closed : bool }
type block = Text | Thinking | Tool of tool_block

type t = {
  blocks : (int, block) Hashtbl.t;
  mutable usage : Usage.t;
  mutable have_usage : bool;
  mutable stop : string option;
  mutable saw_stop : bool;
  mutable terminated : bool;
}

let create () =
  {
    blocks = Hashtbl.create 8;
    usage = Usage.zero;
    have_usage = false;
    stop = None;
    saw_stop = false;
    terminated = false;
  }

let mem (j : Jsont.json) k =
  match j with
  | Jsont.Object (ms, _) -> (
      match Jsont.Json.find_mem k ms with Some (_, v) -> Some v | None -> None)
  | _ -> None

let is_object j = Jsont.Json.sort j = Jsont.Sort.Object
let string_mem k j = match mem j k with Some (Jsont.String (s, _)) -> Some s | _ -> None

let int_mem k j =
  match mem j k with Some (Jsont.Number (f, _)) -> Some (int_of_float f) | _ -> None

let string_of_json (j : Jsont.json) =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json j with
  | Ok s -> s
  | Error _ -> invalid_arg "Anthropic codec received an unencodable JSON value"

let valid_arguments args =
  args = ""
  ||
  match Jsont_bytesrw.decode_string Jsont.json args with
  | Ok _ -> true
  | Error _ -> false

let or_zero = Option.value ~default:0
let replace_present current = function Some n -> n | None -> current

let reasoning_of (j : Jsont.json) current =
  match mem j "output_tokens_details" with
  | Some d when is_object d -> replace_present current (int_mem "thinking_tokens" d)
  | Some _ | None -> current

let snapshot_usage (u : Usage.t) (j : Jsont.json) =
  {
    Usage.input = replace_present u.input (int_mem "input_tokens" j);
    output = replace_present u.output (int_mem "output_tokens" j);
    cache_write = replace_present u.cache_write (int_mem "cache_creation_input_tokens" j);
    cache_read = replace_present u.cache_read (int_mem "cache_read_input_tokens" j);
    reasoning = reasoning_of j u.reasoning;
  }

let usage_part t = if t.have_usage then [ Stream_part.Usage t.usage ] else []

let stop_of = function
  | "end_turn" | "pause_turn" | "stop_sequence" -> `Stop
  | "max_tokens" | "model_context_window_exceeded" -> `Length
  | "tool_use" -> `Tool_calls
  | "refusal" | "content_filtered" | "guardrail_intervened" -> `Content_filter
  | other -> `Error (Fmt.str "unsupported stop reason %S" other)

let terminal t parts =
  t.terminated <- true;
  usage_part t @ parts

let malformed t msg = terminal t [ Stream_part.Finish (`Error msg) ]

let event_name ~event j =
  if event = "" || event = "message" then Option.value ~default:"" (string_mem "type" j)
  else event

let content_block_start t j =
  let index = or_zero (int_mem "index" j) in
  match mem j "content_block" with
  | None -> malformed t "content_block_start has no content_block"
  | Some cb when not (Jsont.Json.sort cb = Jsont.Sort.Object) ->
      malformed t "content_block_start content_block is not an object"
  | Some cb -> (
      match string_mem "type" cb with
      | Some "text" ->
          Hashtbl.replace t.blocks index Text;
          []
      | Some "thinking" | Some "redacted_thinking" ->
          Hashtbl.replace t.blocks index Thinking;
          []
      | Some "tool_use" -> (
          match (string_mem "id" cb, string_mem "name" cb) with
          | Some id, Some name when id <> "" && name <> "" ->
              let arguments =
                match mem cb "input" with
                | Some input when Jsont.Json.sort input <> Jsont.Sort.Null ->
                    let encoded = string_of_json input in
                    if encoded = "{}" then "" else encoded
                | Some _ | None -> ""
              in
              let block = { id; arguments; closed = false } in
              Hashtbl.replace t.blocks index (Tool block);
              let initial =
                if arguments = "" || arguments = "{}" then []
                else [ Stream_part.Tool_input_delta { id; delta = arguments } ]
              in
              Stream_part.Tool_call_start { id; name } :: initial
          | _ -> malformed t "tool_use block is missing id or name")
      | Some _ -> []
      | None -> malformed t "content_block_start has no type")

let content_block_delta t j =
  let index = or_zero (int_mem "index" j) in
  match mem j "delta" with
  | None -> []
  | Some d -> (
      match string_mem "type" d with
      | Some "text_delta" -> (
          match string_mem "text" d with
          | Some s -> [ Stream_part.Text_delta s ]
          | None -> [])
      | Some "thinking_delta" -> (
          match string_mem "thinking" d with
          | Some s -> [ Stream_part.Reasoning_delta s ]
          | None -> [])
      | Some "input_json_delta" -> (
          match Hashtbl.find_opt t.blocks index with
          | Some (Tool block) when not block.closed -> (
              match string_mem "partial_json" d with
              | Some s when s <> "" ->
                  block.arguments <- block.arguments ^ s;
                  [ Stream_part.Tool_input_delta { id = block.id; delta = s } ]
              | Some _ | None -> [])
          | Some (Tool _) -> malformed t "tool input arrived after content_block_stop"
          | Some Text | Some Thinking | None -> [])
      | _ -> [])

let content_block_stop t j =
  let index = or_zero (int_mem "index" j) in
  match Hashtbl.find_opt t.blocks index with
  | Some (Tool block) when block.closed -> []
  | Some (Tool block) ->
      if valid_arguments block.arguments then begin
        block.closed <- true;
        [ Stream_part.Tool_call_end block.id ]
      end
      else malformed t "tool call arguments are not valid JSON"
  | Some Text | Some Thinking | None -> []

let message_start t j =
  (match mem j "message" with
  | Some m when Jsont.Json.sort m = Jsont.Sort.Object -> (
      match mem m "usage" with
      | Some u when Jsont.Json.sort u = Jsont.Sort.Object ->
          t.usage <- snapshot_usage t.usage u;
          t.have_usage <- true
      | Some _ | None -> ())
  | Some _ | None -> ());
  []

let message_delta t j =
  (match mem j "usage" with
  | Some u when Jsont.Json.sort u = Jsont.Sort.Object ->
      t.usage <- snapshot_usage t.usage u;
      t.have_usage <- true
  | Some _ | None -> ());
  (match mem j "delta" with
  | Some d -> (
      match string_mem "stop_reason" d with
      | Some s when s <> "" -> t.stop <- Some s
      | _ -> ())
  | None -> ());
  []

let has_open_tool t =
  Hashtbl.fold
    (fun _ block open_ ->
      open_
      || match block with Tool block -> not block.closed | Text | Thinking -> false)
    t.blocks false

let message_stop t =
  if has_open_tool t then malformed t "message_stop arrived before tool call completion"
  else begin
    t.saw_stop <- true;
    let finish =
      match t.stop with
      | Some s -> Stream_part.Finish (stop_of s)
      | None ->
          Stream_part.Finish
            (`Error "stream ended after message_stop without a stop reason")
    in
    terminal t [ finish ]
  end

let error_event t j =
  let detail =
    match mem j "error" with
    | Some e -> (
        match string_mem "message" e with Some m -> m | None -> "unknown provider error")
    | None -> "unknown provider error"
  in
  terminal t [ Stream_part.Finish (`Error detail) ]

let feed t ~event ~data =
  if t.terminated then []
  else
    match Jsont_bytesrw.decode_string Jsont.json data with
    | Error e ->
        malformed t
          (Fmt.str "malformed stream event %s: %s"
             (if event = "" then "data" else event)
             e)
    | Ok j -> (
        if not (is_object j) then malformed t "event is not a JSON object"
        else
          match event_name ~event j with
          | "" -> malformed t (Fmt.str "stream event without a type: %S" data)
          | "message_start" -> message_start t j
          | "content_block_start" -> content_block_start t j
          | "content_block_delta" -> content_block_delta t j
          | "content_block_stop" -> content_block_stop t j
          | "message_delta" -> message_delta t j
          | "message_stop" -> message_stop t
          | "error" -> error_event t j
          | "ping" -> []
          | _ -> [])

let finish t =
  if t.terminated then []
  else
    terminal t
      [
        Stream_part.Finish
          (`Error
             (if t.saw_stop then "stream ended without a stop reason"
              else "stream closed before message_stop"));
      ]
