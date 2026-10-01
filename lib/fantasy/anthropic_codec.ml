open Json

type item = bool -> Jsont.json

let jm k v = Jsont.Json.mem (n k) v
let cache_control = obj [ jm "type" (str "ephemeral") ]

let with_cache (ms : Jsont.mem list) cache =
  if cache then ms @ [ jm "cache_control" cache_control ] else ms

let base64_source mime data =
  obj [ jm "type" (str "base64"); jm "media_type" (str mime); jm "data" (str data) ]

let text_source data = obj [ jm "type" (str "text"); jm "data" (str data) ]

let text_item s cache =
  obj (with_cache [ jm "type" (str "text"); jm "text" (str s) ] cache)

let image_item mime data cache =
  obj
    (with_cache [ jm "type" (str "image"); jm "source" (base64_source mime data) ] cache)

let document_item ?name source cache =
  let ms = [ jm "type" (str "document"); jm "source" source ] in
  let ms = match name with Some n -> ms @ [ jm "title" (str n) ] | None -> ms in
  obj (with_cache ms cache)

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
      jm "type" (str "tool_result"); jm "tool_use_id" (str id); jm "content" (arr content);
    ]
  in
  let ms = if is_error then ms @ [ jm "is_error" (bool true) ] else ms in
  obj (with_cache ms cache)

let tool_use_item ~id ~name input cache =
  obj
    (with_cache
       [
         jm "type" (str "tool_use");
         jm "id" (str id);
         jm "name" (str name);
         jm "input" input;
       ]
       cache)

let thinking_item ~text ~signature cache =
  obj
    (with_cache
       [
         jm "type" (str "thinking");
         jm "thinking" (str text);
         jm "signature" (str signature);
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
    match r.Request.auth with Request.Oauth -> [ oauth_system ] | Request.Api_key -> []
  in
  prefix @ Request.system_blocks r

let system_json (r : Request.t) =
  match system_texts r with
  | [] -> []
  | texts ->
      let last = List.length texts - 1 in
      [ jm "system" (arr (List.mapi (fun i s -> text_item s (i = last)) texts)) ]

let wire_role (m : Message.t) =
  match m.Message.role with
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
    List.concat_map
      (fun p -> if is_user then user_part p else assistant_part p)
      m.Message.parts
  in
  List.concat_map one ms

let message_json is_user items cache =
  let last = List.length items - 1 in
  let role = if is_user then "user" else "assistant" in
  obj
    [
      jm "role" (str role);
      jm "content" (arr (List.mapi (fun i f -> f (cache && i = last)) items));
    ]

let messages_json (r : Request.t) =
  let wire_messages =
    List.filter (fun m -> Option.is_some (wire_role m)) r.Request.messages
  in
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
          (arr
             (List.mapi
                (fun i (t : Tool.t) ->
                  obj
                    (with_cache
                       [
                         jm "name" (str t.Tool.name);
                         jm "description" (str t.Tool.description);
                         jm "input_schema" t.Tool.schema;
                       ]
                       (i = last)))
                tools));
      ]

let budget = function
  | Request.Off -> None
  | Request.Low -> Some 1024
  | Request.Medium -> Some 8192
  | Request.High -> Some 32768

let thinking_budget (r : Request.t) =
  Option.bind (budget r.Request.reasoning) (fun requested ->
      let clamped = min requested (Request.effective_max_tokens r - 1) in
      if clamped > 0 then Some clamped else None)

let thinking_json (r : Request.t) =
  match thinking_budget r with
  | None -> []
  | Some b ->
      [ jm "thinking" (obj [ jm "type" (str "enabled"); jm "budget_tokens" (int b) ]) ]

let temperature_json (r : Request.t) =
  match (thinking_budget r, r.Request.temperature) with
  | Some _, _ -> []
  | None, None -> []
  | None, Some t -> [ jm "temperature" (num t) ]

let encode (r : Request.t) =
  obj
    ([
       jm "model" (str r.Request.model.Model.id);
       jm "max_tokens" (int (Request.effective_max_tokens r));
       jm "stream" (bool true);
       jm "messages" (arr (messages_json r));
     ]
    @ system_json r @ tools_json r.Request.tools @ thinking_json r @ temperature_json r)

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

let valid_arguments args = args = "" || Json.valid_json args
let or_zero = Option.value ~default:0
let replace_present current = function Some n -> n | None -> current

let reasoning_of (j : Jsont.json) current =
  match Json.oopt j "output_tokens_details" with
  | Some d when Json.is_object d ->
      replace_present current (Json.int_option_mem d "thinking_tokens")
  | Some _ | None -> current

let snapshot_usage (u : Usage.t) (j : Jsont.json) =
  {
    Usage.input = replace_present u.Usage.input (Json.int_option_mem j "input_tokens");
    output = replace_present u.Usage.output (Json.int_option_mem j "output_tokens");
    cache_write =
      replace_present u.Usage.cache_write
        (Json.int_option_mem j "cache_creation_input_tokens");
    cache_read =
      replace_present u.Usage.cache_read (Json.int_option_mem j "cache_read_input_tokens");
    reasoning = reasoning_of j u.Usage.reasoning;
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
  if event = "" || event = "message" then
    Option.value ~default:"" (Json.string_mem j "type")
  else event

let content_block_start t j =
  let index = or_zero (Json.int_option_mem j "index") in
  match Json.oopt j "content_block" with
  | None -> malformed t "content_block_start has no content_block"
  | Some cb when not (Json.is_object cb) ->
      malformed t "content_block_start content_block is not an object"
  | Some cb -> (
      match Json.string_mem cb "type" with
      | Some "text" ->
          Hashtbl.replace t.blocks index Text;
          []
      | Some "thinking" | Some "redacted_thinking" ->
          Hashtbl.replace t.blocks index Thinking;
          []
      | Some "tool_use" -> (
          match (Json.string_mem cb "id", Json.string_mem cb "name") with
          | Some id, Some name when id <> "" && name <> "" ->
              let arguments =
                match Json.oopt cb "input" with
                | Some input when Jsont.Json.sort input <> Jsont.Sort.Null ->
                    let encoded = Json.string_of_json input in
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
  let index = or_zero (Json.int_option_mem j "index") in
  match Json.oopt j "delta" with
  | None -> []
  | Some d -> (
      match Json.string_mem d "type" with
      | Some "text_delta" -> (
          match Json.string_mem d "text" with
          | Some s -> [ Stream_part.Text_delta s ]
          | None -> [])
      | Some "thinking_delta" -> (
          match Json.string_mem d "thinking" with
          | Some s -> [ Stream_part.Reasoning_delta s ]
          | None -> [])
      | Some "input_json_delta" -> (
          match Hashtbl.find_opt t.blocks index with
          | Some (Tool block) when not block.closed -> (
              match Json.string_mem d "partial_json" with
              | Some s when s <> "" ->
                  block.arguments <- block.arguments ^ s;
                  [ Stream_part.Tool_input_delta { id = block.id; delta = s } ]
              | Some _ | None -> [])
          | Some (Tool _) -> malformed t "tool input arrived after content_block_stop"
          | Some Text | Some Thinking | None -> [])
      | _ -> [])

let content_block_stop t j =
  let index = or_zero (Json.int_option_mem j "index") in
  match Hashtbl.find_opt t.blocks index with
  | Some (Tool block) when block.closed -> []
  | Some (Tool block) ->
      if valid_arguments block.arguments then begin
        block.closed <- true;
        [ Stream_part.Tool_call_end block.id ]
      end
      else malformed t "tool call arguments are not valid JSON"
  | Some Text | Some Thinking | None -> []

let take_usage t usage =
  match usage with
  | Some u when Json.is_object u ->
      t.usage <- snapshot_usage t.usage u;
      t.have_usage <- true
  | Some _ | None -> ()

let message_start t j =
  (match Json.oopt j "message" with
  | Some m when Json.is_object m -> take_usage t (Json.oopt m "usage")
  | Some _ | None -> ());
  []

let message_delta t j =
  take_usage t (Json.oopt j "usage");
  (match Json.oopt j "delta" with
  | Some d -> (
      match Json.string_mem d "stop_reason" with
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
    match Json.oopt j "error" with
    | Some e -> (
        match Json.string_mem e "message" with
        | Some m -> m
        | None -> "unknown provider error")
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
        if not (Json.is_object j) then malformed t "event is not a JSON object"
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
