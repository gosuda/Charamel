open Lwt.Infix

type auth = Api_key of string | Oauth of Oauth.Credential.t

type t = {
  kind : [ `Anthropic | `Openai_compat | `Responses | `Google ];
  base_url : string;
  auth : auth;
  extra_headers : (string * string) list;
}

let anthropic ?(base_url = "https://api.anthropic.com") ~auth () =
  { kind = `Anthropic; base_url; auth; extra_headers = [] }

let openai_compatible ~base_url ?(headers = []) ~auth () =
  { kind = `Openai_compat; base_url; auth; extra_headers = headers }

let openai_responses ?(base_url = "https://api.openai.com/v1") ~auth () =
  { kind = `Responses; base_url; auth; extra_headers = [] }

let google ?(base_url = "https://generativelanguage.googleapis.com") ~auth () =
  { kind = `Google; base_url; auth; extra_headers = [] }

module type CODEC = sig
  type t

  val encode : Request.t -> Jsont.json
  val create : unit -> t
  val feed : t -> event:string -> data:string -> Stream_part.t list
  val finish : t -> Stream_part.t list
end

let codec_for = function
  | `Anthropic -> (module Anthropic_codec : CODEC)
  | `Openai_compat -> (module Openai_compat_codec : CODEC)
  | `Responses -> (module Responses_codec : CODEC)
  | `Google -> (module Google_codec : CODEC)

let oauth_beta_header =
  String.concat ","
    [
      "oauth-2025-04-20";
      "interleaved-thinking-2025-05-14";
      "context-management-2025-06-27";
      "prompt-caching-scope-2026-01-05";
      "structured-outputs-2025-12-15";
    ]

let claude_code_user_agent = "claude-cli/2.1.257 (external, cli)"

let anthropic_builtin_tool_names =
  [ "web_search"; "code_execution"; "text_editor"; "computer" ]

module String_set = Set.Make (String)

type tool_mapping = { to_wire : string -> string; of_wire : string -> string }

let oauth_anthropic ~kind = function Oauth _ when kind = `Anthropic -> true | _ -> false

let unique_names names =
  let _seen, reversed =
    List.fold_left
      (fun (seen, reversed) name ->
        if String_set.mem name seen then (seen, reversed)
        else (String_set.add name seen, name :: reversed))
      (String_set.empty, []) names
  in
  List.rev reversed

let message_tool_names messages =
  List.concat_map
    (fun (message : Message.t) ->
      List.filter_map
        (function
          | Message.Tool_call { name; _ } | Message.Tool_result { name; _ } -> Some name
          | _ -> None)
        message.Message.parts)
    messages

let build_tool_mapping ~kind ~auth ~tools ~messages =
  if not (oauth_anthropic ~kind auth) then { to_wire = Fun.id; of_wire = Fun.id }
  else
    let names =
      unique_names
        (List.map (fun (tool : Tool.t) -> tool.Tool.name) tools
        @ message_tool_names messages)
    in
    let reserved =
      List.fold_left
        (fun set name -> String_set.add name set)
        (String_set.of_list anthropic_builtin_tool_names)
        names
    in
    let is_builtin name =
      List.mem (String.lowercase_ascii name) anthropic_builtin_tool_names
    in
    let rec assign reserved mapping = function
      | [] -> (mapping, reserved)
      | name :: rest when List.assoc_opt name mapping <> None ->
          assign reserved mapping rest
      | name :: rest when is_builtin name ->
          let rec choose prefix =
            let candidate = String.make prefix '_' ^ name in
            if String_set.mem candidate reserved then choose (prefix + 1) else candidate
          in
          let wire = choose 1 in
          assign (String_set.add wire reserved) ((name, wire) :: mapping) rest
      | name :: rest -> assign reserved ((name, name) :: mapping) rest
    in
    let mapping, _ = assign reserved [] names in
    let lookup pairs name = Option.value ~default:name (List.assoc_opt name pairs) in
    {
      to_wire = lookup mapping;
      of_wire = lookup (List.map (fun (original, wire) -> (wire, original)) mapping);
    }

let wire_tools mapping tools =
  List.map
    (fun (tool : Tool.t) ->
      let name = mapping.to_wire tool.Tool.name in
      if String.equal name tool.Tool.name then tool else { tool with name })
    tools

let wire_messages mapping messages =
  List.map
    (fun (message : Message.t) ->
      let parts =
        List.map
          (function
            | Message.Tool_call ({ name; _ } as call) ->
                Message.Tool_call { call with name = mapping.to_wire name }
            | Message.Tool_result ({ name; _ } as result) ->
                Message.Tool_result { result with name = mapping.to_wire name }
            | part -> part)
          message.Message.parts
      in
      { message with parts })
    messages

let caller_stream_part mapping = function
  | Stream_part.Tool_call_start ({ name; _ } as call) ->
      Stream_part.Tool_call_start { call with name = mapping.of_wire name }
  | part -> part

let request_auth = function Api_key _ -> Request.Api_key | Oauth _ -> Request.Oauth

let fresh_auth t ~clock : (auth, Error.t) result Lwt.t =
  match (t.kind, t.auth) with
  | `Anthropic, Oauth credential ->
      Lwt_result.map
        (fun credential -> Oauth credential)
        (Oauth.Anthropic.ensure_fresh ~clock credential)
  | _ -> Lwt.return (Ok t.auth)

let auth_headers ~kind ~auth =
  match (kind, auth) with
  | `Anthropic, Api_key key -> [ ("x-api-key", key); ("anthropic-version", "2023-06-01") ]
  | `Anthropic, Oauth credential ->
      [
        ("Authorization", "Bearer " ^ credential.Oauth.Credential.access);
        ("anthropic-version", "2023-06-01");
        ("anthropic-beta", oauth_beta_header);
        ("User-Agent", claude_code_user_agent);
        ("x-app", "cli");
        ("anthropic-dangerous-direct-browser-access", "true");
      ]
  | (`Openai_compat | `Responses), Api_key key -> [ ("Authorization", "Bearer " ^ key) ]
  | (`Openai_compat | `Responses), Oauth credential ->
      [ ("Authorization", "Bearer " ^ credential.Oauth.Credential.access) ]
  | `Google, Api_key key -> [ ("x-goog-api-key", key) ]
  | `Google, Oauth credential ->
      [ ("Authorization", "Bearer " ^ credential.Oauth.Credential.access) ]

let url_and_headers t auth model =
  match t.kind with
  | `Anthropic -> (t.base_url ^ "/v1/messages", auth_headers ~kind:t.kind ~auth)
  | `Openai_compat ->
      (t.base_url ^ "/chat/completions", auth_headers ~kind:t.kind ~auth @ t.extra_headers)
  | `Responses -> (t.base_url ^ "/responses", auth_headers ~kind:t.kind ~auth)
  | `Google ->
      ( Fmt.str "%s/v1beta/models/%s:streamGenerateContent?alt=sse" t.base_url
          model.Model.id,
        auth_headers ~kind:t.kind ~auth )

(* [Lwt_stream] has no blocking push, so the queue a consumer drains is unbounded: a slow
   consumer buffers the response rather than throttling the connection, and [?stop] — not
   back-pressure — is what ends a stream nobody reads any more. *)
let stream t ?stop ~clock ~model ?(system = []) ?(tools = []) ?max_tokens ?temperature
    ?(reasoning = `Off) ?(on_error = fun (_ : Error.t) -> ()) messages =
  let output, push = Lwt_stream.create () in
  let finished = ref false in
  let reported = ref false in
  let emit parts =
    List.iter
      (fun part ->
        if not !finished then (
          (match part with Stream_part.Finish _ -> finished := true | _ -> ());
          push (Some part)))
      parts
  in
  let finish_error error =
    if not !reported then (
      reported := true;
      on_error error);
    if not !finished then emit [ Stream_part.Finish (`Error (Error.message error)) ]
  in
  let call auth =
    let module Codec = (val codec_for t.kind : CODEC) in
    let mapping = build_tool_mapping ~kind:t.kind ~auth ~tools ~messages in
    let caller_parts parts = List.map (caller_stream_part mapping) parts in
    let request =
      Request.of_call ~model ~auth:(request_auth auth) ~system
        ~tools:(wire_tools mapping tools) ?max_tokens ?temperature
        ~reasoning:
          (match reasoning with
          | `Off -> Request.Off
          | `Low -> Request.Low
          | `Medium -> Request.Medium
          | `High -> Request.High)
        (wire_messages mapping messages)
    in
    let codec = Codec.create () in
    let body = Json.string_of_json (Codec.encode request) in
    let url, headers = url_and_headers t auth model in
    let transport = Transport.make () in
    let consume =
      Transport.Sse
        (fun ~event ~data -> emit (caller_parts (Codec.feed codec ~event ~data)))
    in
    Transport.call transport ~url ~meth:`POST ~headers ~body ~consume () >>= function
    | Ok _ ->
        emit (caller_parts (Codec.finish codec));
        emit [ Stream_part.Finish (`Error "stream ended without a terminal event") ];
        Lwt.return_unit
    | Error error ->
        finish_error error;
        Lwt.return_unit
  in
  let work () =
    Lwt.finalize
      (fun () ->
        fresh_auth t ~clock >>= function
        | Error error ->
            finish_error error;
            Lwt.return_unit
        | Ok auth -> call auth)
      (fun () ->
        push None;
        Lwt.return_unit)
  in
  let worker = work () in
  (match stop with
  | None -> ()
  | Some stop ->
      Lwt_switch.add_hook (Some stop) (fun () ->
          Lwt.cancel worker;
          Lwt.return_unit));
  (* Lwt reports a promise rejected by cancellation through the async exception hook,
     which aborts the process; an abandoned turn is the normal case here, so only the
     cancellation is absorbed and every other failure still surfaces. The worker's own
     Lwt.finalize closes the stream and releases the connection either way. *)
  Lwt.async (fun () ->
      Lwt.catch
        (fun () -> worker)
        (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn));
  output
