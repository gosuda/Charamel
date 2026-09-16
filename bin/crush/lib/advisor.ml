type verdict = { severity : [ `Nit | `Concern | `Blocker ]; guidance : string }

let severity_jsont =
  Jsont.enum [ ("nit", `Nit); ("concern", `Concern); ("blocker", `Blocker) ]

let verdict_jsont : verdict Jsont.t =
  Jsont.Object.map (fun severity guidance -> { severity; guidance })
  |> Jsont.Object.mem "severity" severity_jsont ~enc:(fun verdict -> verdict.severity)
  |> Jsont.Object.mem "guidance" Jsont.string ~enc:(fun verdict -> verdict.guidance)
  |> Jsont.Object.finish

type t = {
  enabled : bool;
  _model : [ `Small | `Large ];
  _every_n_turns : int;
  mutable previous : string option;
  mutable consecutive : int;
  mutable quarantined : bool;
}

let create (config : Config.advisor) =
  {
    enabled = config.enabled;
    _model = config.model;
    _every_n_turns = max 1 config.every_n_turns;
    previous = None;
    consecutive = 0;
    quarantined = false;
  }

let json_string json =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json json with
  | Ok text -> text
  | Error _ -> "<invalid-json>"

let output_string = function
  | `Text text -> text
  | `Error text -> "error: " ^ text
  | `Media (mime, data) -> Fmt.str "media <%s> (%d bytes)" mime (String.length data)

let part_text = function
  | Charm_fantasy.Message.Text text -> text
  | Charm_fantasy.Message.Reasoning { text; _ } -> "<reasoning>" ^ text ^ "</reasoning>"
  | Charm_fantasy.Message.File { mime; data; name } ->
      Fmt.str "<file mime=%s name=%s bytes=%d>" mime
        (Option.value ~default:"" name)
        (String.length data)
  | Charm_fantasy.Message.Tool_call { id; name; input } ->
      Fmt.str "call %s (%s): %s" id name (json_string input)
  | Charm_fantasy.Message.Tool_result { id; name; output } ->
      Fmt.str "result %s (%s): %s" id name (output_string output)

let message_text { Charm_fantasy.Message.role; parts } =
  let role =
    match role with
    | Charm_fantasy.Message.System -> "system"
    | Charm_fantasy.Message.User -> "user"
    | Charm_fantasy.Message.Assistant -> "assistant"
    | Charm_fantasy.Message.Tool -> "tool"
  in
  role ^ ": " ^ String.concat "" (List.map part_text parts)

let render_last_turn messages = String.concat "\n" (List.map message_text messages)

let first_json_object text =
  match String.index_opt text '{' with
  | None -> None
  | Some start ->
      let length = String.length text in
      let rec scan index depth in_string escaped =
        if index >= length then None
        else
          let character = String.get text index in
          if in_string then
            if escaped then scan (index + 1) depth true false
            else if character = '\\' then scan (index + 1) depth true true
            else if character = '"' then scan (index + 1) depth false false
            else scan (index + 1) depth true false
          else if character = '"' then scan (index + 1) depth true false
          else if character = '{' then scan (index + 1) (depth + 1) false false
          else if character = '}' then
            if depth = 1 then Some (String.sub text start (index - start + 1))
            else scan (index + 1) (depth - 1) false false
          else scan (index + 1) depth false false
      in
      scan start 0 false false

let decoded_verdict text =
  match first_json_object text with
  | None -> None
  | Some object_text -> (
      match Jsont_bytesrw.decode_string Jsont.json object_text with
      | Error _ -> None
      | Ok json -> (
          match Jsont.Json.decode verdict_jsont json with
          | Ok verdict -> Some verdict
          | Error _ -> None))

let fingerprint verdict =
  let severity =
    match verdict.severity with
    | `Nit -> "nit"
    | `Concern -> "concern"
    | `Blocker -> "blocker"
  in
  Digestif.SHA256.(digest_string (severity ^ "\000" ^ verdict.guidance) |> to_hex)

let review t ~sw ~clock ~net (model : Models.resolved) ~context ~last_turn =
  if (not t.enabled) || t.quarantined then Ok None
  else
    let user = render_last_turn last_turn in
    let stream =
      Charm_fantasy.Provider.stream model.provider ~sw ~clock ~net ~model:model.model
        ~system:[ Prompt_advisor.text; context ]
        ~max_tokens:512
        [ Charm_fantasy.Message.text Charm_fantasy.Message.User user ]
    in
    let text = Buffer.create 512 in
    let rec consume () =
      match Eio.Stream.take stream with
      | Charm_fantasy.Stream_part.Text_delta delta ->
          Buffer.add_string text delta;
          consume ()
      | Reasoning_delta _ -> consume ()
      | Tool_call_start _ -> consume ()
      | Tool_input_delta _ -> consume ()
      | Tool_call_end _ -> consume ()
      | Usage _ -> consume ()
      | Finish (`Error message) -> Error (`Provider message)
      | Finish (`Stop | `Length | `Content_filter | `Tool_calls) -> (
          match decoded_verdict (Buffer.contents text) with
          | None -> Ok None
          | Some verdict ->
              let current = fingerprint verdict in
              (match t.previous with
              | Some previous when String.equal previous current ->
                  t.consecutive <- t.consecutive + 1
              | _ ->
                  t.previous <- Some current;
                  t.consecutive <- 1);
              if t.consecutive >= 2 then (
                t.quarantined <- true;
                Ok None)
              else Ok (Some verdict))
    in
    consume ()

let steering_message verdict =
  Charm_fantasy.Message.text Charm_fantasy.Message.User
    ("[advisor blocker] " ^ verdict.guidance)

let reset t =
  t.previous <- None;
  t.consecutive <- 0;
  t.quarantined <- false
