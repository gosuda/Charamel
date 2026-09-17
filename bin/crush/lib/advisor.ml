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
    enabled = config.Config.enabled;
    _model = config.Config.model;
    _every_n_turns = max 1 config.Config.every_n_turns;
    previous = None;
    consecutive = 0;
    quarantined = false;
  }

let output_string = function
  | `Text text -> text
  | `Error text -> "error: " ^ text
  | `Media (mime, data) -> Fmt.str "media <%s> (%d bytes)" mime (String.length data)

let part_text = function
  | Charamel_fantasy.Message.Text text -> text
  | Charamel_fantasy.Message.Reasoning { text; _ } ->
      "<reasoning>" ^ text ^ "</reasoning>"
  | Charamel_fantasy.Message.File { mime; data; name } ->
      Fmt.str "<file mime=%s name=%s bytes=%d>" mime
        (Option.value ~default:"" name)
        (String.length data)
  | Charamel_fantasy.Message.Tool_call { id; name; input } ->
      Fmt.str "call %s (%s): %s" id name (Jsonx.display_string input)
  | Charamel_fantasy.Message.Tool_result { id; name; output } ->
      Fmt.str "result %s (%s): %s" id name (output_string output)

let message_text { Charamel_fantasy.Message.role; parts } =
  let role =
    match role with
    | Charamel_fantasy.Message.System -> "system"
    | Charamel_fantasy.Message.User -> "user"
    | Charamel_fantasy.Message.Assistant -> "assistant"
    | Charamel_fantasy.Message.Tool -> "tool"
  in
  role ^ ": " ^ String.concat "" (List.map part_text parts)

let render_last_turn messages = String.concat "\n" (List.map message_text messages)

let first_json_object text =
  Option.bind (String.index_opt text '{') (fun start ->
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
      scan start 0 false false)

let decoded_verdict text =
  Option.bind (first_json_object text) (fun object_text ->
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
      Charamel_fantasy.Provider.stream model.Models.provider ~sw ~clock ~net
        ~model:model.Models.model
        ~system:[ Prompt_advisor.text; context ]
        ~max_tokens:512
        [ Charamel_fantasy.Message.text Charamel_fantasy.Message.User user ]
    in
    let text = Buffer.create 512 in
    let rec consume () =
      match Eio.Stream.take stream with
      | Charamel_fantasy.Stream_part.Text_delta delta ->
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
  Charamel_fantasy.Message.text Charamel_fantasy.Message.User
    ("[advisor blocker] " ^ verdict.guidance)

let reset t =
  t.previous <- None;
  t.consecutive <- 0;
  t.quarantined <- false
