open Lwt.Infix

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

let render_last_turn messages =
  String.concat "\n" (List.map Session.message_text messages)

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

let review t ~sw ~clock (model : Models.resolved) ~context ~last_turn =
  if (not t.enabled) || t.quarantined then Lwt.return (Ok None)
  else
    let user = render_last_turn last_turn in
    let stream =
      Charamel_fantasy.Provider.stream model.Models.provider ~stop:sw ~clock
        ~model:model.Models.model
        ~system:[ Prompt_advisor.text; context ]
        ~max_tokens:512
        [ Charamel_fantasy.Message.text Charamel_fantasy.Message.User user ]
    in
    let text = Buffer.create 512 in
    let settle () =
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
          else Ok (Some verdict)
    in
    let rec consume () =
      Lwt_stream.get stream >>= fun (item : Charamel_fantasy.Stream_part.t option) ->
      match item with
      | None -> Lwt.return (Error (`Provider "advisor stream ended before completion"))
      | Some (Text_delta delta) ->
          Buffer.add_string text delta;
          consume ()
      | Some (Reasoning_delta _) -> consume ()
      | Some (Tool_call_start _) -> consume ()
      | Some (Tool_input_delta _) -> consume ()
      | Some (Tool_call_end _) -> consume ()
      | Some (Usage _) -> consume ()
      | Some (Finish (`Error message)) -> Lwt.return (Error (`Provider message))
      | Some (Finish (`Stop | `Length | `Content_filter | `Tool_calls)) ->
          Lwt.return (settle ())
    in
    consume ()

let steering_message verdict =
  Charamel_fantasy.Message.text Charamel_fantasy.Message.User
    ("[advisor blocker] " ^ verdict.guidance)

let reset t =
  t.previous <- None;
  t.consecutive <- 0;
  t.quarantined <- false
