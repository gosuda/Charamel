open Lwt.Infix

let utf8_prefix s limit =
  if limit <= 0 then ""
  else
    let length = String.length s in
    let rec loop index count =
      if index >= length || count >= limit then String.sub s 0 index
      else
        let decoded = String.get_utf_8_uchar s index in
        let step = max 1 (Uchar.utf_decode_length decoded) in
        loop (min length (index + step)) (count + 1)
    in
    loop 0 0

let strip_think text =
  let block = Re.Perl.compile_pat ~opts:[ `Dotall ] "<think>.*?</think>" in
  let tags = Re.Perl.compile_pat "</?think>" in
  text |> Re.replace_string block ~by:"" |> Re.replace_string tags ~by:""

let generate ~sw ~clock ~(small : Models.resolved) ~first_prompt =
  let prompt = utf8_prefix first_prompt 2_000 in
  let messages = [ Charamel_fantasy.Message.text Charamel_fantasy.Message.User prompt ] in
  let stream =
    Charamel_fantasy.Provider.stream small.Models.provider ~stop:sw ~clock
      ~model:small.Models.model ~system:[ Prompt_title.text ] ~max_tokens:40 messages
  in
  let output = Buffer.create 128 in
  let cleaned_title () =
    let cleaned = String.trim (strip_think (Buffer.contents output)) in
    if cleaned = "" then utf8_prefix prompt 60 else utf8_prefix cleaned 80
  in
  let rec consume () =
    Lwt_stream.get stream >>= fun (item : Charamel_fantasy.Stream_part.t option) ->
    match item with
    | None -> Lwt.return (Error (`Provider "title stream ended before completion"))
    | Some (Text_delta text) ->
        Buffer.add_string output text;
        consume ()
    | Some (Reasoning_delta _) -> consume ()
    | Some (Tool_call_start _) -> consume ()
    | Some (Tool_input_delta _) -> consume ()
    | Some (Tool_call_end _) -> consume ()
    | Some (Usage _) -> consume ()
    | Some (Finish (`Error message)) -> Lwt.return (Error (`Provider message))
    | Some (Finish (`Stop | `Length | `Content_filter | `Tool_calls)) ->
        Lwt.return (Ok (cleaned_title ()))
  in
  consume ()
