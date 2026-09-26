open Lwt.Infix

let reserve ~context_window =
  if context_window <= 0 then 0
  else min 20_000 (int_of_float (Float.floor (float_of_int context_window *. 0.2)))

let needed ~context_window ~prompt_tokens ~completion_tokens ~disabled =
  if disabled || context_window <= 0 then false
  else
    let remaining = context_window - (max 0 prompt_tokens + max 0 completion_tokens) in
    remaining <= reserve ~context_window

let output_string = function
  | `Text text -> text
  | `Error text -> "error: " ^ text
  | `Media (mime, data) -> Fmt.str "media <%s> (%d bytes)" mime (String.length data)

let part_text = function
  | Charamel_fantasy.Message.Text text -> text
  | Charamel_fantasy.Message.Reasoning { text; _ } ->
      "<reasoning>" ^ text ^ "</reasoning>"
  | Charamel_fantasy.Message.File { mime; data; name } ->
      Fmt.str "file <%s> <%s> (%d bytes)" mime
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

let event_bytes = function
  | Session.Message { message; _ } -> String.length (message_text message)
  | _ -> 0

let suffix events first =
  let first = max 0 (min first (Array.length events)) in
  let rec loop index acc =
    if index = Array.length events then List.rev acc
    else loop (index + 1) (events.(index) :: acc)
  in
  loop first []

let split events ~keep_tokens =
  let n = Array.length events in
  let budget = max 0 keep_tokens * 4 in
  let newest = if n = 0 then 0 else event_bytes events.(n - 1) in
  let rec find index used =
    if index < 0 then -1
    else
      let bytes = event_bytes events.(index) in
      if bytes > 0 && used + bytes > budget then index else find (index - 1) (used + bytes)
  in
  (* The newest event is always retained; the summary prefix stops before the
     first older event that no longer fits the keep budget. *)
  let through = find (n - 2) newest in
  let first_kept = if through < 0 then 0 else through + 1 in
  (through, suffix events first_kept)

let render_prefix events through =
  if through < 0 then ""
  else
    let limit = min through (Array.length events - 1) in
    let lines = ref [] in
    for index = 0 to limit do
      match events.(index) with
      | Session.Message { message; _ } -> lines := message_text message :: !lines
      | Session.Summary { text; _ } -> lines := ("summary: " ^ text) :: !lines
      | Session.Note { text; _ } -> lines := ("note: " ^ text) :: !lines
      | Session.Tool_call { name; input; _ } ->
          lines := Fmt.str "call %s: %s" name (Jsonx.display_string input) :: !lines
      | Session.Tool_result { name; output; _ } ->
          let result =
            match output with
            | `Text text -> text
            | `Error text -> "error: " ^ text
            | `Media (mime, data) ->
                Fmt.str "media <%s> (%d bytes)" mime (String.length data)
          in
          lines := Fmt.str "result %s: %s" name result :: !lines
      | Session.Usage { usage; cost_usd; _ } ->
          lines :=
            Fmt.str "usage: %a cost=$%.6f" Charamel_fantasy.Usage.pp usage cost_usd
            :: !lines
      | Session.Permission { tool; action; path; decision; _ } ->
          let decision =
            match decision with
            | Session.Allow_once -> "allow_once"
            | Session.Allow_session -> "allow_session"
            | Session.Deny -> "deny"
          in
          lines := Fmt.str "permission: %s %s %s %s" decision tool action path :: !lines
    done;
    String.concat "\n" (List.rev !lines)

let run ~sw ~clock ~(small : Models.resolved) ~auth session =
  ignore auth;
  let events = Session.events session in
  let through, _recent = split events ~keep_tokens:20_000 in
  if through < 0 then Lwt.return (Ok "")
  else
    let rendered = render_prefix events through in
    let messages =
      [ Charamel_fantasy.Message.text Charamel_fantasy.Message.User rendered ]
    in
    let stream =
      Charamel_fantasy.Provider.stream small.Models.provider ~stop:sw ~clock
        ~model:small.Models.model ~system:[ Prompt_summarize.text ] ~max_tokens:4096
        messages
    in
    let summary = Buffer.create 1024 in
    let usage = ref Charamel_fantasy.Usage.zero in
    let persist text =
      let ms = int_of_float (Charamel_os.Time.now clock *. 1000.) in
      let model =
        {
          Session.provider = small.Models.provider_id;
          model = small.Models.model.Charamel_fantasy.Model.id;
        }
      in
      let persist_usage () =
        if Charamel_fantasy.Usage.total !usage = 0 then Lwt.return (Ok ())
        else
          Session.append session ~clock
            (Session.Usage
               {
                 ms;
                 usage = !usage;
                 cost_usd = Models.cost small.Models.model !usage;
                 model;
               })
      in
      persist_usage () >>= function
      | Error error -> Lwt.return (Error (`Session error))
      | Ok () -> (
          Session.append session ~clock (Session.Summary { ms; text; through })
          >|= function
          | Ok () -> Ok text
          | Error error -> Error (`Session error))
    in
    let rec consume () =
      Lwt_stream.get stream >>= fun (item : Charamel_fantasy.Stream_part.t option) ->
      match item with
      | None -> Lwt.return (Error (`Provider "compaction stream ended before completion"))
      | Some (Text_delta text) ->
          Buffer.add_string summary text;
          consume ()
      | Some (Reasoning_delta _) -> consume ()
      | Some (Tool_call_start _) -> consume ()
      | Some (Tool_input_delta _) -> consume ()
      | Some (Tool_call_end _) -> consume ()
      | Some (Usage value) ->
          usage := Charamel_fantasy.Usage.add !usage value;
          consume ()
      | Some (Finish (`Error message)) -> Lwt.return (Error (`Provider message))
      | Some (Finish (`Stop | `Length | `Content_filter | `Tool_calls)) ->
          persist (String.trim (Buffer.contents summary))
    in
    consume ()
