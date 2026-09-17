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
  | Charm_fantasy.Message.Text text -> text
  | Charm_fantasy.Message.Reasoning { text; _ } -> "<reasoning>" ^ text ^ "</reasoning>"
  | Charm_fantasy.Message.File { mime; data; name } ->
      Fmt.str "file <%s> <%s> (%d bytes)" mime
        (Option.value ~default:"" name)
        (String.length data)
  | Charm_fantasy.Message.Tool_call { id; name; input } ->
      Fmt.str "call %s (%s): %s" id name (Jsonx.display_string input)
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
            Fmt.str "usage: %a cost=$%.6f" Charm_fantasy.Usage.pp usage cost_usd :: !lines
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

let run ~sw ~clock ~net ~(small : Models.resolved) ~auth session =
  ignore auth;
  let events = Session.events session in
  let through, _recent = split events ~keep_tokens:20_000 in
  if through < 0 then Ok ""
  else
    let rendered = render_prefix events through in
    let messages = [ Charm_fantasy.Message.text Charm_fantasy.Message.User rendered ] in
    let stream =
      Charm_fantasy.Provider.stream small.Models.provider ~sw ~clock ~net
        ~model:small.Models.model ~system:[ Prompt_summarize.text ] ~max_tokens:4096
        messages
    in
    let summary = Buffer.create 1024 in
    let usage = ref Charm_fantasy.Usage.zero in
    let rec consume () =
      match Eio.Stream.take stream with
      | Charm_fantasy.Stream_part.Text_delta text ->
          Buffer.add_string summary text;
          consume ()
      | Reasoning_delta _ -> consume ()
      | Tool_call_start _ -> consume ()
      | Tool_input_delta _ -> consume ()
      | Tool_call_end _ -> consume ()
      | Usage value ->
          usage := Charm_fantasy.Usage.add !usage value;
          consume ()
      | Finish (`Error message) -> Error (`Provider message)
      | Finish (`Stop | `Length | `Content_filter | `Tool_calls) -> (
          let text = String.trim (Buffer.contents summary) in
          let ms = int_of_float (Eio.Time.now clock *. 1000.) in
          let model =
            {
              Session.provider = small.Models.provider_id;
              model = small.Models.model.Charm_fantasy.Model.id;
            }
          in
          let persist_usage () =
            if Charm_fantasy.Usage.total !usage = 0 then Ok ()
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
          match persist_usage () with
          | Error error -> Error (`Session error)
          | Ok () -> (
              match
                Session.append session ~clock (Session.Summary { ms; text; through })
              with
              | Ok () -> Ok text
              | Error error -> Error (`Session error)))
    in
    consume ()
