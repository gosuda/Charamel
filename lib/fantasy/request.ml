type reasoning = Off | Low | Medium | High
type auth = Api_key | Oauth

type t = {
  model : Model.t;
  auth : auth;
  system : string list;
  tools : Tool.t list;
  max_tokens : int;
  temperature : float option;
  reasoning : reasoning;
  messages : Message.t list;
}

let of_call ~model ~auth ?(system = []) ?(tools = []) ?max_tokens ?temperature
    ?(reasoning = Off) messages =
  {
    model;
    auth;
    system;
    tools;
    max_tokens =
      (match max_tokens with
      | Some n when n > 0 -> n
      | Some _ | None -> model.Model.default_max_tokens);
    temperature;
    reasoning;
    messages;
  }

let system_blocks (r : t) =
  let from_messages =
    List.concat_map
      (fun (m : Message.t) ->
        match m.Message.role with
        | Message.System ->
            List.filter_map
              (fun p -> match p with Message.Text s -> Some s | _ -> None)
              m.Message.parts
        | Message.User | Message.Assistant | Message.Tool -> [])
      r.messages
  in
  r.system @ from_messages

let effective_max_tokens (r : t) =
  if r.max_tokens > 0 then r.max_tokens else r.model.Model.default_max_tokens
