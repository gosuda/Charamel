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
    max_tokens = Option.value ~default:model.Model.default_max_tokens max_tokens;
    temperature;
    reasoning;
    messages;
  }
