module String_map = Map.Make (String)
open Lwt.Infix
open Result.Syntax

type provider_kind = Anthropic | Openai | Openai_compatible | Openai_responses | Google

type provider = {
  kind : provider_kind;
  base_url : string option;
  api_key : string option;
  headers : (string * string) list;
  models : Charamel_fantasy.Model.t list;
}

type reasoning = [ `Off | `Low | `Medium | `High ]

type selected_model = {
  provider : string;
  model : string;
  reasoning : reasoning option;
  max_tokens : int option;
}

type models = { large : selected_model option; small : selected_model option }
type permissions = { allowed_tools : string list; deny : string list }
type mcp_transport = Stdio | Http

type mcp = {
  transport : mcp_transport;
  command : string option;
  args : string list;
  env : (string * string) list;
  url : string option;
  headers : (string * string) list;
  timeout_s : int;
}

type lsp = {
  command : string;
  args : string list;
  filetypes : string list;
  root_markers : string list;
  init_options : Jsont.json option;
}

type hook_event = Pre_tool | Post_tool | Session_start | Stop

type hook = {
  event : hook_event;
  matcher : string option;
  command : string;
  timeout_s : int;
}

type trailer = Trailer_none | Co_authored | Assisted
type attribution = { trailer : trailer; generated_with : bool }
type advisor = { enabled : bool; model : [ `Small | `Large ]; every_n_turns : int }
type budgets = { subagent_requests : int }

type options = {
  data_dir : string;
  debug : bool;
  disable_auto_compaction : bool;
  auto_lsp : bool;
  attribution : attribution;
  advisor : advisor;
  budgets : budgets;
}

type t = {
  providers : (string * provider) list;
  models : models;
  permissions : permissions;
  mcp : (string * mcp) list;
  lsp : (string * lsp) list;
  context_paths : string list;
  skills_paths : string list;
  hooks : hook list;
  options : options;
}

let default =
  {
    providers = [];
    models = { large = None; small = None };
    permissions = { allowed_tools = []; deny = [] };
    mcp = [];
    lsp = [];
    context_paths = [ "AGENTS.md"; "CLAUDE.md"; ".crush/rules/" ];
    skills_paths = [];
    hooks = [];
    options =
      {
        data_dir = ".crush";
        debug = false;
        disable_auto_compaction = false;
        auto_lsp = true;
        attribution = { trailer = Co_authored; generated_with = true };
        advisor = { enabled = false; model = `Small; every_n_turns = 1 };
        budgets = { subagent_requests = 200 };
      };
  }

let string_map_codec =
  let module M = Map.Make (String) in
  let map_codec = Jsont.Object.as_string_map Jsont.string in
  Jsont.map
    ~dec:(fun values -> M.bindings values)
    ~enc:(fun values ->
      List.fold_left (fun map (key, value) -> M.add key value map) M.empty values)
    map_codec

let provider_kind_codec =
  Jsont.enum
    [
      ("anthropic", Anthropic);
      ("openai", Openai);
      ("openai_compatible", Openai_compatible);
      ("openai_responses", Openai_responses);
      ("google", Google);
    ]

let reasoning_codec =
  Jsont.enum [ ("off", `Off); ("low", `Low); ("medium", `Medium); ("high", `High) ]

let mcp_transport_codec = Jsont.enum [ ("stdio", Stdio); ("http", Http) ]

let hook_event_codec =
  Jsont.enum
    [
      ("pre_tool", Pre_tool);
      ("post_tool", Post_tool);
      ("session_start", Session_start);
      ("stop", Stop);
    ]

let trailer_codec =
  Jsont.enum
    [ ("none", Trailer_none); ("co_authored", Co_authored); ("assisted", Assisted) ]

let advisor_model_codec = Jsont.enum [ ("small", `Small); ("large", `Large) ]

let provider_codec : provider Jsont.t =
  let open Jsont in
  Object.map (fun kind base_url api_key headers models ->
      ({ kind; base_url; api_key; headers; models } : provider))
  |> Object.mem "type" provider_kind_codec ~enc:(fun (provider : provider) ->
      provider.kind)
  |> Object.mem "base_url" (option string) ~dec_absent:None
       ~enc:(fun (provider : provider) -> provider.base_url)
       ~enc_omit:Option.is_none
  |> Object.mem "api_key" (option string) ~dec_absent:None
       ~enc:(fun (provider : provider) -> provider.api_key)
       ~enc_omit:Option.is_none
  |> Object.mem "headers" string_map_codec ~dec_absent:[]
       ~enc:(fun (provider : provider) -> provider.headers)
  |> Object.mem "models"
       (list (Charamel_fantasy.Model.jsont ~unknown:`Error))
       ~dec_absent:[]
       ~enc:(fun (provider : provider) -> provider.models)
  |> Object.error_unknown |> Object.finish

let selected_model_codec : selected_model Jsont.t =
  let open Jsont in
  Object.map (fun provider model reasoning max_tokens ->
      { provider; model; reasoning; max_tokens })
  |> Object.mem "provider" string ~enc:(fun (value : selected_model) -> value.provider)
  |> Object.mem "model" string ~enc:(fun (value : selected_model) -> value.model)
  |> Object.mem "reasoning" (option reasoning_codec) ~dec_absent:None
       ~enc:(fun (value : selected_model) -> value.reasoning)
       ~enc_omit:Option.is_none
  |> Object.mem "max_tokens" (option int) ~dec_absent:None
       ~enc:(fun (value : selected_model) -> value.max_tokens)
       ~enc_omit:Option.is_none
  |> Object.error_unknown |> Object.finish

let models_codec : models Jsont.t =
  let open Jsont in
  Object.map (fun large small -> { large; small })
  |> Object.mem "large" (option selected_model_codec) ~dec_absent:None
       ~enc:(fun value -> value.large)
       ~enc_omit:Option.is_none
  |> Object.mem "small" (option selected_model_codec) ~dec_absent:None
       ~enc:(fun value -> value.small)
       ~enc_omit:Option.is_none
  |> Object.error_unknown |> Object.finish

let permissions_codec =
  let open Jsont in
  Object.map (fun allowed_tools deny -> { allowed_tools; deny })
  |> Object.mem "allowed_tools" (list string) ~dec_absent:[] ~enc:(fun value ->
      value.allowed_tools)
  |> Object.mem "deny" (list string) ~dec_absent:[] ~enc:(fun value -> value.deny)
  |> Object.error_unknown |> Object.finish

let mcp_codec =
  let open Jsont in
  Object.map (fun transport command args env url headers timeout_s ->
      { transport; command; args; env; url; headers; timeout_s })
  |> Object.mem "transport" mcp_transport_codec ~dec_absent:Stdio
       ~enc:(fun (value : mcp) -> value.transport)
  |> Object.mem "command" (option string) ~dec_absent:None
       ~enc:(fun (value : mcp) -> value.command)
       ~enc_omit:Option.is_none
  |> Object.mem "args" (list string) ~dec_absent:[] ~enc:(fun (value : mcp) -> value.args)
  |> Object.mem "env" string_map_codec ~dec_absent:[] ~enc:(fun (value : mcp) ->
      value.env)
  |> Object.mem "url" (option string) ~dec_absent:None
       ~enc:(fun (value : mcp) -> value.url)
       ~enc_omit:Option.is_none
  |> Object.mem "headers" string_map_codec ~dec_absent:[] ~enc:(fun (value : mcp) ->
      value.headers)
  |> Object.mem "timeout_s" int ~dec_absent:10 ~enc:(fun (value : mcp) -> value.timeout_s)
  |> Object.error_unknown |> Object.finish

let lsp_codec =
  let open Jsont in
  Object.map (fun command args filetypes root_markers init_options ->
      { command; args; filetypes; root_markers; init_options })
  |> Object.mem "command" string ~enc:(fun (value : lsp) -> value.command)
  |> Object.mem "args" (list string) ~dec_absent:[] ~enc:(fun (value : lsp) -> value.args)
  |> Object.mem "filetypes" (list string) ~dec_absent:[] ~enc:(fun value ->
      value.filetypes)
  |> Object.mem "root_markers" (list string) ~dec_absent:[] ~enc:(fun value ->
      value.root_markers)
  |> Object.mem "init_options" (option json) ~dec_absent:None
       ~enc:(fun value -> value.init_options)
       ~enc_omit:Option.is_none
  |> Object.error_unknown |> Object.finish

let hook_codec =
  let open Jsont in
  Object.map (fun event matcher command timeout_s ->
      { event; matcher; command; timeout_s })
  |> Object.mem "event" hook_event_codec ~enc:(fun value -> value.event)
  |> Object.mem "matcher" (option string) ~dec_absent:None
       ~enc:(fun value -> value.matcher)
       ~enc_omit:Option.is_none
  |> Object.mem "command" string ~enc:(fun (value : hook) -> value.command)
  |> Object.mem "timeout_s" int ~dec_absent:30 ~enc:(fun (value : hook) ->
      value.timeout_s)
  |> Object.error_unknown |> Object.finish

let attribution_codec =
  let open Jsont in
  Object.map (fun trailer generated_with -> { trailer; generated_with })
  |> Object.mem "trailer" trailer_codec ~dec_absent:Co_authored ~enc:(fun value ->
      value.trailer)
  |> Object.mem "generated_with" bool ~dec_absent:true ~enc:(fun value ->
      value.generated_with)
  |> Object.error_unknown |> Object.finish

let advisor_codec =
  let open Jsont in
  Object.map (fun enabled model every_n_turns -> { enabled; model; every_n_turns })
  |> Object.mem "enabled" bool ~dec_absent:false ~enc:(fun value -> value.enabled)
  |> Object.mem "model" advisor_model_codec ~dec_absent:`Small
       ~enc:(fun (value : advisor) -> value.model)
  |> Object.mem "every_n_turns" int ~dec_absent:1 ~enc:(fun value -> value.every_n_turns)
  |> Object.error_unknown |> Object.finish

let budgets_codec =
  let open Jsont in
  Object.map (fun subagent_requests -> { subagent_requests })
  |> Object.mem "subagent_requests" int ~dec_absent:200 ~enc:(fun value ->
      value.subagent_requests)
  |> Object.error_unknown |> Object.finish

let options_codec =
  let open Jsont in
  Object.map
    (fun data_dir debug disable_auto_compaction auto_lsp attribution advisor budgets ->
      {
        data_dir;
        debug;
        disable_auto_compaction;
        auto_lsp;
        attribution;
        advisor;
        budgets;
      })
  |> Object.mem "data_dir" string ~dec_absent:".crush" ~enc:(fun value -> value.data_dir)
  |> Object.mem "debug" bool ~dec_absent:false ~enc:(fun value -> value.debug)
  |> Object.mem "disable_auto_compaction" bool ~dec_absent:false ~enc:(fun value ->
      value.disable_auto_compaction)
  |> Object.mem "auto_lsp" bool ~dec_absent:true ~enc:(fun value -> value.auto_lsp)
  |> Object.mem "attribution" attribution_codec ~dec_absent:default.options.attribution
       ~enc:(fun value -> value.attribution)
  |> Object.mem "advisor" advisor_codec ~dec_absent:default.options.advisor
       ~enc:(fun value -> value.advisor)
  |> Object.mem "budgets" budgets_codec ~dec_absent:default.options.budgets
       ~enc:(fun value -> value.budgets)
  |> Object.error_unknown |> Object.finish

let stamp_provider id (provider : provider) =
  let models =
    List.map
      (fun model -> { model with Charamel_fantasy.Model.provider = id })
      provider.models
  in
  { provider with models }

let list_to_map values =
  List.fold_left
    (fun map (key, value) -> String_map.add key value map)
    String_map.empty values

let provider_map_codec =
  let map_codec = Jsont.Object.as_string_map provider_codec in
  Jsont.map
    ~dec:(fun values ->
      List.map
        (fun (id, provider) -> (id, stamp_provider id provider))
        (String_map.bindings values))
    ~enc:list_to_map map_codec

let mcp_map_codec =
  let map_codec = Jsont.Object.as_string_map mcp_codec in
  Jsont.map ~dec:String_map.bindings ~enc:list_to_map map_codec

let lsp_map_codec =
  let map_codec = Jsont.Object.as_string_map lsp_codec in
  Jsont.map ~dec:String_map.bindings ~enc:list_to_map map_codec

let raw_jsont =
  let open Jsont in
  Object.map
    (fun providers models permissions mcp lsp context_paths skills_paths hooks options ->
      {
        providers;
        models;
        permissions;
        mcp;
        lsp;
        context_paths;
        skills_paths;
        hooks;
        options;
      })
  |> Object.mem "providers" provider_map_codec ~dec_absent:[] ~enc:(fun value ->
      value.providers)
  |> Object.mem "models" models_codec ~dec_absent:default.models ~enc:(fun value ->
      value.models)
  |> Object.mem "permissions" permissions_codec ~dec_absent:default.permissions
       ~enc:(fun value -> value.permissions)
  |> Object.mem "mcp" mcp_map_codec ~dec_absent:[] ~enc:(fun value -> value.mcp)
  |> Object.mem "lsp" lsp_map_codec ~dec_absent:[] ~enc:(fun value -> value.lsp)
  |> Object.mem "context_paths" (list string) ~dec_absent:default.context_paths
       ~enc:(fun value -> value.context_paths)
  |> Object.mem "skills_paths" (list string) ~dec_absent:[] ~enc:(fun value ->
      value.skills_paths)
  |> Object.mem "hooks" (list hook_codec) ~dec_absent:[] ~enc:(fun value -> value.hooks)
  |> Object.mem "options" options_codec ~dec_absent:default.options ~enc:(fun value ->
      value.options)
  |> Object.error_unknown |> Object.finish

let nonempty label value =
  if value = "" then Error (label ^ " must not be empty") else Ok ()

let rec all_nonempty label = function
  | [] -> Ok ()
  | value :: rest ->
      let* () = nonempty label value in
      all_nonempty label rest

let valid_permission_entry value =
  match String.split_on_char ':' value with
  | [ tool ] -> tool <> ""
  | [ tool; action ] -> tool <> "" && action <> ""
  | _ -> false

let validate_model (model : Charamel_fantasy.Model.t) =
  let* () = nonempty "model id" model.Charamel_fantasy.Model.id in
  let* () = nonempty "model name" model.Charamel_fantasy.Model.name in
  if model.Charamel_fantasy.Model.context_window <= 0 then
    Error "model context_window must be positive"
  else if model.Charamel_fantasy.Model.default_max_tokens <= 0 then
    Error "model default_max_tokens must be positive"
  else if
    not
      (Float.is_finite model.Charamel_fantasy.Model.cost_in
      && Float.is_finite model.Charamel_fantasy.Model.cost_out
      && Float.is_finite model.Charamel_fantasy.Model.cost_cache_read
      && Float.is_finite model.Charamel_fantasy.Model.cost_cache_write)
  then Error "model costs must be finite"
  else if
    model.Charamel_fantasy.Model.cost_in < 0.
    || model.Charamel_fantasy.Model.cost_out < 0.
    || model.Charamel_fantasy.Model.cost_cache_read < 0.
    || model.Charamel_fantasy.Model.cost_cache_write < 0.
  then Error "model costs must be non-negative"
  else Ok ()

let rec validate_models = function
  | [] -> Ok ()
  | model :: rest ->
      let* () = validate_model model in
      validate_models rest

let validate_provider id (provider : provider) =
  let* () = nonempty "provider id" id in
  validate_models provider.models

let validate_selected_model (value : selected_model) =
  let* () = nonempty "selected model provider" value.provider in
  let* () = nonempty "selected model id" value.model in
  match value.max_tokens with
  | Some max_tokens when max_tokens <= 0 ->
      Error "selected model max_tokens must be positive"
  | _ -> Ok ()

let validate_permissions value =
  if
    List.exists
      (fun entry -> not (valid_permission_entry entry))
      (value.allowed_tools @ value.deny)
  then Error "permission entries must be <tool> or <tool>:<action>"
  else Ok ()

let validate_mcp id (value : mcp) =
  let* () = nonempty "MCP server id" id in
  if value.timeout_s <= 0 || value.timeout_s > 3600 then
    Error "MCP timeout_s must be between 1 and 3600"
  else
    let* () = all_nonempty "MCP environment name" (List.map fst value.env) in
    let* () = all_nonempty "MCP header name" (List.map fst value.headers) in
    match value.transport with
    | Stdio -> (
        match value.command with
        | Some command when command <> "" ->
            if Option.is_some value.url then Error "stdio MCP cannot set url" else Ok ()
        | _ -> Error "stdio MCP requires command")
    | Http -> (
        match value.url with
        | Some url when url <> "" ->
            if Option.is_some value.command then Error "HTTP MCP cannot set command"
            else Ok ()
        | _ -> Error "HTTP MCP requires url")

let validate_lsp id (value : lsp) =
  let* () = nonempty "LSP server id" id in
  let* () = nonempty "LSP command" value.command in
  Ok ()

let validate_hook (value : hook) =
  let* () = nonempty "hook command" value.command in
  if value.timeout_s <= 0 || value.timeout_s > 3600 then
    Error "hook timeout_s must be between 1 and 3600"
  else
    match value.matcher with
    | None -> Ok ()
    | Some pattern -> (
        try
          ignore (Re.Perl.compile_pat pattern);
          Ok ()
        with
        | Re.Perl.Parse_error -> Error "invalid hook matcher"
        | Re.Perl.Not_supported -> Error "unsupported hook matcher"
        | Invalid_argument message -> Error (Fmt.str "invalid hook matcher: %s" message)
        | Failure message -> Error (Fmt.str "invalid hook matcher: %s" message))

let rec validate_assoc validate = function
  | [] -> Ok ()
  | (key, value) :: rest ->
      let* () = validate key value in
      validate_assoc validate rest

let validate (config : t) =
  let* () = validate_assoc validate_provider config.providers in
  let* () = Option.fold ~none:(Ok ()) ~some:validate_selected_model config.models.large in
  let* () = Option.fold ~none:(Ok ()) ~some:validate_selected_model config.models.small in
  let* () = validate_permissions config.permissions in
  let* () = validate_assoc validate_mcp config.mcp in
  let* () = validate_assoc validate_lsp config.lsp in
  let* () = all_nonempty "context path" config.context_paths in
  let* () = all_nonempty "skill path" config.skills_paths in
  let rec hooks = function
    | [] -> Ok ()
    | hook :: rest ->
        let* () = validate_hook hook in
        hooks rest
  in
  let* () = hooks config.hooks in
  let* () = nonempty "options.data_dir" config.options.data_dir in
  let* () =
    if config.options.advisor.every_n_turns <= 0 then
      Error "advisor every_n_turns must be positive"
    else Ok ()
  in
  if config.options.budgets.subagent_requests < 0 then
    Error "subagent_requests must be non-negative"
  else Ok ()

let jsont =
  Jsont.iter
    ~dec:(fun config ->
      match validate config with
      | Ok () -> ()
      | Error message -> Jsont.Error.msg Jsont.Meta.none message)
    raw_jsont

type error = [ `Parse of string * string | `Io of string * string ]

let pp_error ppf = function
  | `Parse (path, message) -> Fmt.pf ppf "cannot parse %s: %s" path message
  | `Io (path, message) -> Fmt.pf ppf "cannot read %s: %s" path message

let is_var_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
let is_var_char c = is_var_start c || (c >= '0' && c <= '9')

let expand_env ~env text =
  let length = String.length text in
  let buffer = Buffer.create length in
  let rec copy index =
    if index >= length then ()
    else if String.get text index <> '$' then (
      Buffer.add_char buffer (String.get text index);
      copy (index + 1))
    else if index + 1 < length && String.get text (index + 1) = '$' then (
      Buffer.add_char buffer '$';
      copy (index + 2))
    else if index + 1 < length && String.get text (index + 1) = '{' then (
      match String.index_from_opt text (index + 2) '}' with
      | Some close when close > index + 2 ->
          let name = String.sub text (index + 2) (close - index - 2) in
          if is_var_start (String.get name 0) && String.for_all is_var_char name then (
            Buffer.add_string buffer (Option.value (env name) ~default:"");
            copy (close + 1))
          else (
            Buffer.add_char buffer '$';
            copy (index + 1))
      | _ ->
          Buffer.add_char buffer '$';
          copy (index + 1))
    else if index + 1 < length && is_var_start (String.get text (index + 1)) then (
      let rec finish position =
        if position < length && is_var_char (String.get text position) then
          finish (position + 1)
        else position
      in
      let last = finish (index + 1) in
      let name = String.sub text (index + 1) (last - index - 1) in
      Buffer.add_string buffer (Option.value (env name) ~default:"");
      copy last)
    else (
      Buffer.add_char buffer '$';
      copy (index + 1))
  in
  copy 0;
  Buffer.contents buffer

let expand_pairs env values =
  List.map (fun (key, value) -> (key, expand_env ~env value)) values

let expand_config ~env config =
  let providers =
    List.map
      (fun ((id, value) : string * provider) ->
        let base_url = Option.map (expand_env ~env) value.base_url in
        let api_key = Option.map (expand_env ~env) value.api_key in
        (id, { value with base_url; api_key; headers = expand_pairs env value.headers }))
      config.providers
  in
  let mcp =
    List.map
      (fun ((id, value) : string * mcp) ->
        let command = Option.map (expand_env ~env) value.command in
        let args = List.map (expand_env ~env) value.args in
        let env_values = expand_pairs env value.env in
        let url = Option.map (expand_env ~env) value.url in
        let headers = expand_pairs env value.headers in
        (id, { value with command; args; env = env_values; url; headers }))
      config.mcp
  in
  let lsp =
    List.map
      (fun ((id, value) : string * lsp) ->
        ( id,
          {
            value with
            command = expand_env ~env value.command;
            args = List.map (expand_env ~env) value.args;
          } ))
      config.lsp
  in
  let hooks =
    List.map
      (fun (value : hook) -> { value with command = expand_env ~env value.command })
      config.hooks
  in
  { config with providers; mcp; lsp; hooks }

let dedup_strings values =
  List.fold_left
    (fun acc value -> if List.mem value acc then acc else acc @ [ value ])
    [] values

let merge_assoc left right =
  let right_map = list_to_map right in
  let seen = Hashtbl.create (List.length left + List.length right) in
  let existing =
    List.filter_map
      (fun (key, value) ->
        if Hashtbl.mem seen key then None
        else (
          Hashtbl.add seen key ();
          Some (key, Option.value (String_map.find_opt key right_map) ~default:value)))
      left
  in
  existing @ List.filter (fun (key, _) -> not (Hashtbl.mem seen key)) right

let same_hook a b =
  a.event = b.event && a.matcher = b.matcher && a.command = b.command
  && a.timeout_s = b.timeout_s

let dedup_hooks values =
  List.fold_left
    (fun acc value -> if List.exists (same_hook value) acc then acc else acc @ [ value ])
    [] values

let merge left right =
  {
    providers = merge_assoc left.providers right.providers;
    models = right.models;
    permissions =
      {
        allowed_tools =
          dedup_strings (left.permissions.allowed_tools @ right.permissions.allowed_tools);
        deny = dedup_strings (left.permissions.deny @ right.permissions.deny);
      };
    mcp = merge_assoc left.mcp right.mcp;
    lsp = merge_assoc left.lsp right.lsp;
    context_paths = dedup_strings (left.context_paths @ right.context_paths);
    skills_paths = dedup_strings (left.skills_paths @ right.skills_paths);
    hooks = dedup_hooks (left.hooks @ right.hooks);
    options = right.options;
  }

let object_members = function
  | Jsont.Object (members, _) ->
      Some (List.map (fun ((name, _), value) -> (name, value)) members)
  | _ -> None

let object_json members =
  Jsont.Json.object'
    (List.map (fun (name, value) -> Jsont.Json.mem (Jsont.Json.name name) value) members)

let member_json name members =
  List.find_opt (fun (key, _) -> key = name) members |> Option.map snd

let remove_member name members = List.filter (fun (key, _) -> key <> name) members

let dedup_json values =
  List.fold_left
    (fun acc value ->
      if List.exists (Jsont.Json.equal value) acc then acc else acc @ [ value ])
    [] values

let merge_keyed_objects left right =
  let left = Option.value (object_members left) ~default:[] in
  let right = Option.value (object_members right) ~default:[] in
  let right_map =
    List.fold_left
      (fun map (key, value) -> String_map.add key value map)
      String_map.empty right
  in
  let seen = Hashtbl.create (List.length left + List.length right) in
  let existing =
    List.filter_map
      (fun (key, value) ->
        if Hashtbl.mem seen key then None
        else (
          Hashtbl.add seen key ();
          Some (key, Option.value (String_map.find_opt key right_map) ~default:value)))
      left
  in
  object_json (existing @ List.filter (fun (key, _) -> not (Hashtbl.mem seen key)) right)

let rec merge_json left right =
  match (object_members left, object_members right) with
  | Some left_members, Some right_members ->
      let merged =
        List.fold_left
          (fun members (name, value) ->
            let replacement =
              match member_json name members with
              | None -> value
              | Some old -> (
                  match name with
                  | "providers" | "mcp" | "lsp" -> merge_keyed_objects old value
                  | "context_paths" | "skills_paths" | "allowed_tools" | "deny" | "hooks"
                    -> (
                      match (old, value) with
                      | Jsont.Array (old_values, _), Jsont.Array (new_values, _) ->
                          Jsont.Json.list (dedup_json (old_values @ new_values))
                      | _ -> value)
                  | _ -> (
                      match (object_members old, object_members value) with
                      | Some _, Some _ -> merge_json old value
                      | _ -> value))
            in
            remove_member name members @ [ (name, replacement) ])
          left_members right_members
      in
      object_json merged
  | _ -> right

let normalize_home_config path =
  if Filename.basename path = "crush.json" then path
  else Filename.concat path "crush.json"

let search_paths ~cwd ~git_root ~home_config =
  let home = normalize_home_config home_config in
  let cwd =
    if Filename.is_relative cwd then Filename.concat (Sys.getcwd ()) cwd else cwd
  in
  let git_root =
    Option.map
      (fun path ->
        if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path)
      git_root
  in
  let rec ancestors path =
    let parent = Filename.dirname path in
    if parent = path then [ path ] else path :: ancestors parent
  in
  let roots =
    match git_root with
    | None -> [ cwd ]
    | Some root ->
        let rec from_root = function
          | [] -> [ cwd ]
          | path :: rest -> if path = root then path :: rest else from_root rest
        in
        from_root (List.rev (ancestors cwd))
  in
  let candidates =
    home :: List.map (fun path -> Filename.concat path "crush.json") roots
  in
  let seen = Hashtbl.create (List.length candidates) in
  List.filter
    (fun path ->
      if Hashtbl.mem seen path then false
      else (
        Hashtbl.add seen path ();
        true))
    candidates

let absolute_path path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path

let exists fs_root path =
  Lwt.catch
    (fun () -> Lwt_unix.lstat (Path.under ~root:fs_root path) >|= fun _ -> true)
    (function Unix.Unix_error _ | Sys_error _ -> Lwt.return_false | exn -> Lwt.fail exn)

let rec find_git_root fs_root cwd =
  exists fs_root (Filename.concat cwd ".git") >>= fun found ->
  if found then Lwt.return (Some cwd)
  else
    let parent = Filename.dirname cwd in
    if String.equal parent cwd then Lwt.return_none else find_git_root fs_root parent

let io_message exn =
  match exn with
  | Unix.Unix_error (error, function_name, argument) ->
      Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument
  | Charamel_os.Fs.E (`Not_found, target) -> Fmt.str "not found: %s" target
  | Charamel_os.Fs.E (`Permission_denied, target) ->
      Fmt.str "permission denied: %s" target
  | Charamel_os.Fs.E (`Already_exists, target) -> Fmt.str "already exists: %s" target
  | Charamel_os.Fs.E (`Is_directory, target) -> Fmt.str "is a directory: %s" target
  | exn -> Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())

let read_file fs_root path =
  let file = Path.under ~root:fs_root path in
  Lwt.catch
    (fun () ->
      Lwt_unix.stat file >>= fun stats ->
      if stats.Unix.st_kind <> Unix.S_REG then
        Lwt.return_error (`Io (path, "configuration path is not a regular file"))
      else
        Lwt_io.with_file ~mode:Lwt_io.Input file (fun channel -> Lwt_io.read channel)
        >|= fun contents -> Ok (Some contents))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) | Unix.Unix_error (Unix.ENOTDIR, _, _) ->
          Lwt.return_ok None
      | (Unix.Unix_error _ | Charamel_os.Fs.E _ | Sys_error _) as exn ->
          Lwt.return_error (`Io (path, io_message exn))
      | exn -> Lwt.fail exn)

let environment env name =
  match env name with Some _ as value -> value | None -> Sys.getenv_opt name

let config_home ~env =
  match environment env "XDG_CONFIG_HOME" with
  | Some path when path <> "" && not (Filename.is_relative path) ->
      Filename.concat path "crush"
  | _ -> (
      match environment env "HOME" with
      | Some home when home <> "" && not (Filename.is_relative home) ->
          Filename.concat home ".config/crush"
      | _ -> Charamel_cli.Xdg.config_dir ~app:"crush")

let load ~fs_root ~env ~cwd =
  let cwd = absolute_path cwd in
  let home_config = config_home ~env in
  let rec read_all paths merged contributors =
    match paths with
    | [] -> Lwt.return_ok (merged, List.rev contributors)
    | path :: rest -> (
        read_file fs_root path >>= function
        | Error _ as failure -> Lwt.return failure
        | Ok None -> read_all rest merged contributors
        | Ok (Some text) -> (
            match Jsonx.json_of_string text with
            | Error message -> Lwt.return_error (`Parse (path, message))
            | Ok json -> (
                match Jsont.Json.decode jsont json with
                | Error message -> Lwt.return_error (`Parse (path, message))
                | Ok _ -> read_all rest (merge_json merged json) (path :: contributors))))
  in
  find_git_root fs_root cwd >>= fun git_root ->
  read_all (search_paths ~cwd ~git_root ~home_config) (Jsont.Json.object' []) []
  >>= function
  | Error _ as failure -> Lwt.return failure
  | Ok (merged, contributors) -> (
      match Jsont.Json.decode jsont merged with
      | Error message ->
          let path =
            match List.rev contributors with
            | path :: _ -> path
            | [] -> "<merged configuration>"
          in
          Lwt.return_error (`Parse (path, message))
      | Ok config -> (
          let config = expand_config ~env config in
          match validate config with
          | Ok () -> Lwt.return_ok (config, contributors)
          | Error message ->
              let path =
                match List.rev contributors with
                | path :: _ -> path
                | [] -> "<expanded configuration>"
              in
              Lwt.return_error (`Parse (path, message))))

let data_dir config ~cwd =
  if Filename.is_relative config.options.data_dir then
    Filename.concat (absolute_path cwd) config.options.data_dir
  else config.options.data_dir

let project_key ~cwd = Digestif.SHA256.(digest_string (absolute_path cwd) |> to_hex)
let json_string value = Jsont.Json.string value
let json_bool value = Jsont.Json.bool value
let json_int value = Jsont.Json.int value
let json_array values = Jsont.Json.list values
let json_object members = object_json members

let schema_string ?enum () =
  let members = [ ("type", json_string "string") ] in
  let members =
    match enum with
    | None -> members
    | Some values -> ("enum", json_array (List.map json_string values)) :: members
  in
  json_object members

let schema_integer ?default () =
  let members = [ ("type", json_string "integer") ] in
  let members =
    match default with
    | None -> members
    | Some value -> ("default", json_int value) :: members
  in
  json_object members

let schema_number () = json_object [ ("type", json_string "number") ]

let schema_boolean ?default () =
  let members = [ ("type", json_string "boolean") ] in
  let members =
    match default with
    | None -> members
    | Some value -> ("default", json_bool value) :: members
  in
  json_object members

let schema_array item = json_object [ ("type", json_string "array"); ("items", item) ]

let schema_any =
  json_object
    [
      ( "type",
        json_array
          (List.map json_string
             [ "object"; "array"; "string"; "number"; "integer"; "boolean"; "null" ]) );
    ]

let schema_object properties =
  json_object
    [
      ("type", json_string "object");
      ("properties", json_object properties);
      ("additionalProperties", json_bool false);
    ]

let schema_map item =
  json_object [ ("type", json_string "object"); ("additionalProperties", item) ]

let model_schema =
  schema_object
    [
      ("id", schema_string ());
      ("name", schema_string ());
      ("cost_per_1m_in", schema_number ());
      ("cost_per_1m_out", schema_number ());
      ("cost_per_1m_in_cached", schema_number ());
      ("cost_per_1m_out_cached", schema_number ());
      ("context_window", schema_integer ());
      ("default_max_tokens", schema_integer ());
      ("can_reason", schema_boolean ());
      ("supports_attachments", schema_boolean ());
    ]

let selected_schema =
  schema_object
    [
      ("provider", schema_string ());
      ("model", schema_string ());
      ("reasoning", schema_string ~enum:[ "off"; "low"; "medium"; "high" ] ());
      ("max_tokens", schema_integer ());
    ]

let provider_schema =
  schema_object
    [
      ( "type",
        schema_string
          ~enum:
            [ "anthropic"; "openai"; "openai_compatible"; "openai_responses"; "google" ]
          () );
      ("base_url", schema_string ());
      ("api_key", schema_string ());
      ("headers", schema_map (schema_string ()));
      ("models", schema_array model_schema);
    ]

let mcp_schema =
  schema_object
    [
      ("transport", schema_string ~enum:[ "stdio"; "http" ] ());
      ("command", schema_string ());
      ("args", schema_array (schema_string ()));
      ("env", schema_map (schema_string ()));
      ("url", schema_string ());
      ("headers", schema_map (schema_string ()));
      ("timeout_s", schema_integer ~default:10 ());
    ]

let lsp_schema =
  schema_object
    [
      ("command", schema_string ());
      ("args", schema_array (schema_string ()));
      ("filetypes", schema_array (schema_string ()));
      ("root_markers", schema_array (schema_string ()));
      ("init_options", schema_any);
    ]

let hook_schema =
  schema_object
    [
      ( "event",
        schema_string ~enum:[ "pre_tool"; "post_tool"; "session_start"; "stop" ] () );
      ("matcher", schema_string ());
      ("command", schema_string ());
      ("timeout_s", schema_integer ~default:30 ());
    ]

let options_schema =
  schema_object
    [
      ("data_dir", schema_string ());
      ("debug", schema_boolean ~default:false ());
      ("disable_auto_compaction", schema_boolean ~default:false ());
      ("auto_lsp", schema_boolean ~default:true ());
      ( "attribution",
        schema_object
          [
            ("trailer", schema_string ~enum:[ "none"; "co_authored"; "assisted" ] ());
            ("generated_with", schema_boolean ~default:true ());
          ] );
      ( "advisor",
        schema_object
          [
            ("enabled", schema_boolean ~default:false ());
            ("model", schema_string ~enum:[ "small"; "large" ] ());
            ("every_n_turns", schema_integer ~default:1 ());
          ] );
      ("budgets", schema_object [ ("subagent_requests", schema_integer ~default:200 ()) ]);
    ]

let schema =
  json_object
    [
      ("$schema", json_string "https://json-schema.org/draft/2020-12/schema");
      ("type", json_string "object");
      ( "properties",
        json_object
          [
            ("providers", schema_map provider_schema);
            ( "models",
              schema_object [ ("large", selected_schema); ("small", selected_schema) ] );
            ( "permissions",
              schema_object
                [
                  ("allowed_tools", schema_array (schema_string ()));
                  ("deny", schema_array (schema_string ()));
                ] );
            ("mcp", schema_map mcp_schema);
            ("lsp", schema_map lsp_schema);
            ("context_paths", schema_array (schema_string ()));
            ("skills_paths", schema_array (schema_string ()));
            ("hooks", schema_array hook_schema);
            ("options", options_schema);
          ] );
      ("additionalProperties", json_bool false);
    ]
