type resolved = {
  role : [ `Large | `Small ];
  provider_id : string;
  provider : Charm_fantasy.Provider.t;
  model : Charm_fantasy.Model.t;
  reasoning : Config.reasoning;
  max_tokens : int;
}

type error =
  [ `No_model of [ `Large | `Small ]
  | `Unknown_provider of string
  | `Unknown_model of string * string
  | `No_credential of string
  | `Disabled of string
  | `Auth of Auth.error ]

open Result.Syntax

let contains needle haystack =
  let needle = String.lowercase_ascii needle in
  let haystack = String.lowercase_ascii haystack in
  let n = String.length needle and h = String.length haystack in
  let rec at index =
    if index + n > h then false
    else if String.sub haystack index n = needle then true
    else at (index + 1)
  in
  n = 0 || at 0

let pp_error ppf = function
  | `No_model `Large -> Fmt.string ppf "no large model is configured"
  | `No_model `Small -> Fmt.string ppf "no small model is configured"
  | `Unknown_provider provider -> Fmt.pf ppf "unknown provider: %s" provider
  | `Unknown_model (provider, model) ->
      Fmt.pf ppf "unknown model %s for provider %s" model provider
  | `No_credential provider -> Fmt.pf ppf "no credential for provider %s" provider
  | `Disabled reason -> Fmt.pf ppf "provider credential disabled: %s" reason
  | `Auth error -> Fmt.pf ppf "%a" Auth.pp_error error

let catalog_cache_path () =
  Filename.concat (Charm_cli.Xdg.cache_dir ~app:"crush") "providers.json"

let provider_json json = Jsont.Json.decode Charm_fantasy.Provider_info.jsont json

let catalog_json json =
  match json with
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem "providers" members with
      | Some (_, Jsont.Array (values, _)) ->
          let rec decode acc = function
            | [] -> Ok (List.rev acc)
            | value :: rest -> (
                match provider_json value with
                | Ok provider -> decode (provider :: acc) rest
                | Error message -> Error message)
          in
          decode [] values
      | Some _ -> Error "providers cache member is not an array"
      | None -> Error "providers cache has no providers member")
  | _ -> Error "providers cache must be an object"

let load_cache fs =
  let filename = catalog_cache_path () in
  try
    let file = Eio.Path.(fs / filename) in
    match Eio.Path.kind ~follow:false file with
    | `Regular_file -> (
        let stat = Eio.Path.stat ~follow:false file in
        if Optint.Int63.to_int stat.Eio.File.Stat.size > 10 * 1024 * 1024 then
          Error "providers cache exceeds 10 MiB"
        else
          match Jsonx.json_of_string (Eio.Path.load file) with
          | Ok json -> catalog_json json
          | Error _ -> Error "providers cache is not valid JSON")
    | _ -> Error "providers cache is not a regular file"
  with
  | Eio.Io _ -> Error "providers cache cannot be read"
  | Unix.Unix_error _ -> Error "providers cache cannot be read"

let model_id model = model.Charm_fantasy.Model.id

let prepend_models configured existing =
  let configured_ids = List.map model_id configured in
  let without_duplicates =
    List.filter (fun model -> not (List.mem (model_id model) configured_ids)) existing
  in
  configured @ without_duplicates

let configured_provider id config = List.assoc_opt id config.Config.providers

let overlay_provider (id, existing) config =
  match configured_provider id config with
  | None -> (id, existing)
  | Some configured ->
      let base_url =
        Option.value configured.Config.base_url
          ~default:existing.Charm_fantasy.Provider_info.base_url
      in
      let models =
        prepend_models configured.Config.models
          existing.Charm_fantasy.Provider_info.models
      in
      ( id,
        {
          existing with
          Charm_fantasy.Provider_info.base_url;
          Charm_fantasy.Provider_info.models;
        } )

let configured_only config existing_ids =
  List.filter_map
    (fun (id, (provider : Config.provider)) ->
      if List.mem id existing_ids then None
      else
        let base_url = Option.value provider.Config.base_url ~default:"" in
        Some
          ( id,
            {
              Charm_fantasy.Provider_info.id;
              Charm_fantasy.Provider_info.name = id;
              Charm_fantasy.Provider_info.base_url;
              Charm_fantasy.Provider_info.models = provider.Config.models;
            } ))
    config.Config.providers

let catalog ~fs config =
  let base =
    match load_cache fs with
    | Ok values -> values
    | Error _ -> Charm_fantasy.Catalog.embedded
  in
  let with_ids = List.map (fun p -> (p.Charm_fantasy.Provider_info.id, p)) base in
  let known_ids = List.map fst with_ids in
  let overlaid = List.map (fun pair -> overlay_provider pair config) with_ids in
  List.map snd (overlaid @ configured_only config known_ids)

let provider_kind_for_id id config =
  match configured_provider id config with
  | Some p -> Some p.Config.kind
  | None -> (
      match String.lowercase_ascii id with
      | "anthropic" -> Some Config.Anthropic
      | "openai" -> Some Config.Openai
      | "google" | "gemini" -> Some Config.Google
      | "openrouter" | "groq" | "mistral" | "xai" | "deepseek" | "cerebras" | "ollama"
      | "lmstudio" | "lm-studio" | "hyper" ->
          Some Config.Openai_compatible
      | _ -> None)

let provider_config id config = List.assoc_opt id config.Config.providers

let make_provider ~env ~catalog_provider ~config_provider ~auth ~id config =
  let kind =
    Option.value (provider_kind_for_id id config) ~default:Config.Openai_compatible
  in
  let configured_base = Option.bind config_provider (fun p -> p.Config.base_url) in
  let catalog_base =
    Config.expand_env ~env catalog_provider.Charm_fantasy.Provider_info.base_url
  in
  let base_url = configured_base in
  match kind with
  | Config.Anthropic ->
      let base =
        Option.value base_url
          ~default:
            (if catalog_base = "" then "https://api.anthropic.com" else catalog_base)
      in
      Ok (Charm_fantasy.Provider.anthropic ~base_url:base ~auth ())
  | Config.Openai ->
      let base = Option.value base_url ~default:"https://api.openai.com/v1" in
      let headers =
        Option.fold ~none:[]
          ~some:(fun (p : Config.provider) -> p.Config.headers)
          config_provider
      in
      Ok (Charm_fantasy.Provider.openai_compatible ~base_url:base ~headers ~auth ())
  | Config.Openai_compatible ->
      let base = Option.value base_url ~default:catalog_base in
      if base = "" then Error (`Unknown_provider id)
      else
        let headers =
          Option.fold ~none:[]
            ~some:(fun (p : Config.provider) -> p.Config.headers)
            config_provider
        in
        Ok (Charm_fantasy.Provider.openai_compatible ~base_url:base ~headers ~auth ())
  | Config.Openai_responses ->
      let base = Option.value base_url ~default:"https://api.openai.com/v1" in
      Ok (Charm_fantasy.Provider.openai_responses ~base_url:base ~auth ())
  | Config.Google ->
      let base =
        Option.value base_url
          ~default:
            (if catalog_base = "" then "https://generativelanguage.googleapis.com"
             else catalog_base)
      in
      Ok (Charm_fantasy.Provider.google ~base_url:base ~auth ())

let find_provider providers id =
  List.find_opt (fun p -> String.equal p.Charm_fantasy.Provider_info.id id) providers

let find_model provider wanted_id =
  List.find_opt
    (fun model -> String.equal (model_id model) wanted_id)
    provider.Charm_fantasy.Provider_info.models

let preferred_model id models =
  let predicate =
    match String.lowercase_ascii id with
    | "anthropic" -> [ "sonnet" ]
    | "openai" -> [ "gpt-5" ]
    | "google" | "gemini" -> [ "gemini-2.5-pro" ]
    | _ -> []
  in
  match
    List.find_opt
      (fun model ->
        List.exists (fun needle -> contains needle (model_id model)) predicate)
      models
  with
  | Some model -> Some model
  | None -> List.nth_opt models 0

let small_model id models =
  match
    List.find_opt
      (fun model ->
        List.exists
          (fun needle -> contains needle (model_id model))
          [ "haiku"; "mini"; "flash" ])
      models
  with
  | Some model -> Some model
  | None -> preferred_model id models

let small_model_with_fallback _id models ~fallback_id =
  match
    List.find_opt
      (fun model ->
        List.exists
          (fun needle -> contains needle (model_id model))
          [ "haiku"; "mini"; "flash" ])
      models
  with
  | Some model -> Some model
  | None -> List.find_opt (fun model -> String.equal (model_id model) fallback_id) models

let credential_for ~auth ~config ~env provider_id =
  match Auth.resolve auth ~config ~env ~provider:provider_id with
  | Error error -> Error (`Auth error)
  | Ok None -> Error (`No_credential provider_id)
  | Ok (Some (Auth.Disabled { reason; _ })) -> Error (`Disabled reason)
  | Ok (Some credential) -> (
      match Auth.to_fantasy credential with
      | Some auth -> Ok auth
      | None -> Error (`No_credential provider_id))

let selection_for_role config role =
  match role with
  | `Large -> config.Config.models.Config.large
  | `Small -> config.Config.models.Config.small

let resolve ~fs config ~auth ~env ~role =
  let providers = catalog ~fs config in
  let inherited_small =
    role = `Small && Option.is_none config.Config.models.Config.small
  in
  let configured =
    match selection_for_role config role with
    | Some selected -> Some selected
    | None when inherited_small -> config.Config.models.Config.large
    | None -> None
  in
  let default_selection () =
    let preferred = [ "anthropic"; "openai"; "openrouter"; "google" ] in
    let configured_ids = List.map fst config.Config.providers in
    let candidates =
      List.fold_left
        (fun acc id -> if List.mem id acc then acc else acc @ [ id ])
        preferred configured_ids
    in
    let rec choose = function
      | [] -> Error (`No_model role)
      | id :: rest -> (
          match find_provider providers id with
          | None -> choose rest
          | Some provider -> (
              match credential_for ~auth ~config ~env id with
              | Error (`No_credential _) | Error (`Disabled _) -> choose rest
              | Error error -> Error error
              | Ok _ -> (
                  match
                    preferred_model id provider.Charm_fantasy.Provider_info.models
                  with
                  | None -> choose rest
                  | Some model -> Ok (id, provider, model, None, None))))
    in
    choose candidates
  in
  match configured with
  | Some selected -> (
      match find_provider providers selected.Config.provider with
      | None -> Error (`Unknown_provider selected.Config.provider)
      | Some provider -> (
          let model =
            if inherited_small then
              Option.map
                (fun large ->
                  Option.value
                    (small_model_with_fallback selected.Config.provider
                       provider.Charm_fantasy.Provider_info.models
                       ~fallback_id:selected.Config.model)
                    ~default:large)
                (find_model provider selected.Config.model)
            else find_model provider selected.Config.model
          in
          match model with
          | None ->
              Error (`Unknown_model (selected.Config.provider, selected.Config.model))
          | Some model ->
              let* auth_value =
                credential_for ~auth ~config ~env selected.Config.provider
              in
              let config_provider = provider_config selected.Config.provider config in
              let* provider_handle =
                make_provider ~env ~catalog_provider:provider ~config_provider
                  ~auth:auth_value ~id:selected.Config.provider config
              in
              let reasoning = Option.value selected.Config.reasoning ~default:`Off in
              let max_tokens =
                Option.value selected.Config.max_tokens
                  ~default:model.Charm_fantasy.Model.default_max_tokens
              in
              Ok
                {
                  role;
                  provider_id = selected.Config.provider;
                  provider = provider_handle;
                  model;
                  reasoning;
                  max_tokens;
                }))
  | None ->
      let* provider_id, provider, large_model, _, _ = default_selection () in
      let model =
        match role with
        | `Large -> large_model
        | `Small ->
            Option.value
              (small_model provider_id provider.Charm_fantasy.Provider_info.models)
              ~default:large_model
      in
      let config_provider = provider_config provider_id config in
      let* auth_value = credential_for ~auth ~config ~env provider_id in
      let* provider_handle =
        make_provider ~env ~catalog_provider:provider ~config_provider ~auth:auth_value
          ~id:provider_id config
      in
      let reasoning = `Off in
      Ok
        {
          role;
          provider_id;
          provider = provider_handle;
          model;
          reasoning;
          max_tokens = model.Charm_fantasy.Model.default_max_tokens;
        }

let with_auth ~fs config ~env resolved provider_auth =
  let providers = catalog ~fs config in
  match find_provider providers resolved.provider_id with
  | None -> Error (`Unknown_provider resolved.provider_id)
  | Some catalog_provider ->
      let config_provider = provider_config resolved.provider_id config in
      let* provider =
        make_provider ~env ~catalog_provider ~config_provider ~auth:provider_auth
          ~id:resolved.provider_id config
      in
      Ok { resolved with provider }

let cost (model : Charm_fantasy.Model.t) (usage : Charm_fantasy.Usage.t) =
  ((float_of_int usage.Charm_fantasy.Usage.input *. model.Charm_fantasy.Model.cost_in)
  +. (float_of_int usage.Charm_fantasy.Usage.output *. model.Charm_fantasy.Model.cost_out)
  +. float_of_int usage.Charm_fantasy.Usage.cache_read
     *. model.Charm_fantasy.Model.cost_cache_read
  +. float_of_int usage.Charm_fantasy.Usage.cache_write
     *. model.Charm_fantasy.Model.cost_cache_write)
  /. 1_000_000.

let list ~fs config ~auth ~env =
  let providers = catalog ~fs config in
  List.map
    (fun provider ->
      let id = provider.Charm_fantasy.Provider_info.id in
      let state =
        match Auth.resolve auth ~config ~env ~provider:id with
        | Ok (Some (Auth.Disabled _)) -> `Disabled
        | Ok (Some credential) when Option.is_some (Auth.to_fantasy credential) -> `Ready
        | _ -> `No_credential
      in
      (id, provider.Charm_fantasy.Provider_info.models, state))
    providers

type update_result = Updated of string | Not_modified

type update_error =
  [ `Io of string * string | `Parse of string | `Fetch of Charm_fantasy.Error.t ]

let cache_etag fs =
  let filename = catalog_cache_path () in
  try
    let file = Eio.Path.(fs / filename) in
    match Eio.Path.kind ~follow:false file with
    | `Regular_file -> (
        let stat = Eio.Path.stat ~follow:false file in
        if Optint.Int63.to_int stat.Eio.File.Stat.size > 10 * 1024 * 1024 then None
        else
          match Jsonx.json_of_string (Eio.Path.load file) with
          | Ok json -> Jsonx.string_member "etag" json
          | Error _ -> None)
    | _ -> None
  with Eio.Io _ | Unix.Unix_error _ -> None

let cache_document ~etag providers =
  let providers_text =
    Jsonx.encode (Jsont.list Charm_fantasy.Provider_info.jsont) providers
  in
  let providers_json =
    match Jsonx.json_of_string providers_text with
    | Ok json -> json
    | Error message -> invalid_arg message
  in
  Jsont.Json.object'
    [
      Jsont.Json.mem (Jsont.Json.name "etag") (Jsont.Json.string etag);
      Jsont.Json.mem (Jsont.Json.name "providers") providers_json;
    ]

let pp_update_error ppf = function
  | `Io (path, message) -> Fmt.pf ppf "catalog cache I/O error at %s: %s" path message
  | `Parse message -> Fmt.pf ppf "catalog cache parse error: %s" message
  | `Fetch error -> Fmt.pf ppf "catalog fetch error: %a" Charm_fantasy.Error.pp error

let save_catalog ~fs ~etag providers =
  let filename = catalog_cache_path () in
  try
    Eio.Cancel.protect (fun () ->
        let parent = Filename.dirname filename in
        Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(fs / parent);
        let contents =
          match
            Jsont_bytesrw.encode_string Jsont.json (cache_document ~etag providers)
          with
          | Ok text -> text
          | Error message -> raise (Invalid_argument message)
        in
        match State_file.replace Eio.Path.(fs / filename) contents with
        | Ok () -> Ok ()
        | Error (`Io (path, message)) -> Error (`Io (path, message)))
  with
  | Eio.Io _ as exn -> Error (`Io (filename, Fmt.str "%a" Eio.Exn.pp exn))
  | Unix.Unix_error (error, fn, arg) ->
      Error (`Io (filename, Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg))
  | Invalid_argument message -> Error (`Parse (Fmt.str "%s: %s" filename message))
  | Failure message -> Error (`Parse (Fmt.str "%s: %s" filename message))

let update_catalog ?source ~fs ~net ~clock () =
  let etag = cache_etag fs in
  match Charm_fantasy.Catalog.fetch ?base_url:source ?etag ~net ~clock () with
  | Error `Not_modified -> Ok Not_modified
  | Error (#Charm_fantasy.Error.t as error) -> Error (`Fetch error)
  | Ok (providers, new_etag) ->
      save_catalog ~fs ~etag:new_etag providers |> Result.map (fun () -> Updated new_etag)
