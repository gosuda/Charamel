type resolved = {
  role : [ `Large | `Small ];
  provider_id : string;
  provider : Charamel_fantasy.Provider.t;
  model : Charamel_fantasy.Model.t;
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

open Lwt.Infix
open Lwt_result.Syntax

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
  Filename.concat (Charamel_cli.Xdg.cache_dir ~app:"crush") "providers.json"

let provider_json json = Jsont.Json.decode Charamel_fantasy.Provider_info.jsont json

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

let cache_size_limit = 10 * 1024 * 1024
let cache_path fs_root = Filename.concat fs_root (catalog_cache_path ())

let read_cache_file path =
  Lwt.catch
    (fun () ->
      Lwt_unix.lstat path >>= fun stat ->
      if stat.Unix.st_kind <> Unix.S_REG then
        Lwt.return (Error "providers cache is not a regular file")
      else if stat.Unix.st_size > cache_size_limit then
        Lwt.return (Error "providers cache exceeds 10 MiB")
      else
        Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel -> Lwt_io.read channel)
        >|= fun text -> Ok text)
    (function
      | Unix.Unix_error _ | Sys_error _ ->
          Lwt.return (Error "providers cache cannot be read")
      | exn -> Lwt.fail exn)

let load_cache fs_root =
  read_cache_file (cache_path fs_root) >>= function
  | Error _ as failure -> Lwt.return failure
  | Ok text ->
      Lwt.return
        (match Jsonx.json_of_string text with
        | Ok json -> catalog_json json
        | Error _ -> Error "providers cache is not valid JSON")

let model_id model = model.Charamel_fantasy.Model.id

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
          ~default:existing.Charamel_fantasy.Provider_info.base_url
      in
      let models =
        prepend_models configured.Config.models
          existing.Charamel_fantasy.Provider_info.models
      in
      ( id,
        {
          existing with
          Charamel_fantasy.Provider_info.base_url;
          Charamel_fantasy.Provider_info.models;
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
              Charamel_fantasy.Provider_info.id;
              Charamel_fantasy.Provider_info.name = id;
              Charamel_fantasy.Provider_info.base_url;
              Charamel_fantasy.Provider_info.models = provider.Config.models;
            } ))
    config.Config.providers

let catalog ~fs_root config =
  load_cache fs_root >|= fun base ->
  let base =
    match base with Ok values -> values | Error _ -> Charamel_fantasy.Catalog.embedded
  in
  let with_ids = List.map (fun p -> (p.Charamel_fantasy.Provider_info.id, p)) base in
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
    Config.expand_env ~env catalog_provider.Charamel_fantasy.Provider_info.base_url
  in
  let base_url = configured_base in
  match kind with
  | Config.Anthropic ->
      let base =
        Option.value base_url
          ~default:
            (if catalog_base = "" then "https://api.anthropic.com" else catalog_base)
      in
      Ok (Charamel_fantasy.Provider.anthropic ~base_url:base ~auth ())
  | Config.Openai ->
      let base = Option.value base_url ~default:"https://api.openai.com/v1" in
      let headers =
        Option.fold ~none:[]
          ~some:(fun (p : Config.provider) -> p.Config.headers)
          config_provider
      in
      Ok (Charamel_fantasy.Provider.openai_compatible ~base_url:base ~headers ~auth ())
  | Config.Openai_compatible ->
      let base = Option.value base_url ~default:catalog_base in
      if base = "" then Error (`Unknown_provider id)
      else
        let headers =
          Option.fold ~none:[]
            ~some:(fun (p : Config.provider) -> p.Config.headers)
            config_provider
        in
        Ok (Charamel_fantasy.Provider.openai_compatible ~base_url:base ~headers ~auth ())
  | Config.Openai_responses ->
      let base = Option.value base_url ~default:"https://api.openai.com/v1" in
      Ok (Charamel_fantasy.Provider.openai_responses ~base_url:base ~auth ())
  | Config.Google ->
      let base =
        Option.value base_url
          ~default:
            (if catalog_base = "" then "https://generativelanguage.googleapis.com"
             else catalog_base)
      in
      Ok (Charamel_fantasy.Provider.google ~base_url:base ~auth ())

let find_provider providers id =
  List.find_opt (fun p -> String.equal p.Charamel_fantasy.Provider_info.id id) providers

let find_model provider wanted_id =
  List.find_opt
    (fun model -> String.equal (model_id model) wanted_id)
    provider.Charamel_fantasy.Provider_info.models

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
  Auth.resolve auth ~config ~env ~provider:provider_id >>= function
  | Error error -> Lwt.return (Error (`Auth error))
  | Ok None -> Lwt.return (Error (`No_credential provider_id))
  | Ok (Some (Auth.Disabled { reason; _ })) -> Lwt.return (Error (`Disabled reason))
  | Ok (Some credential) ->
      Lwt.return
        (match Auth.to_fantasy credential with
        | Some auth -> Ok auth
        | None -> Error (`No_credential provider_id))

let selection_for_role config role =
  match role with
  | `Large -> config.Config.models.Config.large
  | `Small -> config.Config.models.Config.small

let select config ~providers ~auth ~env ~role =
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
      | [] -> Lwt.return (Error (`No_model role))
      | id :: rest -> (
          match find_provider providers id with
          | None -> choose rest
          | Some provider -> (
              credential_for ~auth ~config ~env id >>= function
              | Error (`No_credential _) | Error (`Disabled _) -> choose rest
              | Error error -> Lwt.return (Error error)
              | Ok _ -> (
                  match
                    preferred_model id provider.Charamel_fantasy.Provider_info.models
                  with
                  | None -> choose rest
                  | Some model -> Lwt.return (Ok (id, provider, model, None, None)))))
    in
    choose candidates
  in
  match configured with
  | Some selected -> (
      match find_provider providers selected.Config.provider with
      | None -> Lwt.return (Error (`Unknown_provider selected.Config.provider))
      | Some provider -> (
          let model =
            if inherited_small then
              Option.map
                (fun large ->
                  Option.value
                    (small_model_with_fallback selected.Config.provider
                       provider.Charamel_fantasy.Provider_info.models
                       ~fallback_id:selected.Config.model)
                    ~default:large)
                (find_model provider selected.Config.model)
            else find_model provider selected.Config.model
          in
          match model with
          | None ->
              Lwt.return
                (Error (`Unknown_model (selected.Config.provider, selected.Config.model)))
          | Some model ->
              let* auth_value =
                credential_for ~auth ~config ~env selected.Config.provider
              in
              let config_provider = provider_config selected.Config.provider config in
              let* provider_handle =
                Lwt.return
                  (make_provider ~env ~catalog_provider:provider ~config_provider
                     ~auth:auth_value ~id:selected.Config.provider config)
              in
              let reasoning = Option.value selected.Config.reasoning ~default:`Off in
              let max_tokens =
                Option.value selected.Config.max_tokens
                  ~default:model.Charamel_fantasy.Model.default_max_tokens
              in
              Lwt.return
                (Ok
                   {
                     role;
                     provider_id = selected.Config.provider;
                     provider = provider_handle;
                     model;
                     reasoning;
                     max_tokens;
                   })))
  | None ->
      let* provider_id, provider, large_model, _, _ = default_selection () in
      let model =
        match role with
        | `Large -> large_model
        | `Small ->
            Option.value
              (small_model provider_id provider.Charamel_fantasy.Provider_info.models)
              ~default:large_model
      in
      let config_provider = provider_config provider_id config in
      let* auth_value = credential_for ~auth ~config ~env provider_id in
      let* provider_handle =
        Lwt.return
          (make_provider ~env ~catalog_provider:provider ~config_provider ~auth:auth_value
             ~id:provider_id config)
      in
      let reasoning = `Off in
      Lwt.return
        (Ok
           {
             role;
             provider_id;
             provider = provider_handle;
             model;
             reasoning;
             max_tokens = model.Charamel_fantasy.Model.default_max_tokens;
           })

let resolve ~fs_root config ~auth ~env ~role =
  catalog ~fs_root config >>= fun providers -> select config ~providers ~auth ~env ~role

let with_auth ~fs_root config ~env resolved provider_auth =
  catalog ~fs_root config >>= fun providers ->
  match find_provider providers resolved.provider_id with
  | None -> Lwt.return (Error (`Unknown_provider resolved.provider_id))
  | Some catalog_provider ->
      let config_provider = provider_config resolved.provider_id config in
      let* provider =
        Lwt.return
          (make_provider ~env ~catalog_provider ~config_provider ~auth:provider_auth
             ~id:resolved.provider_id config)
      in
      Lwt.return (Ok { resolved with provider })

let cost (model : Charamel_fantasy.Model.t) (usage : Charamel_fantasy.Usage.t) =
  (float_of_int usage.Charamel_fantasy.Usage.input
   *. model.Charamel_fantasy.Model.cost_in
  +. float_of_int usage.Charamel_fantasy.Usage.output
     *. model.Charamel_fantasy.Model.cost_out
  +. float_of_int usage.Charamel_fantasy.Usage.cache_read
     *. model.Charamel_fantasy.Model.cost_cache_read
  +. float_of_int usage.Charamel_fantasy.Usage.cache_write
     *. model.Charamel_fantasy.Model.cost_cache_write)
  /. 1_000_000.

let list ~fs_root config ~auth ~env =
  catalog ~fs_root config >>= fun providers ->
  Lwt_list.map_s
    (fun provider ->
      let id = provider.Charamel_fantasy.Provider_info.id in
      Auth.resolve auth ~config ~env ~provider:id >>= fun credential ->
      let state =
        match credential with
        | Ok (Some (Auth.Disabled _)) -> `Disabled
        | Ok (Some credential) when Option.is_some (Auth.to_fantasy credential) -> `Ready
        | _ -> `No_credential
      in
      Lwt.return (id, provider.Charamel_fantasy.Provider_info.models, state))
    providers

type update_result = Updated of string | Not_modified

type update_error =
  [ `Io of string * string | `Parse of string | `Fetch of Charamel_fantasy.Error.t ]

let cache_etag fs_root =
  read_cache_file (cache_path fs_root) >|= function
  | Error _ -> None
  | Ok text -> (
      match Jsonx.json_of_string text with
      | Ok json -> Jsonx.string_member "etag" json
      | Error _ -> None)

let cache_document ~etag providers =
  let providers_text =
    Jsonx.encode (Jsont.list Charamel_fantasy.Provider_info.jsont) providers
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
  | `Fetch error -> Fmt.pf ppf "catalog fetch error: %a" Charamel_fantasy.Error.pp error

let document_text filename ~etag providers =
  try
    match Jsont_bytesrw.encode_string Jsont.json (cache_document ~etag providers) with
    | Ok text -> Ok text
    | Error message -> Error (`Parse (Fmt.str "%s: %s" filename message))
  with
  | Failure message -> Error (`Parse (Fmt.str "%s: %s" filename message))
  | Invalid_argument message -> Error (`Parse (Fmt.str "%s: %s" filename message))

let prepare_dir path =
  Charamel_os.Fs.mkdir_p path >>= function
  | Error error -> Lwt.return (Error (`Io (path, Io.fs_error error)))
  | Ok () ->
      Lwt.catch (fun () -> Lwt_unix.chmod path 0o700) (fun _ -> Lwt.return_unit)
      >|= fun () -> Ok ()

let save_catalog ~fs_root ~etag providers =
  let filename = cache_path fs_root in
  let parent = Filename.dirname filename in
  prepare_dir parent >>= function
  | Error _ as failure -> Lwt.return failure
  | Ok () -> (
      match document_text filename ~etag providers with
      | Error _ as failure -> Lwt.return failure
      | Ok contents -> (
          State_file.replace filename contents >|= function
          | Ok () -> Ok ()
          | Error (`Io (path, message)) -> Error (`Io (path, message))))

let update_catalog ?source ~fs_root () =
  cache_etag fs_root >>= fun etag ->
  Charamel_fantasy.Catalog.fetch ?base_url:source ?etag () >>= function
  | Error `Not_modified -> Lwt.return (Ok Not_modified)
  | Error (#Charamel_fantasy.Error.t as error) -> Lwt.return (Error (`Fetch error))
  | Ok (providers, new_etag) ->
      save_catalog ~fs_root ~etag:new_etag providers
      >|= Result.map (fun () -> Updated new_etag)
