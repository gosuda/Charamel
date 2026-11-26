open Lwt.Infix

let path () = Filename.concat (Charamel_cli.Xdg.cache_dir ~app:"crush") "providers.json"

let catalog_json = function
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem "providers" members with
      | Some (_, Jsont.Array (values, _)) ->
          let rec decode acc = function
            | [] -> Ok (List.rev acc)
            | value :: rest -> (
                match Jsont.Json.decode Charamel_fantasy.Provider_info.jsont value with
                | Ok provider -> decode (provider :: acc) rest
                | Error message -> Error message)
          in
          decode [] values
      | Some _ -> Error "providers cache member is not an array"
      | None -> Error "providers cache has no providers member")
  | _ -> Error "providers cache must be an object"

let read ~fs_root =
  let filename =
    try Ok (Path.under ~root:fs_root (path ()))
    with Invalid_argument message -> Error message
  in
  match filename with
  | Error message -> Lwt.return (Error message)
  | Ok filename ->
      Lwt.catch
        (fun () ->
          Lwt_unix.lstat filename >>= fun stat ->
          if stat.Unix.st_kind <> Unix.S_REG then
            Lwt.return (Error "providers cache is not a regular file")
          else if stat.Unix.st_size > 10 * 1024 * 1024 then
            Lwt.return (Error "providers cache exceeds 10 MiB")
          else
            Lwt_io.with_file ~mode:Lwt_io.Input filename (fun channel ->
                Lwt_io.read channel)
            >|= fun text -> Ok text)
        (function
          | Unix.Unix_error _ | Sys_error _ ->
              Lwt.return (Error "providers cache cannot be read")
          | exn -> Lwt.fail exn)

let load ~fs_root =
  read ~fs_root >|= fun contents ->
  let decoded =
    match contents with
    | Error message -> Error message
    | Ok text -> (
        match Jsonx.json_of_string text with
        | Ok json -> catalog_json json
        | Error _ -> Error "providers cache is not valid JSON")
  in
  match decoded with
  | Ok providers -> providers
  | Error _ -> Charamel_fantasy.Catalog.embedded
