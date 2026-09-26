module Store = Skate_core.Store
module Env = Charamel_cli.Env
open Lwt.Infix

let max_value_size = 64 * 1024 * 1024

type target = { key : string; db : string option }

let root () = Charamel_cli.Xdg.data_dir ~app:"skate"

let store_error = function
  | `No_such_db _ -> "skate: no such database"
  | `No_such_key _ -> "skate: no such key"
  | `Corrupt message -> Fmt.str "skate: %s" message
  | `Io message -> Fmt.str "skate: %s" message
  | `Invalid_db _ -> "skate: invalid database name"

let unwrap = function
  | Ok value -> value
  | Error error -> Charamel_cli.error (store_error error)

let parse_key target =
  if target = "" then Error "skate: key must not be empty"
  else
    match (String.index_opt target '@', String.rindex_opt target '@') with
    | None, None -> Ok { key = target; db = None }
    | Some first, Some last when first <> last ->
        Error "skate: a key may contain at most one @DB suffix"
    | Some at, Some _ ->
        let key = String.sub target 0 at in
        let db = String.sub target (at + 1) (String.length target - at - 1) in
        if key = "" then Error "skate: key must not be empty"
        else if db = "" then Ok { key; db = None }
        else Ok { key; db = Some db }
    | _ -> Error "skate: invalid key"

let parse_database argument default =
  let name =
    match argument with
    | None -> default
    | Some name when String.length name > 0 && Char.equal name.[0] '@' ->
        String.sub name 1 (String.length name - 1)
    | Some name -> name
  in
  if name = "" then Ok default else Ok name

let read_stdin env =
  Lwt.catch
    (fun () ->
      Lwt_io.read ~count:(max_value_size + 1) env.Env.stdin >>= fun value ->
      if String.length value > max_value_size then
        Lwt.return (Error "stdin exceeds the value size limit")
      else Lwt.return (Ok value))
    (function
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn -> Lwt.return (Error (Printexc.to_string exn)))

let write env data = Lwt_io.write env.Env.stdout data
let write_line env data = write env (data ^ "\n")

let render_value ~show_binary = function
  | Store.Text value -> value
  | Store.Binary value ->
      if show_binary then value else Fmt.str "[binary %d bytes]" (String.length value)

let output_get env ~show_binary = function
  | Store.Text value -> write_line env value
  | Store.Binary value ->
      if show_binary then write env value
      else write_line env (Fmt.str "[binary %d bytes]" (String.length value))

let run_get env ~target ~show_binary =
  match parse_key target with
  | Error message -> Charamel_cli.error message
  | Ok target ->
      let db = Option.value target.db ~default:"default" in
      Store.get ~root:(root ()) ~db target.key >>= fun value ->
      output_get env ~show_binary (unwrap value)

let run_set env ~target ~value =
  match parse_key target with
  | Error message -> Charamel_cli.error message
  | Ok target ->
      let db = Option.value target.db ~default:"default" in
      let value =
        match value with
        | Some value when not (String.equal value "-") -> Lwt.return (Ok value)
        | Some _ | None -> read_stdin env
      in
      value >>= fun value ->
      let value =
        match value with
        | Ok value -> value
        | Error message ->
            Charamel_cli.error (Fmt.str "skate: could not read stdin: %s" message)
      in
      Store.set ~root:(root ()) ~db target.key value >>= fun stored ->
      Lwt.return (unwrap stored)

let run_delete _env ~target =
  match parse_key target with
  | Error message -> Charamel_cli.error message
  | Ok target ->
      let db = Option.value target.db ~default:"default" in
      Store.delete ~root:(root ()) ~db target.key >>= fun deleted ->
      Lwt.return (unwrap deleted)

let run_list env ~database ~keys_only ~values_only ~show_binary =
  if keys_only && values_only then
    Charamel_cli.error "skate: --keys-only and --values-only cannot be combined"
  else
    match parse_database database "default" with
    | Error message -> Charamel_cli.error message
    | Ok db ->
        Store.list ~root:(root ()) ~db >>= fun entries ->
        let entries = unwrap entries in
        let rec emit = function
          | [] -> Lwt.return_unit
          | (key, value) :: rest ->
              let line =
                if keys_only then key
                else if values_only then render_value ~show_binary value
                else key ^ "\t" ^ render_value ~show_binary value
              in
              write_line env line >>= fun () -> emit rest
        in
        emit entries

let run_delete_db _env database =
  match parse_database (Some database) database with
  | Error message -> Charamel_cli.error message
  | Ok db ->
      Store.delete_db ~root:(root ()) ~db >>= fun deleted -> Lwt.return (unwrap deleted)

let run_dbs env =
  Store.dbs ~root:(root ()) >>= fun names ->
  let names = unwrap names in
  Lwt_list.iter_s (write_line env) names

let show_binary_arg () =
  let open Cmdliner in
  Arg.(
    value (flag (info [ "show-binary" ] ~doc:"Write binary values instead of a summary.")))

let key_arg ~docv ~doc =
  let open Cmdliner in
  Arg.(required (pos 0 (some string) None (info [] ~docv ~doc)))

let optional_value_arg () =
  let open Cmdliner in
  Arg.(
    value
      (pos 1 (some string) None
         (info [] ~docv:"VALUE|-" ~doc:"Value to store. A missing value or - reads stdin.")))

let optional_database_arg () =
  let open Cmdliner in
  Arg.(
    value
      (pos 0 (some string) None
         (info [] ~docv:"@DB" ~doc:"Database to list. The default database is default.")))

let command_info name doc = Cmdliner.Cmd.info name ~doc

let get env =
  let open Cmdliner in
  let term =
    let open Term.Syntax in
    let+ target = key_arg ~docv:"KEY[@DB]" ~doc:"Key to read."
    and+ show_binary = show_binary_arg () in
    run_get env ~target ~show_binary
  in
  Cmd.v (command_info "get" "Read a value.") term

let set env =
  let open Cmdliner in
  let term =
    let open Term.Syntax in
    let+ target = key_arg ~docv:"KEY[@DB]" ~doc:"Key to write."
    and+ value = optional_value_arg () in
    run_set env ~target ~value
  in
  Cmd.v (command_info "set" "Write a value.") term

let delete env =
  let open Cmdliner in
  let term =
    let open Term.Syntax in
    let+ target = key_arg ~docv:"KEY[@DB]" ~doc:"Key to remove." in
    run_delete env ~target
  in
  Cmd.v (command_info "delete" "Remove a key.") term

let list env =
  let open Cmdliner in
  let keys_only = Arg.(value (flag (info [ "keys-only" ] ~doc:"Print keys only."))) in
  let values_only =
    Arg.(value (flag (info [ "values-only" ] ~doc:"Print values only.")))
  in
  let term =
    let open Term.Syntax in
    let+ database = optional_database_arg ()
    and+ keys_only = keys_only
    and+ values_only = values_only
    and+ show_binary = show_binary_arg () in
    run_list env ~database ~keys_only ~values_only ~show_binary
  in
  Cmd.v (command_info "list" "List keys and values.") term

let delete_db env =
  let open Cmdliner in
  let database =
    Arg.(
      required (pos 0 (some string) None (info [] ~docv:"DB" ~doc:"Database to remove.")))
  in
  let action = run_delete_db env in
  let term = Term.(const action $ database) in
  Cmd.v (command_info "delete-db" "Remove a database.") term

let dbs env =
  let action () = run_dbs env in
  let term = Cmdliner.Term.(const action $ const ()) in
  Cmdliner.Cmd.v (command_info "dbs" "List databases.") term
