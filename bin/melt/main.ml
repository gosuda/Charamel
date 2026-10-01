module Key = Charamel_ssh_keygen
module Mnemonic = Melt_core.Mnemonic
module Env = Charamel_cli.Env
open Lwt.Infix

type error =
  [ `No_home
  | `Read_key of string * string
  | `Parse_key of Key.error
  | `Unsupported_key
  | `Mnemonic of Mnemonic.error
  | `Write_key of Key.error ]

let resolve_path path =
  Result.map
    (fun expanded ->
      if Filename.is_relative expanded then Filename.concat (Sys.getcwd ()) expanded
      else expanded)
    (Charamel_os.Dirs.expand_tilde path)

(* The key both subcommands act on when the user names none: the conventional OpenSSH
   Ed25519 location in the user's home. *)
let default_key_path = "~/.ssh/id_ed25519"

(* Mnemonic phrases are separated by any ASCII whitespace — a backup printed one word per
   line must restore from a file pasted with its newlines intact. Shell word splitting is
   the wrong tool here: it knows only spaces and tabs. *)
let split_words text =
  let is_space = function ' ' | '\t' | '\n' | '\r' | '\012' -> true | _ -> false in
  let length = String.length text in
  let rec skip index =
    if index < length && is_space text.[index] then skip (index + 1) else index
  in
  let rec take index =
    if index < length && not (is_space text.[index]) then take (index + 1) else index
  in
  let rec collect index words =
    let start = skip index in
    if start = length then List.rev words
    else
      let stop = take start in
      collect stop (String.sub text start (stop - start) :: words)
  in
  collect 0 []

let key_error_message = function
  | `Malformed -> "malformed OpenSSH private key"
  | `Unsupported_type -> "unsupported private key format"
  | `Encrypted_key -> "encrypted private keys are not supported"
  | `Already_exists path -> Fmt.str "%s already exists" path
  | `Io message -> message

let pp_error ppf = function
  | `No_home -> Format.pp_print_string ppf "melt: HOME is not set to an absolute path"
  | `Read_key (path, message) -> Fmt.pf ppf "melt: could not read key %s: %s" path message
  | `Parse_key error ->
      Fmt.pf ppf "melt: could not parse key: %s" (key_error_message error)
  | `Unsupported_key -> Format.pp_print_string ppf "melt only supports ed25519 keys"
  | `Mnemonic error -> Mnemonic.pp_error ppf error
  | `Write_key (`Already_exists path) -> Fmt.pf ppf "melt: %s already exists" path
  | `Write_key error ->
      Fmt.pf ppf "melt: could not write key: %s" (key_error_message error)

let read_key path =
  Lwt.catch
    (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.input path (fun channel -> Lwt_io.read channel)
      >|= fun body -> Ok body)
    (function
      | Unix.Unix_error (error, function_name, argument) ->
          Lwt.return
            (Error
               (`Read_key
                  ( path,
                    Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument
                  )))
      | Lwt.Canceled as exn -> Lwt.fail exn
      | exn -> Lwt.return (Error (`Read_key (path, Printexc.to_string exn))))

let backup ~fs_root:_ ~path =
  match resolve_path path with
  | Error _ as e -> Lwt.return e
  | Ok path -> (
      read_key path >>= function
      | Error _ as e -> Lwt.return e
      | Ok private_key -> (
          match Key.of_openssh_private private_key with
          | Error `Unsupported_type -> Lwt.return (Error `Unsupported_key)
          | Error error -> Lwt.return (Error (`Parse_key error))
          | Ok key -> (
              match Key.ed25519_seed key with
              | None -> Lwt.return (Error `Unsupported_key)
              | Some seed ->
                  Lwt.return
                    (Result.map_error
                       (fun error -> `Mnemonic error)
                       (Mnemonic.encode seed)))))

let restore ~fs_root ~words ~output =
  match resolve_path output with
  | Error _ as e -> Lwt.return e
  | Ok output -> (
      match Mnemonic.decode (split_words words) with
      | Error error -> Lwt.return (Error (`Mnemonic error))
      | Ok seed -> (
          match Key.of_ed25519_seed seed with
          | Error error -> Lwt.return (Error (`Write_key error))
          | Ok key ->
              Key.write ~fs_root ~path:output key >|= fun result ->
              Result.map_error (fun error -> `Write_key error) result))

let run_backup env path =
  backup ~fs_root:env.Env.fs_root ~path >>= function
  | Error error -> Charamel_cli.error (Fmt.str "%a" pp_error error)
  | Ok words -> Lwt_io.write env.Env.stdout (String.concat " " words ^ "\n")

let run_restore env words output =
  restore ~fs_root:env.Env.fs_root ~words ~output >>= function
  | Error error -> Charamel_cli.error (Fmt.str "%a" pp_error error)
  | Ok () -> Lwt.return_unit

let backup_path_arg () =
  let open Cmdliner in
  Arg.(
    value
      (pos 0 string default_key_path
         (info [] ~docv:"KEY_PATH"
            ~doc:
              (Fmt.str "OpenSSH Ed25519 private key to back up (default: %s)."
                 default_key_path))))

let backup_term (env : Charamel_cli.Env.t) =
  let open Cmdliner.Term.Syntax in
  let+ path = backup_path_arg () in
  run_backup env path

let restore_words_arg () =
  let open Cmdliner in
  Arg.(
    required
      (opt (some string) None
         (info [ "words" ] ~docv:"WORDS" ~doc:"The 24-word BIP-39 seed phrase.")))

let restore_output_arg () =
  let open Cmdliner in
  Arg.(
    value
      (opt string default_key_path
         (info [ "output" ] ~docv:"PATH"
            ~doc:(Fmt.str "Private-key output path (default: %s)." default_key_path))))

let restore_term (env : Charamel_cli.Env.t) =
  let open Cmdliner.Term.Syntax in
  let+ words = restore_words_arg () and+ output = restore_output_arg () in
  run_restore env words output

let backup_command env =
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "backup" ~doc:"Print the Ed25519 key seed as 24 BIP-39 words.")
    (backup_term env)

let restore_command env =
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "restore" ~doc:"Restore an Ed25519 key pair from 24 BIP-39 words.")
    (restore_term env)

let () =
  Mirage_crypto_rng_unix.use_default ();
  Charamel_cli.run ~name:"melt" ~version:Charamel_cli.Version.current
    ~doc:"Back up and restore OpenSSH Ed25519 keys with a 24-word seed phrase."
    ~default:backup_term
    [ backup_command; restore_command ]
