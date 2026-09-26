module Key = Charamel_ssh_keygen
open Result.Syntax
open Lwt.Infix

type error =
  [ `No_home
  | `Invalid_path of string
  | `Already_exists of string
  | `Target_is_directory of string
  | `Io of string ]

let pp_error ppf = function
  | `No_home -> Format.pp_print_string ppf "keygen: HOME is not set to an absolute path"
  | `Invalid_path path -> Fmt.pf ppf "keygen: invalid path %S" path
  | `Already_exists path -> Fmt.pf ppf "keygen: %s already exists" path
  | `Target_is_directory path -> Fmt.pf ppf "keygen: %s is a directory" path
  | `Io message -> Fmt.pf ppf "keygen: %s" message

let home_dir () =
  match Sys.getenv_opt "HOME" with
  | Some home when home <> "" && not (Filename.is_relative home) -> Ok home
  | _ -> Error `No_home

let resolve_path path =
  let length = String.length path in
  if String.equal path "" then Error (`Invalid_path path)
  else if String.equal path "~" then home_dir ()
  else if length >= 2 && Char.equal path.[0] '~' && Char.equal path.[1] '/' then
    let* home = home_dir () in
    Ok (Filename.concat home (String.sub path 2 (length - 2)))
  else if Filename.is_relative path then Ok (Filename.concat (Sys.getcwd ()) path)
  else Ok path

let default_path algorithm =
  let* home = home_dir () in
  Ok
    (Filename.concat home (Filename.concat ".ssh" ("id_" ^ Key.algorithm_name algorithm)))

let fs_text = function
  | `Already_exists -> "already exists"
  | `Is_directory -> "is a directory"
  | `Not_found -> "no such file or directory"
  | `Permission_denied -> "permission denied"

let io_text exn =
  match exn with
  | Unix.Unix_error (kind, _, _) -> Unix.error_message kind
  | Charamel_os.Fs.E (error, path) -> Fmt.str "%s: %s" path (fs_text error)
  | exn -> Printexc.to_string exn

type target_state = Missing | Existing | Directory

let target_state path =
  Lwt.catch
    (fun () ->
      Lwt_unix.lstat path >|= fun stats ->
      Ok (if stats.Unix.st_kind = Unix.S_DIR then Directory else Existing))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok Missing)
      | Lwt.Canceled as exn -> Lwt.fail exn
      | exn -> Lwt.return (Error (`Io (io_text exn))))

let check_target ~force ~name path =
  target_state path >|= function
  | Error _ as e -> e
  | Ok Missing -> Ok ()
  | Ok Existing -> if force then Ok () else Error (`Already_exists name)
  | Ok Directory ->
      if force then Error (`Target_is_directory name) else Error (`Already_exists name)

let check_targets ~path ~force =
  let public_name = path ^ ".pub" in
  check_target ~force ~name:path path >>= function
  | Error _ as e -> Lwt.return e
  | Ok () -> check_target ~force ~name:public_name public_name

let mkdir_exists perm path =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir path perm >|= fun () -> Ok ())
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          Lwt.catch
            (fun () ->
              Lwt_unix.stat path >|= fun stats ->
              if stats.Unix.st_kind = Unix.S_DIR then Ok () else Error Unix.ENOTDIR)
            (fun _ -> Lwt.return (Error Unix.ENOTDIR))
      | Unix.Unix_error (code, _, _) -> Lwt.return (Error code)
      | Lwt.Canceled as exn -> Lwt.fail exn
      | exn -> Lwt.fail exn)

let ensure_dir ~perm path =
  let rec up path =
    if path = Filename.dir_sep || path = "." then Lwt.return (Ok ())
    else
      mkdir_exists perm path >>= function
      | Ok () -> Lwt.return (Ok ())
      | Error Unix.ENOENT when not (String.equal (Filename.dirname path) path) -> (
          up (Filename.dirname path) >>= function
          | Ok () -> mkdir_exists perm path
          | Error _ as e -> Lwt.return e)
      | Error code -> Lwt.return (Error code)
  in
  up path

let ensure_parent path =
  match Filename.dirname path with
  | parent when String.equal parent path -> Lwt.return (Ok ())
  | parent -> (
      ensure_dir ~perm:0o700 parent >|= function
      | Ok () -> Ok ()
      | Error code -> Error (`Io (Unix.error_message code)))

let generated_error = function
  | `Already_exists path -> Error (`Already_exists path)
  | `Io message -> Error (`Io message)
  | `Malformed -> Error (`Io "generated private key is malformed")
  | `Unsupported_type -> Error (`Io "generated key type is unsupported")
  | `Encrypted_key -> Error (`Io "generated key is encrypted")

let write_nonforce ~fs_root ~path ~comment key =
  Key.write ~fs_root ~path ~comment key >|= function
  | Ok () -> Ok (Key.fingerprint_sha256 key)
  | Error error -> generated_error error

let is_ascii_alphanumeric = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let temporary_suffix fingerprint =
  String.map
    (fun character -> if is_ascii_alphanumeric character then character else '_')
    fingerprint

let force_write ~path ~comment key =
  let private_name = path in
  let public_name = path ^ ".pub" in
  let fingerprint = Key.fingerprint_sha256 key in
  let suffix = temporary_suffix fingerprint in
  let private_tmp = private_name ^ ".charamel-keygen-" ^ suffix in
  let public_tmp = public_name ^ ".charamel-keygen-" ^ suffix in
  let private_body = Key.to_openssh_private ~comment key in
  let public_body = Key.authorized_key ~comment key in
  let created = ref [] in
  let cleanup () =
    Lwt_list.iter_s
      (fun path ->
        Lwt.catch (fun () -> Lwt_unix.unlink path) (fun _exn -> Lwt.return_unit))
      !created
  in
  let save path perm body =
    Lwt_unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] perm >>= fun fd ->
    created := path :: !created;
    let channel = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    Lwt.finalize
      (fun () -> Lwt_io.write channel body >>= fun () -> Lwt_unix.fchmod fd perm)
      (fun () -> Lwt_io.close channel)
  in
  Lwt.catch
    (fun () ->
      save private_tmp 0o600 private_body >>= fun () ->
      save public_tmp 0o644 public_body >>= fun () ->
      Lwt_unix.rename private_tmp private_name >>= fun () ->
      Lwt_unix.rename public_tmp public_name >>= fun () ->
      created := [];
      Lwt.return (Ok fingerprint))
    (function
      | Lwt.Canceled as exn -> Lwt.fail exn
      | exn -> Lwt.return (Error (`Io (io_text exn))))
  >>= function
  | Ok _ as ok -> Lwt.return ok
  | Error _ as e -> cleanup () >>= fun () -> Lwt.return e

let generate ~fs_root ~path ~algorithm ?(comment = "") ~force () =
  match resolve_path path with
  | Error _ as e -> Lwt.return e
  | Ok path -> (
      check_targets ~path ~force >>= function
      | Error _ as e -> Lwt.return e
      | Ok () -> (
          ensure_parent path >>= function
          | Error _ as e -> Lwt.return e
          | Ok () ->
              let key = Key.generate algorithm in
              if force then force_write ~path ~comment key
              else write_nonforce ~fs_root ~path ~comment key))
