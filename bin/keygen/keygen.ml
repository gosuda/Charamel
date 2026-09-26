module Key = Charamel_ssh_keygen
open Result.Syntax
open Lwt.Infix

type error =
  [ `No_home | `Invalid_path of string | `Already_exists of string | `Io of string ]

let pp_error ppf = function
  | `No_home -> Format.pp_print_string ppf "keygen: HOME is not set to an absolute path"
  | `Invalid_path path -> Fmt.pf ppf "keygen: invalid path %S" path
  | `Already_exists path -> Fmt.pf ppf "keygen: %s already exists" path
  | `Io message -> Fmt.pf ppf "keygen: %s" message

(* A leading [~] names the user's home and a bare relative name is the current
   directory's: the two spellings a shell hands to [-f]. An empty name is refused,
   because it would otherwise resolve to the working directory itself. *)
let resolve_path path =
  if String.equal path "" then Error (`Invalid_path path)
  else
    Result.map
      (fun expanded ->
        if Filename.is_relative expanded then Filename.concat (Sys.getcwd ()) expanded
        else expanded)
      (Charamel_os.Dirs.expand_tilde path)

let default_path algorithm =
  let* home = Charamel_os.Dirs.home () in
  Ok
    (Filename.concat home (Filename.concat ".ssh" ("id_" ^ Key.algorithm_name algorithm)))

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

let write_key ~fs_root ~path ~comment ~force key =
  Key.write ~fs_root ~path ~comment ~overwrite:force key >|= function
  | Ok () -> Ok (Key.fingerprint_sha256 key)
  | Error error -> generated_error error

let generate ~fs_root ~path ~algorithm ?(comment = "") ~force () =
  match resolve_path path with
  | Error _ as e -> Lwt.return e
  | Ok path -> (
      ensure_parent path >>= function
      | Error _ as e -> Lwt.return e
      | Ok () -> write_key ~fs_root ~path ~comment ~force (Key.generate algorithm))
