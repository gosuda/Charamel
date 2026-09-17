module Key = Charamel_ssh_keygen
open Result.Syntax

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

let io_result fn =
  try Ok (fn ())
  with Eio.Io (Eio.Fs.E _, _) as exn ->
    Eio.Fiber.check ();
    Error (`Io (Fmt.str "%a" Eio.Exn.pp exn))

let path_of fs path = Eio.Path.(Eio.Path.of_dir fs / path)

type target_state = Missing | Existing | Directory

let target_state path =
  match io_result (fun () -> Eio.Path.kind ~follow:false path) with
  | Error error -> Error error
  | Ok `Not_found -> Ok Missing
  | Ok `Directory -> Ok Directory
  | Ok _ -> Ok Existing

let check_target ~force ~name path =
  let* state = target_state path in
  match state with
  | Missing -> Ok ()
  | Existing -> if force then Ok () else Error (`Already_exists name)
  | Directory ->
      if force then Error (`Target_is_directory name) else Error (`Already_exists name)

let check_targets ~fs ~path ~force =
  let private_path = path_of fs path in
  let public_name = path ^ ".pub" in
  let public_path = path_of fs public_name in
  let* () = check_target ~force ~name:path private_path in
  check_target ~force ~name:public_name public_path

let ensure_parent ~fs path =
  match Eio.Path.split (path_of fs path) with
  | None -> Ok ()
  | Some (parent, _) ->
      io_result (fun () -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 parent)

let generated_error = function
  | `Already_exists path -> Error (`Already_exists path)
  | `Io message -> Error (`Io message)
  | `Malformed -> Error (`Io "generated private key is malformed")
  | `Unsupported_type -> Error (`Io "generated key type is unsupported")
  | `Encrypted_key -> Error (`Io "generated key is encrypted")

let write_nonforce ~fs ~path ~comment key =
  match Key.write ~fs ~path ~comment key with
  | Ok () -> Ok (Key.fingerprint_sha256 key)
  | Error error -> generated_error error

let is_ascii_alphanumeric = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let temporary_suffix fingerprint =
  String.map
    (fun character -> if is_ascii_alphanumeric character then character else '_')
    fingerprint

let force_write ~fs ~path ~comment key =
  let private_name = path in
  let public_name = path ^ ".pub" in
  let private_path = path_of fs private_name in
  let public_path = path_of fs public_name in
  let fingerprint = Key.fingerprint_sha256 key in
  let suffix = temporary_suffix fingerprint in
  let private_tmp = path_of fs (private_name ^ ".charamel-keygen-" ^ suffix) in
  let public_tmp = path_of fs (public_name ^ ".charamel-keygen-" ^ suffix) in
  let private_body = Key.to_openssh_private ~comment key in
  let public_body = Key.authorized_key ~comment key in
  let created = ref [] in
  let cleanup () =
    List.iter
      (fun path ->
        try Eio.Path.unlink ~missing_ok:true path with Eio.Io (Eio.Fs.E _, _) -> ())
      !created
  in
  let save path perm body =
    Eio.Path.with_open_out ~create:(`Exclusive perm) path (fun flow ->
        created := path :: !created;
        Eio.Flow.copy_string body flow);
    Eio.Path.chmod ~follow:false ~perm path
  in
  let result =
    Eio.Cancel.protect (fun () ->
        try
          save private_tmp 0o600 private_body;
          save public_tmp 0o644 public_body;
          Eio.Path.rename private_tmp private_path;
          Eio.Path.rename public_tmp public_path;
          created := [];
          Ok ()
        with Eio.Io (Eio.Fs.E _, _) as exn -> Error (`Io (Fmt.str "%a" Eio.Exn.pp exn)))
  in
  match result with
  | Ok () -> Ok fingerprint
  | Error error ->
      Eio.Cancel.protect (fun () -> cleanup ());
      Error error

let generate ~fs ~path ~algorithm ?(comment = "") ~force () =
  let* path = resolve_path path in
  let* () = check_targets ~fs ~path ~force in
  let* () = ensure_parent ~fs path in
  let key = Key.generate algorithm in
  if force then force_write ~fs ~path ~comment key
  else write_nonforce ~fs ~path ~comment key
