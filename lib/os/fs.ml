open Lwt.Infix

type error = [ `Already_exists | `Is_directory | `Not_found | `Permission_denied ]

exception E of error * string

let classify = function
  | Unix.ENOENT | Unix.ENOTDIR -> Some `Not_found
  | Unix.EEXIST -> Some `Already_exists
  | Unix.EACCES | Unix.EPERM -> Some `Permission_denied
  | Unix.EISDIR -> Some `Is_directory
  | _ -> None

(* An error the taxonomy names becomes the caller's [Error]; anything else is re-raised
   unchanged, because inventing a plausible variant for [ENAMETOOLONG] would hide the only
   information that explains it. *)
let decide path = function
  | Unix.Unix_error (code, _, _) as exn -> (
      match classify code with Some error -> raise (E (error, path)) | None -> raise exn)
  | exn -> raise exn

let raised path f = Lwt.catch f (fun exn -> decide path exn)

let to_result f =
  Lwt.catch
    (fun () -> f () >|= fun value -> Ok value)
    (fun exn ->
      match exn with
      | Unix.Unix_error (code, _, _) -> (
          match classify code with
          | Some error -> Lwt.return (Error error)
          | None -> Lwt.fail exn)
      | _ -> Lwt.fail exn)

let rename_replace ~src ~dst = raised dst (fun () -> Lwt_unix.rename src dst)

let with_open_out ~perm path k =
  raised path (fun () ->
      Lwt_unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] perm
      >|= fun fd -> Lwt_io.of_fd ~mode:Lwt_io.output fd)
  >>= fun channel -> Lwt.finalize (fun () -> k channel) (fun () -> Lwt_io.close channel)

let hidden = [ "."; ".." ]

let read_dir path =
  Lwt.catch
    (fun () ->
      Lwt_stream.to_list (Lwt_unix.files_of_directory path) >|= fun names -> Ok names)
    (fun exn ->
      match exn with
      | Unix.Unix_error (code, operation, detail) ->
          Lwt.return (Error (Unix.error_message code ^ ": " ^ operation ^ " " ^ detail))
      | _ -> Lwt.fail exn)
  >|= Result.map (List.filter (fun name -> not (List.mem name hidden)))

(* [EINVAL] is how POSIX reports "this exists but is no link", which is the same answer a
   caller needs as when the path is missing; Windows reports [ENOTDIR] for the same case. *)
let read_link path =
  Lwt.catch
    (fun () -> Lwt_unix.readlink path >|= fun text -> Ok text)
    (fun exn ->
      match exn with
      | Unix.Unix_error (Unix.EINVAL, _, _) | Unix.Unix_error (Unix.ENOTDIR, _, _) ->
          Lwt.return (Error `Not_found)
      | Unix.Unix_error (code, _, _) -> (
          match classify code with
          | Some error -> Lwt.return (Error error)
          | None -> Lwt.fail exn)
      | _ -> Lwt.fail exn)

let stat path = to_result (fun () -> Lwt_unix.stat path)

(* [Filename.dirname] owns the decomposition: it already knows drive roots ([C:\]),
   UNC share roots ([\\server\share]), trailing separators and relative paths, and it
   answers each parent in the spelling the filesystem expects. The climb stops where the
   parent stops changing, which is exactly the root — [.] for a relative path. *)
let prefixes path =
  let rec climb dir acc =
    let parent = Filename.dirname dir in
    if String.equal parent dir then dir :: acc else climb parent (dir :: acc)
  in
  climb path []

let is_directory path =
  Lwt.catch
    (fun () -> Lwt_unix.stat path >|= fun stats -> stats.Unix.st_kind = Unix.S_DIR)
    (function Unix.Unix_error _ -> Lwt.return false | exn -> Lwt.fail exn)

(* Darwin refuses a directory's [unlink] with EPERM instead of the POSIX EISDIR, so the
   kind is asked first and the documented answer does not depend on the platform. *)
let unlink path =
  is_directory path >>= function
  | true -> Lwt.return (Error `Is_directory)
  | false -> to_result (fun () -> Lwt_unix.unlink path)

let mkdir_one path =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir path 0o777 >|= fun () -> Ok ())
    (fun exn ->
      match exn with
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          Lwt.map
            (function true -> Ok () | false -> Error `Already_exists)
            (is_directory path)
      | Unix.Unix_error ((Unix.EACCES | Unix.EPERM), _, _) ->
          (* Windows denies [mkdir] on an existing directory — a drive root
             reports EACCES rather than EEXIST — so a directory that is already
             there still counts as made; anything else is a real denial. *)
          Lwt.map
            (function true -> Ok () | false -> Error `Permission_denied)
            (is_directory path)
      | Unix.Unix_error (code, _, _) -> (
          match classify code with
          | Some error -> Lwt.return (Error error)
          | None -> Lwt.fail exn)
      | _ -> Lwt.fail exn)

let rec create_all = function
  | [] -> Lwt.return (Ok ())
  | first :: rest -> (
      mkdir_one first >>= function
      | Error _ as outcome -> Lwt.return outcome
      | Ok () -> create_all rest)

let mkdir_p path = create_all (prefixes path)
