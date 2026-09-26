open Lwt.Infix

type error = [ `Io of string * string ]

let counter = Atomic.make 0
let pp_error ppf (`Io (path, message)) = Fmt.pf ppf "cannot replace %s: %s" path message

let describe = function
  | Unix.Unix_error (error, function_name, argument) ->
      Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument
  | Charamel_os.Fs.E (error, path) ->
      let name =
        match error with
        | `Not_found -> "not found"
        | `Already_exists -> "already exists"
        | `Permission_denied -> "permission denied"
        | `Is_directory -> "is a directory"
      in
      Fmt.str "%s: %s" name path
  | exn -> Printexc.to_string exn

let temp_path parent basename attempt =
  let nonce = Atomic.fetch_and_add counter 1 in
  Filename.concat parent
    (Fmt.str ".%s.crush-state.%d.%d" basename (Unix.getpid ()) (nonce + attempt))

let write_new path contents =
  Lwt_unix.openfile path [ O_WRONLY; O_CREAT; O_EXCL ] 0o600 >>= fun fd ->
  let channel = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.finalize (fun () -> Lwt_io.write channel contents) (fun () -> Lwt_io.close channel)

let create_temp parent basename contents temporary =
  let rec attempt count =
    let path = temp_path parent basename count in
    temporary := Some path;
    Lwt.catch
      (fun () -> write_new path contents >>= fun () -> Lwt.return path)
      (function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> attempt (count + 1) | exn -> Lwt.fail exn)
  in
  attempt 0

let remove path = Charamel_os.Fs.unlink path >|= fun _ -> ()

let replace destination contents =
  let parent = Filename.dirname destination in
  let basename = Filename.basename destination in
  let temporary = ref None in
  let committed = ref false in
  let operation () =
    create_temp parent basename contents temporary >>= fun path ->
    Charamel_os.Fs.rename_replace ~src:path ~dst:destination >>= fun () ->
    committed := true;
    Lwt.return_ok ()
  in
  let cleanup () =
    match (!temporary, !committed) with
    | Some path, false -> remove path
    | _ -> Lwt.return_unit
  in
  Lwt.catch
    (fun () -> Lwt.finalize operation cleanup)
    (fun exn -> Lwt.return_error (`Io (destination, describe exn)))
