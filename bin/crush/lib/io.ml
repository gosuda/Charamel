open Lwt.Infix

type error = [ `Io of string * string | `Not_found of string ]

let fs_error = function
  | `Not_found -> "not found"
  | `Already_exists -> "already exists"
  | `Permission_denied -> "permission denied"
  | `Is_directory -> "is a directory"

let message = function
  | Unix.Unix_error (error, function_name, argument) ->
      Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument
  | Charamel_os.Fs.E (error, target) -> Fmt.str "%s: %s" (fs_error error) target
  | Sys_error text -> text
  | exn -> Printexc.to_string exn

let classify target exn =
  match exn with
  | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) | Charamel_os.Fs.E (`Not_found, _)
    ->
      Some (Error (`Not_found target))
  | (Unix.Unix_error _ | Charamel_os.Fs.E _ | Sys_error _) as known ->
      Some (Error (`Io (target, message known)))
  | _ -> None

(* [Lwt.reraise] is the documented way to re-raise from a [try_bind] handler; the raw
   backtrace is only trustworthy inside the synchronous [with] clause below. *)
let reraise_now exn = Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())

let trap target operation =
  Lwt.try_bind operation
    (fun value -> Lwt.return_ok value)
    (fun exn ->
      match classify target exn with
      | Some failure -> Lwt.return failure
      | None -> Lwt.reraise exn)

let trap_await target operation =
  try Ok (Lwt_direct.await (operation ()))
  with exn -> (
    match classify target exn with Some failure -> failure | None -> reraise_now exn)

let read_bounded path ~max =
  let buffer = Buffer.create 4096 in
  let rec pump channel =
    Lwt_io.read ~count:4096 channel >>= fun chunk ->
    if String.length chunk = 0 then Lwt.return_some (Buffer.contents buffer)
    else if Buffer.length buffer + String.length chunk > max then Lwt.return_none
    else (
      Buffer.add_string buffer chunk;
      pump channel)
  in
  Lwt.catch
    (fun () -> Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel -> pump channel))
    (function Unix.Unix_error _ | Sys_error _ -> Lwt.return_none | exn -> Lwt.fail exn)

let trim_cr line =
  let length = String.length line in
  if length > 0 && Char.equal line.[length - 1] '\r' then String.sub line 0 (length - 1)
  else line

let lines text = List.map trim_cr (String.split_on_char '\n' text)
