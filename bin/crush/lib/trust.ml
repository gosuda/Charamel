open Lwt.Infix

type record = { path : string; digest : string }
type t = record list
type error = [ `Parse of string * string | `Io of string * string ]

let record_codec =
  let open Jsont in
  Object.map (fun path digest -> { path; digest })
  |> Object.mem "path" string ~enc:(fun record -> record.path)
  |> Object.mem "sha256" string ~enc:(fun record -> record.digest)
  |> Object.error_unknown |> Object.finish

let codec : t Jsont.t =
  let open Jsont in
  Object.map Fun.id
  |> Object.mem "trusted" (list record_codec) ~dec_absent:[] ~enc:Fun.id
  |> Object.error_unknown |> Object.finish

let digest bytes = Digestif.SHA256.(digest_string bytes |> to_hex)
let mem t record = List.mem record t

let add t records =
  List.fold_left
    (fun kept record ->
      List.filter (fun held -> not (String.equal held.path record.path)) kept @ [ record ])
    t records

let filesystem_failure path = function
  | (Unix.Unix_error _ | Charamel_os.Fs.E _ | Sys_error _) as exn ->
      Lwt.return_error (`Io (path, Io.message exn))
  | exn -> Lwt.fail exn

let load ~fs_root path =
  Lwt.catch
    (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.Input (Path.under ~root:fs_root path) Lwt_io.read
      >|= fun text ->
      match Jsonx.decode codec text with
      | Ok records -> Ok records
      | Error message -> Error (`Parse (path, message)))
    (function
      | Unix.Unix_error ((Unix.ENOENT | Unix.ENOTDIR), _, _) -> Lwt.return_ok []
      | exn -> filesystem_failure path exn)

let save ~fs_root path t =
  let file = Path.under ~root:fs_root path in
  Lwt.catch
    (fun () ->
      Charamel_os.Fs.mkdir_p (Filename.dirname file) >>= function
      | Error error -> Lwt.return_error (`Io (path, Io.fs_error error))
      | Ok () -> (
          State_file.replace file (Jsonx.encode ~minify:false codec t ^ "\n") >|= function
          | Ok () -> Ok ()
          | Error (`Io (_, message)) -> Error (`Io (path, message))))
    (filesystem_failure path)
