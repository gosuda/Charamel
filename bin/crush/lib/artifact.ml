open Lwt.Infix

type t = { fs_root : string; dir : string; mutex : Lwt_mutex.t }

let max_inline_bytes = 65_536
let head_lines = 50
let tail_lines = 20
let path t name = Filename.concat (Filename.concat t.fs_root t.dir) name

let create_dir path =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir path 0o700)
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
      | Unix.Unix_error (Unix.ENOENT, _, _) -> (
          Charamel_os.Fs.mkdir_p path >>= function
          | Ok () -> Lwt.return_unit
          | Error error -> Lwt.fail (Charamel_os.Fs.E (error, path)))
      | exn -> Lwt.fail exn)

let save_path path contents =
  Lwt.catch
    (fun () ->
      Lwt_unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] 0o600
      >>= fun fd ->
      let channel = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
      Lwt.finalize
        (fun () -> Lwt_io.write channel contents)
        (fun () -> Lwt_io.close channel)
      >>= fun () -> Lwt.return_ok ())
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_error `Exists
      | exn -> Lwt.return_error (`Io (path, Io.message exn)))

let create ~fs_root ~dir = { fs_root; dir; mutex = Lwt_mutex.create () }
let hex_byte c = Fmt.str "%02x" (Char.code c)

let id_of_random random =
  let bytes = random 4 in
  if String.length bytes <> 4 then
    invalid_arg "Artifact.save: random function must return exactly 4 bytes"
  else "art-" ^ String.concat "" (List.init 4 (fun index -> hex_byte bytes.[index]))

let save t ~random contents =
  Lwt_mutex.with_lock t.mutex (fun () ->
      let directory = Filename.concat t.fs_root t.dir in
      Io.trap t.dir (fun () -> create_dir directory) >>= function
      | Error (`Not_found path) ->
          Lwt.return_error (`Io (path, "artifact directory does not exist"))
      | Error (`Io _) as error -> Lwt.return error
      | Ok () ->
          let rec attempt remaining =
            if remaining = 0 then
              Lwt.return_error (`Io (t.dir, "could not allocate a unique artifact id"))
            else
              let id = id_of_random random in
              save_path (path t (id ^ ".txt")) contents >>= function
              | Ok () -> Lwt.return_ok id
              | Error `Exists -> attempt (remaining - 1)
              | Error (`Io (_, message)) -> Lwt.return_error (`Io (t.dir, message))
          in
          attempt 32)

let valid_id id =
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') in
  String.length id = 12
  && String.sub id 0 4 = "art-"
  &&
  let rec loop index =
    if index = 12 then true else is_hex id.[index] && loop (index + 1)
  in
  loop 4

let read_all path =
  Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel -> Lwt_io.read channel)

let load t ~id =
  if not (valid_id id) then Lwt.return_error (`Not_found id)
  else
    let target = Filename.concat t.dir (id ^ ".txt") in
    let file = path t (id ^ ".txt") in
    Io.trap target (fun () -> Lwt_unix.lstat file) >>= function
    | Error error -> Lwt.return_error error
    | Ok { Unix.st_kind = Unix.S_REG; _ } -> Io.trap target (fun () -> read_all file)
    | Ok _ -> Lwt.return_error (`Not_found target)

let is_utf8_continuation c = Char.code c land 0xC0 = 0x80

let utf8_prefix text limit =
  let length = min (String.length text) (max 0 limit) in
  let rec boundary index =
    if index = 0 || not (is_utf8_continuation text.[index]) then index
    else boundary (index - 1)
  in
  let length = if length = String.length text then length else boundary length in
  String.sub text 0 length

let utf8_suffix text limit =
  let length = String.length text in
  let start = max 0 (length - max 0 limit) in
  let rec boundary index =
    if index = length || not (is_utf8_continuation text.[index]) then index
    else boundary (index + 1)
  in
  let start = if start = 0 then 0 else boundary start in
  String.sub text start (length - start)

let lines_of_text text =
  if text = "" then []
  else
    let lines = String.split_on_char '\n' text in
    if List.length lines > 0 && List.hd (List.rev lines) = "" then
      List.rev (List.tl (List.rev lines))
    else lines

let preview ~contents ~id =
  let lines = lines_of_text contents in
  let total = List.length lines in
  let first_count = min head_lines total in
  let tail_start = max first_count (total - tail_lines) in
  let omitted = tail_start - first_count in
  let first = List.filteri (fun index _ -> index < first_count) lines in
  let tail = List.filteri (fun index _ -> index >= tail_start) lines in
  let footer =
    Fmt.str
      "[... %d lines elided; full output at artifact://%s; read it with the read tool \
       ...]"
      omitted id
  in
  let selected = first @ [ footer ] @ tail in
  let trailing =
    String.length contents > 0 && contents.[String.length contents - 1] = '\n'
  in
  let join values =
    let result = String.concat "\n" values in
    if trailing then result ^ "\n" else result
  in
  let complete = join selected in
  if String.length complete <= max_inline_bytes then complete
  else
    let head = String.concat "\n" first in
    let tail = String.concat "\n" tail in
    let trailing_bytes = if trailing then 1 else 0 in
    let available =
      max 0 (max_inline_bytes - String.length footer - 2 - trailing_bytes)
    in
    let head_budget = available / 2 in
    let tail_budget = available - head_budget in
    let head_piece = utf8_prefix head head_budget in
    let tail_source = if omitted = 0 then head else tail in
    let tail_piece = utf8_suffix tail_source tail_budget in
    let result = head_piece ^ "\n" ^ footer ^ "\n" ^ tail_piece in
    if trailing then result ^ "\n" else result

let truncate t ~random contents =
  if String.length contents <= max_inline_bytes then Lwt.return (contents, None)
  else
    save t ~random contents >>= function
    | Ok id -> Lwt.return (preview ~contents ~id, Some id)
    | Error _ -> Lwt.return (contents, None)
