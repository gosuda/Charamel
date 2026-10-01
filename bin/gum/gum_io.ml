module Env = Charamel_cli.Env

exception No_tty

let stdin_is_tty (_ : Charamel_cli.Env.t) = Charamel_os.Tty.is_tty_stdin
let stdout_is_tty (_ : Charamel_cli.Env.t) = Charamel_os.Tty.is_tty_stdout

let stderr_is_tty (_ : Charamel_cli.Env.t) =
  try Unix.isatty Unix.stderr with Unix.Unix_error (_, _, _) -> false

let stdin_is_empty env =
  if stdin_is_tty env then Lwt.return true
  else
    Lwt.return
      (try
         let stat = Unix.fstat Unix.stdin in
         match stat.Unix.st_kind with Unix.S_FIFO -> false | _ -> stat.Unix.st_size = 0
       with Unix.Unix_error (_, _, _) -> true)

let max_stdin = 64 * 1024 * 1024

let read_all_stdin () =
  let buffer = Buffer.create 4096 in
  let rec loop () =
    Lwt.bind (Lwt_io.read ~count:65536 Lwt_io.stdin) (fun chunk ->
        if chunk = "" then Lwt.return (Buffer.contents buffer)
        else begin
          Buffer.add_string buffer chunk;
          if Buffer.length buffer > max_stdin then
            Lwt.fail (Failure "stdin exceeds 64 MiB")
          else loop ()
        end)
  in
  loop ()

let read_line_stdin () =
  let buffer = Buffer.create 256 in
  let rec loop () =
    Lwt.bind (Lwt_io.read_char_opt Lwt_io.stdin) (function
      | None ->
          Lwt.return
            (if Buffer.length buffer = 0 then None else Some (Buffer.contents buffer))
      | Some '\n' -> Lwt.return (Some (Buffer.contents buffer))
      | Some character ->
          Buffer.add_char buffer character;
          if Buffer.length buffer > max_stdin then
            Lwt.fail (Failure "stdin exceeds 64 MiB")
          else loop ())
  in
  loop ()

let read_stdin ?(strip_ansi = true) ?(single_line = false) env =
  Lwt.bind (stdin_is_empty env) (fun empty ->
      if empty then Lwt.return (Error `Empty)
      else
        let raw =
          if single_line then read_line_stdin ()
          else Lwt.map (fun text -> Some text) (read_all_stdin ())
        in
        Lwt.map
          (function
            | None -> Error `Empty
            | Some raw ->
                let value = String.trim raw in
                let value =
                  if strip_ansi then Charamel_ansi.Text.strip value else value
                in
                if value = "" then Error `Empty else Ok value)
          raw)

let split ~delimiter text =
  if delimiter = "" then [ text ]
  else
    let delimiter_length = String.length delimiter in
    let text_length = String.length text in
    let rec scan index start_rev acc =
      if index + delimiter_length > text_length then
        List.rev (String.sub text start_rev (text_length - start_rev) :: acc)
      else if String.sub text index delimiter_length = delimiter then
        let piece = String.sub text start_rev (index - start_rev) in
        scan (index + delimiter_length) (index + delimiter_length) (piece :: acc)
      else scan (index + 1) start_rev acc
    in
    scan 0 0 []

let println (env : Charamel_cli.Env.t) text =
  let profile =
    Charamel_colorprofile.detect ~is_tty:(stdout_is_tty env) ~env:Sys.getenv_opt
  in
  let writer = Charamel_colorprofile.Writer.create ~profile env.Env.stdout in
  Charamel_colorprofile.Writer.write writer (text ^ "\n")

let print_raw (env : Charamel_cli.Env.t) text = Lwt_io.write env.Env.stdout (text ^ "\n")

let skipped_component component =
  component = "." || component = ".." || component = ".git" || component = "node_modules"
  || (String.length component > 0 && component.[0] = '.')

let list_files (env : Charamel_cli.Env.t) =
  let root = env.Env.cwd in
  let rec visit path relative acc =
    let names = try Array.to_list (Sys.readdir path) with Sys_error _ -> [] in
    List.fold_left
      (fun acc name ->
        if skipped_component name then acc
        else
          let child = Filename.concat path name in
          let child_relative = if relative = "" then name else relative ^ "/" ^ name in
          match Unix.lstat child with
          | { Unix.st_kind = Unix.S_DIR; _ } -> visit child child_relative acc
          | { Unix.st_kind = Unix.S_REG | Unix.S_LNK; _ } -> child_relative :: acc
          | _ -> acc
          | exception Unix.Unix_error (_, _, _) -> acc)
      acc names
  in
  visit root "" [] |> List.rev

let fallback_size () =
  let positive name default =
    match Sys.getenv_opt name with
    | Some value -> (
        match int_of_string_opt (String.trim value) with
        | Some n when n > 0 -> n
        | _ -> default)
    | None -> default
  in
  (positive "LINES" 24, positive "COLUMNS" 80)

(* The custom transport paints to stderr while stdout may carry data, so the size is
   the one [stderr] reports; the environment fallback chain stays behind it. *)
let terminal_size (_ : Charamel_cli.Env.t) =
  match Charamel_os.Tty.size_of_output `Stderr with
  | Some size -> size
  | None -> fallback_size ()

let ui_terminal (env : Charamel_cli.Env.t) =
  if stdin_is_tty env && stderr_is_tty env then
    Charamel_tea.Terminal.local ~output:`Stderr ()
  else if not (stderr_is_tty env) then raise No_tty
  else
    let input =
      try Charamel_os.Console_input.of_channel (Charamel_os.Tty.open_controlling_in ())
      with Unix.Unix_error (_, _, _) -> raise No_tty
    in
    Charamel_tea.Terminal.custom ~input ~output:env.Env.stderr
      ~size:(fun () -> terminal_size env)
      ~on_resize:None ~env:Sys.getenv_opt ~is_tty:true

let frame_origin frame =
  let rec search row = function
    | [] -> (0, 0)
    | line :: rest -> (
        let plain = Charamel_ansi.Text.strip line in
        match String.index_opt plain '\001' with
        | None -> search (row + 1) rest
        | Some byte -> (row, Charamel_ansi.Text.width (String.sub plain 0 byte)))
  in
  search 0 (String.split_on_char '\n' (frame "\001"))

let place_cursor ~frame = function
  | None -> None
  | Some (cursor : Charamel_tea.Cursor.t) ->
      let row, col = frame_origin frame in
      Some { cursor with row = cursor.row + row; col = cursor.col + col }
