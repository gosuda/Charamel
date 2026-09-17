exception No_tty

let fd_is_tty flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> false
  | Some fd -> (
      try Eio_unix.Fd.use_exn "isatty" fd Unix.isatty
      with Unix.Unix_error (_, _, _) -> false)

let stdin_is_tty env = fd_is_tty env#stdin
let stdout_is_tty env = fd_is_tty env#stdout
let stderr_is_tty env = fd_is_tty env#stderr

let stdin_is_empty env =
  if stdin_is_tty env then true
  else
    match Eio_unix.Resource.fd_opt env#stdin with
    | None -> false
    | Some fd -> (
        try
          Eio_unix.Fd.use_exn "fstat" fd (fun fd ->
              let stat = Unix.fstat fd in
              match stat.Unix.st_kind with
              | Unix.S_FIFO -> false
              | _ -> stat.Unix.st_size = 0)
        with Unix.Unix_error (_, _, _) -> true)

let read_stdin ?(strip_ansi = true) ?(single_line = false) env =
  if stdin_is_empty env then Error `Empty
  else
    let reader = Eio.Buf_read.of_flow ~max_size:(64 * 1024 * 1024) env#stdin in
    let raw =
      try
        Some
          (if single_line then Eio.Buf_read.line reader else Eio.Buf_read.take_all reader)
      with End_of_file -> None
    in
    match raw with
    | None -> Error `Empty
    | Some raw ->
        let value = String.trim raw in
        let value = if strip_ansi then Charamel_ansi.Text.strip value else value in
        if value = "" then Error `Empty else Ok value

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

let println (env : Eio_unix.Stdenv.base) text =
  let profile =
    Charamel_colorprofile.detect ~is_tty:(stdout_is_tty env) ~env:Sys.getenv_opt
  in
  let writer =
    Charamel_colorprofile.Writer.create ~profile
      (env#stdout :> Eio.Flow.sink_ty Eio.Resource.t)
  in
  Charamel_colorprofile.Writer.write writer (text ^ "\n")

let print_raw env text = Eio.Flow.copy_string (text ^ "\n") env#stdout

let skipped_component component =
  component = "." || component = ".." || component = ".git" || component = "node_modules"
  || (String.length component > 0 && component.[0] = '.')

let list_files env =
  let root = env#cwd in
  let rec visit path relative acc =
    let entries = Eio.Path.read_dir_entries path in
    List.fold_left
      (fun acc (kind, name) ->
        if skipped_component name then acc
        else
          let child = Eio.Path.(path / name) in
          let child_relative = if relative = "" then name else relative ^ "/" ^ name in
          match kind with
          | `Directory -> visit child child_relative acc
          | `Regular_file | `Symbolic_link -> child_relative :: acc
          | _ -> acc)
      acc entries
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

let terminal_size env =
  match Eio_unix.Resource.fd_opt env#stderr with
  | None -> fallback_size ()
  | Some fd -> (
      try
        let size = Eio_unix.Pty.get_window_size fd in
        (size.Eio_unix.Pty.rows, size.Eio_unix.Pty.cols)
      with Unix.Unix_error (_, _, _) -> fallback_size ())

let ui_terminal ?sw env =
  if stdin_is_tty env && stderr_is_tty env then
    Charamel_tea.Terminal.local ~output:`Stderr env
  else if not (stderr_is_tty env) then raise No_tty
  else
    let tty_switch = match sw with Some sw -> sw | None -> raise No_tty in
    let input =
      try Eio.Path.open_in ~sw:tty_switch Eio.Path.(env#fs / "/dev/tty") with
      | Eio.Io (Eio.Fs.E _, _) -> raise No_tty
      | Eio.Io (Eio.Exn.Not_available _, _) -> raise No_tty
      | Unix.Unix_error (_, _, _) -> raise No_tty
    in
    Charamel_tea.Terminal.custom ~input ~output:env#stderr
      ~size:(fun () -> terminal_size env)
      ~on_resize:None ~env:Sys.getenv_opt ~is_tty:true
