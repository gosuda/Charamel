type error =
  [ `Invalid_command of string
  | `Spawn of string
  | `Exit of int * string
  | `Signaled of int * string
  | `Timeout of string ]

let read_pty source output =
  let chunk = Cstruct.create 4096 in
  let rec loop () =
    let count = Eio.Flow.single_read source chunk in
    if count > 0 then begin
      Buffer.add_string output (Cstruct.to_string (Cstruct.sub chunk 0 count));
      loop ()
    end
    else ()
  in
  try loop () with End_of_file | Eio.Io _ -> ()

let terminal_size () =
  try
    let size = Eio_unix.Pty.get_window_size Eio_unix.Fd.stdout in
    (size.Eio_unix.Pty.cols, size.Eio_unix.Pty.rows)
  with Unix.Unix_error _ -> (80, 24)

let with_term ~env width height =
  let keep entry =
    (not (String.starts_with ~prefix:"TERM=" entry))
    && (not (String.starts_with ~prefix:"COLUMNS=" entry))
    && not (String.starts_with ~prefix:"LINES=" entry)
  in
  let inherited = Array.to_list env |> List.filter keep |> Array.of_list in
  let columns = "COLUMNS=" ^ string_of_int width in
  let rows = "LINES=" ^ string_of_int height in
  let term = "TERM=xterm-256color" in
  Array.append inherited [| term; columns; rows |]

let execute ~sw ~clock ~process_mgr ~env ?width ?height ~timeout command =
  if String.trim command = "" then Error (`Invalid_command "empty command")
  else
    let detected_width, detected_height = terminal_size () in
    let width = Option.value width ~default:detected_width |> max 1 in
    let height = Option.value height ~default:detected_height |> max 1 in
    let output = Buffer.create 4096 in
    let child = ref None in
    let action () =
      let terminal = Eio_unix.Pty.open_pty ~sw () in
      let winsize =
        { Eio_unix.Pty.rows = height; cols = width; xpixel = 0; ypixel = 0 }
      in
      Eio_unix.Pty.set_window_size (Eio_unix.Pty.pty terminal) winsize;
      let process =
        try
          let process =
            Eio_unix.Process.spawn_unix ~sw process_mgr
              ~login_tty:(Eio_unix.Pty.tty terminal) ~env:(with_term ~env width height)
              ~fds:[] [ "/bin/sh"; "-c"; command ]
          in
          Eio_unix.Fd.close (Eio_unix.Pty.tty terminal);
          process
        with Eio.Io _ -> raise (Failure "could not spawn command")
      in
      child := Some process;
      let source = Eio_unix.Pty.source terminal in
      let status = ref None in
      Eio.Fiber.all
        [
          (fun () -> read_pty source output);
          (fun () -> status := Some (Eio.Process.await process));
        ];
      let status = Option.value !status ~default:(`Exited 1) in
      match status with
      | `Exited 0 -> Ok (Buffer.contents output)
      | `Exited code -> Error (`Exit (code, Buffer.contents output))
      | `Signaled signal -> Error (`Signaled (signal, Buffer.contents output))
    in
    match Eio.Time.with_timeout clock timeout action with
    | Ok value -> Ok value
    | Error `Timeout ->
        (match !child with
        | Some process -> Eio.Process.signal process Sys.sigkill
        | None -> ());
        Error (`Timeout (Buffer.contents output))
    | Error (`Exit _ as exit_error) -> Error exit_error
    | Error (`Signaled _ as signaled_error) -> Error signaled_error
    | exception Failure message -> Error (`Spawn message)
    | exception Unix.Unix_error (error, function_name, argument) ->
        Error
          (`Spawn
             (Fmt.str "could not create PTY: %s (%s %s)" (Unix.error_message error)
                function_name argument))
