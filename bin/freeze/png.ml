let find_executable name =
  if Filename.is_relative name then
    let path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
    path |> String.split_on_char ':'
    |> List.find_map (fun directory ->
        let candidate = Filename.concat directory name in
        match Unix.access candidate [ Unix.X_OK ] with
        | () -> Some candidate
        | exception Unix.Unix_error _ -> None)
  else
    match Unix.access name [ Unix.X_OK ] with
    | () -> Some name
    | exception Unix.Unix_error _ -> None

let convert ~sw ~process_mgr ~svg ~output =
  match find_executable "resvg" with
  | None -> Error "resvg not found on PATH"
  | Some executable -> (
      try
        let stdin = Eio.Flow.string_source svg in
        let process =
          Eio.Process.spawn ~sw process_mgr ~stdin [ executable; "-"; output ]
        in
        match Eio.Process.await process with
        | `Exited 0 -> Ok ()
        | `Exited code -> Error (Fmt.str "resvg exited with status %d" code)
        | `Signaled signal -> Error (Fmt.str "resvg terminated by signal %d" signal)
      with
      | Eio.Io _ -> Error "could not run resvg"
      | Unix.Unix_error (error, function_name, argument) ->
          Error
            (Fmt.str "could not run resvg: %s (%s %s)" (Unix.error_message error)
               function_name argument))
