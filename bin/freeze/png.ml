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

let run_resvg executable svg output =
  let process = Charamel_os.Process.spawn ~stdin:`Pipe [ executable; "-"; output ] in
  let stdin_w = Charamel_os.Process.stdin_w process in
  Lwt.bind (Lwt_io.write stdin_w svg) (fun () ->
      Lwt.bind (Lwt_io.close stdin_w) (fun () ->
          Lwt.bind (Charamel_os.Process.await process) (fun code ->
              if code = 0 then Lwt.return (Ok ())
              else if code > 128 then
                Lwt.return (Error (Fmt.str "resvg terminated by signal %d" (code - 128)))
              else Lwt.return (Error (Fmt.str "resvg exited with status %d" code)))))

let convert ~svg ~output =
  match find_executable "resvg" with
  | None -> Lwt.return (Error "resvg not found on PATH")
  | Some executable ->
      Lwt.catch
        (fun () -> run_resvg executable svg output)
        (function
          | Unix.Unix_error (error, function_name, argument) ->
              Lwt.return
                (Error
                   (Fmt.str "could not run resvg: %s (%s %s)" (Unix.error_message error)
                      function_name argument))
          | exn -> Lwt.fail exn)
