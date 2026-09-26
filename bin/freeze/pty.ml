type error =
  [ `Invalid_command of string
  | `Spawn of string
  | `Exit of int * string
  | `Signaled of int * string
  | `Timeout of string ]

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

let terminal_size () =
  match Charamel_os.Tty.size_stdout () with
  | Some (rows, cols) -> (cols, rows)
  | None -> (80, 24)

let spawn_error message = Lwt.return (Error (`Spawn message))

let execute ~env ?width ?height ~timeout command =
  if String.trim command = "" then Lwt.return (Error (`Invalid_command "empty command"))
  else
    let detected_width, detected_height = terminal_size () in
    let width = max 1 (Option.value width ~default:detected_width) in
    let height = max 1 (Option.value height ~default:detected_height) in
    let output = Buffer.create 4096 in
    let terminal = ref None in
    let cleanup () =
      (match !terminal with None -> () | Some pty -> Charamel_os.Pty.close pty);
      Lwt.return_unit
    in
    let run_exec pty =
      Lwt.bind
        (Charamel_os.Pty.exec ~env:(with_term ~env width height) pty
           [ "/bin/sh"; "-c"; command ])
        (function
          | Error (`Error message) -> spawn_error message
          | Error `Unsupported -> spawn_error "PTY is unsupported on this platform"
          | Ok pid ->
              let rec drain () =
                Lwt.bind (Charamel_os.Pty.read pty 4096) (function
                  | Ok "" | Error _ -> Lwt.return_unit
                  | Ok chunk ->
                      Buffer.add_string output chunk;
                      drain ())
              in
              let status = ref None in
              let wait () =
                Lwt.bind (Lwt_unix.waitpid [] pid) (fun (_pid, value) ->
                    status := Some value;
                    Lwt.return_unit)
              in
              Lwt.bind
                (Lwt.join [ drain (); wait () ])
                (fun () ->
                  match !status with
                  | Some (Unix.WEXITED 0) -> Lwt.return (Ok (Buffer.contents output))
                  | Some (Unix.WEXITED code) ->
                      Lwt.return (Error (`Exit (code, Buffer.contents output)))
                  | Some (Unix.WSIGNALED signal) ->
                      Lwt.return (Error (`Signaled (signal, Buffer.contents output)))
                  | Some (Unix.WSTOPPED _) | None ->
                      Lwt.return (Ok (Buffer.contents output))))
    in
    let run_command () =
      Lwt.bind (Charamel_os.Pty.create ~rows:height ~cols:width ()) (function
        | Error (`Error message) -> spawn_error message
        | Error `Unsupported -> spawn_error "PTY is unsupported on this platform"
        | Ok pty ->
            terminal := Some pty;
            run_exec pty)
    in
    Lwt.finalize
      (fun () ->
        Lwt.catch
          (fun () -> Lwt_unix.with_timeout timeout run_command)
          (function
            | Lwt_unix.Timeout ->
                (match !terminal with
                | Some pty -> Charamel_os.Pty.terminate pty
                | None -> ());
                Lwt.return (Error (`Timeout (Buffer.contents output)))
            | Unix.Unix_error (error, function_name, argument) ->
                spawn_error
                  (Fmt.str "could not create PTY: %s (%s %s)" (Unix.error_message error)
                     function_name argument)
            | exn -> Lwt.fail exn))
      cleanup
