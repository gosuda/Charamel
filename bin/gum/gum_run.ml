type outcome = Submitted | Quit | Aborted

let run ?timeout env app ~finished =
  let terminal = Gum_io.ui_terminal env in
  let execute () = Charamel_tea.run ~terminal ~clock:env.Charamel_cli.Env.clock app in
  let timed =
    match timeout with
    | Some seconds when seconds > 0. ->
        Lwt.catch
          (fun () -> Lwt_unix.with_timeout seconds execute)
          (function
            | Lwt_unix.Timeout -> Charamel_cli.error ~code:124 "timed out"
            | exn -> Lwt.fail exn)
    | _ -> execute ()
  in
  Lwt.bind timed (function
    | Ok model -> (
        match finished model with
        | Submitted | Quit -> Lwt.return model
        | Aborted -> Charamel_cli.exit 130)
    | Error `Interrupted -> Charamel_cli.exit 130
    | Error (`Exn (exception_value, backtrace)) ->
        Printexc.raise_with_backtrace exception_value backtrace)

let run_tui ~name ?timeout env app ~finished =
  Lwt.catch
    (fun () -> run ?timeout env app ~finished)
    (function
      | Gum_io.No_tty -> Charamel_cli.error (Fmt.str "%s: requires a terminal" name)
      | exn -> Lwt.fail exn)
