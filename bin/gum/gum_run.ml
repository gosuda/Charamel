type outcome = Submitted | Quit | Aborted

let run ?timeout env app ~finished =
  Eio.Switch.run (fun sw ->
      let terminal = Gum_io.ui_terminal ~sw env in
      let execute () = Charm_tea.run ~terminal ~clock:env#clock app env in
      let result =
        match timeout with
        | Some seconds when seconds > 0. -> (
            try Eio.Time.with_timeout_exn env#clock seconds execute
            with Eio.Time.Timeout -> Charm_cli.error ~code:124 "timed out")
        | _ -> execute ()
      in
      match result with
      | Ok model -> (
          match finished model with
          | Submitted | Quit -> model
          | Aborted -> Charm_cli.exit 130)
      | Error `Interrupted -> Charm_cli.exit 130
      | Error `Killed -> Charm_cli.exit 124
      | Error (`Exn (exception_value, backtrace)) ->
          Printexc.raise_with_backtrace exception_value backtrace)
