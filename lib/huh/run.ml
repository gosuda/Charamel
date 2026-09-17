type error = [ `Aborted | `Timeout ]
type model = { form : Form.t; timed_out : bool }
type msg = Form_msg of Form.msg | Timed_out

let pp_error ppf = function
  | `Aborted -> Fmt.string ppf "aborted"
  | `Timeout -> Fmt.string ppf "timed out"

let positive_timeout = function
  | Some seconds when seconds > 0. && not (Float.is_nan seconds) -> Some seconds
  | _ -> None

let timeout_command timeout =
  match positive_timeout timeout with
  | None -> Charamel_tea.Cmd.none
  | Some seconds -> Charamel_tea.Cmd.after seconds (fun () -> Timed_out)

let app env ?timeout form =
  let init () =
    let form, command = Form.init env form in
    let command = Charamel_tea.Cmd.map (fun message -> Form_msg message) command in
    let command =
      match Form.state form with
      | `Normal -> Charamel_tea.Cmd.batch [ command; timeout_command timeout ]
      | `Completed _ | `Aborted ->
          Charamel_tea.Cmd.batch [ command; Charamel_tea.Cmd.quit ]
    in
    ({ form; timed_out = false }, command)
  in
  let update message model =
    match message with
    | Timed_out -> (
        match Form.state model.form with
        | `Normal -> ({ model with timed_out = true }, Charamel_tea.Cmd.quit)
        | `Completed _ | `Aborted -> (model, Charamel_tea.Cmd.none))
    | Form_msg message ->
        let form, command = Form.update message model.form in
        let command = Charamel_tea.Cmd.map (fun value -> Form_msg value) command in
        let command =
          match Form.state form with
          | `Normal -> command
          | `Completed _ | `Aborted ->
              Charamel_tea.Cmd.batch [ command; Charamel_tea.Cmd.quit ]
        in
        ({ model with form }, command)
  in
  let view model = Charamel_tea.View.v ~alt_screen:false (Form.view model.form) in
  let subscriptions model =
    if model.timed_out then Charamel_tea.Sub.none
    else
      Charamel_tea.Sub.map
        (fun message -> Form_msg message)
        (Form.subscriptions model.form)
  in
  { Charamel_tea.init; update; view; subscriptions }

let is_tty base =
  let stdin_fd = Eio_unix.Resource.fd base#stdin in
  Eio_unix.Fd.use_exn "isatty" stdin_fd Unix.isatty

let term_is_dumb () = match Sys.getenv_opt "TERM" with Some "dumb" -> true | _ -> false

let default_env base =
  let editor = Form.Env.editor_of_string (Sys.getenv_opt "EDITOR") in
  let temp_dir_name =
    Option.value (Sys.getenv_opt "TMPDIR") ~default:(Filename.get_temp_dir_name ())
  in
  let temp_dir = Eio.Path.(base#fs / temp_dir_name) in
  Form.Env.v ~fs:base#cwd ~temp_dir ~editor

let echo_off_for base ~is_tty =
  if not is_tty then None
  else
    let fd = Eio_unix.Resource.fd base#stdin in
    Some
      (fun () ->
        let original = Eio_unix.Pty.Tc.getattr fd in
        let muted = { original with Unix.c_echo = false } in
        Eio_unix.Pty.Tc.setattr fd Unix.TCSAFLUSH muted;
        fun () -> Eio_unix.Pty.Tc.setattr fd Unix.TCSAFLUSH original)

let run_accessible ~clock ?timeout ~env ~is_tty ~base form =
  let output text = Eio.Flow.copy_string text base#stdout in
  let reader =
    Accessible.reader_of_flow ~stdin:base#stdin ~echo_off:(echo_off_for base ~is_tty)
  in
  let action () = Ok (Form.run_accessible env ~out:output reader form) in
  match positive_timeout timeout with
  | None -> action ()
  | Some seconds -> Eio.Time.with_timeout clock seconds action

let run ?timeout ?(accessible = false) ?env ~clock form base =
  let tty = is_tty base in
  let accessible = accessible || (not tty) || term_is_dumb () in
  let env = match env with Some value -> value | None -> default_env base in
  if accessible then run_accessible ~clock ?timeout ~env ~is_tty:tty ~base form
  else
    let terminal = Charamel_tea.Terminal.local ~output:`Stderr base in
    let application = app env ?timeout form in
    match Charamel_tea.run ~terminal ~clock application base with
    | Error `Interrupted | Error `Killed -> Error `Aborted
    | Error (`Exn (exception_, backtrace)) ->
        Printexc.raise_with_backtrace exception_ backtrace
    | Ok model -> (
        if model.timed_out then Error `Timeout
        else
          match Form.state model.form with
          | `Completed results -> Ok results
          | `Aborted | `Normal -> Error `Aborted)
