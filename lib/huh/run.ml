open Lwt.Syntax

type error = [ `Aborted | `Timeout | `Timeout_unsupported ]
type model = { form : Form.t; timed_out : bool }
type msg = Form_msg of Form.msg | Timed_out

let pp_error ppf = function
  | `Aborted -> Fmt.string ppf "aborted"
  | `Timeout -> Fmt.string ppf "timed out"
  | `Timeout_unsupported -> Fmt.string ppf "timeout unsupported in accessible mode"

let positive_timeout = function
  | Some seconds when seconds > 0. && not (Float.is_nan seconds) -> Some seconds
  | _ -> None

let timeout_command timeout =
  match positive_timeout timeout with
  | None -> Charamel_tea.Cmd.none
  | Some seconds -> Charamel_tea.Cmd.after seconds (fun () -> Timed_out)

let finish form command =
  let extra =
    match Form.state form with
    | `Completed _ ->
        Option.map
          (Charamel_tea.Cmd.map (fun value -> Form_msg value))
          (Form.submit_cmd form)
    | `Aborted ->
        Option.map
          (Charamel_tea.Cmd.map (fun value -> Form_msg value))
          (Form.cancel_cmd form)
    | `Normal -> None
  in
  match extra with
  | Some extra ->
      Charamel_tea.Cmd.batch
        [ command; Charamel_tea.Cmd.seq [ extra; Charamel_tea.Cmd.quit ] ]
  | None -> Charamel_tea.Cmd.batch [ command; Charamel_tea.Cmd.quit ]

let app env ?timeout form =
  let init () =
    let form, command = Form.init env form in
    let command = Charamel_tea.Cmd.map (fun message -> Form_msg message) command in
    let command =
      match Form.state form with
      | `Normal -> Charamel_tea.Cmd.batch [ command; timeout_command timeout ]
      | `Completed _ | `Aborted -> finish form command
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
          | `Completed _ | `Aborted -> finish form command
        in
        ({ model with form }, command)
  in
  let view model =
    let frame =
      Charamel_tea.View.v ?cursor:(Form.cursor model.form) ~alt_screen:false
        ~report_focus:true (Form.view model.form)
    in
    match Form.view_hook model.form with Some hook -> hook frame | None -> frame
  in
  let subscriptions model =
    if model.timed_out then Charamel_tea.Sub.none
    else
      Charamel_tea.Sub.map
        (fun message -> Form_msg message)
        (Form.subscriptions model.form)
  in
  { Charamel_tea.init; update; view; subscriptions }

let is_tty = Charamel_os.Tty.is_tty_stdin
let term_is_dumb () = match Sys.getenv_opt "TERM" with Some "dumb" -> true | _ -> false

let default_env ~clock =
  let editor = Form.Env.editor_of_string (Sys.getenv_opt "EDITOR") in
  let temp_dir =
    Option.value (Sys.getenv_opt "TMPDIR") ~default:(Filename.get_temp_dir_name ())
  in
  Form.Env.v ~fs_root:(Sys.getcwd ()) ~temp_dir ~editor:(Some editor) ~clock

let echo_off ~is_tty = if is_tty then Some (Charamel_os.Tty.echo_off ()) else None

let run_accessible ?timeout ~env ~is_tty form =
  let output text = Lwt_io.write Lwt_io.stdout text in
  if Sys.win32 then
    (* [Lwt] polls readiness with [select], which Windows only supports on
       sockets — a blocking [stdin] reads straight through instead. *)
    Lwt_unix.set_blocking Lwt_unix.stdin true;
  let reader =
    Accessible.reader_of_channel ~stdin:Lwt_io.stdin ~echo_off:(echo_off ~is_tty)
  in
  let action () =
    let* results = Form.run_accessible env ~out:output reader form in
    Lwt.return (Ok results)
  in
  match positive_timeout timeout with
  | None -> action ()
  | Some seconds -> Lwt_unix.with_timeout seconds action

let run ?timeout ?(accessible = false) ?env ~clock form =
  let accessible = accessible || (not is_tty) || term_is_dumb () in
  match positive_timeout timeout with
  | Some _ when accessible -> Lwt.return_error `Timeout_unsupported
  | _ ->
      let env = match env with Some value -> value | None -> default_env ~clock in
      if accessible then run_accessible ~env ~is_tty form
      else
        let terminal = Charamel_tea.Terminal.local ~output:`Stderr () in
        let application = app env ?timeout form in
        let* result = Charamel_tea.run ~terminal ~clock application in
        Lwt.return
          (match result with
          | Error `Interrupted -> Error `Aborted
          | Error (`Exn (exception_, backtrace)) ->
              Printexc.raise_with_backtrace exception_ backtrace
          | Ok model -> (
              if model.timed_out then Error `Timeout
              else
                match Form.state model.form with
                | `Completed results -> Ok results
                | `Aborted | `Normal -> Error `Aborted))

let run_field ?timeout ?accessible ?env ~clock field =
  run ?timeout ?accessible ?env ~clock (Form.v ~show_help:false [ Group.v [ field ] ])
