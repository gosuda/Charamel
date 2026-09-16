type t = {
  input : Eio.Flow.source_ty Eio.Resource.t;
  output : Eio.Flow.sink_ty Eio.Resource.t;
  size : unit -> int * int;
  on_resize : (unit -> unit) Eio.Stream.t option;
  env : string -> string option;
  is_tty : bool;
  enter : unit -> unit;
  leave : unit -> unit;
}

let isatty fd =
  try Eio_unix.Fd.use_exn "isatty" fd Unix.isatty
  with Unix.Unix_error (_, _, _) | Invalid_argument _ -> false

let fd_opt resource =
  try Eio_unix.Resource.fd_opt resource with Invalid_argument _ -> None

let positive_env_int env name =
  match env name with
  | None -> None
  | Some raw -> (
      match int_of_string_opt (String.trim raw) with
      | Some n when n > 0 -> Some n
      | _ -> None)

let fallback_size env =
  let cols = Option.value (positive_env_int env "COLUMNS") ~default:80 in
  let rows = Option.value (positive_env_int env "LINES") ~default:24 in
  (rows, cols)

let query_size ~env fd =
  match fd with
  | None -> fallback_size env
  | Some fd -> (
      try
        let ws : Eio_unix.Pty.winsize = Eio_unix.Pty.get_window_size fd in
        if ws.Eio_unix.Pty.rows > 0 && ws.Eio_unix.Pty.cols > 0 then
          (ws.Eio_unix.Pty.rows, ws.Eio_unix.Pty.cols)
        else fallback_size env
      with Unix.Unix_error (_, _, _) | Invalid_argument _ -> fallback_size env)

(* POSIX cfmakeraw(3): input flags IGNBRK, BRKINT, PARMRK, ISTRIP, INLCR,
   IGNCR, ICRNL and IXON cleared; output flag OPOST cleared; local flags
   ECHO, ECHONL, ICANON and ISIG cleared (IEXTEN has no field in
   [Unix.terminal_io] and is left untouched); character size set to 8 bits
   with parity disabled; one byte satisfies a read with no timeout. *)
let raw_of (orig : Unix.terminal_io) : Unix.terminal_io =
  {
    orig with
    c_ignbrk = false;
    c_brkint = false;
    c_parmrk = false;
    c_istrip = false;
    c_inlcr = false;
    c_igncr = false;
    c_icrnl = false;
    c_ixon = false;
    c_opost = false;
    c_echo = false;
    c_echonl = false;
    c_icanon = false;
    c_isig = false;
    c_csize = 8;
    c_parenb = false;
    c_vmin = 1;
    c_vtime = 0;
  }

let local ?(output = `Stdout) (env : Eio_unix.Stdenv.base) =
  let stdin_flow = env#stdin in
  let output_flow = match output with `Stdout -> env#stdout | `Stderr -> env#stderr in
  let stdin_fd = fd_opt stdin_flow in
  let output_fd = fd_opt output_flow in
  let is_tty =
    match (stdin_fd, output_fd) with
    | Some stdin_fd, Some output_fd -> isatty stdin_fd && isatty output_fd
    | None, Some _ | Some _, None | None, None -> false
  in
  let getenv name = Sys.getenv_opt name in
  let saved = ref None in
  let enter () =
    match (is_tty, stdin_fd, !saved) with
    | true, Some stdin_fd, None ->
        let original = Eio_unix.Pty.Tc.getattr stdin_fd in
        saved := Some original;
        Eio_unix.Pty.Tc.setattr stdin_fd Unix.TCSAFLUSH (raw_of original)
    | _ -> ()
  in
  let leave () =
    if is_tty then
      Eio.Cancel.protect (fun () ->
          match (!saved, stdin_fd) with
          | Some original, Some stdin_fd ->
              Eio_unix.Pty.Tc.setattr stdin_fd Unix.TCSAFLUSH original;
              saved := None
          | None, Some _ | Some _, None | None, None -> ())
  in
  {
    input = (stdin_flow :> Eio.Flow.source_ty Eio.Resource.t);
    output = (output_flow :> Eio.Flow.sink_ty Eio.Resource.t);
    size = (fun () -> query_size ~env:getenv output_fd);
    on_resize = None;
    env = getenv;
    is_tty;
    enter;
    leave;
  }

let custom :
    input:_ Eio.Flow.source ->
    output:_ Eio.Flow.sink ->
    size:(unit -> int * int) ->
    on_resize:(unit -> unit) Eio.Stream.t option ->
    env:(string -> string option) ->
    is_tty:bool ->
    t =
 fun ~input ~output ~size ~on_resize ~env ~is_tty ->
  {
    input :> Eio.Flow.source_ty Eio.Resource.t;
    output :> Eio.Flow.sink_ty Eio.Resource.t;
    size;
    on_resize;
    env;
    is_tty;
    enter = (fun () -> ());
    leave = (fun () -> ());
  }
