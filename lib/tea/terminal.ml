type t = {
  input : Charamel_os.Console_input.console_input;
  output : Lwt_io.output_channel;
  size : unit -> int * int;
  on_resize : (unit -> unit) Lwt_stream.t option;
  env : string -> string option;
  is_tty : bool;
  enter : unit -> unit;
  leave : unit -> unit;
}

let positive_env_int env name =
  Option.bind (env name) (fun raw ->
      match int_of_string_opt (String.trim raw) with
      | Some n when n > 0 -> Some n
      | _ -> None)

let fallback_size env =
  let cols = Option.value (positive_env_int env "COLUMNS") ~default:80 in
  let rows = Option.value (positive_env_int env "LINES") ~default:24 in
  (rows, cols)

(* The size comes from the device frames are actually written to: a tool that renders to
   stderr while stdout carries data ([gum ... > out]) must not paint at the fallback
   size. The environment and 80x24 chain below stays the fallback for both roles. *)
let size ~env ~output () =
  match Charamel_os.Tty.size_of_output output with
  | Some (rows, cols) when rows > 0 && cols > 0 -> (rows, cols)
  | Some _ | None -> fallback_size env

let is_tty_stderr = try Unix.isatty Unix.stderr with Unix.Unix_error (_, _, _) -> false

let local ?(output = `Stdout) () =
  let output_channel =
    match output with `Stdout -> Lwt_io.stdout | `Stderr -> Lwt_io.stderr
  in
  let output_is_tty =
    match (output : [ `Stdout | `Stderr ]) with
    | `Stdout -> Charamel_os.Tty.is_tty_stdout
    | `Stderr -> is_tty_stderr
  in
  let is_tty = Charamel_os.Tty.is_tty_stdin && output_is_tty in
  let input =
    if Sys.win32 then Charamel_os.Console_input.of_console_records ()
    else Charamel_os.Console_input.of_channel Lwt_io.stdin
  in
  let getenv name = Sys.getenv_opt name in
  let restore = ref None in
  let enter () = if is_tty then restore := Some (Charamel_os.Tty.enter_raw ()) in
  let leave () =
    match !restore with
    | Some restore_mode ->
        restore_mode ();
        restore := None
    | None -> ()
  in
  {
    input;
    output = output_channel;
    size = size ~env:getenv ~output;
    on_resize = None;
    env = getenv;
    is_tty;
    enter;
    leave;
  }

let custom :
    input:Charamel_os.Console_input.console_input ->
    output:Lwt_io.output_channel ->
    size:(unit -> int * int) ->
    on_resize:(unit -> unit) Lwt_stream.t option ->
    env:(string -> string option) ->
    is_tty:bool ->
    t =
 fun ~input ~output ~size ~on_resize ~env ~is_tty ->
  {
    input;
    output;
    size;
    on_resize;
    env;
    is_tty;
    enter = (fun () -> ());
    leave = (fun () -> ());
  }
