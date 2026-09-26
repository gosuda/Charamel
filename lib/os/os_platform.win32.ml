open Lwt.Infix

type key_event = {
  down : bool;
  repeat : int;
  virtual_key : int;
  wide_char : int;
  control_key_state : int;
}

type input_record =
  | Key_event of key_event
  | Buffer_size of { rows : int; cols : int }
  | Focus of bool
  | Ignored

let is_macos = false
let default_rows = 24
let default_cols = 80
let utf8_code_page = 65001
let enable_processed_input = 0x0001
let enable_line_input = 0x0002
let enable_echo_input = 0x0004
let enable_virtual_terminal_input = 0x0200
let enable_virtual_terminal_processing = 0x0004
let disable_newline_auto_return = 0x0008
let resize_poll_seconds = 0.25
let input_poll_seconds = 0.01
let input = Charamel_os_win32.std_input ()
let output = Charamel_os_win32.std_output ()
let not_a_console operation = raise (Unix.Unix_error (Unix.ENOTTY, operation, "CON"))

module Tty = struct
  type saved = {
    input : Charamel_os_win32.handle;
    output : Charamel_os_win32.handle;
    input_mode : int;
    output_mode : int;
    input_cp : int;
    output_cp : int;
  }

  let is_stdin = Charamel_os_win32.get_console_mode input <> None
  let is_stdout = Charamel_os_win32.get_console_mode output <> None
  let size_stdout () = Charamel_os_win32.console_screen_size output

  (* The standard handles are read by role: a [Unix.file_descr] is a C runtime
     descriptor here, and an arbitrary one has no [HANDLE] to query. *)
  let size_of_output = function
    | `Stdout -> Charamel_os_win32.console_screen_size output
    | `Stderr -> Charamel_os_win32.console_screen_size (Charamel_os_win32.std_error ())

  let mode handle =
    match Charamel_os_win32.get_console_mode handle with
    | Some mode -> mode
    | None -> not_a_console "GetConsoleMode"

  let saved_state () =
    {
      input;
      output;
      input_mode = mode input;
      output_mode = mode output;
      input_cp = Charamel_os_win32.get_console_cp ();
      output_cp = Charamel_os_win32.get_console_output_cp ();
    }

  (* Raw mode hands key processing to the console's virtual-terminal input translator and
     stops the system from echoing, buffering lines or intercepting [CTRL+C]; output gains
     virtual-terminal processing without the automatic newline insertion that would double
     every line the renderer writes. Both code pages become UTF-8 so the renderer's
     eight-bit output and the input records agree on the encoding. *)
  let enter_raw () =
    let state = saved_state () in
    let clear = enable_echo_input lor enable_line_input lor enable_processed_input in
    let input_flags =
      state.input_mode land lnot clear lor enable_virtual_terminal_input
    in
    let output_flags =
      state.output_mode lor enable_virtual_terminal_processing
      lor disable_newline_auto_return
    in
    if not (Charamel_os_win32.set_console_mode input input_flags) then
      not_a_console "SetConsoleMode";
    if not (Charamel_os_win32.set_console_mode output output_flags) then
      not_a_console "SetConsoleMode";
    ignore (Charamel_os_win32.set_console_cp utf8_code_page);
    ignore (Charamel_os_win32.set_console_output_cp utf8_code_page);
    state

  let echo_off () =
    let state = saved_state () in
    let input_flags = state.input_mode land lnot enable_echo_input in
    if not (Charamel_os_win32.set_console_mode input input_flags) then
      not_a_console "SetConsoleMode";
    state

  let restore { input; output; input_mode; output_mode; input_cp; output_cp } =
    ignore (Charamel_os_win32.set_console_mode input input_mode);
    ignore (Charamel_os_win32.set_console_mode output output_mode);
    ignore (Charamel_os_win32.set_console_cp input_cp);
    ignore (Charamel_os_win32.set_console_output_cp output_cp)

  let controlling_input () =
    let fd = Unix.openfile "CONIN$" [ Unix.O_RDONLY ] 0 in
    Lwt_io.of_fd ~mode:Lwt_io.input (Lwt_unix.of_unix_file_descr fd)

  let watch_resizes callback =
    let rec loop previous =
      Lwt_unix.sleep resize_poll_seconds >>= fun () ->
      let current = Charamel_os_win32.console_screen_size output in
      if current <> previous then callback ();
      loop current
    in
    Lwt.async (fun () -> loop (Charamel_os_win32.console_screen_size output))

  let supports_suspend = false
end

let convert event =
  match event with
  | Charamel_os_win32.Key_event
      { down; repeat; virtual_key; wide_char; control_key_state; _ } ->
      Key_event { down; repeat; virtual_key; wide_char; control_key_state }
  | Charamel_os_win32.Buffer_resize { rows; cols } -> Buffer_size { rows; cols }
  | Charamel_os_win32.Focus_event { focused } -> Focus focused
  | Charamel_os_win32.Other_event -> Ignored

module Console = struct
  let poll () =
    let count = Charamel_os_win32.count_input_events input in
    if count = 0 then [] else Charamel_os_win32.read_console_input input ~max_events:count

  let read_records () =
    let rec attempt () =
      Lwt_preemptive.detach (fun () -> List.map convert (poll ())) () >>= function
      | [] -> Lwt_unix.sleep input_poll_seconds >>= attempt
      | events -> Lwt.return events
    in
    attempt ()
end

(* Windows has no pseudo-terminal that can be attached to a child's standard descriptors:
   [CreatePseudoConsole] produces a handle that only a ConPTY relay understands, so every
   entry point answers [`Unsupported] and callers keep the documented Windows behaviour of
   running child programs without a terminal. *)
module Pty = struct
  type t = unit
  type error = [ `Error of string | `Unsupported ]

  let unsupported () = Lwt.return (Error `Unsupported)

  let create ?(rows = default_rows) ?(cols = default_cols) () =
    if rows < 1 || cols < 1 then
      invalid_arg "Charamel_os.Pty.create: a terminal has at least one row and one column";
    unsupported ()

  let slave_path _ = invalid_arg "Charamel_os.Pty: Windows has no pseudo-terminals"

  let exec ?cwd ?env _ argv =
    ignore (cwd, env, argv);
    unsupported ()

  let read _ count =
    ignore count;
    unsupported ()

  let write _ text offset length =
    ignore (text, offset, length);
    unsupported ()

  let size _ = Error `Unsupported

  let resize _ ~rows ~cols =
    ignore (rows, cols);
    Error `Unsupported

  let terminate _ = ()
  let close _ = ()
end

module Process = struct
  type redir = [ `Inherit | `Null | `Pipe ]

  type t = {
    process : Lwt_process.process_none;
    stdin_w : Lwt_io.output_channel option;
    stdout_r : Lwt_io.input_channel option;
    stderr_r : Lwt_io.input_channel option;
    code : int Lwt.t;
  }

  let exit_code status =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + Sys.signal_to_int signal
    | Unix.WSTOPPED signal -> 128 + Sys.signal_to_int signal

  (* [Lwt_process] handles the Windows process and thread handles, the [NUL] device and the
     inheritable-versus-close-on-exec decision for the pipe ends it creates, which is why
     this variant defers to it instead of binding [CreateProcess] directly. *)
  let input_redir = function
    | `Pipe ->
        let child, parent = Lwt_unix.pipe_out ~cloexec:true () in
        (`FD_move child, Some (Lwt_io.of_fd ~mode:Lwt_io.output parent))
    | `Inherit -> (`Keep, None)
    | `Null -> (`Dev_null, None)

  let output_redir = function
    | `Pipe ->
        let parent, child = Lwt_unix.pipe_in ~cloexec:true () in
        (`FD_move child, Some (Lwt_io.of_fd ~mode:Lwt_io.input parent))
    | `Inherit -> (`Keep, None)
    | `Null -> (`Dev_null, None)

  let get stream operation =
    match stream with
    | Some stream -> stream
    | None ->
        invalid_arg ("Charamel_os.Process." ^ operation ^ ": that stream was not a pipe")

  let spawn ?cwd ?env ?(stdin = `Inherit) ?(stdout = `Inherit) ?(stderr = `Inherit) argv =
    match argv with
    | [] -> invalid_arg "Charamel_os.Process.spawn: argv is empty"
    | _ ->
        let stdin', stdin_w = input_redir stdin in
        let stdout', stdout_r = output_redir stdout in
        let stderr', stderr_r = output_redir stderr in
        let process =
          new Lwt_process.process_none
            ("", Array.of_list argv)
            ?cwd ?env ~stdin:stdin' ~stdout:stdout' ~stderr:stderr'
        in
        { process; stdin_w; stdout_r; stderr_r; code = Lwt.map exit_code process#status }

  let pid { process; _ } = process#pid
  let stdin_w { stdin_w; _ } = get stdin_w "stdin_w"
  let stdout_r { stdout_r; _ } = get stdout_r "stdout_r"
  let stderr_r { stderr_r; _ } = get stderr_r "stderr_r"
  let await { code; _ } = code
  let terminate { process; _ } = process#terminate
  let kill_tree { process; _ } = process#terminate
  let alive { process; _ } = process#state = Lwt_process.Running
end
