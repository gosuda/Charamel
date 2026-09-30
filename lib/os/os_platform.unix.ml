open Ctypes
open PosixTypes
open Unsigned
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

let stdin_fd = 0
let stdout_fd = 1
let stderr_fd = 2
let c_uname = Foreign.foreign "uname" (ptr char @-> returning int)

let c_ioctl =
  Foreign.foreign ~check_errno:true "ioctl" (int @-> ulong @-> ptr void @-> returning int)

(* ctypes cannot describe a variadic call, and on arm64 Darwin a variadic argument goes
   on the stack rather than in a register, so a fixed-arity [ioctl] binding hands the
   kernel a garbage pointer there. The POSIX.1-2024 [tcgetwinsize]/[tcsetwinsize] entry
   points are ordinary functions; they are preferred wherever the system exports them
   (glibc 2.39+, recent FreeBSD), with [ioctl] kept for the systems where the ABI lets
   this process issue one. macOS exports neither the new calls nor a callable [ioctl],
   so [window_size]/[set_window_size] fall back to a spawned [stty] there. *)
let c_tcgetwinsize =
  try
    Some
      (Foreign.foreign ~check_errno:true "tcgetwinsize"
         (int @-> ptr void @-> returning int))
  with Dl.DL_error _ -> None

let c_tcsetwinsize =
  try
    Some
      (Foreign.foreign ~check_errno:true "tcsetwinsize"
         (int @-> ptr void @-> returning int))
  with Dl.DL_error _ -> None

(* [~release_runtime_lock:true] gives up the OCaml runtime lock for the duration of the
   call; [~stub:true] on its own does not. {!Pty.read} and {!Pty.write} run these on
   [Lwt_preemptive] workers precisely because they block until the child speaks; holding
   the lock across that wait would freeze the scheduler, so a parent that must feed a
   child's stdin while draining its terminal would deadlock. Both calls touch only
   C-allocated buffers and never re-enter OCaml, which is what lock release requires. *)
let c_read =
  Foreign.foreign ~check_errno:true ~stub:true ~release_runtime_lock:true "read"
    (int @-> ptr char @-> size_t @-> returning ssize_t)

let c_write =
  Foreign.foreign ~check_errno:true ~stub:true ~release_runtime_lock:true "write"
    (int @-> ptr char @-> size_t @-> returning ssize_t)

let c_close = Foreign.foreign ~check_errno:true "close" (int @-> returning int)

(* [open] is variadic only for the mode argument that accompanies [O_CREAT]; opening an
   existing path needs no mode, so a two-argument binding is ABI-correct everywhere. *)
let c_open = Foreign.foreign ~check_errno:true "open" (string @-> int @-> returning int)

let c_posix_openpt =
  Foreign.foreign ~check_errno:true "posix_openpt" (int @-> returning int)

let c_grantpt = Foreign.foreign ~check_errno:true "grantpt" (int @-> returning int)
let c_unlockpt = Foreign.foreign ~check_errno:true "unlockpt" (int @-> returning int)
let c_ptsname = Foreign.foreign "ptsname" (int @-> returning string)
let c_ptsname_opt = Foreign.foreign "ptsname" (int @-> returning string_opt)
let c_ttyname = Foreign.foreign "ttyname" (int @-> returning string_opt)

let c_actions_init =
  Foreign.foreign "posix_spawn_file_actions_init" (ptr void @-> returning int)

let c_actions_addopen =
  Foreign.foreign "posix_spawn_file_actions_addopen"
    (ptr void @-> int @-> string @-> int @-> int @-> returning int)

let c_actions_adddup2 =
  Foreign.foreign "posix_spawn_file_actions_adddup2"
    (ptr void @-> int @-> int @-> returning int)

let c_actions_destroy =
  Foreign.foreign "posix_spawn_file_actions_destroy" (ptr void @-> returning int)

let c_actions_addchdir =
  try
    Some
      (Foreign.foreign "posix_spawn_file_actions_addchdir_np"
         (ptr void @-> string @-> returning int))
  with Dl.DL_error _ -> None

let c_attr_init = Foreign.foreign "posix_spawnattr_init" (ptr void @-> returning int)
let c_attr_destroy = Foreign.foreign "posix_spawnattr_destroy" (ptr void @-> returning int)

let c_attr_setflags =
  Foreign.foreign "posix_spawnattr_setflags" (ptr void @-> uint16_t @-> returning int)

let c_attr_setpgroup =
  Foreign.foreign "posix_spawnattr_setpgroup" (ptr void @-> int @-> returning int)

(* [posix_spawnp], not [posix_spawn]: a command with no directory separator is looked up in
   [PATH] by the system, which is the behaviour both {!Process} and {!Pty} document. *)
let c_spawn =
  Foreign.foreign "posix_spawnp"
    (ptr int @-> string @-> ptr void @-> ptr void
    @-> ptr (ptr char)
    @-> ptr (ptr char)
    @-> returning int)

let sysname =
  let buffer = allocate_n char ~count:512 in
  if c_uname buffer <> 0 then ""
  else
    let rec stop index =
      if index >= 256 || !@(buffer +@ index) = '\000' then index else stop (index + 1)
    in
    Bytes.to_string (Bytes.init (stop 0) (fun index -> !@(buffer +@ index)))

let is_bsd =
  match sysname with "Darwin" | "FreeBSD" | "OpenBSD" | "NetBSD" -> true | _ -> false

let o_wronly = 1
let o_rdonly = 0
let o_rdwr = 2
let o_noctty = if is_bsd then 0x20000 else 0o400
let o_cloexec = if is_bsd then 0x01000000 else 0o2000000
let is_macos = sysname = "Darwin"
let tiocgwinsz = if is_bsd then 0x40087468 else 0x5413
let tiocswinsz = if is_bsd then 0x80087467 else 0x5414
let spawn_setpgroup = 2

(* [POSIX_SPAWN_SETSID] is a vendor extension, not a POSIX flag: glibc and musl define it
   as 0x80, Apple's spawn.h as 0x400, and FreeBSD, OpenBSD and NetBSD do not implement it
   at all. It is asked for only by {!Pty.exec}, whose child must become a session leader so
   that opening the slave makes it the controlling terminal; [0] marks the platforms where
   that request cannot be honoured. *)
let spawn_setsid = if is_macos then 0x400 else if is_bsd then 0 else 0x80
let winsize_bytes = 8
let opaque_bytes = 1024
let default_rows = 24
let default_cols = 80

let le16 buffer index =
  Char.code !@(buffer +@ index) lor (Char.code !@(buffer +@ (index + 1)) lsl 8)

let put_le16 buffer index value =
  buffer +@ index <-@ Char.chr (value land 0xff);
  buffer +@ (index + 1) <-@ Char.chr ((value lsr 8) land 0xff)

let voidp buffer = coerce (ptr char) (ptr void) buffer

let c_string text =
  let length = String.length text in
  let buffer = allocate_n char ~count:(length + 1) in
  String.iteri (fun index character -> buffer +@ index <-@ character) text;
  buffer +@ length <-@ '\000';
  buffer

let string_array texts =
  let count = Array.length texts in
  let buffers = Array.init count (fun index -> c_string texts.(index)) in
  let array = allocate_n (ptr char) ~count:(count + 1) in
  Array.iteri (fun index buffer -> array +@ index <-@ buffer) buffers;
  array +@ count <-@ coerce (ptr void) (ptr char) null;
  (array, buffers)

let read_at fd buffer count =
  let rec attempt () =
    try Ssize.to_int (c_read fd buffer (Size_t.of_int count))
    with Unix.Unix_error (Unix.EINTR, _, _) -> attempt ()
  in
  attempt ()

let write_at fd buffer count =
  let rec attempt () =
    try Ssize.to_int (c_write fd buffer (Size_t.of_int count))
    with Unix.Unix_error (Unix.EINTR, _, _) -> attempt ()
  in
  attempt ()

let string_of_c_buffer buffer count =
  Bytes.to_string (Bytes.init count (fun index -> !@(buffer +@ index)))

let describe operation = function
  | Unix.Unix_error (code, _, _) ->
      Error (`Error (operation ^ ": " ^ Unix.error_message code))
  | Failure reason -> Error (`Error (operation ^ ": " ^ reason))
  | exn -> raise exn

let io_failure operation reason = Error (`Error (operation ^ ": " ^ reason))

let error_of_spawn code =
  match code with
  | 2 -> Unix.ENOENT
  | 7 -> Unix.E2BIG
  | 8 -> Unix.ENOEXEC
  | 12 -> Unix.ENOMEM
  | 13 -> Unix.EACCES
  | 20 -> Unix.ENOTDIR
  | value -> Unix.EUNKNOWNERR value

(* [posix_spawn_file_actions_t] and [posix_spawnattr_t] are opaque records whose size belongs
   to libc, so both get a buffer larger than any implementation needs and are touched only
   through the library entry points, and both are destroyed once the spawn has returned.
   Every spawn puts the child in a process group of its own so a caller can signal the whole
   tree; [additions] are the [addopen] actions that wire the child's standard descriptors. A
   refused action aborts the preparation rather than spawning with a stream left unwired. *)
let build_actions actions additions dups cwd =
  let opened =
    List.for_all
      (fun (number, path, access) ->
        c_actions_addopen actions number path access 0o600 = 0)
      additions
  in
  let dupped =
    List.for_all
      (fun (source, number) -> c_actions_adddup2 actions source number = 0)
      dups
  in
  if not (opened && dupped) then
    io_failure "file actions" "a descriptor could not be wired"
  else
    match (cwd, c_actions_addchdir) with
    | None, _ -> Ok ()
    | Some directory, Some addchdir ->
        if addchdir actions directory = 0 then Ok ()
        else io_failure "file actions" "the working directory could not be set"
    | Some _, None ->
        io_failure "file actions"
          "this system has no posix_spawn working-directory action"

let prepare ?cwd ?(owns_session = false) ?(dups = []) additions =
  if owns_session && spawn_setsid = 0 then
    io_failure "spawn attributes" "this system has no posix_spawn session flag"
  else
    (* A session-owning spawn asks for [SETSID] alone: [setsid] already makes the child a
       process-group leader, and glibc applies the session attribute before the group
       one, so a [SETPGROUP] would then ask a fresh session leader to change its group —
       which [setpgid] refuses. *)
    let flags = if owns_session then spawn_setsid else spawn_setpgroup in
    let actions = allocate_n char ~count:opaque_bytes in
    let attributes = allocate_n char ~count:opaque_bytes in
    let actions' = voidp actions in
    let attributes' = voidp attributes in
    ignore (c_actions_init actions');
    match build_actions actions' additions dups cwd with
    | Error _ as failure ->
        (* Only the actions object is destroyed here: the attributes have not been
         initialised, so destroying them would be undefined. *)
        ignore (c_actions_destroy actions');
        failure
    | Ok () ->
        ignore (c_attr_init attributes');
        if c_attr_setflags attributes' (UInt16.of_int flags) <> 0 then (
          ignore (c_actions_destroy actions');
          ignore (c_attr_destroy attributes');
          io_failure "spawn attributes"
            "the system refused the process-group or session request")
        else if c_attr_setpgroup attributes' 0 <> 0 then (
          ignore (c_actions_destroy actions');
          ignore (c_attr_destroy attributes');
          io_failure "spawn attributes" "the system refused the process-group number")
        else Ok (actions', attributes')

let run actions attributes program arguments env =
  let argv', argv_keep = string_array (Array.of_list (program :: arguments)) in
  let source = match env with None -> Unix.environment () | Some source -> source in
  let envp, env_keep = string_array source in
  let pid = allocate_n int ~count:1 in
  let code = c_spawn pid program actions attributes argv' envp in
  ignore (argv_keep, env_keep);
  ignore (c_actions_destroy actions);
  ignore (c_attr_destroy attributes);
  if code <> 0 then Error (error_of_spawn code) else Ok !@pid

(* A geometry spawn asks for no wiring at all, so it carries neither file actions nor
   spawn attributes: plain [posix_spawnp] is the whole mechanism. *)
let spawn_plain program arguments =
  let argv', argv_keep = string_array (Array.of_list (program :: arguments)) in
  let envp, env_keep = string_array (Unix.environment ()) in
  let pid = allocate_n int ~count:1 in
  let code = c_spawn pid program null null argv' envp in
  ignore (argv_keep, env_keep);
  if code <> 0 then Error (error_of_spawn code) else Ok !@pid

(* The terminal behind an arbitrary descriptor is named by [ttyname]; a pty master
   answers through [ptsname] when the slave is still unopened or when the master is
   not itself a tty. *)
let tty_path fd =
  match c_ttyname fd with Some path -> Some path | None -> c_ptsname_opt fd

let read_all name =
  let ic = open_in_bin name in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> really_input_string ic (in_channel_length ic))

(* Where the ABI makes the variadic [ioctl] uncallable — arm64 Darwin — and no
   [tc*getwinsize] exists, the last resort is [stty]. Wiring the descriptor to [stty]'s
   standard input would need the [dup2] file action, which the in-kernel spawner on
   macOS 26 reports as [ENOENT], so the terminal is reached by name: [stty -f] opens the
   device itself and performs its own ioctl, and the calling convention never reaches
   this process. When [capture] is set the answer goes to a file rather than a wired
   pipe; a failed spawn or a non-zero exit — which is also [stty]'s way of saying the
   path is no terminal — answers [None]. *)
let stty_winsize fd arguments capture =
  let awaited = function
    | _, Unix.WEXITED 0 -> true
    | _, (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _) -> false
  in
  match tty_path fd with
  | None -> None
  | Some path -> (
      if capture then
        let tmp = Filename.temp_file "charamel-stty" ".size" in
        Fun.protect
          ~finally:(fun () -> try Unix.unlink tmp with Unix.Unix_error _ -> ())
          (fun () ->
            let command =
              String.concat " " ("stty" :: "-f" :: Filename.quote path :: arguments)
              ^ " > " ^ Filename.quote tmp
            in
            match spawn_plain "/bin/sh" [ "-c"; command ] with
            | Error _ -> None
            | Ok pid ->
                if awaited (Unix.waitpid [] pid) then Some (read_all tmp) else None)
      else
        match spawn_plain "stty" ("-f" :: path :: arguments) with
        | Error _ -> None
        | Ok pid -> if awaited (Unix.waitpid [] pid) then Some "" else None)

(* [ioctl] reports through [errno], which the binding turns into [Unix_error]; "no size
   for this descriptor" is an ordinary answer — a pipe, a closed master, a device that
   is no terminal — so both directions swallow it rather than raising. macOS skips
   [ioctl] outright: its variadic argument cannot be marshalled on arm64, so the call
   would corrupt the heap or refuse, and a spawned [stty] stands in. *)
let window_size fd =
  match c_tcgetwinsize with
  | Some tcgetwinsize -> (
      let buffer = allocate_n char ~count:winsize_bytes in
      match tcgetwinsize fd (voidp buffer) with
      | exception Unix.Unix_error _ -> None
      | failure when failure <> 0 -> None
      | _ ->
          let rows = le16 buffer 0 in
          let cols = le16 buffer 2 in
          if rows = 0 || cols = 0 then None else Some (rows, cols))
  | None when is_macos -> (
      match stty_winsize fd [ "size" ] true with
      | Some report -> (
          match
            List.filter_map int_of_string_opt
              (String.split_on_char ' ' (String.trim report))
          with
          | [ rows; cols ] when rows > 0 && cols > 0 -> Some (rows, cols)
          | _ -> None)
      | None -> None)
  | None -> (
      let buffer = allocate_n char ~count:winsize_bytes in
      match c_ioctl fd (ULong.of_int tiocgwinsz) (voidp buffer) with
      | exception Unix.Unix_error _ -> None
      | failure when failure <> 0 -> None
      | _ ->
          let rows = le16 buffer 0 in
          let cols = le16 buffer 2 in
          if rows = 0 || cols = 0 then None else Some (rows, cols))

let set_window_size fd ~rows ~cols =
  match c_tcsetwinsize with
  | Some tcsetwinsize -> (
      let buffer = allocate_n char ~count:winsize_bytes in
      put_le16 buffer 0 rows;
      put_le16 buffer 2 cols;
      put_le16 buffer 4 0;
      put_le16 buffer 6 0;
      match tcsetwinsize fd (voidp buffer) with
      | exception Unix.Unix_error _ -> false
      | failure -> failure = 0)
  | None when is_macos ->
      stty_winsize fd [ "rows"; string_of_int rows; "cols"; string_of_int cols ] false
      <> None
  | None -> (
      let buffer = allocate_n char ~count:winsize_bytes in
      put_le16 buffer 0 rows;
      put_le16 buffer 2 cols;
      put_le16 buffer 4 0;
      put_le16 buffer 6 0;
      match c_ioctl fd (ULong.of_int tiocswinsz) (voidp buffer) with
      | exception Unix.Unix_error _ -> false
      | failure -> failure = 0)

module Tty = struct
  type saved = { fd : Unix.file_descr; terminal : Unix.terminal_io }

  let is_stdin = Unix.isatty Unix.stdin
  let is_stdout = Unix.isatty Unix.stdout
  let size_stdout () = if is_stdout then window_size stdout_fd else None
  let is_stderr = Unix.isatty Unix.stderr

  (* The selection is by role because a [Unix.file_descr] is abstract: the numbers
     below are the descriptors [Unix.stdout] and [Unix.stderr] always name. *)
  let size_of_output output =
    match output with
    | `Stdout -> if is_stdout then window_size stdout_fd else None
    | `Stderr -> if is_stderr then window_size stderr_fd else None

  (* POSIX cfmakeraw(3): clear the input mapping and flow-control flags, clear [OPOST],
     clear [ECHO], [ECHONL], [ICANON] and [ISIG], select eight data bits without parity, and
     let one byte satisfy a read. [IEXTEN] has no field in [Unix.terminal_io] and stays
     untouched, as in [lib/tea]. *)
  let raw_of (terminal : Unix.terminal_io) : Unix.terminal_io =
    {
      terminal with
      Unix.c_ignbrk = false;
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

  (* Darwin's [tcgetattr] answers ENODEV rather than the POSIX ENOTTY for a descriptor
     that is no terminal; normalising keeps the single error callers are told to
     expect. *)
  let tcgetattr fd =
    try Unix.tcgetattr fd
    with Unix.Unix_error (Unix.ENODEV, label, argument) ->
      raise (Unix.Unix_error (Unix.ENOTTY, label, argument))

  let enter_raw () =
    let terminal = tcgetattr Unix.stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW (raw_of terminal);
    { fd = Unix.stdin; terminal }

  let echo_off () =
    let terminal = tcgetattr Unix.stdin in
    Unix.tcsetattr Unix.stdin Unix.TCSANOW { terminal with Unix.c_echo = false };
    { fd = Unix.stdin; terminal }

  let restore { fd; terminal } = Unix.tcsetattr fd Unix.TCSADRAIN terminal

  let controlling_input () =
    let fd = Unix.openfile "/dev/tty" [ Unix.O_RDONLY ] 0 in
    Lwt_io.of_fd ~mode:Lwt_io.input (Lwt_unix.of_unix_file_descr fd)

  let watch_resizes callback =
    ignore (Lwt_unix.on_signal (Sys.signal_to_int Sys.sigwinch) (fun _ -> callback ()))

  let supports_suspend = true
end

module Console = struct
  (* Yield once rather than answering synchronously: a caller that polls this in a loop, as
     the Windows input fiber does, must not be able to starve the scheduler. *)
  let read_records () = Lwt.pause () >>= fun () -> Lwt.return_nil
end

module Pty = struct
  type t = { mutable master : int; slave : string; mutable child : int option }
  type error = [ `Error of string | `Unsupported ]

  let create ?(rows = default_rows) ?(cols = default_cols) () =
    if rows < 1 || cols < 1 then
      invalid_arg "Charamel_os.Pty.create: a terminal has at least one row and one column";
    let work () =
      (* The parent never opens the slave: a reader on the master blocks while the
         slave has never been opened, but answers EIO once it has been opened and
         closed — so keeping the child-facing open in the child is what lets an early
         drain wait for the child's first bytes instead of racing it to a hang-up. *)
      try
        let master = c_posix_openpt (o_rdwr lor o_noctty lor o_cloexec) in
        let refused reason =
          (* The master is ours the moment [posix_openpt] answers, so every later failure
             has to give it back or the descriptor leaks. *)
          (try ignore (c_close master) with Unix.Unix_error _ | Failure _ -> ());
          io_failure "pty" reason
        in
        if c_grantpt master <> 0 then refused "the slave could not be granted"
        else if c_unlockpt master <> 0 then refused "the slave could not be unlocked"
        else
          let slave = c_ptsname master in
          if slave = "" then refused "the slave has no name"
          else if not (set_window_size master ~rows ~cols) then
            refused "the size was rejected"
          else Ok { master; slave; child = None }
      with (Unix.Unix_error _ | Invalid_argument _ | Failure _) as exn ->
        describe "pty" exn
    in
    Lwt_preemptive.detach work ()

  let slave_path { slave; _ } = slave

  let exec ?cwd ?env pty argv =
    let work () =
      try
        match argv with
        | [] -> io_failure "spawn" "argv is empty"
        | program :: arguments -> (
            let additions =
              [
                (stdin_fd, pty.slave, o_rdwr);
                (stdout_fd, pty.slave, o_wronly);
                (stderr_fd, pty.slave, o_wronly);
              ]
            in
            match prepare ?cwd ~owns_session:true additions with
            | Error reason -> Error reason
            | Ok (actions, attributes) -> (
                match run actions attributes program arguments env with
                | Error code -> io_failure "spawn" (Unix.error_message code)
                | Ok child ->
                    pty.child <- Some child;
                    Ok child))
      with (Unix.Unix_error _ | Invalid_argument _ | Failure _) as exn ->
        describe "spawn" exn
    in
    Lwt_preemptive.detach work ()

  let read pty count =
    if count <= 0 then Lwt.return (Ok "")
    else
      let work () =
        let buffer = allocate_n char ~count in
        try Ok (string_of_c_buffer buffer (read_at pty.master buffer count)) with
        | Unix.Unix_error (Unix.EIO, _, _) -> Ok ""
        | Unix.Unix_error (Unix.EBADF, _, _) -> io_failure "read" "the master is closed"
        | (Unix.Unix_error _ | Failure _) as exn -> describe "read" exn
      in
      Lwt_preemptive.detach work ()

  let write pty text offset length =
    if offset < 0 || length < 0 || offset + length > String.length text then
      invalid_arg "Charamel_os.Pty.write: range outside the string"
    else if length = 0 then Lwt.return (Ok 0)
    else
      let work () =
        let buffer = c_string (String.sub text offset length) in
        try Ok (write_at pty.master buffer length) with
        | Unix.Unix_error (Unix.EIO, _, _) -> Ok 0
        | (Unix.Unix_error _ | Failure _) as exn -> describe "write" exn
      in
      Lwt_preemptive.detach work ()

  let size pty =
    match window_size pty.master with
    | Some (rows, cols) -> Ok (rows, cols)
    | None -> io_failure "size" "the master is closed"

  let resize pty ~rows ~cols =
    if set_window_size pty.master ~rows ~cols then Ok ()
    else io_failure "resize" "the size was rejected"

  let terminate { child; _ } =
    match child with
    | None -> ()
    | Some pid -> (
        try Unix.kill (0 - pid) Sys.sigkill
        with Unix.Unix_error (Unix.ESRCH, _, _) | Unix.Unix_error (Unix.EPERM, _, _) ->
          ())

  let close pty =
    if pty.master < 0 then ()
    else
      let master = pty.master in
      pty.master <- -1;
      try ignore (c_close master) with Unix.Unix_error _ | Failure _ -> ()
end

module Process = struct
  type redir = [ `Inherit | `Null | `Pipe ]

  type t = {
    pid : int;
    stdin_w : Lwt_io.output_channel option;
    stdout_r : Lwt_io.input_channel option;
    stderr_r : Lwt_io.input_channel option;
    code : int Lwt.t;
    signal : int option Lwt.t;
  }

  let counter = ref 0

  let pipe_path number =
    incr counter;
    Filename.concat (Dirs.temp_dir ())
      ("charamel-os-"
      ^ string_of_int (Unix.getpid ())
      ^ "-" ^ string_of_int !counter ^ "-" ^ string_of_int number)

  (* A [Pipe] becomes a named pipe the child opens by path, so a descriptor number never
     crosses the boundary between [Unix.file_descr] and a raw [int]: the parent holds real
     descriptors for the event loop, and the child receives [0], [1], [2] by [addopen]. The
     stdin end is opened [O_RDWR] so neither side blocks waiting for the other, and the
     output ends are opened read-only before the spawn, which is also immediate. *)
  let make_pipes kinds =
    List.filter_map
      (fun (number, kind) ->
        match kind with
        | `Pipe ->
            let path = pipe_path number in
            Unix.mkfifo path 0o600;
            Some (number, path)
        | `Inherit | `Null -> None)
      kinds

  let open_pipe (number, path) =
    (* [O_CLOEXEC] matters as much as the access mode: without it the child inherits this very
       descriptor along with its duplicated standard handles, and a leftover [O_RDWR] writer on
       the stdin pipe would keep the child from ever seeing end-of-file on its own stdin. *)
    let access = if number = stdin_fd then Unix.O_RDWR else Unix.O_RDONLY in
    let flags = [ access; Unix.O_NONBLOCK; Unix.O_CLOEXEC ] in
    (number, path, Unix.openfile path flags 0o600)

  (* The child reads its stdin: an [O_RDWR] open there would leave the child holding a writer
     on its own input pipe, so closing the parent's end would never deliver end-of-file. *)
  let child_access number = if number = stdin_fd then o_rdonly else o_wronly

  let null_numbers kinds =
    List.filter_map
      (fun (number, kind) ->
        match kind with `Null -> Some number | `Inherit | `Pipe -> None)
      kinds

  let remove paths =
    List.iter (fun path -> try Unix.unlink path with Unix.Unix_error _ -> ()) paths

  let close descriptors =
    List.iter (fun fd -> try Unix.close fd with Unix.Unix_error _ -> ()) descriptors

  let exit_code status =
    match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + Sys.signal_to_int signal
    | Unix.WSTOPPED signal -> 128 + Sys.signal_to_int signal

  let signal_of status =
    match status with
    | Unix.WSIGNALED signal -> Some (Sys.signal_to_int signal)
    | Unix.WEXITED _ | Unix.WSTOPPED _ -> None

  let get stream operation =
    match stream with
    | Some stream -> stream
    | None ->
        invalid_arg ("Charamel_os.Process." ^ operation ^ ": that stream was not a pipe")

  (* Every descriptor here is a pipe this module opened [O_NONBLOCK], so the blocking
     mode is already known. Leaving [~blocking] out would make Lwt [fstat] the descriptor
     as a job on the first I/O and, for anything it guesses to be blocking, run every
     later read and write on a worker thread — which starves against the [Lwt_preemptive]
     workers that {!Pty.read} holds while a child waits for its input. [~set_flags:false]
     skips the redundant [set_nonblock] because the flag is already set. *)
  let input_channel fd =
    Lwt_io.of_fd ~mode:Lwt_io.input
      (Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false fd)

  let output_channel fd =
    Lwt_io.of_fd ~mode:Lwt_io.output
      (Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:false fd)

  let start program arguments nulls handles cwd env paths =
    (* A [Null] stream is wired by [addopen] of [/dev/null] on macOS — where the
       [dup2] file action reports [ENOENT] — and by [dup2] of a parent-opened descriptor
       elsewhere: the descriptor is already bound when the spawn runs, so no in-kernel
       open can refuse it, and the duplicated child descriptor is non-cloexec as a
       standard stream must be. *)
    let null_fd =
      match (nulls, is_macos) with
      | [], _ | _ :: _, true -> None
      | _ :: _, false -> Some (c_open "/dev/null" (o_rdwr lor o_cloexec))
    in
    let dups =
      match null_fd with
      | Some fd -> List.map (fun number -> (fd, number)) nulls
      | None -> []
    in
    let additions =
      List.map (fun (number, path, _) -> (number, path, child_access number)) handles
      @ List.map
          (fun number -> (number, "/dev/null", child_access number))
          (if is_macos then nulls else [])
    in
    let abandon reason =
      close (List.map (fun (_, _, fd) -> fd) handles);
      (match null_fd with
      | Some fd -> ( try ignore (c_close fd) with Unix.Unix_error _ -> ())
      | None -> ());
      remove paths;
      raise reason
    in
    match prepare ?cwd ~dups additions with
    | Error (`Error reason) ->
        abandon (Unix.Unix_error (Unix.EINVAL, "posix_spawn_file_actions", reason))
    | Ok (actions, attributes) -> (
        match run actions attributes program arguments env with
        | Error code -> abandon (Unix.Unix_error (code, "posix_spawnp", program))
        | Ok child ->
            (match null_fd with
            | Some fd -> ( try ignore (c_close fd) with Unix.Unix_error _ -> ())
            | None -> ());
            let descriptor number =
              match
                List.assoc_opt number (List.map (fun (n, _, fd) -> (n, fd)) handles)
              with
              | Some fd -> Some fd
              | None -> None
            in
            let wait = Lwt_unix.wait4 [] child in
            {
              pid = child;
              stdin_w = Option.map output_channel (descriptor stdin_fd);
              stdout_r = Option.map input_channel (descriptor stdout_fd);
              stderr_r = Option.map input_channel (descriptor stderr_fd);
              code =
                Lwt.catch
                  (fun () ->
                    wait >|= fun (_, status, _) ->
                    remove paths;
                    exit_code status)
                  (fun exn ->
                    remove paths;
                    Lwt.fail exn);
              signal = (wait >|= fun (_, status, _) -> signal_of status);
            })

  let spawn ?cwd ?env ?(stdin = `Inherit) ?(stdout = `Inherit) ?(stderr = `Inherit) argv =
    match argv with
    | [] -> invalid_arg "Charamel_os.Process.spawn: argv is empty"
    | program :: arguments -> (
        let kinds = [ (stdin_fd, stdin); (stdout_fd, stdout); (stderr_fd, stderr) ] in
        let pipes = make_pipes kinds in
        try
          let handles = List.map open_pipe pipes in
          start program arguments (null_numbers kinds) handles cwd env
            (List.map snd pipes)
        with (Unix.Unix_error _ | Invalid_argument _ | Sys.Break) as exn ->
          remove (List.map snd pipes);
          raise exn)

  let pid { pid; _ } = pid
  let stdin_w { stdin_w; _ } = get stdin_w "stdin_w"
  let stdout_r { stdout_r; _ } = get stdout_r "stdout_r"
  let stderr_r { stderr_r; _ } = get stderr_r "stderr_r"
  let await { code; _ } = code
  let signal { signal; _ } = signal

  let terminate { pid; _ } =
    try Unix.kill pid Sys.sigterm with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

  let kill_tree { pid; _ } =
    try Unix.kill (0 - pid) Sys.sigkill
    with Unix.Unix_error _ -> (
      try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())

  let alive { code; _ } = Lwt.is_sleeping code
end
