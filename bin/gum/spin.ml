type options = {
  command : string list;
  show_output : bool;
  show_error : bool;
  show_stdout : bool;
  show_stderr : bool;
  spinner : string;
  title : string;
  align : string;
  timeout : float option;
  padding : string;
  spinner_style : Gum_style.t;
  title_style : Gum_style.t;
}

type child_result = {
  status : int;
  stdout : string;
  stderr : string;
  output : string;
  timed_out : bool;
}

let style ?foreground () = Gum_style.defaults ?foreground ()

let default_options =
  {
    command = [];
    show_output = false;
    show_error = false;
    show_stdout = false;
    show_stderr = false;
    spinner = "dot";
    title = "Loading...";
    align = "left";
    timeout = None;
    padding = "0 0";
    spinner_style = style ~foreground:"212" ();
    title_style = style ();
  }

let child_abort : (unit -> unit) ref = ref (fun () -> ())

type capture_file = { path : string; fd : Unix.file_descr }
type captures = { stdout : capture_file; stderr : capture_file; output : capture_file }

let run_system ~label f = Eio_unix.run_in_systhread ~label f

let create_capture suffix =
  let path =
    run_system ~label:"gum spin capture create" (fun () ->
        Filename.temp_file "gum-spin-" suffix)
  in
  try
    let fd =
      run_system ~label:"gum spin capture open" (fun () ->
          Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CLOEXEC ] 0o600)
    in
    { path; fd }
  with exn ->
    Eio.Cancel.protect (fun () ->
        try run_system ~label:"gum spin capture cleanup" (fun () -> Unix.unlink path)
        with Unix.Unix_error _ -> ());
    raise exn

let close_capture capture =
  try run_system ~label:"gum spin capture close" (fun () -> Unix.close capture.fd)
  with Unix.Unix_error _ -> ()

let remove_capture capture =
  close_capture capture;
  try run_system ~label:"gum spin capture cleanup" (fun () -> Unix.unlink capture.path)
  with Unix.Unix_error _ -> ()

let create_captures () =
  let created = ref [] in
  let make suffix =
    let capture = create_capture suffix in
    created := capture :: !created;
    capture
  in
  try
    let stdout = make ".stdout" in
    let stderr = make ".stderr" in
    let output = make ".output" in
    { stdout; stderr; output }
  with exn ->
    Eio.Cancel.protect (fun () -> List.iter remove_capture !created);
    raise exn

let close_captures captures =
  List.iter close_capture [ captures.stdout; captures.stderr; captures.output ]

let remove_captures captures =
  Eio.Cancel.protect (fun () ->
      List.iter remove_capture [ captures.stdout; captures.stderr; captures.output ])

let write_capture capture text =
  if text <> "" then
    run_system ~label:"gum spin capture write" (fun () ->
        let length = String.length text in
        let rec loop offset =
          if offset < length then
            try
              let written =
                Unix.single_write_substring capture.fd text offset (length - offset)
              in
              if written <= 0 then raise End_of_file else loop (offset + written)
            with Unix.Unix_error (Unix.EINTR, _, _) -> loop offset
        in
        loop 0)

let read_capture capture =
  run_system ~label:"gum spin capture read" (fun () ->
      let fd = Unix.openfile capture.path [ Unix.O_RDONLY; Unix.O_CLOEXEC ] 0 in
      Fun.protect
        ~finally:(fun () -> try Unix.close fd with Unix.Unix_error _ -> ())
        (fun () ->
          let bytes = Bytes.create 65536 in
          let output = Buffer.create 65536 in
          let rec loop () =
            match Unix.read fd bytes 0 (Bytes.length bytes) with
            | 0 -> Buffer.contents output
            | count ->
                Buffer.add_subbytes output bytes 0 count;
                loop ()
          in
          loop ()))

let status_code = function `Exited code -> max 0 code | `Signaled _ -> 1

let process_error_message = function
  | Eio.Process.Executable_not_found path -> Fmt.str "executable not found: %s" path
  | Eio.Process.Child_error status ->
      Fmt.str "child error: %a" Eio.Process.pp_status status
  | Eio.Process.Argument_list_too_long -> "argument list too long"
  | Eio.Process.Permission_denied path -> Fmt.str "permission denied: %s" path
  | Eio.Process.Executable_format_error path -> Fmt.str "executable format error: %s" path

let wait_with_timeout env ~timeout process =
  match timeout with
  | None -> `Status (Eio.Process.await process)
  | Some seconds ->
      Eio.Fiber.first
        (fun () -> `Status (Eio.Process.await process))
        (fun () ->
          Eio.Time.sleep (Eio.Stdenv.clock env) seconds;
          `Timed_out)

let finish_captured captures ~status ~timed_out =
  close_captures captures;
  let stdout = read_capture captures.stdout in
  let stderr = read_capture captures.stderr in
  let output = read_capture captures.output in
  Ok
    {
      status = (if timed_out then 124 else status_code status);
      stdout;
      stderr;
      output;
      timed_out;
    }

(* A Unix PTY master reports EIO after its slave closes. Every successful read
   is committed to both capture files before this terminal EOF is accepted. *)
let drain source ~destination ~combined ~mutex ~pty =
  let chunk = Cstruct.create 65536 in
  let rec loop () =
    let count = Eio.Flow.single_read source chunk in
    let text = Cstruct.to_string (Cstruct.sub chunk 0 count) in
    Eio.Mutex.use_rw ~protect:true mutex (fun () ->
        write_capture destination text;
        write_capture combined text);
    loop ()
  in
  try loop () with
  | End_of_file -> ()
  | Eio.Io (Eio.Exn.X (Eio_unix.Unix_error (Unix.EIO, _, _)), _) when pty -> ()

let run_direct_child env ~command ~timeout =
  let previous_abort = !child_abort in
  Fun.protect
    ~finally:(fun () -> child_abort := previous_abort)
    (fun () ->
      try
        Eio.Switch.run @@ fun sw ->
        let process =
          Eio.Process.spawn ~sw (Eio.Stdenv.process_mgr env) ~stdin:env#stdin
            ~stdout:env#stdout ~stderr:env#stderr command
        in
        (child_abort := fun () -> Eio.Process.signal process Sys.sigint);
        let timed_out = ref false in
        let status =
          match wait_with_timeout env ~timeout process with
          | `Status status -> status
          | `Timed_out ->
              timed_out := true;
              Eio.Process.signal process Sys.sigterm;
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
              Eio.Process.signal process Sys.sigkill;
              Eio.Process.await process
        in
        Ok
          {
            status = (if !timed_out then 124 else status_code status);
            stdout = "";
            stderr = "";
            output = "";
            timed_out = !timed_out;
          }
      with
      | Eio.Io (Eio.Process.E error, _) ->
          Error (Fmt.str "unable to run action: %s" (process_error_message error))
      | Eio.Io _ -> Error "unable to run action: process I/O"
      | Unix.Unix_error (error, _, _) ->
          Error (Fmt.str "unable to run action: %s" (Unix.error_message error)))

let run_pty_child env ~command ~timeout =
  let previous_abort = !child_abort in
  Fun.protect
    ~finally:(fun () -> child_abort := previous_abort)
    (fun () ->
      try
        let captures = create_captures () in
        Fun.protect
          ~finally:(fun () -> remove_captures captures)
          (fun () ->
            Eio.Switch.run @@ fun sw ->
            let stdout_pty = Eio_unix.Pty.open_pty ~sw () in
            let stderr_pty = Eio_unix.Pty.open_pty ~sw () in
            let window_size =
              try Eio_unix.Pty.get_window_size (Eio_unix.Resource.fd env#stdout)
              with Unix.Unix_error _ ->
                { Eio_unix.Pty.rows = 24; cols = 80; xpixel = 0; ypixel = 0 }
            in
            Eio_unix.Pty.set_window_size (Eio_unix.Pty.pty stdout_pty) window_size;
            Eio_unix.Pty.set_window_size (Eio_unix.Pty.pty stderr_pty) window_size;
            let stdin_fd = Eio_unix.Resource.fd env#stdin in
            let command =
              [ "/bin/sh"; "-c"; "exec 2>&3 3>&- 0<&4 4<&-; exec \"$@\""; "gum-spin" ]
              @ command
            in
            let process =
              Eio_unix.Process.spawn_unix ~sw (Eio.Stdenv.process_mgr env)
                ~login_tty:(Eio_unix.Pty.tty stdout_pty)
                ~fds:
                  [
                    (3, Eio_unix.Pty.tty stderr_pty, `Blocking);
                    (4, stdin_fd, `Preserve_blocking);
                  ]
                command
            in
            Eio_unix.Fd.close (Eio_unix.Pty.tty stdout_pty);
            Eio_unix.Fd.close (Eio_unix.Pty.tty stderr_pty);
            (child_abort := fun () -> Eio.Process.signal process Sys.sigint);
            let mutex = Eio.Mutex.create () in
            let status = ref None in
            let timed_out = ref false in
            Eio.Fiber.all
              [
                (fun () ->
                  drain
                    (Eio_unix.Pty.source stdout_pty)
                    ~destination:captures.stdout ~combined:captures.output ~mutex
                    ~pty:true);
                (fun () ->
                  drain
                    (Eio_unix.Pty.source stderr_pty)
                    ~destination:captures.stderr ~combined:captures.output ~mutex
                    ~pty:true);
                (fun () ->
                  match wait_with_timeout env ~timeout process with
                  | `Status value -> status := Some value
                  | `Timed_out ->
                      timed_out := true;
                      Eio.Process.signal process Sys.sigterm;
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
                      Eio.Process.signal process Sys.sigkill;
                      status := Some (Eio.Process.await process));
              ];
            let status = Option.value !status ~default:(`Exited 1) in
            finish_captured captures ~status ~timed_out:!timed_out)
      with
      | Eio.Io (Eio.Process.E error, _) ->
          Error (Fmt.str "unable to run action: %s" (process_error_message error))
      | Eio.Io _ -> Error "unable to run action: process I/O"
      | Unix.Unix_error (error, _, _) ->
          Error (Fmt.str "unable to run action: %s" (Unix.error_message error)))

let run_pipe_child env ~command ~timeout =
  let previous_abort = !child_abort in
  Fun.protect
    ~finally:(fun () -> child_abort := previous_abort)
    (fun () ->
      try
        let captures = create_captures () in
        Fun.protect
          ~finally:(fun () -> remove_captures captures)
          (fun () ->
            Eio.Switch.run @@ fun sw ->
            let process_mgr = Eio.Stdenv.process_mgr env in
            let stdout_source, stdout_sink = Eio.Process.pipe ~sw process_mgr in
            let stderr_source, stderr_sink = Eio.Process.pipe ~sw process_mgr in
            let process =
              Eio.Process.spawn ~sw process_mgr ~stdin:env#stdin ~stdout:stdout_sink
                ~stderr:stderr_sink command
            in
            Eio.Flow.close stdout_sink;
            Eio.Flow.close stderr_sink;
            (child_abort := fun () -> Eio.Process.signal process Sys.sigint);
            let mutex = Eio.Mutex.create () in
            let status = ref None in
            let timed_out = ref false in
            Eio.Fiber.all
              [
                (fun () ->
                  drain stdout_source ~destination:captures.stdout
                    ~combined:captures.output ~mutex ~pty:false);
                (fun () ->
                  drain stderr_source ~destination:captures.stderr
                    ~combined:captures.output ~mutex ~pty:false);
                (fun () ->
                  match wait_with_timeout env ~timeout process with
                  | `Status value -> status := Some value
                  | `Timed_out ->
                      timed_out := true;
                      Eio.Process.signal process Sys.sigterm;
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
                      Eio.Process.signal process Sys.sigkill;
                      status := Some (Eio.Process.await process));
              ];
            let status = Option.value !status ~default:(`Exited 1) in
            finish_captured captures ~status ~timed_out:!timed_out)
      with
      | Eio.Io (Eio.Process.E error, _) ->
          Error (Fmt.str "unable to run action: %s" (process_error_message error))
      | Eio.Io _ -> Error "unable to run action: process I/O"
      | Unix.Unix_error (error, _, _) ->
          Error (Fmt.str "unable to run action: %s" (Unix.error_message error)))

let run_child ?(capture = true) env ~command ~timeout =
  match command with
  | [] -> Error "empty command"
  | _ when not capture -> run_direct_child env ~command ~timeout
  | _ when Gum_io.stdout_is_tty env -> run_pty_child env ~command ~timeout
  | _ -> run_pipe_child env ~command ~timeout

let write flow text = if text <> "" then Eio.Flow.copy_string text flow

let route_output (env : Eio_unix.Stdenv.base) (options : options) (result : child_result)
    =
  let stdout_tty = Gum_io.stdout_is_tty env in
  let explicit =
    options.show_output || options.show_error || options.show_stdout
    || options.show_stderr
  in
  if result.status = 0 then
    begin if options.show_output || (options.show_stdout && options.show_stderr) then
      write env#stdout result.output
    else if options.show_stdout then write env#stdout result.stdout
    else if options.show_stderr then write env#stdout result.stderr
    else if (not explicit) && not stdout_tty then begin
      write env#stdout result.stdout;
      write env#stderr result.stderr
    end
    end
  else if options.show_error then write env#stdout result.output
  else if (not explicit) && not stdout_tty then begin
    write env#stdout result.stdout;
    write env#stderr result.stderr
  end

type msg =
  | Tick of Charm_bubbles.Spinner.msg
  | Finished of (child_result, string) result
  | Key of Charm_tea.Key.t

type model = {
  spinner : Charm_bubbles.Spinner.t;
  result : (child_result, string) result option;
  title : string;
  align : string;
}

let key_name key = Charm_tea.Key.to_string key

let spinner_kind value =
  Option.value
    (Charm_bubbles.Spinner.kind_of_string value)
    ~default:Charm_bubbles.Spinner.Dot

let make_app (env : Eio_unix.Stdenv.base) (options : options) padding ~capture =
  let spinner =
    Charm_bubbles.Spinner.v
      ~kind:(spinner_kind options.spinner)
      ~style:(Gum_style.to_style options.spinner_style)
      ()
  in
  let title =
    Gum_style.to_style options.title_style |> fun style ->
    Charm_lipgloss.Style.render style options.title
  in
  let initial = { spinner; result = None; title; align = options.align } in
  let start =
    Charm_tea.Cmd.perform (fun () ->
        Finished
          (run_child ~capture env ~command:options.command ~timeout:options.timeout))
  in
  let update message model =
    match message with
    | Finished result -> ({ model with result = Some result }, Charm_tea.Cmd.quit)
    | Key key when String.equal (key_name key) "ctrl+c" ->
        !child_abort ();
        (model, Charm_tea.Cmd.interrupt)
    | Key _ -> (model, Charm_tea.Cmd.none)
    | Tick message ->
        let spinner, command = Charm_bubbles.Spinner.update message model.spinner in
        ({ model with spinner }, Charm_tea.Cmd.map (fun message -> Tick message) command)
  in
  let view model =
    let spinner = Charm_bubbles.Spinner.view model.spinner in
    let line =
      if String.equal model.align "right" then model.title ^ " " ^ spinner
      else spinner ^ " " ^ model.title
    in
    Charm_tea.View.v ~alt_screen:false
      (Charm_lipgloss.Style.render
         (Charm_lipgloss.Style.padding padding Charm_lipgloss.Style.empty)
         line)
  in
  let subscriptions model =
    Charm_tea.Sub.batch
      [
        Charm_tea.Sub.key (fun key -> Key key);
        Charm_tea.Sub.map
          (fun message -> Tick message)
          (Charm_bubbles.Spinner.subscriptions model.spinner);
      ]
  in
  { Charm_tea.init = (fun () -> (initial, start)); update; view; subscriptions }

let run env (options : options) =
  if options.command = [] then Charm_cli.error "unable to run action: empty command";
  let padding =
    match Gum_flag.parse_padding options.padding with
    | Ok value -> value
    | Error (`Msg message) -> Charm_cli.error message
  in
  let explicit =
    options.show_output || options.show_error || options.show_stdout
    || options.show_stderr
  in
  let capture = Gum_io.stdout_is_tty env || explicit in
  let run_direct () =
    run_child ~capture env ~command:options.command ~timeout:options.timeout
  in
  let result =
    if Gum_io.stderr_is_tty env then begin
      let app = make_app env options padding ~capture in
      try
        let model =
          Gum_run.run env app ~finished:(fun model ->
              match model.result with Some _ -> Gum_run.Submitted | None -> Gum_run.Quit)
        in
        Option.value model.result ~default:(Error "unable to run action")
      with Gum_io.No_tty -> run_direct ()
    end
    else begin
      write env#stderr (options.title ^ "\n");
      run_direct ()
    end
  in
  match result with
  | Error message -> Charm_cli.error message
  | Ok result ->
      route_output env options result;
      if result.timed_out then Charm_cli.exit 124 else Charm_cli.exit result.status

let options command show_output show_error show_stdout show_stderr spinner title align
    timeout padding spinner_style title_style =
  {
    command;
    show_output;
    show_error;
    show_stdout;
    show_stderr;
    spinner;
    title;
    align;
    timeout;
    padding;
    spinner_style;
    title_style;
  }

let cmd env =
  let open Cmdliner in
  let command =
    Arg.(
      non_empty
        (pos_all string [] (info [] ~docv:"COMMAND" ~doc:"Command and arguments.")))
  in
  let show_output =
    Gum_flag.flag ~cmd:"spin" ~default:false ~doc:"Show command output." "show-output"
  in
  let show_error =
    Gum_flag.flag ~cmd:"spin" ~default:false ~doc:"Show output on error." "show-error"
  in
  let show_stdout =
    Gum_flag.flag ~cmd:"spin" ~default:false ~doc:"Show stdout." "show-stdout"
  in
  let show_stderr =
    Gum_flag.flag ~cmd:"spin" ~default:false ~doc:"Show stderr." "show-stderr"
  in
  let spinner =
    Arg.(
      value
        (opt
           (Gum_flag.enum ~docv:"SPINNER"
              [
                ("line", "line");
                ("dot", "dot");
                ("minidot", "minidot");
                ("jump", "jump");
                ("pulse", "pulse");
                ("points", "points");
                ("globe", "globe");
                ("moon", "moon");
                ("monkey", "monkey");
                ("meter", "meter");
                ("hamburger", "hamburger");
              ])
           "dot"
           (info [ "spinner"; "s" ]
              ~env:(Gum_flag.env ~cmd:"spin" "spinner")
              ~doc:"Spinner kind.")))
  in
  let title =
    Arg.(
      value
        (opt string "Loading..."
           (info [ "title" ]
              ~env:(Gum_flag.env ~cmd:"spin" "title")
              ~doc:"Spinner title.")))
  in
  let align =
    Arg.(
      value
        (opt
           (Gum_flag.enum ~docv:"ALIGN" [ ("left", "left"); ("right", "right") ])
           "left"
           (info [ "align"; "a" ]
              ~env:(Gum_flag.env ~cmd:"spin" "align")
              ~doc:"Spinner alignment.")))
  in
  let timeout =
    Gum_flag.seconds ~cmd:"spin" ~doc:"Terminate after this duration." "timeout"
  in
  let padding =
    let parse value =
      match Gum_flag.parse_padding value with
      | Ok _ -> Ok value
      | Error (`Msg message) -> Error (`Msg message)
    in
    let padding_conv =
      Arg.conv (parse, fun formatter _ -> Stdlib.Format.pp_print_string formatter "")
    in
    Arg.(
      value
        (opt padding_conv "0 0"
           (info [ "padding" ] ~env:(Gum_flag.env ~cmd:"spin" "padding") ~doc:"Padding.")))
  in
  let spinner_style =
    Gum_style.term ~cmd:"spin" ~prefix:"spinner" ~defaults:default_options.spinner_style
      ()
  in
  let title_style =
    Gum_style.term ~cmd:"spin" ~prefix:"title" ~defaults:default_options.title_style ()
  in
  let action command show_output show_error show_stdout show_stderr spinner title align
      timeout padding spinner_style title_style =
    run env
      (options command show_output show_error show_stdout show_stderr spinner title align
         timeout padding spinner_style title_style)
  in
  let term =
    Term.(
      const action $ command $ show_output $ show_error $ show_stdout $ show_stderr
      $ spinner $ title $ align $ timeout $ padding $ spinner_style $ title_style)
  in
  Cmd.v (Cmd.info "spin" ~doc:"Run a command with a spinner.") term
