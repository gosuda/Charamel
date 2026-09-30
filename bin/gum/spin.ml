module Env = Charamel_cli.Env

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
    spinner_style = Gum_style.defaults ~foreground:"212" ();
    title_style = Gum_style.defaults ();
  }

let child_abort : (unit -> unit) ref = ref (fun () -> ())

type capture_file = { path : string; fd : Unix.file_descr }
type captures = { stdout : capture_file; stderr : capture_file; output : capture_file }

let create_capture suffix =
  Lwt.bind
    (Lwt_preemptive.detach (fun () -> Filename.temp_file "gum-spin-" suffix) ())
    (fun path ->
      Lwt.catch
        (fun () ->
          Lwt.map
            (fun fd -> { path; fd })
            (Lwt_preemptive.detach
               (fun () ->
                 Unix.openfile path [ Unix.O_WRONLY; Unix.O_TRUNC; Unix.O_CLOEXEC ] 0o600)
               ()))
        (fun exn ->
          Lwt.bind
            (Lwt.catch
               (fun () -> Lwt_preemptive.detach (fun () -> Unix.unlink path) ())
               (function Unix.Unix_error _ -> Lwt.return_unit | exn -> Lwt.fail exn))
            (fun () -> Lwt.fail exn)))

let close_capture capture =
  Lwt.catch
    (fun () -> Lwt_preemptive.detach (fun () -> Unix.close capture.fd) ())
    (function Unix.Unix_error _ -> Lwt.return_unit | exn -> Lwt.fail exn)

let remove_capture capture =
  Lwt.bind (close_capture capture) (fun () ->
      Lwt.catch
        (fun () -> Lwt_preemptive.detach (fun () -> Unix.unlink capture.path) ())
        (function Unix.Unix_error _ -> Lwt.return_unit | exn -> Lwt.fail exn))

let create_captures () =
  let created = ref [] in
  let make suffix =
    Lwt.map
      (fun capture ->
        created := capture :: !created;
        capture)
      (create_capture suffix)
  in
  Lwt.catch
    (fun () ->
      Lwt.bind (make ".stdout") (fun stdout ->
          Lwt.bind (make ".stderr") (fun stderr ->
              Lwt.map (fun output -> { stdout; stderr; output }) (make ".output"))))
    (fun exn ->
      Lwt.bind (Lwt_list.iter_s remove_capture !created) (fun () -> Lwt.fail exn))

let close_captures captures =
  Lwt_list.iter_s close_capture [ captures.stdout; captures.stderr; captures.output ]

let remove_captures captures =
  Lwt_list.iter_s remove_capture [ captures.stdout; captures.stderr; captures.output ]

let write_capture capture text =
  if text = "" then Lwt.return_unit
  else
    Lwt_preemptive.detach
      (fun () ->
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
      ()

let read_capture capture =
  Lwt_preemptive.detach
    (fun () ->
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
    ()

let finish_captured captures ~status ~timed_out =
  Lwt.bind (close_captures captures) (fun () ->
      Lwt.bind (read_capture captures.stdout) (fun stdout ->
          Lwt.bind (read_capture captures.stderr) (fun stderr ->
              Lwt.map
                (fun output ->
                  Ok
                    {
                      status = (if timed_out then 124 else status);
                      stdout;
                      stderr;
                      output;
                      timed_out;
                    })
                (read_capture captures.output))))

let drain_channel channel ~destination ~combined ~mutex =
  let rec loop () =
    Lwt.bind (Lwt_io.read ~count:65536 channel) (function
      | "" -> Lwt.return_unit
      | text ->
          Lwt.bind
            (Lwt_mutex.with_lock mutex (fun () ->
                 Lwt.bind (write_capture destination text) (fun () ->
                     write_capture combined text)))
            loop)
  in
  loop ()

(* Charamel_os.Pty.read folds the master's end-of-input signal — an EIO on POSIX once the
   child's slave closes — into an empty read. Every successful chunk is committed to both
   capture files before that terminal signal is accepted. *)
let drain_pty pty ~destination ~combined ~mutex =
  let rec loop () =
    Lwt.bind (Charamel_os.Pty.read pty 65536) (function
      | Ok "" | Error _ -> Lwt.return_unit
      | Ok text ->
          Lwt.bind
            (Lwt_mutex.with_lock mutex (fun () ->
                 Lwt.bind (write_capture destination text) (fun () ->
                     write_capture combined text)))
            loop)
  in
  loop ()

let wait_status ~timeout process =
  let finished =
    Lwt.map (fun status -> (status, false)) (Charamel_os.Process.await process)
  in
  match timeout with
  | None -> finished
  | Some seconds ->
      let arrived = Lwt.map (fun result -> Some result) finished in
      let elapsed = Lwt.map (fun () -> None) (Lwt_unix.sleep seconds) in
      Lwt.bind
        (Lwt.choose [ arrived; elapsed ])
        (function
          | Some result -> Lwt.return result
          | None ->
              Charamel_os.Process.terminate process;
              Lwt.bind (Lwt_unix.sleep 0.1) (fun () ->
                  Charamel_os.Process.kill_tree process;
                  Lwt.map
                    (fun status -> (status, true))
                    (Charamel_os.Process.await process)))

let run_protected thunk =
  Lwt.catch thunk (function
    | Unix.Unix_error (error, _, _) ->
        Lwt.return (Error (Fmt.str "unable to run action: %s" (Unix.error_message error)))
    | Invalid_argument message ->
        Lwt.return (Error (Fmt.str "unable to run action: %s" message))
    | exn -> Lwt.fail exn)

let with_child_abort action =
  let previous_abort = !child_abort in
  Lwt.finalize action (fun () ->
      child_abort := previous_abort;
      Lwt.return_unit)

let with_child_guard action = with_child_abort (fun () -> run_protected action)

let with_pty ~rows ~cols f =
  Lwt.bind (Charamel_os.Pty.create ~rows ~cols ()) (function
    | Error (`Error message) ->
        Lwt.return (Error (Fmt.str "unable to run action: %s" message))
    | Error `Unsupported ->
        Lwt.return (Error "unable to run action: PTY is unsupported on this platform")
    | Ok pty ->
        Lwt.finalize
          (fun () -> f pty)
          (fun () ->
            Charamel_os.Pty.close pty;
            Lwt.return_unit))

let run_direct_child ~command ~timeout =
  with_child_guard (fun () ->
      let process = Charamel_os.Process.spawn ~stdin:`Inherit command in
      (child_abort := fun () -> Charamel_os.Process.terminate process);
      Lwt.map
        (fun (status, timed_out) ->
          Ok
            {
              status = (if timed_out then 124 else status);
              stdout = "";
              stderr = "";
              output = "";
              timed_out;
            })
        (wait_status ~timeout process))

let run_with_channels captures ~timeout process =
  let mutex = Lwt_mutex.create () in
  let status = ref None in
  Lwt.bind
    (Lwt.join
       [
         drain_channel
           (Charamel_os.Process.stdout_r process)
           ~destination:captures.stdout ~combined:captures.output ~mutex;
         drain_channel
           (Charamel_os.Process.stderr_r process)
           ~destination:captures.stderr ~combined:captures.output ~mutex;
         Lwt.map (fun result -> status := Some result) (wait_status ~timeout process);
       ])
    (fun () ->
      let status, timed_out = Option.value !status ~default:(1, false) in
      finish_captured captures ~status ~timed_out)

let with_captured_child action =
  with_child_abort (fun () ->
      Lwt.bind (create_captures ()) (fun captures ->
          Lwt.finalize
            (fun () -> run_protected (fun () -> action captures))
            (fun () -> remove_captures captures)))

let run_pipe_child ~command ~timeout =
  with_captured_child (fun captures ->
      let process =
        Charamel_os.Process.spawn ~stdin:`Inherit ~stdout:`Pipe ~stderr:`Pipe command
      in
      (child_abort := fun () -> Charamel_os.Process.terminate process);
      run_with_channels captures ~timeout process)

let run_with_ptys ?stdin_text captures ~command ~timeout stdout_pty stderr_pty =
  let wrapped_command =
    [
      "/bin/sh";
      "-c";
      "exec 1>\"$1\" 2>\"$2\"; shift 2; exec \"$@\"";
      "gum-spin";
      Charamel_os.Pty.slave_path stdout_pty;
      Charamel_os.Pty.slave_path stderr_pty;
    ]
    @ command
  in
  (* A caller that supplies text feeds a real stdin pipe, exactly as an interactive run
     inherits the terminal: the child's [read] sees the bytes and then end-of-file. *)
  let process =
    Charamel_os.Process.spawn
      ~stdin:(match stdin_text with Some _ -> `Pipe | None -> `Inherit)
      wrapped_command
  in
  (child_abort := fun () -> Charamel_os.Process.terminate process);
  let feed_stdin =
    match stdin_text with
    | None -> Lwt.return_unit
    | Some text ->
        let input = Charamel_os.Process.stdin_w process in
        Lwt.bind (Lwt_io.write input text) (fun () ->
            Lwt.bind (Lwt_io.flush input) (fun () -> Lwt_io.close input))
  in
  let mutex = Lwt_mutex.create () in
  let status = ref None in
  Lwt.bind
    (Lwt.join
       [
         feed_stdin;
         drain_pty stdout_pty ~destination:captures.stdout ~combined:captures.output
           ~mutex;
         drain_pty stderr_pty ~destination:captures.stderr ~combined:captures.output
           ~mutex;
         Lwt.map (fun result -> status := Some result) (wait_status ~timeout process);
       ])
    (fun () ->
      let status, timed_out = Option.value !status ~default:(1, false) in
      finish_captured captures ~status ~timed_out)

let spawn_pty_pair ?rows ?cols ?stdin_text captures ~command ~timeout =
  let default_rows, default_cols =
    match Charamel_os.Tty.size_stdout () with Some size -> size | None -> (24, 80)
  in
  let rows = Option.value rows ~default:default_rows in
  let cols = Option.value cols ~default:default_cols in
  with_pty ~rows ~cols (fun stdout_pty ->
      with_pty ~rows ~cols (fun stderr_pty ->
          run_with_ptys ?stdin_text captures ~command ~timeout stdout_pty stderr_pty))

let run_pty_child ~command ~timeout =
  with_captured_child (fun captures -> spawn_pty_pair captures ~command ~timeout)

(* [run_child] takes the two-PTY path only when the real standard output is a terminal,
   which a piped [dune runtest] never is. This entry point drives that path directly,
   with the geometry and the stdin text a caller controls. *)
let run_pty_pair ?rows ?cols ?stdin_text ~command ~timeout () =
  with_captured_child (fun captures ->
      spawn_pty_pair ?rows ?cols ?stdin_text captures ~command ~timeout)

let run_child ?(capture = true) env ~command ~timeout =
  match command with
  | [] -> Lwt.return (Error "empty command")
  | _ when not capture -> run_direct_child ~command ~timeout
  | _ when Gum_io.stdout_is_tty env -> run_pty_child ~command ~timeout
  | _ -> run_pipe_child ~command ~timeout

let write channel text = if text = "" then Lwt.return_unit else Lwt_io.write channel text

let route_output (env : Charamel_cli.Env.t) (options : options) (result : child_result) =
  let stdout_tty = Gum_io.stdout_is_tty env in
  let explicit =
    options.show_output || options.show_error || options.show_stdout
    || options.show_stderr
  in
  if result.status = 0 then
    if options.show_output || (options.show_stdout && options.show_stderr) then
      write env.Env.stdout result.output
    else if options.show_stdout then write env.Env.stdout result.stdout
    else if options.show_stderr then write env.Env.stdout result.stderr
    else if (not explicit) && not stdout_tty then
      Lwt.bind (write env.Env.stdout result.stdout) (fun () ->
          write env.Env.stderr result.stderr)
    else Lwt.return_unit
  else if options.show_error then write env.Env.stdout result.output
  else if (not explicit) && not stdout_tty then
    Lwt.bind (write env.Env.stdout result.stdout) (fun () ->
        write env.Env.stderr result.stderr)
  else Lwt.return_unit

type msg =
  | Tick of Charamel_bubbles.Spinner.msg
  | Finished of (child_result, string) result
  | Key of Charamel_tea.Key.t

type model = {
  spinner : Charamel_bubbles.Spinner.t;
  result : (child_result, string) result option;
  title : string;
  align : string;
}

let spinner_kind value =
  Option.value
    (Charamel_bubbles.Spinner.kind_of_string value)
    ~default:Charamel_bubbles.Spinner.Dot

let make_app (env : Charamel_cli.Env.t) (options : options) padding ~capture =
  let spinner =
    Charamel_bubbles.Spinner.v
      ~kind:(spinner_kind options.spinner)
      ~style:(Gum_style.to_style options.spinner_style)
      ()
  in
  let title =
    Gum_style.to_style options.title_style |> fun style ->
    Charamel_lipgloss.Style.render style options.title
  in
  let initial = { spinner; result = None; title; align = options.align } in
  let start =
    Charamel_tea.Cmd.await
      (Lwt.map
         (fun result -> Finished result)
         (run_child ~capture env ~command:options.command ~timeout:options.timeout))
  in
  let update message model =
    match message with
    | Finished result -> ({ model with result = Some result }, Charamel_tea.Cmd.quit)
    | Key key when Gum_flag.is_abort key ->
        !child_abort ();
        (model, Charamel_tea.Cmd.interrupt)
    | Key _ -> (model, Charamel_tea.Cmd.none)
    | Tick message ->
        let spinner, command = Charamel_bubbles.Spinner.update message model.spinner in
        ( { model with spinner },
          Charamel_tea.Cmd.map (fun message -> Tick message) command )
  in
  let view model =
    let spinner = Charamel_bubbles.Spinner.view model.spinner in
    let line =
      if String.equal model.align "right" then model.title ^ " " ^ spinner
      else spinner ^ " " ^ model.title
    in
    Charamel_tea.View.v ~alt_screen:false
      (Charamel_lipgloss.Style.render
         (Charamel_lipgloss.Style.padding padding Charamel_lipgloss.Style.empty)
         line)
  in
  let subscriptions model =
    Charamel_tea.Sub.batch
      [
        Charamel_tea.Sub.key (fun key -> Key key);
        Charamel_tea.Sub.map
          (fun message -> Tick message)
          (Charamel_bubbles.Spinner.subscriptions model.spinner);
      ]
  in
  { Charamel_tea.init = (fun () -> (initial, start)); update; view; subscriptions }

let run env (options : options) =
  if options.command = [] then Charamel_cli.error "unable to run action: empty command";
  let padding =
    match Gum_flag.parse_padding options.padding with
    | Ok value -> value
    | Error (`Msg message) -> Charamel_cli.error message
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
    if Gum_io.stderr_is_tty env then
      let app = make_app env options padding ~capture in
      Lwt.catch
        (fun () ->
          Lwt.map
            (fun model ->
              Option.value model.result ~default:(Error "unable to run action"))
            (Gum_run.run env app ~finished:(fun model ->
                 match model.result with
                 | Some _ -> Gum_run.Submitted
                 | None -> Gum_run.Quit)))
        (function Gum_io.No_tty -> run_direct () | exn -> Lwt.fail exn)
    else Lwt.bind (write env.Env.stderr (options.title ^ "\n")) (fun () -> run_direct ())
  in
  Lwt.bind result (function
    | Error message -> Charamel_cli.error message
    | Ok result ->
        Lwt.bind (route_output env options result) (fun () ->
            if result.timed_out then Charamel_cli.exit 124
            else Charamel_cli.exit result.status))

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
    Gum_flag.validated_padding_term ~doc:"Padding."
      ~pp:(fun formatter _ -> Stdlib.Format.pp_print_string formatter "")
      ~cmd:"spin" ()
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
