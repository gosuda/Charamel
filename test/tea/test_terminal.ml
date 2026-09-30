open Lwt.Syntax
module Terminal = Charamel_tea__Terminal

let buffer_output buffer =
  Lwt_io.make ~mode:Lwt_io.output (fun source offset length ->
      Buffer.add_subbytes buffer (Lwt_bytes.to_bytes source) offset length;
      Lwt.return length)

let await name promise =
  match Lwt.poll promise with
  | Some value -> value
  | None -> Alcotest.failf "%s did not settle synchronously" name

let test_custom_io_boundary () =
  let output_buffer = Buffer.create 16 in
  let resize_events, _push = Lwt_stream.create () in
  let terminal =
    Terminal.custom
      ~input:(Charamel_os.Console_input.of_queue [ "input" ])
      ~output:(buffer_output output_buffer)
      ~size:(fun () -> (13, 47))
      ~on_resize:(Some resize_events)
      ~env:(function "TERM" -> Some "xterm-256color" | _ -> None)
      ~is_tty:true
  in
  Alcotest.(check bool) "custom tty flag" true terminal.Terminal.is_tty;
  Alcotest.(check (pair int int)) "custom size" (13, 47) (terminal.Terminal.size ());
  Alcotest.(check (option string))
    "custom environment" (Some "xterm-256color")
    (terminal.Terminal.env "TERM");
  Alcotest.(check bool)
    "custom resize stream" true
    (Option.is_some terminal.Terminal.on_resize);
  terminal.Terminal.enter ();
  terminal.Terminal.leave ();
  Alcotest.(check string)
    "custom input" "input"
    (await "custom input" (Charamel_os.Console_input.read terminal.Terminal.input));
  await "custom write"
    (let* () = Lwt_io.write terminal.Terminal.output "output" in
     Lwt_io.flush terminal.Terminal.output);
  Alcotest.(check string) "custom output" "output" (Buffer.contents output_buffer)

let restore_env name value =
  match value with Some text -> Unix.putenv name text | None -> Unix.putenv name ""

let with_size_env columns lines f =
  let previous = (Sys.getenv_opt "COLUMNS", Sys.getenv_opt "LINES") in
  Unix.putenv "COLUMNS" columns;
  Unix.putenv "LINES" lines;
  Fun.protect
    ~finally:(fun () ->
      restore_env "COLUMNS" (fst previous);
      restore_env "LINES" (snd previous))
    f

let test_local_transport () =
  let terminal = Terminal.local () in
  Alcotest.(check bool)
    "local transport resize stream matches the platform" true
    ((if Sys.win32 then Option.is_some else Option.is_none) terminal.Terminal.on_resize);
  Alcotest.(check bool)
    "local tty flag matches the descriptors"
    (Charamel_os.Tty.is_tty_stdin && Charamel_os.Tty.is_tty_stdout)
    terminal.Terminal.is_tty;
  Alcotest.(check (option string))
    "local environment is the process environment" (Sys.getenv_opt "TERM")
    (terminal.Terminal.env "TERM");
  terminal.Terminal.enter ();
  terminal.Terminal.leave ();
  terminal.Terminal.leave ();
  if Charamel_os.Tty.is_tty_stdout then ()
  else
    with_size_env "132" "43" (fun () ->
        Alcotest.(check (pair int int))
          "environment size" (43, 132) (terminal.Terminal.size ());
        with_size_env "" "" (fun () ->
            Alcotest.(check (pair int int))
              "default size" (24, 80) (terminal.Terminal.size ())))

let pty_marker = "PTY-STATUS"

let report_field name report =
  let prefix = name ^ "=" in
  match
    List.find_opt (String.starts_with ~prefix)
      (String.split_on_char ' ' (String.trim report))
  with
  | None -> Alcotest.failf "child report has no %s field: %S" name report
  | Some text -> (
      let value =
        String.sub text (String.length prefix) (String.length text - String.length prefix)
      in
      match bool_of_string_opt value with
      | Some flag -> flag
      | None -> Alcotest.failf "child report field %s is not a bool: %S" name value)

let pty_child () =
  let before = Unix.tcgetattr Unix.stdin in
  let observed = ref (true, true, true) in
  let app =
    {
      Charamel_tea.init = (fun () -> (0, Charamel_tea.Cmd.after 0.05 (fun () -> `Check)));
      update =
        (fun message count ->
          match message with
          | `Check ->
              let raw = Unix.tcgetattr Unix.stdin in
              observed := (raw.Unix.c_icanon, raw.Unix.c_echo, raw.Unix.c_isig);
              (count, Charamel_tea.Cmd.quit));
      view = (fun _ -> Charamel_tea.View.v "pty-frame");
      subscriptions = (fun _ -> Charamel_tea.Sub.none);
    }
  in
  let* result = Charamel_tea.run ~clock:Charamel_os.Time.lwt app in
  let finished = Result.is_ok result in
  let restored = finished && Unix.tcgetattr Unix.stdin = before in
  let icanon, echo, isig = !observed in
  Printf.printf "%s icanon=%B echo=%B isig=%B restored=%B finished=%B\n" pty_marker icanon
    echo isig restored finished;
  Lwt.return (if finished && restored then 0 else 1)

let run_pty_child () = exit (Lwt_main.run (pty_child ()))

let rec collect pty attempts acc =
  if attempts <= 0 then Lwt.return acc
  else
    let* chunk = Charamel_os.Pty.read pty 4096 in
    match chunk with
    | Error _ | Ok "" -> Lwt.return acc
    | Ok text ->
        let acc = acc ^ text in
        if Test_support.contains ~needle:pty_marker ~haystack:acc then Lwt.return acc
        else collect pty (attempts - 1) acc

let read_pty_report pty = collect pty 200 ""

let check_pty_report report =
  let painted = Test_support.contains ~needle:"pty-frame" ~haystack:report in
  Alcotest.(check bool) "the child painted its frame" true painted;
  Alcotest.(check bool)
    "raw mode clears canonical input" false
    (report_field "icanon" report);
  Alcotest.(check bool) "raw mode clears echo" false (report_field "echo" report);
  Alcotest.(check bool) "raw mode clears signals" false (report_field "isig" report);
  Alcotest.(check bool) "the run finished" true (report_field "finished" report);
  Alcotest.(check bool)
    "leave restores the initial termios" true
    (report_field "restored" report)

let start_child pty =
  let* spawned = Charamel_os.Pty.exec pty [ Sys.argv.(0); "--tea-pty-child" ] in
  match spawned with
  | Error `Unsupported -> Lwt.return_none
  | Error (`Error message) -> Alcotest.failf "child did not start: %s" message
  | Ok _pid ->
      let* report = read_pty_report pty in
      Lwt.return (Some report)

let with_pty body =
  let* created = Charamel_os.Pty.create ~rows:31 ~cols:73 () in
  match created with
  | Error (`Error message) -> Alcotest.failf "pseudo-terminal unavailable: %s" message
  | Error `Unsupported -> Lwt.return `Skip
  | Ok pty ->
      Lwt.finalize
        (fun () -> body pty)
        (fun () ->
          Charamel_os.Pty.close pty;
          Lwt.return_unit)

let test_local_pty_lifecycle () =
  let* outcome =
    with_pty (fun pty ->
        let* started = start_child pty in
        match started with
        | None -> Lwt.return `Skip
        | Some report ->
            check_pty_report report;
            Lwt.return `Run)
  in
  match outcome with `Skip -> Alcotest.skip () | `Run -> Lwt.return_unit

let cases =
  [
    Alcotest.test_case "custom IO boundary" `Quick test_custom_io_boundary;
    Alcotest.test_case "local transport" `Quick test_local_transport;
    Alcotest.test_case "local PTY lifecycle" `Quick (fun () ->
        Lwt_main.run (test_local_pty_lifecycle ()));
  ]
