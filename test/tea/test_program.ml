open Lwt.Syntax
open Charamel_tea

let view count = View.v (string_of_int count)

let app_for_update update subscriptions =
  { init = (fun () -> (0, Cmd.none)); update; view; subscriptions }

let test_scripted_message () =
  let app =
    app_for_update (fun message count -> (count + message, Cmd.none)) (fun _ -> Sub.none)
  in
  let count, frame = Test.run app ~events:[ `Msg 3 ] ~size:(4, 20) in
  Alcotest.(check int) "message reaches update" 3 count;
  Alcotest.(check string) "last frame" "3" frame

let test_key_subscription () =
  let app =
    app_for_update
      (fun message count -> match message with `Increment -> (count + 1, Cmd.none))
      (fun _ -> Sub.key (fun _ -> `Increment))
  in
  let key = Key.v (Key.Char (Uchar.of_char 'x')) in
  let count, _ = Test.run app ~events:[ `Key key ] ~size:(4, 20) in
  Alcotest.(check int) "key subscription" 1 count

let test_message_guard () =
  let app =
    app_for_update
      (fun message count ->
        if message = 2 then (count, Cmd.none) else (count + message, Cmd.none))
      (fun _ -> Sub.none)
  in
  let count, _ = Test.run app ~events:[ `Msg 2 ] ~size:(4, 20) in
  Alcotest.(check int) "guarded message" 0 count

let test_after_command () =
  let app =
    {
      init = (fun () -> (0, Cmd.after 1.0 (fun () -> `Done)));
      update = (fun message count -> match message with `Done -> (count + 1, Cmd.quit));
      view;
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let count, _ = Test.run app ~events:[ `Wait 1.0 ] ~size:(4, 20) in
  Alcotest.(check int) "after command" 1 count

let test_every_subscription () =
  let app =
    {
      init = (fun () -> (0, Cmd.none));
      update = (fun (`Tick _) count -> (count + 1, Cmd.none));
      view;
      subscriptions = (fun _ -> Sub.every 1.0 (fun now -> `Tick now));
    }
  in
  let count, _ = Test.run app ~events:[ `Wait 2.1 ] ~size:(4, 20) in
  Alcotest.(check int) "every timer" 2 count

let test_command_sequence () =
  let app =
    {
      init = (fun () -> (0, Cmd.seq [ Cmd.msg 1; Cmd.msg 2 ]));
      update = (fun message count -> (count + message, Cmd.none));
      view;
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let count, _ = Test.run app ~events:[] ~size:(4, 20) in
  Alcotest.(check int) "sequential commands" 3 count

let test_command_map () =
  let app =
    {
      init = (fun () -> (0, Cmd.map (fun value -> value + 1) (Cmd.msg 1)));
      update = (fun message count -> (count + message, Cmd.none));
      view;
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let count, _ = Test.run app ~events:[] ~size:(4, 20) in
  Alcotest.(check int) "mapped command" 2 count

let test_print_then_message () =
  let app =
    {
      init = (fun () -> (0, Cmd.seq [ Cmd.print "line"; Cmd.msg 1 ]));
      update = (fun message count -> (count + message, Cmd.none));
      view;
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let count, _ = Test.run app ~events:[] ~size:(4, 20) in
  Alcotest.(check int) "print command completes" 1 count

let test_large_command_batch () =
  let app =
    {
      init = (fun () -> (0, Cmd.batch (List.init 300 (fun _ -> Cmd.msg 1))));
      update = (fun message count -> (count + message, Cmd.none));
      view;
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let count, _ = Test.run app ~events:[] ~size:(4, 20) in
  Alcotest.(check int) "bounded command queue" 300 count

let test_quit_cancels_long_command () =
  let app =
    {
      init =
        (fun () -> (0, Cmd.batch [ Cmd.quit; Cmd.after 100.0 (fun () -> `Unexpected) ]));
      update =
        (fun message count -> match message with `Unexpected -> (count + 1, Cmd.none));
      view;
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let count, _ = Test.run app ~events:[] ~size:(4, 20) in
  Alcotest.(check int) "quit cancels pending commands" 0 count

let rendered_bytes app =
  let buffer = Buffer.create 256 in
  let output = Test_terminal.buffer_output buffer in
  let _, _ = Test.run ~output app ~events:[] ~size:(4, 20) in
  Buffer.contents buffer

let test_every_frame_reaches_terminal () =
  let word = function 0 -> "alpha" | 1 -> "bravo" | _ -> "charlie" in
  let step = Cmd.after 0.02 (fun () -> `Step) in
  let app =
    {
      init = (fun () -> (0, step));
      update =
        (fun message count ->
          match message with
          | `Step ->
              let count = count + 1 in
              (count, if count < 2 then step else Cmd.after 0.02 (fun () -> `Quit))
          | `Quit -> (count, Cmd.quit));
      view = (fun count -> View.v (word count));
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let bytes = rendered_bytes app in
  Alcotest.(check bool)
    "alpha frame rendered" true
    (Test_support.contains ~needle:"alpha" ~haystack:bytes);
  Alcotest.(check bool)
    "bravo frame rendered" true
    (Test_support.contains ~needle:"bravo" ~haystack:bytes);
  Alcotest.(check bool)
    "charlie frame rendered" true
    (Test_support.contains ~needle:"charlie" ~haystack:bytes)

let test_stream_subscription_delivers_once_per_stream () =
  let source, push = Lwt_stream.create () in
  let app =
    {
      init = (fun () -> (([] : string list), Cmd.none));
      update =
        (fun text received ->
          if text = "!emit" then begin
            push (Some "a");
            push (Some "b");
            (received, Cmd.none)
          end
          else (text :: received, Cmd.none));
      view = (fun received -> View.v (String.concat "" (List.rev received)));
      subscriptions =
        (fun _ ->
          Sub.batch [ Sub.map (fun text -> text) (Sub.stream source); Sub.stream source ]);
    }
  in
  let received, frame = Test.run app ~events:[ `Msg "!emit"; `Wait 0.1 ] ~size:(4, 20) in
  Alcotest.(check (list string))
    "one reader per distinct stream" [ "a"; "b" ] (List.rev received);
  Alcotest.(check string) "stream messages reach the view" "ab" frame

type stream_toggle = { subscribed : bool; got : string list }

let counted_stream () =
  let source, push = Lwt_stream.create () in
  ( source,
    push,
    fun message state ->
      (match message with
      | "!later" -> push (Some "x")
      | "!send2" ->
          push (Some "y");
          push None
      | _ -> ());
      let subscribed =
        match message with
        | "!on" | "!on2" -> true
        | "!off" -> false
        | _ -> state.subscribed
      in
      let got =
        if message = "x" || message = "y" then message :: state.got else state.got
      in
      ({ subscribed; got }, Cmd.none) )

let stream_app (source, _push, update) =
  {
    init = (fun () -> ({ subscribed = false; got = [] }, Cmd.none));
    update;
    view = (fun _ -> View.v "x");
    subscriptions =
      (fun state -> if state.subscribed then Sub.stream source else Sub.none);
  }

let count_in got name = List.length (List.filter (fun message -> message = name) got)

let test_stream_unsubscribed_in_its_subscribe_batch_is_not_read () =
  let app = stream_app (counted_stream ()) in
  let final, _ =
    Test.run app
      ~events:[ `Msg "!on"; `Msg "!off"; `Msg "!later"; `Wait 0.5 ]
      ~size:(4, 20)
  in
  Alcotest.(check int) "a cancelled reader delivers nothing" 0 (count_in final.got "x")

let test_stream_resubscribe_reads_through_one_reader () =
  let app = stream_app (counted_stream ()) in
  let final, _ =
    Test.run app
      ~events:
        [ `Msg "!on"; `Msg "!off"; `Msg "!later"; `Msg "!on2"; `Msg "!send2"; `Wait 0.5 ]
      ~size:(4, 20)
  in
  Alcotest.(check int)
    "the resubscribed stream delivers each value once" 1 (count_in final.got "y")

let test_timer_unsubscribed_in_its_subscribe_batch_stops_ticking () =
  let app =
    {
      init = (fun () -> (((0, false) : int * bool), Cmd.none));
      update =
        (fun message (ticks, ticking) ->
          ( (match message with
            | `Tick -> (ticks + 1, ticking)
            | `On -> (ticks, true)
            | `Off -> (ticks, false)),
            Cmd.none ));
      view = (fun _ -> View.v "x");
      subscriptions =
        (fun (_, ticking) ->
          if ticking then Sub.every 1.0 (fun _now -> `Tick) else Sub.none);
    }
  in
  let ticks, _ = Test.run app ~events:[ `Msg `On; `Msg `Off; `Wait 3.0 ] ~size:(4, 20) in
  Alcotest.(check int)
    "a timer cancelled in its subscribe batch never ticks" 0 (fst ticks)

let test_stream_ends_cleanly () =
  let source, push = Lwt_stream.create () in
  let app =
    {
      init = (fun () -> (0, Cmd.none));
      update =
        (fun text count ->
          if text = "!close" then begin
            push (Some "last");
            push None;
            (count, Cmd.none)
          end
          else if text = "last" then (count + 1, Cmd.none)
          else (count, Cmd.none));
      view;
      subscriptions = (fun _ -> Sub.stream source);
    }
  in
  let count, _ = Test.run app ~events:[ `Msg "!close"; `Wait 0.1 ] ~size:(4, 20) in
  Alcotest.(check int) "closed stream delivers its tail and stops" 1 count

let capture ?renderer ?color_profile ?exec app =
  Lwt_main.run
    (let path = Filename.temp_file "charamel-tea" ".out" in
     let* channel = Lwt_io.open_file ~mode:Lwt_io.Output path in
     let input = Charamel_os.Console_input.blocked () in
     let size () = (4, 20) in
     let terminal =
       match exec with
       | None ->
           Charamel_tea.Terminal.custom ~input ~output:channel ~size ~on_resize:None
             ~env:(fun _ -> None)
             ~is_tty:false
       | Some exec ->
           Charamel_tea.Terminal.custom_with_exec ~input ~output:channel ~size
             ~on_resize:None
             ~env:(fun _ -> None)
             ~is_tty:false ~exec
     in
     let* result =
       Charamel_tea.run ?renderer ?color_profile ~terminal ~clock:Charamel_os.Time.lwt app
     in
     let* () = Lwt_io.close channel in
     let* bytes = Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic -> Lwt_io.read ic) in
     let () = Sys.remove path in
     match result with
     | Ok model -> Lwt.return (model, bytes)
     | Error _ -> Lwt.fail_with "scripted Tea run ended in error")

let test_held_cluster_flushes_on_the_input_pause () =
  let app =
    {
      init = (fun () -> (([] : string list), Cmd.none));
      update = (fun event seen -> (event :: seen, Cmd.none));
      view = (fun _ -> View.v "x");
      subscriptions = (fun _ -> Sub.key (fun key -> "key:" ^ key.Key.text));
    }
  in
  let seen, _ =
    Test.run app
      ~events:[ `Text "\xe1\x84\x92\xe1\x85\xa1"; `Wait 0.2; `Msg "marker"; `Wait 0.2 ]
      ~size:(4, 20)
  in
  Alcotest.(check (list string))
    "the pause flush delivers the cluster before later input"
    [ "key:\u{1112}\u{1161}"; "marker" ]
    (List.rev seen)

let test_custom_transport_exec_runs_the_child () =
  let calls = ref [] in
  let exec argv =
    calls := argv :: !calls;
    Lwt.return 42
  in
  let app =
    {
      init =
        (fun () ->
          (None, Cmd.exec ~argv:[ "vim"; "-R"; "notes.md" ] (fun code -> `Exited code)));
      update =
        (fun message exit_code ->
          match message with
          | `Exited code -> (Some code, Cmd.quit)
          | _ -> (exit_code, Cmd.none));
      view = (fun _ -> View.v "x");
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let code, _ = capture ~exec app in
  Alcotest.(check (list (list string)))
    "the transport supplied the exec implementation"
    [ [ "vim"; "-R"; "notes.md" ] ]
    (List.rev !calls);
  Alcotest.(check (option int)) "the transport's exit code reaches on_exit" (Some 42) code

let byte_app =
  {
    init =
      (fun () ->
        ( (),
          Cmd.seq
            [
              Cmd.print "printed";
              Cmd.raw "\x1b[?741h";
              Cmd.set_clipboard ~selection:`Primary "hi";
              Cmd.read_clipboard `System;
              Cmd.query (`Capability "RGB");
              Cmd.quit;
            ] ));
    update = (fun _ _ -> ((), Cmd.none));
    view = (fun _ -> View.v "screen-text");
    subscriptions = (fun _ -> Sub.none);
  }

let writes needle haystack = Test_support.contains ~needle ~haystack

let test_raw_clipboard_and_query_bytes () =
  let _, bytes = capture byte_app in
  Alcotest.(check bool) "print reaches the output" true (writes "printed\n" bytes);
  Alcotest.(check bool) "raw bytes are written verbatim" true (writes "\x1b[?741h" bytes);
  Alcotest.(check bool) "primary clipboard write" true (writes "\x1b]52;p;aGk=\x07" bytes);
  Alcotest.(check bool) "clipboard read query" true (writes "\x1b]52;c;?\x07" bytes);
  Alcotest.(check bool) "XTGETTCAP request" true (writes "\x1bP+q524742\x1b\\" bytes);
  Alcotest.(check bool) "the view is painted" true (writes "screen-text" bytes)

let test_renderer_none_prints_without_painting () =
  let _, bytes = capture ~renderer:`None byte_app in
  Alcotest.(check bool)
    "print still flows with no renderer" true (writes "printed\n" bytes);
  Alcotest.(check bool) "no frame is painted" false (writes "screen-text" bytes);
  Alcotest.(check bool) "raw bytes are dropped" false (writes "\x1b[?741h" bytes);
  Alcotest.(check bool) "clipboard bytes are dropped" false (writes "\x1b]52;" bytes);
  Alcotest.(check bool) "query bytes are dropped" false (writes "\x1bP+q" bytes)

let profile_app =
  {
    init = (fun () -> (None, Cmd.after 0.5 (fun () -> `Timeout)));
    update =
      (fun message seen ->
        match message with
        | `Report (Charamel_tea.Event.Profile detected) -> (Some detected, Cmd.quit)
        | `Report _ -> (seen, Cmd.none)
        | `Timeout -> (seen, Cmd.quit));
    view = (fun _ -> View.v "x");
    subscriptions = (fun _ -> Sub.terminal (fun event -> `Report event));
  }

let test_color_profile_override () =
  let detected, _ = capture ~color_profile:Charamel_colorprofile.True_color profile_app in
  Alcotest.(check bool)
    "the override is the profile the program is told about" true
    (detected = Some Charamel_colorprofile.True_color);
  let plain, _ = capture profile_app in
  Alcotest.(check bool)
    "without an override a non-tty detects no color" true
    (plain = Some Charamel_colorprofile.No_tty)

let test_suspend_is_a_noop_on_a_non_local_transport () =
  let app =
    {
      init =
        (fun () -> ((), Cmd.seq [ Cmd.suspend; Cmd.print "resumed-after"; Cmd.quit ]));
      update = (fun _ _ -> ((), Cmd.none));
      view = (fun _ -> View.v "x");
      subscriptions = (fun _ -> Sub.none);
    }
  in
  let _, bytes = capture app in
  Alcotest.(check bool)
    "suspend on a remote transport does not abort the run" true
    (writes "resumed-after" bytes)

type resume_probe = { resumes : int; resume_events : int; exit_code : int option }

let test_resume_is_silent_on_a_custom_transport () =
  if Sys.win32 then ()
  else
    let app =
      {
        init =
          (fun () ->
            ( { resumes = 0; resume_events = 0; exit_code = None },
              Cmd.exec ~argv:[ "/bin/false" ] (fun code -> `Exited code) ));
        update =
          (fun message state ->
            match message with
            | `Exited code ->
                ({ state with exit_code = Some code }, Cmd.after 0.2 (fun () -> `Stop))
            | `Resumed -> ({ state with resumes = state.resumes + 1 }, Cmd.none)
            | `Resume_event ->
                ({ state with resume_events = state.resume_events + 1 }, Cmd.none)
            | `Other -> (state, Cmd.none)
            | `Stop -> (state, Cmd.quit));
        view = (fun _ -> View.v "x");
        subscriptions =
          (fun _ ->
            Sub.batch
              [
                Sub.resume (fun () -> `Resumed);
                Sub.terminal (function Event.Resume -> `Resume_event | _ -> `Other);
              ]);
      }
    in
    let final, _ = capture app in
    Alcotest.(check (option int))
      "the exec round trip still completes" (Some 1) final.exit_code;
    Alcotest.(check int)
      "Sub.resume delivers nothing on a non-local transport" 0 final.resumes;
    Alcotest.(check int)
      "Event.Resume is never synthesized on a non-local transport" 0 final.resume_events

let observed ?(init = Cmd.none) ?(show = fun () -> View.v "x") subscriptions events =
  let app =
    {
      init = (fun () -> (([] : string list), init));
      update = (fun label seen -> (label :: seen, Cmd.none));
      view = (fun _ -> show ());
      subscriptions = (fun _ -> subscriptions);
    }
  in
  let seen, _ = Test.run app ~events ~size:(4, 20) in
  List.rev seen

let mouse_label mouse =
  let action =
    match mouse.Mouse.action with
    | Mouse.Press -> "press"
    | Mouse.Release -> "release"
    | Mouse.Motion -> "motion"
  in
  let button =
    match mouse.Mouse.button with
    | Mouse.Left -> "left"
    | Mouse.Middle -> "middle"
    | Mouse.Right -> "right"
    | Mouse.Wheel_up -> "wheel_up"
    | Mouse.Wheel_down -> "wheel_down"
    | Mouse.Wheel_left -> "wheel_left"
    | Mouse.Wheel_right -> "wheel_right"
    | Mouse.Backward -> "backward"
    | Mouse.Forward -> "forward"
    | Mouse.Button_10 -> "button_10"
    | Mouse.Button_11 -> "button_11"
    | Mouse.None_ -> "none"
  in
  Fmt.str "%s %s %d %d" action button mouse.Mouse.x mouse.Mouse.y

let color_label = function
  | Charamel_ansi.Color.Rgb (r, g, b) -> Fmt.str "%d %d %d" r g b
  | Charamel_ansi.Color.Basic index -> Fmt.str "basic %d" index
  | Charamel_ansi.Color.Indexed index -> Fmt.str "indexed %d" index
  | Charamel_ansi.Color.Default -> "default"

let report_label = function
  | Event.Background_color color -> Fmt.str "bg %s" (color_label color)
  | Event.Foreground_color color -> Fmt.str "fg %s" (color_label color)
  | Event.Cursor_color color -> Fmt.str "cursor %s" (color_label color)
  | Event.Terminal_version version -> Fmt.str "version %s" version
  | Event.Kitty_flags flags -> Fmt.str "kitty %d" flags
  | Event.Cursor_position { row; col } -> Fmt.str "position %d %d" row col
  | Event.Capability (Some value) -> Fmt.str "capability %s" value
  | Event.Capability None -> "capability none"
  | Event.Clipboard { selection; content } ->
      let which = match selection with `System -> "system" | `Primary -> "primary" in
      Fmt.str "clipboard %s %s" which content
  | Event.Profile _ -> "profile"
  | Event.Unknown raw -> Fmt.str "unknown %S" raw
  | Event.Key key -> Fmt.str "key %s" (Key.to_string key)
  | Event.Mouse mouse -> mouse_label mouse
  | Event.Paste text -> Fmt.str "paste %s" text
  | Event.Focus -> "focus"
  | Event.Blur -> "blur"
  | Event.Resize { rows; cols } -> Fmt.str "resize %dx%d" rows cols
  | Event.Mode_report { mode; value } -> Fmt.str "mode %d %d" mode value
  | Event.Resume -> "resume"

let size_label ~rows ~cols = Fmt.str "%dx%d" rows cols

let test_sub_mouse_follows_the_reported_mode () =
  let events = [ `Text "\027[<0;10;5M" ] in
  Alcotest.(check (list string))
    "delivered while tracking" [ "press left 9 4" ]
    (observed (Sub.mouse mouse_label)
       ~show:(fun () -> View.v ~mouse:View.Mouse_click "x")
       events);
  Alcotest.(check (list string))
    "dropped while not tracking" []
    (observed (Sub.mouse mouse_label) events)

let test_sub_focus_follows_report_focus () =
  let label = function `Focused -> "focused" | `Blurred -> "blurred" in
  let events = [ `Text "\027[I\027[O" ] in
  Alcotest.(check (list string))
    "delivered while reporting" [ "focused"; "blurred" ]
    (observed (Sub.focus label) ~show:(fun () -> View.v ~report_focus:true "x") events);
  Alcotest.(check (list string))
    "dropped while silent" []
    (observed (Sub.focus label) events)

let test_sub_paste_follows_bracketed_paste () =
  let events = [ `Text "\027[200~hello\027[201~" ] in
  Alcotest.(check (list string))
    "payload delivered by default" [ "hello" ]
    (observed (Sub.paste Fun.id) events);
  Alcotest.(check (list string))
    "payload dropped when disabled" []
    (observed (Sub.paste Fun.id)
       ~show:(fun () -> View.v ~bracketed_paste:false "x")
       events)

let test_sub_resize_delivers_startup_and_change () =
  Alcotest.(check (list string))
    "startup size then a scripted change" [ "4x20"; "10x30" ]
    (observed (Sub.resize size_label) [ `Resize (10, 30) ])

let test_window_size_redelivers_the_terminal_size () =
  Alcotest.(check (list string))
    "Cmd.window_size re-delivers the current size" [ "4x20"; "4x20" ]
    (observed ~init:Cmd.window_size (Sub.resize size_label) [])

let test_in_band_window_size_report_is_not_a_resize () =
  let raw = "\027[18t" in
  Alcotest.(check (list string))
    "an in-band report stays raw bytes"
    [ "profile"; Fmt.str "unknown %S" raw ]
    (observed (Sub.terminal report_label) [ `Text raw ])

let test_key_press_and_release_use_separate_subscriptions () =
  let press = Sub.key (fun key -> "press:" ^ Key.to_string key) in
  let release = Sub.key_release (fun key -> "release:" ^ Key.to_string key) in
  Alcotest.(check (list string))
    "releases never reach Sub.key" [ "press:a"; "release:a" ]
    (observed (Sub.batch [ press; release ]) [ `Text "\027[97;1u"; `Text "\027[97;1:3u" ])

let test_resume_subscription_is_silent_without_job_control () =
  Alcotest.(check (list string))
    "a no-op suspend delivers no resume" [ "alive" ]
    (observed ~init:Cmd.suspend (Sub.resume (fun () -> "resumed")) [ `Msg "alive" ])

let query_reports =
  [
    ("query_background", Cmd.query `Background, "\027]11;rgb:1c/1c/1c\007", "bg 28 28 28");
    ("query_foreground", Cmd.query `Foreground, "\027]10;rgb:ff/00/ff\007", "fg 255 0 255");
    ( "query_cursor_color",
      Cmd.query `Cursor_color,
      "\027]12;#aabbcc\007",
      "cursor 170 187 204" );
    ( "query_terminal_version",
      Cmd.query `Terminal_version,
      "\027P>|kitty 1.2\027\\",
      "version kitty 1.2" );
    ("query_kitty_flags", Cmd.query `Kitty_flags, "\027[?7u", "kitty 7");
    ("query_cursor_position", Cmd.query `Cursor_position, "\027[12;34R", "position 11 33");
    ( "query_capability",
      Cmd.query (`Capability "RGB"),
      "\027P1+r524742=38\027\\",
      "capability 8" );
    ( "query_capability_absent",
      Cmd.query (`Capability "RGB"),
      "\027P0+r524742\027\\",
      "capability none" );
    ( "clipboard_read_reply",
      Cmd.read_clipboard `System,
      "\027]52;c;aGVsbG8=\007",
      "clipboard system hello" );
  ]

let query_case (name, command, reply, expected) =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check (list string))
        expected [ "profile"; expected ]
        (observed ~init:command (Sub.terminal report_label) [ `Text reply ]))

let cases =
  [
    Alcotest.test_case "scripted_message" `Quick test_scripted_message;
    Alcotest.test_case "key_subscription" `Quick test_key_subscription;
    Alcotest.test_case "message_guard" `Quick test_message_guard;
    Alcotest.test_case "after_command" `Quick test_after_command;
    Alcotest.test_case "every_subscription" `Quick test_every_subscription;
    Alcotest.test_case "command_sequence" `Quick test_command_sequence;
    Alcotest.test_case "command_map" `Quick test_command_map;
    Alcotest.test_case "print_then_message" `Quick test_print_then_message;
    Alcotest.test_case "large_command_batch" `Quick test_large_command_batch;
    Alcotest.test_case "quit_cancels_long_command" `Quick test_quit_cancels_long_command;
    Alcotest.test_case "every_frame_reaches_terminal" `Quick
      test_every_frame_reaches_terminal;
    Alcotest.test_case "stream_subscription" `Quick
      test_stream_subscription_delivers_once_per_stream;
    Alcotest.test_case "stream_ends_cleanly" `Quick test_stream_ends_cleanly;
    Alcotest.test_case "stream_unsubscribe_in_subscribe_batch" `Quick
      test_stream_unsubscribed_in_its_subscribe_batch_is_not_read;
    Alcotest.test_case "stream_resubscribe_one_reader" `Quick
      test_stream_resubscribe_reads_through_one_reader;
    Alcotest.test_case "timer_unsubscribe_in_subscribe_batch" `Quick
      test_timer_unsubscribed_in_its_subscribe_batch_stops_ticking;
    Alcotest.test_case "held_cluster_pause_flush" `Quick
      test_held_cluster_flushes_on_the_input_pause;
    Alcotest.test_case "custom_transport_exec" `Quick
      test_custom_transport_exec_runs_the_child;
    Alcotest.test_case "raw_clipboard_query_bytes" `Quick
      test_raw_clipboard_and_query_bytes;
    Alcotest.test_case "renderer_none_prints_without_painting" `Quick
      test_renderer_none_prints_without_painting;
    Alcotest.test_case "color_profile_override" `Quick test_color_profile_override;
    Alcotest.test_case "suspend_noop_on_remote_transport" `Quick
      test_suspend_is_a_noop_on_a_non_local_transport;
    Alcotest.test_case "resume_silent_on_custom_transport" `Quick
      test_resume_is_silent_on_a_custom_transport;
    Alcotest.test_case "sub_mouse_mode_gate" `Quick
      test_sub_mouse_follows_the_reported_mode;
    Alcotest.test_case "sub_focus_gate" `Quick test_sub_focus_follows_report_focus;
    Alcotest.test_case "sub_paste_gate" `Quick test_sub_paste_follows_bracketed_paste;
    Alcotest.test_case "sub_resize_startup_and_change" `Quick
      test_sub_resize_delivers_startup_and_change;
    Alcotest.test_case "cmd_window_size" `Quick
      test_window_size_redelivers_the_terminal_size;
    Alcotest.test_case "in_band_window_size_report" `Quick
      test_in_band_window_size_report_is_not_a_resize;
    Alcotest.test_case "key_release_split" `Quick
      test_key_press_and_release_use_separate_subscriptions;
    Alcotest.test_case "sub_resume_no_job_control" `Quick
      test_resume_subscription_is_silent_without_job_control;
  ]
  @ List.map query_case query_reports
