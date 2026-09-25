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

let open_input =
  let module Open = struct
    type t = unit

    let read_methods = []
    let single_read () _ = Eio.Promise.await (fst (Eio.Promise.create ()))
  end in
  let ops = Eio.Flow.Pi.source (module Open) in
  Eio.Resource.T ((), ops)

let rendered_bytes app =
  Eio_main.run (fun env ->
      let output = Buffer.create 256 in
      let terminal =
        Terminal.custom ~input:open_input ~output:(Eio.Flow.buffer_sink output)
          ~size:(fun () -> (4, 20))
          ~on_resize:None
          ~env:(fun _ -> None)
          ~is_tty:false
      in
      match run ~terminal ~fps:120 ~clock:env#clock app env with
      | Ok _ -> Buffer.contents output
      | Error _ -> Alcotest.fail "program did not finish normally")

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
  ]
