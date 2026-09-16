type 'msg script_event = 'msg Program.script_event

let run app ~events ~size =
  Eio_mock.Backend.run_full (fun env ->
      let output = Buffer.create 256 in
      let input = Eio.Flow.string_source "" in
      let sink = Eio.Flow.buffer_sink output in
      let terminal =
        Terminal.custom ~input ~output:sink
          ~size:(fun () -> size)
          ~on_resize:None
          ~env:(fun _ -> None)
          ~is_tty:false
      in
      let result =
        Program.run_core ~terminal ~fps:120
          ~filter:(fun _ message -> Some message)
          ~clock:env#clock
          ~now:(fun () -> Eio.Time.Mono.now env#mono_clock)
          ~exec:(fun _ -> invalid_arg "exec is unavailable in scripted tests")
          ~suspend:(fun () -> invalid_arg "suspend is unavailable in scripted tests")
          ~signals:false ~script:events app
      in
      match result with
      | Ok value -> value
      | Error `Interrupted -> invalid_arg "scripted Tea program was interrupted"
      | Error `Killed -> invalid_arg "scripted Tea program was killed"
      | Error (`Exn (exn, backtrace)) -> Printexc.raise_with_backtrace exn backtrace)
