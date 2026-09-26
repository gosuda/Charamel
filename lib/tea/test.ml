type 'msg script_event = 'msg Program.script_event

let stamp clock () =
  Mtime.of_uint64_ns (Int64.of_float (Charamel_os.Time.now clock *. 1e9))

let rec step virtual_clock advance clock promise attempts =
  match Lwt.state promise with
  | Lwt.Return result -> result
  | Lwt.Fail exn -> raise exn
  | Lwt.Sleep -> (
      Lwt.wakeup_paused ();
      match Charamel_os.Time.next_deadline virtual_clock with
      | Some deadline ->
          advance (deadline -. Charamel_os.Time.now clock);
          step virtual_clock advance clock promise 0
      | None ->
          if attempts >= 100_000 then failwith "scripted Tea program made no progress"
          else step virtual_clock advance clock promise (attempts + 1))

let run ?output app ~events ~size =
  let virtual_clock, advance = Charamel_os.Time.create_virtual () in
  let clock = Charamel_os.Time.of_virtual virtual_clock in
  let terminal =
    Terminal.custom
      ~input:(Charamel_os.Console_input.blocked ())
      ~output:(Option.value output ~default:Lwt_io.null)
      ~size:(fun () -> size)
      ~on_resize:None
      ~env:(fun _ -> None)
      ~is_tty:false
  in
  let promise =
    Program.run_core ~terminal ~fps:120
      ~filter:(fun _ message -> Some message)
      ~clock ~now:(stamp clock)
      ~exec:(fun _ -> invalid_arg "exec is unavailable in scripted tests")
      ~suspend:(fun () -> invalid_arg "suspend is unavailable in scripted tests")
      ~signals:false ~script:events app
  in
  match step virtual_clock advance clock promise 0 with
  | Ok value -> value
  | Error `Interrupted -> invalid_arg "scripted Tea program was interrupted"
  | Error `Killed -> invalid_arg "scripted Tea program was killed"
  | Error (`Exn (exn, backtrace)) -> Printexc.raise_with_backtrace exn backtrace
