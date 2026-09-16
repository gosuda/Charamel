module Jobs = Crush_core.Jobs

let with_jobs f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let artifacts = Crush_core.Artifact.create ~fs:env#fs ~dir:"/tmp" in
  let jobs = Jobs.create ~sw ~proc_mgr:env#process_mgr ~clock:env#clock ~artifacts in
  f env jobs

let output_case () =
  with_jobs (fun _env jobs ->
      let id =
        Jobs.start jobs ~cwd:"/tmp" ~command:"printf 'hello'; printf 'oops' >&2" ~env:[]
          ~timeout_s:30
      in
      match Jobs.output jobs ~id ~wait:true with
      | Error (`Not_found missing) -> Alcotest.failf "job %s was not retained" missing
      | Ok (output, status) ->
          Alcotest.(check string) "merged output" "hellooops" output;
          Alcotest.(check bool)
            "completed" true
            (match status with Jobs.Exited 0 -> true | _ -> false))

let kill_case () =
  with_jobs (fun _env jobs ->
      let id = Jobs.start jobs ~cwd:"/tmp" ~command:"sleep 30" ~env:[] ~timeout_s:30 in
      Eio.Fiber.yield ();
      (match Jobs.kill jobs ~id with
      | Error (`Not_found missing) -> Alcotest.failf "job %s was not retained" missing
      | Ok () -> ());
      match Jobs.output jobs ~id ~wait:true with
      | Error (`Not_found missing) ->
          Alcotest.failf "job %s was removed too early" missing
      | Ok (_output, status) ->
          Alcotest.(check bool)
            "killed" true
            (match status with Jobs.Killed -> true | _ -> false))

let timeout_case () =
  with_jobs (fun _env jobs ->
      let id = Jobs.start jobs ~cwd:"/tmp" ~command:"sleep 5" ~env:[] ~timeout_s:1 in
      match Jobs.output jobs ~id ~wait:true with
      | Error (`Not_found missing) ->
          Alcotest.failf "job %s was removed too early" missing
      | Ok (_output, status) ->
          Alcotest.(check bool)
            "deadline kills job" true
            (match status with Jobs.Killed -> true | _ -> false))

let retention_case () =
  with_jobs (fun _env jobs ->
      let ids =
        List.init 130 (fun _ ->
            Jobs.start jobs ~cwd:"/tmp" ~command:"true" ~env:[] ~timeout_s:30)
      in
      List.iter
        (fun id ->
          match Jobs.output jobs ~id ~wait:true with
          | Ok _ -> ()
          | Error (`Not_found _) -> ())
        ids;
      let retained = Jobs.list jobs in
      Alcotest.(check bool)
        "completed history is bounded" true
        (List.length retained <= 128);
      Alcotest.(check bool)
        "latest job retained" true
        (List.exists (fun (id, _) -> id = List.hd (List.rev ids)) retained))

let cases =
  [
    Alcotest.test_case "captures stdout and stderr" `Quick output_case;
    Alcotest.test_case "kills process group" `Quick kill_case;
    Alcotest.test_case "deadline owns timeout" `Quick timeout_case;
    Alcotest.test_case "bounds completed retention" `Quick retention_case;
  ]
