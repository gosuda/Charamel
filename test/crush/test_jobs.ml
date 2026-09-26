module Jobs = Crush_core.Jobs
open Lwt_direct

let with_jobs f =
  Test_tools_test_support.with_scratch (fun root ->
      await
      @@ Lwt_switch.with_switch (fun sw ->
          let artifacts = Crush_core.Artifact.create ~fs_root:"/" ~dir:root in
          let jobs = Jobs.create ~sw ~artifacts in
          Lwt_direct.spawn (fun () -> f jobs)))

let output_case () =
  with_jobs (fun jobs ->
      let id =
        Jobs.start jobs ~cwd:"/tmp" ~command:"printf 'hello'; printf 'oops' >&2" ~env:[]
          ~timeout_s:30
      in
      match await (Jobs.output jobs ~id ~wait:true) with
      | Error (`Not_found missing) -> Alcotest.failf "job %s was not retained" missing
      | Ok (output, status) ->
          Alcotest.(check string) "merged output" "hellooops" output;
          Alcotest.(check bool)
            "completed" true
            (match status with Jobs.Exited 0 -> true | _ -> false))

let kill_case () =
  with_jobs (fun jobs ->
      let id = Jobs.start jobs ~cwd:"/tmp" ~command:"sleep 30" ~env:[] ~timeout_s:30 in
      Lwt_direct.yield ();
      (match await (Jobs.kill jobs ~id) with
      | Error (`Not_found missing) -> Alcotest.failf "job %s was not retained" missing
      | Ok () -> ());
      match await (Jobs.output jobs ~id ~wait:true) with
      | Error (`Not_found missing) ->
          Alcotest.failf "job %s was removed too early" missing
      | Ok (_output, status) ->
          Alcotest.(check bool)
            "killed" true
            (match status with Jobs.Killed -> true | _ -> false))

let timeout_case () =
  with_jobs (fun jobs ->
      let id = Jobs.start jobs ~cwd:"/tmp" ~command:"sleep 5" ~env:[] ~timeout_s:1 in
      match await (Jobs.output jobs ~id ~wait:true) with
      | Error (`Not_found missing) ->
          Alcotest.failf "job %s was removed too early" missing
      | Ok (_output, status) ->
          Alcotest.(check bool)
            "deadline kills job" true
            (match status with Jobs.Killed -> true | _ -> false))

(* The deadline owns the kill. A child that ignores SIGTERM proves the whole
   SIGTERM -> graceful window -> SIGKILL sequence ran and that the job finished at the
   deadline rather than at the child's natural exit — and that the shared exit promise
   survived the race: a poisoned await surfaces as [Lwt.Canceled] and a kill deferred
   behind the pipe drain would strand the job in [Running]. *)
let timeout_kills_a_term_immune_child_case () =
  with_jobs (fun jobs ->
      let started = Unix.gettimeofday () in
      let id =
        Jobs.start jobs ~cwd:"/tmp" ~command:"trap '' TERM; sleep 60" ~env:[] ~timeout_s:1
      in
      match await (Jobs.output jobs ~id ~wait:true) with
      | Error (`Not_found missing) ->
          Alcotest.failf "job %s was removed too early" missing
      | Ok (_output, status) ->
          let elapsed = Unix.gettimeofday () -. started in
          Alcotest.(check bool)
            "deadline killed the child" true
            (match status with Jobs.Killed -> true | _ -> false);
          Alcotest.(check bool)
            "SIGTERM was ignored for the whole graceful window" true
            (Float.compare elapsed 2.5 >= 0);
          Alcotest.(check bool)
            "the kill completed promptly" true
            (Float.compare elapsed 15. < 0))

let retention_case () =
  with_jobs (fun jobs ->
      let ids =
        List.init 130 (fun _ ->
            Jobs.start jobs ~cwd:"/tmp" ~command:"true" ~env:[] ~timeout_s:30)
      in
      List.iter
        (fun id ->
          match await (Jobs.output jobs ~id ~wait:true) with
          | Ok _ -> ()
          | Error (`Not_found _) -> ())
        ids;
      let retained = await (Jobs.list jobs) in
      Alcotest.(check bool)
        "completed history is bounded" true
        (List.length retained <= 128);
      Alcotest.(check bool)
        "latest job retained" true
        (List.exists (fun (id, _) -> id = List.hd (List.rev ids)) retained))

let cases =
  [
    Test_tools_test_support.case "captures stdout and stderr" `Quick output_case;
    Test_tools_test_support.case "kills process group" `Quick kill_case;
    Test_tools_test_support.case "deadline owns timeout" `Quick timeout_case;
    Test_tools_test_support.case "deadline kills a SIGTERM-immune child" `Quick
      timeout_kills_a_term_immune_child_case;
    Test_tools_test_support.case "bounds completed retention" `Quick retention_case;
  ]
