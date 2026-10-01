module Config = Crush_core.Config
module Permission = Crush_core.Permission
open Lwt_direct

let request ?(session = "session") ?(tool = "read") ?(action = "read")
    ?(path = "/project/file") ?(description = "test request") ?(read_only = true) () :
    Permission.request =
  { session; tool; action; path; description; read_only }

let policy ?(cwd = "/project") ?(plans_dir = "/project/.crush/plans") ?(yolo = false)
    ?(allowed_tools = []) ?(deny = []) ?asker ?hook ?on_decision () =
  let config : Config.permissions = { allowed_tools; deny } in
  Permission.create ~config ~yolo ?asker ?hook ?on_decision ~cwd ~plans_dir ()

let allowed = function Permission.Allowed -> true | Permission.Denied _ -> false

let denied_reason = function
  | Permission.Allowed -> None
  | Permission.Denied reason -> Some reason

let check_allowed name outcome = Alcotest.check Alcotest.bool name true (allowed outcome)
let check_denied name outcome = Alcotest.check Alcotest.bool name false (allowed outcome)

let check_reason name expected outcome =
  Alcotest.check (Alcotest.option Alcotest.string) name expected (denied_reason outcome)

let plan_ceiling () =
  let p = policy ~asker:(fun _ -> Permission.Allow_once) () in
  Permission.set_plan_mode p true;
  let outside =
    request ~tool:"edit" ~action:"edit" ~path:"/project/source.ml" ~read_only:false ()
  in
  check_reason "edit outside plans is denied by plan ceiling"
    (Some
       "plan mode: writes are limited to /project/.crush/plans; run /propose to leave \
        plan mode")
    (Permission.resolve p outside);
  let inside =
    request ~tool:"edit" ~action:"edit" ~path:"/project/.crush/plans/plan.md"
      ~read_only:false ()
  in
  check_allowed "known edit target inside plans reaches later policy"
    (Permission.resolve p inside)

let plan_unknown_mutations () =
  let p = policy ~asker:(fun _ -> Permission.Allow_once) () in
  Permission.set_plan_mode p true;
  let missing = request ~tool:"write" ~action:"write" ~path:"" ~read_only:false () in
  check_denied "write without a complete target is denied" (Permission.resolve p missing);
  List.iter
    (fun tool ->
      let request =
        request ~tool ~action:"run" ~path:"/project/.crush/plans/claimed-target"
          ~read_only:false ()
      in
      check_denied
        (tool ^ " has an unknown mutation target")
        (Permission.resolve p request))
    [ "bash"; "mcp_server_tool"; "agent"; "job_kill"; "lsp_rename" ]

let yolo_and_plan_order () =
  let p = policy ~yolo:true ~deny:[ "edit" ] ~asker:(fun _ -> Permission.Deny) () in
  let ordinary =
    request ~tool:"edit" ~action:"edit" ~path:"/project/source.ml" ~read_only:false ()
  in
  check_allowed "yolo overrides ordinary configured deny" (Permission.resolve p ordinary);
  Permission.set_plan_mode p true;
  check_denied "yolo cannot override plan ceiling" (Permission.resolve p ordinary);
  let planned =
    request ~tool:"edit" ~action:"edit" ~path:"/project/.crush/plans/p.md"
      ~read_only:false ()
  in
  check_allowed "yolo allows a known planned target" (Permission.resolve p planned)

let deny_before_allow_and_readonly () =
  let p = policy ~allowed_tools:[ "read" ] ~deny:[ "read" ] () in
  let local =
    request ~tool:"read" ~action:"read" ~path:"/project/README.md" ~read_only:true ()
  in
  check_reason "configured deny beats configured allow and local auto approval"
    (Some "denied by permissions.deny: read") (Permission.resolve p local)

let config_entries_are_exact () =
  let p = policy ~allowed_tools:[ "bash:git status --short" ] () in
  let exact =
    request ~tool:"bash" ~action:"git status --short" ~path:"/outside" ~read_only:false ()
  in
  check_allowed "configured action entry matches the whole action"
    (Permission.resolve p exact);
  let widened =
    request ~tool:"bash" ~action:"git status --short --porcelain" ~path:"/outside"
      ~read_only:false ()
  in
  check_denied "configured action entry does not widen to later arguments"
    (Permission.resolve p widened);
  let whole_tool = policy ~allowed_tools:[ "bash" ] () in
  check_allowed "tool-only entry allows every action"
    (Permission.resolve whole_tool widened)

let session_grants_are_exact () =
  let p = policy () in
  let original =
    request ~tool:"bash" ~action:"git status --short" ~path:"/project" ~read_only:false ()
  in
  Permission.grant_session p original;
  check_allowed "exact session grant is reused" (Permission.resolve p original);
  let changed_action =
    request ~tool:"bash" ~action:"git status" ~path:"/project" ~read_only:false ()
  in
  check_denied "session grant does not widen the action"
    (Permission.resolve p changed_action);
  let changed_path =
    request ~tool:"bash" ~action:"git status --short" ~path:"/project/subdir"
      ~read_only:false ()
  in
  check_denied "session grant does not widen the path" (Permission.resolve p changed_path);
  let changed_session =
    request ~session:"other-session" ~tool:"bash" ~action:"git status --short"
      ~path:"/project" ~read_only:false ()
  in
  check_denied "session grant does not cross session boundaries"
    (Permission.resolve p changed_session);
  Permission.auto_approve_session p ~session:"session";
  check_allowed "explicit session-wide approval covers a different request"
    (Permission.resolve p changed_path)

let asker_session_grant_is_exact () =
  let calls = ref 0 in
  let p =
    policy
      ~asker:(fun _ ->
        incr calls;
        Permission.Allow_session)
      ()
  in
  let original =
    request ~tool:"bash" ~action:"git status --short" ~path:"/outside" ~read_only:false ()
  in
  check_allowed "asker allow-session resolves the original request"
    (Permission.resolve p original);
  check_allowed "asker grant is reused for the exact request"
    (Permission.resolve p original);
  Alcotest.check Alcotest.int "asker is called once for an exact grant" 1 !calls;
  let widened =
    request ~tool:"bash" ~action:"git status --short --porcelain" ~path:"/outside"
      ~read_only:false ()
  in
  check_allowed "a changed request is independently resolved"
    (Permission.resolve p widened);
  Alcotest.check Alcotest.int "changed request asks separately" 2 !calls

let deny_beats_session_approval () =
  let p = policy ~deny:[ "read" ] () in
  Permission.auto_approve_session p ~session:"session";
  let req =
    request ~tool:"read" ~action:"read" ~path:"/project/file" ~read_only:true ()
  in
  check_reason "configured deny beats session-wide approval"
    (Some "denied by permissions.deny: read") (Permission.resolve p req)

let hook_precedes_local_autoapproval () =
  let called = ref 0 in
  let p =
    policy
      ~hook:(fun _ ->
        incr called;
        `Deny "blocked by hook")
      ()
  in
  let local =
    request ~tool:"read" ~action:"read" ~path:"/project/file" ~read_only:true ()
  in
  check_reason "hook denial beats local read-only approval" (Some "blocked by hook")
    (Permission.resolve p local);
  Alcotest.check Alcotest.int "hook called once" 1 !called

let readonly_local_autoapproval () =
  let p = policy () in
  let local = request ~path:"/project/src/file.ml" ~read_only:true () in
  check_allowed "read-only target inside cwd is automatic" (Permission.resolve p local);
  let outside = request ~path:"/other/file.ml" ~read_only:true () in
  check_reason "read-only target outside cwd asks and default asker denies"
    (Some "denied by user")
    (Permission.resolve p outside)

let readonly_skips_plan_ceiling_but_not_asker () =
  let p = policy () in
  Permission.set_plan_mode p true;
  let unknown =
    request ~tool:"mcp_server_tool" ~action:"inspect" ~path:"" ~read_only:true ()
  in
  check_reason "read-only unknown request is not mistaken for a plan mutation"
    (Some "denied by user")
    (Permission.resolve p unknown)

let sibling_prefix_is_not_contained () =
  let p =
    policy ~cwd:"/work" ~plans_dir:"/work/plans"
      ~asker:(fun _ -> Permission.Allow_once)
      ()
  in
  Permission.set_plan_mode p true;
  let sibling =
    request ~tool:"write" ~action:"write" ~path:"/work/plans-old/result" ~read_only:false
      ()
  in
  check_denied "sibling directory with a shared string prefix is outside plans"
    (Permission.resolve p sibling)

let observability_runs_after_unlock () =
  let policy_ref = ref None in
  let count = ref 0 in
  let observe request _outcome =
    incr count;
    match !policy_ref with None -> () | Some p -> Permission.grant_session p request
  in
  let p = policy ~asker:(fun _ -> Permission.Allow_once) ~on_decision:observe () in
  policy_ref := Some p;
  let req = request ~tool:"read" ~action:"read" ~path:"/outside" ~read_only:true () in
  check_allowed "first decision reaches asker" (Permission.resolve p req);
  Alcotest.check Alcotest.int "observer runs exactly once for first resolution" 1 !count;
  check_allowed "observer can mutate policy after resolution" (Permission.resolve p req);
  Alcotest.check Alcotest.int "observer runs once per actual resolution" 2 !count

let policy_lock_is_free_while_asking () =
  let started, started_resolver = Lwt.wait () in
  let release, release_resolver = Lwt.wait () in
  let p =
    policy
      ~asker:(fun _ ->
        Lwt.wakeup_later started_resolver ();
        await release;
        Permission.Allow_once)
      ()
  in
  let req =
    request ~tool:"edit" ~action:"edit" ~path:"/project/source.ml" ~read_only:false ()
  in
  let result = ref None in
  let resolving =
    Lwt_direct.spawn (fun () -> result := Some (Permission.resolve p req))
  in
  let observing =
    Lwt_direct.spawn (fun () ->
        await started;
        Fun.protect
          (fun () ->
            Alcotest.check Alcotest.bool "plan getter remains live while asker waits"
              false (Permission.plan_mode p);
            Permission.set_plan_mode p true;
            Alcotest.check Alcotest.bool "plan mode changes while asker waits" true
              (Permission.plan_mode p))
          ~finally:(fun () -> Lwt.wakeup_later release_resolver ()))
  in
  await (Lwt.join [ resolving; observing ]);
  match !result with
  | None -> Alcotest.fail "permission resolution did not finish"
  | Some outcome ->
      check_reason "answer is rechecked against the live plan ceiling"
        (Some
           "plan mode: writes are limited to /project/.crush/plans; run /propose to \
            leave plan mode")
        outcome

let matches_cases () =
  let req = request ~tool:"bash" ~action:"git status --short" () in
  Alcotest.check Alcotest.bool "exact action match" true
    (Permission.matches ~entry:"bash:git status --short" req);
  Alcotest.check Alcotest.bool "different action does not match" false
    (Permission.matches ~entry:"bash:git status" req);
  Alcotest.check Alcotest.bool "tool-only match" true
    (Permission.matches ~entry:"bash" req);
  Alcotest.check Alcotest.bool "different tool does not match" false
    (Permission.matches ~entry:"git" req)

let cases =
  [
    Test_tools_test_support.case "plan ceiling" `Quick plan_ceiling;
    Test_tools_test_support.case "unknown mutation targets" `Quick plan_unknown_mutations;
    Test_tools_test_support.case "yolo and plan order" `Quick yolo_and_plan_order;
    Test_tools_test_support.case "deny before allow" `Quick deny_before_allow_and_readonly;
    Test_tools_test_support.case "deny before session approval" `Quick
      deny_beats_session_approval;
    Test_tools_test_support.case "exact config entries" `Quick config_entries_are_exact;
    Test_tools_test_support.case "exact session grants" `Quick session_grants_are_exact;
    Test_tools_test_support.case "asker session grant" `Quick asker_session_grant_is_exact;
    Test_tools_test_support.case "hook order" `Quick hook_precedes_local_autoapproval;
    Test_tools_test_support.case "local read-only approval" `Quick
      readonly_local_autoapproval;
    Test_tools_test_support.case "read-only plan behavior" `Quick
      readonly_skips_plan_ceiling_but_not_asker;
    Test_tools_test_support.case "component-aware containment" `Quick
      sibling_prefix_is_not_contained;
    Test_tools_test_support.case "decision observability" `Quick
      observability_runs_after_unlock;
    Test_tools_test_support.case "policy lock while asking" `Quick
      policy_lock_is_free_while_asking;
    Test_tools_test_support.case "entry matching" `Quick matches_cases;
  ]
