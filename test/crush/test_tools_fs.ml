module Tool = Crush_core.Tool
module Permission = Crush_core.Permission
module Tools_fs = Crush_core.Tools_fs
module Patch = Tools_fs.Patch
open Test_tools_test_support
open Lwt_direct

let with_context = Test_tools_test_support.with_context

let check_patch_operations () =
  let parse source =
    match Patch.parse source with
    | Ok patch -> patch
    | Error message -> Alcotest.failf "patch parse failed: %s" message
  in
  let range = parse "[file#0000]\nPUT 2.=2:\n+bee\n" in
  let changed, added, removed =
    match Patch.apply "a\nb\nc\n" range with
    | Ok value -> value
    | Error message -> Alcotest.failf "range apply failed: %s" message
  in
  Alcotest.(check string) "range replacement" "a\nbee\nc\n" changed;
  Alcotest.(check int) "range added" 1 added;
  Alcotest.(check int) "range removed" 1 removed;
  let inserts = parse "[file#0000]\nPUT <1:\n+zero\nPUT >3:\n+last\n" in
  let changed, _, _ =
    match Patch.apply "a\nb\nc\n" inserts with
    | Ok value -> value
    | Error message -> Alcotest.failf "insert apply failed: %s" message
  in
  Alcotest.(check string) "insert before and after" "zero\na\nb\nc\nlast\n" changed;
  let cut = parse "[file#0000]\nCUT 2.=2\n" in
  let changed, _, _ =
    match Patch.apply "a\nb\nc\n" cut with
    | Ok value -> value
    | Error message -> Alcotest.failf "cut apply failed: %s" message
  in
  Alcotest.(check string) "cut" "a\nc\n" changed;
  let empty_body = parse "[file#0000]\nPUT 2.=2:\n" in
  let changed, _, _ =
    match Patch.apply "a\nb\nc\n" empty_body with
    | Ok value -> value
    | Error message -> Alcotest.failf "empty body apply failed: %s" message
  in
  Alcotest.(check string) "empty body" "a\nc\n" changed

let check_patch_rejects_invalid_operations () =
  let parse source =
    match Patch.parse source with
    | Ok patch -> patch
    | Error message -> Alcotest.failf "patch parse failed: %s" message
  in
  let overlap = parse "[file#0000]\nPUT 1.=2:\n+x\nCUT 2.=3\n" in
  begin match Patch.apply "a\nb\nc\n" overlap with
  | Error message ->
      Alcotest.(check bool) "overlap reports an error" true (String.length message > 0)
  | Ok _ -> Alcotest.fail "overlapping operations were accepted"
  end;
  let out_of_bounds = parse "[file#0000]\nPUT 4.=4:\n+x\n" in
  begin match Patch.apply "a\nb\nc\n" out_of_bounds with
  | Error message ->
      Alcotest.(check bool) "bounds reports an error" true (String.length message > 0)
  | Ok _ -> Alcotest.fail "out-of-bounds operation was accepted"
  end;
  begin match Patch.parse "[file#0000]\nPUT <1:\n+x\nnot-an-operation\n" with
  | Error message ->
      Alcotest.(check bool)
        "malformed operation reports an error" true
        (String.length message > 0)
  | Ok _ -> Alcotest.fail "malformed operation was accepted"
  end

let check_read_ranges () =
  await @@ with_context
  @@ fun root context ->
  let file = Filename.concat root "numbers.txt" in
  write_file file "one\ntwo\nthree\nfour\nfive\n";
  let run path =
    run_tool Tools_fs.read context (json_object [ ("path", Jsont.Json.string path) ])
  in
  let from_two = (run (file ^ ":2")).Tool.content in
  Alcotest.(check bool)
    "N starts at the requested line" true
    (Test_support.contains ~needle:"2:two" ~haystack:from_two);
  Alcotest.(check bool)
    "N includes the final line" true
    (Test_support.contains ~needle:"5:five" ~haystack:from_two);
  let span = (run (file ^ ":2-3")).Tool.content in
  Alcotest.(check bool)
    "N-M includes both bounds" true
    (Test_support.contains ~needle:"2:two\n3:three" ~haystack:span);
  let count = (run (file ^ ":2+2")).Tool.content in
  Alcotest.(check bool)
    "N+K has K lines" true
    (Test_support.contains ~needle:"2:two\n3:three" ~haystack:count);
  Alcotest.(check bool)
    "N+K stops at its count" false
    (Test_support.contains ~needle:"4:four" ~haystack:count);
  let last = (run (file ^ ":-2")).Tool.content in
  Alcotest.(check bool)
    "-K selects the tail" true
    (Test_support.contains ~needle:"4:four\n5:five" ~haystack:last);
  begin match
    Tools_fs.read.Tool.run context
      (json_object [ ("path", Jsont.Json.string (file ^ ":0")) ])
  with
  | Error (`Invalid_input _) -> ()
  | Error error -> Alcotest.failf "wrong zero-range error: %a" Tool.pp_error error
  | Ok _ -> Alcotest.fail "zero range was accepted"
  end

let check_binary_and_spill () =
  await @@ with_context
  @@ fun root context ->
  let binary_file = Filename.concat root "binary.bin" in
  write_file binary_file "ok\000bad";
  let binary_output =
    run_tool Tools_fs.read context
      (json_object [ ("path", Jsont.Json.string binary_file) ])
  in
  Alcotest.(check bool) "binary read is rejected" true binary_output.Tool.is_error;
  let rows = List.init 500 (fun _ -> String.make 400 'x') in
  let large_file = Filename.concat root "large.txt" in
  write_file large_file (String.concat "\n" rows);
  let large_output =
    run_tool Tools_fs.read context
      (json_object [ ("path", Jsont.Json.string large_file) ])
  in
  Alcotest.(check bool)
    "large output has an artifact" true
    (Option.is_some large_output.Tool.artifact)

let check_stale_tag_and_atomicity () =
  await @@ with_context
  @@ fun root context ->
  let file = Filename.concat root "edit.txt" in
  write_file file "one\ntwo\nthree\n";
  let original = load_file file in
  let old_tag = Crush_core.Hashline.tag original in
  write_file file "changed\ntwo\nthree\n";
  let stale_patch = Fmt.str "[%s#%s]\nPUT 1.=1:\n+ONE\n" file old_tag in
  let stale =
    run_tool Tools_fs.edit context
      (json_object [ ("patch", Jsont.Json.string stale_patch) ])
  in
  Alcotest.(check bool) "stale tag is an error output" true stale.Tool.is_error;
  Alcotest.(check bool)
    "stale output reports current tag" true
    (Test_support.contains
       ~needle:(Crush_core.Hashline.tag (load_file file))
       ~haystack:stale.Tool.content);
  Alcotest.(check string)
    "stale edit leaves file unchanged" "changed\ntwo\nthree\n" (load_file file);
  let current = load_file file in
  let tag = Crush_core.Hashline.tag current in
  let invalid_patch = Fmt.str "[%s#%s]\nPUT 1.=1:\n+ONE\nCUT 99.=99\n" file tag in
  begin match
    Tools_fs.edit.Tool.run context
      (json_object [ ("patch", Jsont.Json.string invalid_patch) ])
  with
  | Error (`Invalid_input _) -> ()
  | Error error -> Alcotest.failf "wrong atomicity error: %a" Tool.pp_error error
  | Ok _ -> Alcotest.fail "invalid multi-operation patch was accepted"
  end;
  Alcotest.(check string) "invalid patch leaves file unchanged" current (load_file file)

let check_symlink_plan_confinement () =
  await
  @@ with_context ~allowed_tools:[ "write" ]
  @@ fun root context ->
  let plans = Filename.concat root ".crush/plans" in
  let outside = Filename.concat root "outside.txt" in
  let link = Filename.concat plans "link.txt" in
  mkdir_p plans;
  write_file outside "outside";
  Unix.symlink outside link;
  Permission.set_plan_mode context.Tool.permission true;
  begin match
    Tools_fs.write.Tool.run context
      (json_object
         [ ("path", Jsont.Json.string link); ("content", Jsont.Json.string "changed") ])
  with
  | Error (`Denied message) ->
      Alcotest.(check bool)
        "symlink escape is denied by plan mode" true
        (Test_support.contains ~needle:"plan mode" ~haystack:message)
  | Error error -> Alcotest.failf "wrong symlink error: %a" Tool.pp_error error
  | Ok _ -> Alcotest.fail "symlink escape was accepted"
  end;
  Alcotest.(check string) "symlink target is unchanged" "outside" (load_file outside)

let check_write_guard_and_nested_create () =
  await @@ with_context
  @@ fun root context ->
  let nested = Filename.concat root "a/b/new.txt" in
  let created =
    run_tool Tools_fs.write context
      (json_object
         [
           ("path", Jsont.Json.string nested); ("content", Jsont.Json.string "new content");
         ])
  in
  Alcotest.(check bool) "nested write succeeds" false created.Tool.is_error;
  Alcotest.(check string) "nested write creates parents" "new content" (load_file nested);
  let existing = Filename.concat root "existing.txt" in
  write_file existing "old";
  let refused =
    run_tool Tools_fs.write context
      (json_object
         [ ("path", Jsont.Json.string existing); ("content", Jsont.Json.string "new") ])
  in
  Alcotest.(check bool) "unread existing write is refused" true refused.Tool.is_error;
  Alcotest.(check string) "refused write leaves file unchanged" "old" (load_file existing)

let check_registered_tools () =
  let tools = [ Tools_fs.read; Tools_fs.write; Tools_fs.edit ] in
  Alcotest.(check (list string))
    "filesystem tool names" [ "read"; "write"; "edit" ]
    (List.map (fun (tool : Tool.t) -> tool.Tool.name) tools);
  Alcotest.(check bool) "read scheduling metadata" true Tools_fs.read.Tool.read_only;
  Alcotest.(check bool) "write scheduling metadata" false Tools_fs.write.Tool.read_only;
  Alcotest.(check bool) "edit scheduling metadata" false Tools_fs.edit.Tool.read_only;
  List.iter
    (fun (tool : Tool.t) ->
      match tool.Tool.schema with
      | Jsont.Object _ -> ()
      | _ -> Alcotest.failf "%s schema is not an object" tool.Tool.name)
    tools

let cases =
  [
    case "patch operations" `Quick check_patch_operations;
    case "patch validation" `Quick check_patch_rejects_invalid_operations;
    case "read ranges" `Quick check_read_ranges;
    case "binary and spilled reads" `Quick check_binary_and_spill;
    case "stale tags and atomic edits" `Quick check_stale_tag_and_atomicity;
    case "symlink plan confinement" `Quick check_symlink_plan_confinement;
    case "write guards and nested paths" `Quick check_write_guard_and_nested_create;
    case "registered tools" `Quick check_registered_tools;
  ]
