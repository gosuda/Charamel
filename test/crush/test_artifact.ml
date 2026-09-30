module Artifact = Crush_core.Artifact
open Lwt_direct

let with_artifact f =
  Test_tools_test_support.with_scratch (fun root ->
      f root (Artifact.create ~fs_root:root ~dir:"artifacts"))

let random_source = Test_tools_test_support.random_source
let line number = Fmt.str "line-%03d %s" number (String.make 700 'x')

let threshold_is_inline () =
  with_artifact (fun _ store ->
      let contents = String.make Artifact.max_inline_bytes 'x' in
      let preview, id =
        await @@ Artifact.truncate store ~random:(random_source ()) contents
      in
      Alcotest.(check string) "exact threshold is unchanged" contents preview;
      Alcotest.(check bool) "exact threshold has no artifact" true (Option.is_none id))

let spills_complete_content () =
  with_artifact (fun root store ->
      let contents = String.concat "\n" (List.init 100 line) ^ "\n" in
      let preview, id =
        await
        @@ Artifact.truncate store
             ~random:(fun length -> String.init length Char.chr)
             contents
      in
      let id =
        match id with Some id -> id | None -> Alcotest.fail "large output did not spill"
      in
      Alcotest.(check bool)
        "head retained" true
        (Test_support.contains ~needle:"line-000" ~haystack:preview);
      Alcotest.(check bool)
        "tail retained" true
        (Test_support.contains ~needle:"line-099" ~haystack:preview);
      Alcotest.(check bool)
        "middle omitted" false
        (Test_support.contains ~needle:"line-050" ~haystack:preview);
      Alcotest.(check bool)
        "artifact reference" true
        (Test_support.contains ~needle:("artifact://" ^ id) ~haystack:preview);
      begin match await (Artifact.load store ~id) with
      | Error (`Not_found path) -> Alcotest.failf "artifact missing: %s" path
      | Error (`Io (path, message)) -> Alcotest.failf "artifact read %s: %s" path message
      | Ok saved -> Alcotest.(check string) "full content preserved" contents saved
      end;
      let target = Filename.concat (Filename.concat root "artifacts") (id ^ ".txt") in
      Alcotest.(check int)
        "private artifact mode"
        (if Sys.win32 then 0o666 else 0o600)
        ((Unix.lstat target).Unix.st_perm land 0o777))

let long_line_spill_is_bounded () =
  with_artifact (fun _ store ->
      let contents = String.make (Artifact.max_inline_bytes + 4096) 'x' in
      let preview, id =
        await
        @@ Artifact.truncate store
             ~random:(fun length -> String.make length '\000')
             contents
      in
      let id =
        match id with Some id -> id | None -> Alcotest.fail "long line did not spill"
      in
      Alcotest.(check bool)
        "long-line preview is bounded" true
        (String.length preview <= Artifact.max_inline_bytes);
      Alcotest.(check bool)
        "long-line preview has reference" true
        (Test_support.contains ~needle:("artifact://" ^ id) ~haystack:preview);
      match await (Artifact.load store ~id) with
      | Error (`Not_found path) -> Alcotest.failf "long-line artifact missing: %s" path
      | Error (`Io (path, message)) ->
          Alcotest.failf "long-line artifact read %s: %s" path message
      | Ok saved -> Alcotest.(check string) "long-line bytes preserved" contents saved)

let repeated_emoji count =
  let buffer = Buffer.create (count * 4) in
  for _index = 1 to count do
    Buffer.add_string buffer "\xF0\x9F\x98\x80"
  done;
  Buffer.contents buffer

let utf8_preview_keeps_boundaries () =
  with_artifact (fun _ store ->
      let contents = repeated_emoji ((Artifact.max_inline_bytes / 4) + 1024) in
      let preview, id =
        await
        @@ Artifact.truncate store
             ~random:(fun length -> String.make length '\003')
             contents
      in
      let id =
        match id with Some id -> id | None -> Alcotest.fail "UTF-8 output did not spill"
      in
      Alcotest.(check bool)
        "UTF-8 preview is bounded" true
        (String.length preview <= Artifact.max_inline_bytes);
      Alcotest.(check bool)
        "UTF-8 preview remains valid" true
        (String.is_valid_utf_8 preview);
      Alcotest.(check bool)
        "UTF-8 preview has reference" true
        (Test_support.contains ~needle:("artifact://" ^ id) ~haystack:preview);
      match await (Artifact.load store ~id) with
      | Error (`Not_found path) -> Alcotest.failf "UTF-8 artifact missing: %s" path
      | Error (`Io (path, message)) ->
          Alcotest.failf "UTF-8 artifact read %s: %s" path message
      | Ok saved -> Alcotest.(check string) "UTF-8 bytes preserved" contents saved)

let missing_and_invalid_ids () =
  with_artifact (fun _ store ->
      begin match await (Artifact.load store ~id:"../escape") with
      | Error (`Not_found _) -> ()
      | Ok _ -> Alcotest.fail "invalid artifact id escaped its directory"
      | Error (`Io (path, message)) ->
          Alcotest.failf "invalid id I/O error %s: %s" path message
      end;
      begin match await (Artifact.load store ~id:"art-ffffffff") with
      | Error (`Not_found _) -> ()
      | Ok _ -> Alcotest.fail "missing artifact unexpectedly loaded"
      | Error (`Io (path, message)) ->
          Alcotest.failf "missing artifact I/O error %s: %s" path message
      end)

let concurrent_saves_are_unique () =
  with_artifact (fun _ store ->
      let ids =
        await
        @@ Lwt_list.map_p
             (fun index ->
               Lwt.map
                 (function
                   | Ok id -> id
                   | Error (`Io (path, message)) ->
                       Alcotest.failf "save %s: %s" path message)
                 (Artifact.save store
                    ~random:(fun _ ->
                      String.init 4 (fun i -> Char.chr ((index + i) land 0xFF)))
                    (Fmt.str "value-%d" index)))
             (List.init 32 Fun.id)
      in
      let unique = List.sort_uniq String.compare ids in
      Alcotest.(check int) "one artifact per save" 32 (List.length unique))

let permission_boundary () =
  with_artifact (fun root store ->
      (* Windows does not gate directory writes on the read-only attribute, so
         the chmod boundary the POSIX branch asserts does not exist there. *)
      if Sys.win32 || Unix.getuid () = 0 then ()
      else
        let directory = Filename.concat root "artifacts" in
        begin match
          await
          @@ Artifact.save store
               ~random:(fun length -> String.make length '\001')
               "initial"
        with
        | Error (`Io (path, message)) -> Alcotest.failf "initial save %s: %s" path message
        | Ok _ -> ()
        end;
        Unix.chmod directory 0o500;
        begin match
          await
          @@ Artifact.save store
               ~random:(fun length -> String.make length '\002')
               "blocked"
        with
        | Error (`Io _) -> ()
        | Ok _ -> Alcotest.fail "artifact write ignored directory permissions"
        end;
        Unix.chmod directory 0o700)

let cases =
  [
    Test_tools_test_support.case "inline threshold" `Quick threshold_is_inline;
    Test_tools_test_support.case "spill keeps complete output" `Quick
      spills_complete_content;
    Test_tools_test_support.case "long line preview is bounded" `Quick
      long_line_spill_is_bounded;
    Test_tools_test_support.case "UTF-8 preview keeps boundaries" `Quick
      utf8_preview_keeps_boundaries;
    Test_tools_test_support.case "invalid and missing ids" `Quick missing_and_invalid_ids;
    Test_tools_test_support.case "concurrent saves use unique ids" `Quick
      concurrent_saves_are_unique;
    Test_tools_test_support.case "directory permissions are enforced" `Quick
      permission_boundary;
  ]
