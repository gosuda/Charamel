open Lwt.Syntax

let executable () =
  let test_dir = Filename.dirname (Unix.realpath Sys.executable_name) in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "examples/bubbletea_examples.exe"

let cli ?env ?cwd ?timeout ?stdin args =
  Test_support.run_cli ?env ?cwd ?timeout ?stdin ~exe:(executable ()) args

(* A Windows child writes CRLF to its pipe, so the [\r] goes with the line. *)
let lines text =
  text |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (fun line -> line <> "")

let registry () =
  let* status, listed, stderr = cli [ "--list" ] in
  Alcotest.(check int) ("list status " ^ stderr) 0 status;
  let names = lines listed in
  Alcotest.(check int) "count" 63 (List.length names);
  Alcotest.(check (list string)) "names" Bubbletea.Example_names.all names;
  Lwt.return_unit

let smoke_case name =
  Alcotest_lwt.test_case name `Quick (fun _ () ->
      let* status, stdout, stderr = cli [ "--smoke"; name ] in
      if status <> 0 then Alcotest.failf "status %d\n%s%s" status stdout stderr;
      if not (Test_support.contains ~needle:"ok" ~haystack:stdout) then
        Alcotest.failf "no ok line:\n%s%s" stdout stderr;
      Lwt.return_unit)

let interactive_quit () =
  let* status, _, stderr = cli ~stdin:"q" ~timeout:10. [ "simple" ] in
  Alcotest.(check int) ("status " ^ stderr) 0 status;
  Lwt.return_unit

let unknown_name () =
  let* status, _, _ = cli [ "no-such-example" ] in
  Alcotest.(check int) "status" 2 status;
  Lwt.return_unit

let () =
  Test_support.run_lwt "examples"
    [
      ( "dispatch",
        [
          Alcotest_lwt.test_case "registry" `Quick (fun _ () -> registry ());
          Alcotest_lwt.test_case "interactive quit" `Quick (fun _ () ->
              interactive_quit ());
          Alcotest_lwt.test_case "unknown name" `Quick (fun _ () -> unknown_name ());
        ] );
      ("smoke", List.map smoke_case Bubbletea.Example_names.all);
    ]
