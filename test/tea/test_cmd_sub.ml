(* Cmd and Sub builder tests: building a command or subscription runs no
   thunk and applies no mapping function; only the runtime does that. *)

open Charamel_tea

let test_perform_defers_thunk () =
  let runs = ref 0 in
  ignore (Cmd.perform (fun () -> incr runs));
  Alcotest.(check int) "perform evaluations before dispatch" 0 !runs

let test_after_defers_thunk () =
  let runs = ref 0 in
  ignore (Cmd.after 0.5 (fun () -> incr runs));
  Alcotest.(check int) "after evaluations before dispatch" 0 !runs

let test_cmd_map_defers_mapping () =
  let runs = ref 0 in
  ignore (Cmd.map (fun () -> incr runs) (Cmd.msg ()));
  Alcotest.(check int) "map applications before dispatch" 0 !runs

let test_sub_map_defers_mapping () =
  let runs = ref 0 in
  ignore (Sub.map (fun () -> incr runs) (Sub.key (fun _ -> ())));
  Alcotest.(check int) "map applications before dispatch" 0 !runs

let test_every_defers_handler () =
  let runs = ref 0 in
  ignore (Sub.every 1.0 (fun _ -> incr runs));
  Alcotest.(check int) "handler invocations before dispatch" 0 !runs

let cases =
  [
    ("cmd.perform defers its thunk", `Quick, test_perform_defers_thunk);
    ("cmd.after defers its thunk", `Quick, test_after_defers_thunk);
    ("cmd.map defers its mapping function", `Quick, test_cmd_map_defers_mapping);
    ("sub.map defers its mapping function", `Quick, test_sub_map_defers_mapping);
    ("sub.every defers its handler", `Quick, test_every_defers_handler);
  ]
