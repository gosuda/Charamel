(* Cmd and Sub builder tests: building a command or subscription runs no
   thunk and applies no mapping function; only the runtime does that. *)

val cases : unit Alcotest.test_case list
(** The Cmd and Sub builder test cases. *)
