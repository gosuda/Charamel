(* Assumes bin/hotdiva2000's [Words]/[Name] modules are exposed to this test
   stanza wrapped as the internal library [Hotdiva_core] (modules
   [Hotdiva_core.Words], [Hotdiva_core.Name]), shared by the app executable
   and this test executable; [open Hotdiva_core] below brings [Words]/[Name]
   into scope unqualified for the rest of the file.

   RNG is an explicit input to [Name.generate]/[Name.generate_many], never a
   global: every test below supplies its own deterministic [random]
   function, so nothing here seeds or touches the real
   [Mirage_crypto_rng]. *)

open Hotdiva_core

let be16 n = String.init 2 (fun i -> Char.chr ((n lsr (8 * (1 - i))) land 0xff))

(* A fake [random] source that replays a fixed queue of byte strings, one
   per call, failing loudly on a length mismatch or on exhaustion instead of
   silently returning wrong data. *)
let queued byte_strings =
  let remaining = ref byte_strings in
  fun n ->
    match !remaining with
    | [] -> Alcotest.fail (Fmt.str "queued random: exhausted (wanted %d bytes)" n)
    | b :: rest ->
        if String.length b <> n then
          Alcotest.fail
            (Fmt.str "queued random: expected %d byte(s), got %d" n (String.length b));
        remaining := rest;
        b

(* A [random] source that fails the test if ever invoked: used to prove
   validation rejects invalid [tokens]/[count] before any entropy is
   drawn. *)
let unreachable_random n =
  Alcotest.fail (Fmt.str "random unexpectedly called for %d byte(s)" n)

(* Runs [f] purely for its exception: passes iff [f ()] raises
   [Invalid_argument], fails on a normal return or any other exception. *)
let expect_invalid_argument name (f : unit -> unit) =
  Alcotest.test_case name `Quick (fun () ->
      match f () with
      | () -> Alcotest.fail (name ^ ": expected Invalid_argument, got no exception")
      | exception Invalid_argument _ -> ()
      | exception exn ->
          Alcotest.fail
            (Fmt.str "%s: expected Invalid_argument, got %s" name (Printexc.to_string exn)))

(* --- Exact-output tests over a controlled entropy queue.

   Byte values are hand-derived from the real word-list sizes, not sampled:
   Words.nouns has 1107 entries, so index selection masks to 0x7FF (2047)
   and draws 2 bytes; 0x07FF (2047) always lands on the rejected tail (2047
   >= 1107) and 0x0000/0x0001/0x0002 always select indices 0/1/2.
   Words.modifiers has 915 entries, masking to 0x3FF (1023); 0x03FF (1023)
   is its rejected tail (1023 >= 915) and 0x0001 selects index 1. *)

let reject_noun = be16 0x07ff (* masked value 2047 >= 1107: rejected *)
let accept_noun0 = be16 0 (* Words.nouns.(0) = "2-Factor Auth Token" *)
let accept_noun1 = be16 1 (* Words.nouns.(1) = "360 Review" *)
let accept_noun2 = be16 2 (* Words.nouns.(2) = "3D Renderer" *)
let reject_modifier = be16 0x03ff (* masked value 1023 >= 915: rejected *)
let accept_modifier1 = be16 1 (* Words.modifiers.(1) = "180 BPM" *)

let exact_output_suite =
  ( "Name.generate exact output (controlled entropy)",
    [
      Alcotest.test_case "rejects a draw, then accepts, for a single noun token" `Quick
        (fun () ->
          let random = queued [ reject_noun; accept_noun0 ] in
          Alcotest.(check string)
            "output" "2-factor-auth-token"
            (Name.generate ~random ~separator:"-" ~tokens:1 ()));
      Alcotest.test_case
        "rejects then accepts both the modifier and noun slots (multi-char separator, \
         digit/uppercase/multiword entries)"
        `Quick (fun () ->
          let random =
            queued [ reject_modifier; accept_modifier1; reject_noun; accept_noun2 ]
          in
          Alcotest.(check string)
            "output" "180 :: bpm :: 3d :: renderer"
            (Name.generate ~random ~separator:" :: " ~tokens:2 ()));
      Alcotest.test_case "generate_many draws each name independently, in order" `Quick
        (fun () ->
          let random = queued [ accept_noun0; accept_noun1 ] in
          Alcotest.(check (list string))
            "outputs"
            [ "2-factor-auth-token"; "360-review" ]
            (Name.generate_many ~random ~count:2 ~separator:"-" ~tokens:1 ()));
    ] )

(* --- Boundary validation: invalid input is rejected before [random] is
   ever called, proved with [unreachable_random] rather than merely
   asserting that no exception is raised in the benign case. *)

let boundaries_suite =
  ( "Name boundaries",
    [
      Alcotest.test_case "generate_many ~count:0 is [] and never calls random" `Quick
        (fun () ->
          Alcotest.(check (list string))
            "empty" []
            (Name.generate_many ~random:unreachable_random ~count:0 ~separator:"-"
               ~tokens:2 ()));
      expect_invalid_argument "generate_many ~count:(-1) raises without drawing"
        (fun () ->
          ignore
            (Name.generate_many ~random:unreachable_random ~count:(-1) ~separator:"-"
               ~tokens:2 ()));
      expect_invalid_argument "generate ~tokens:0 raises without drawing" (fun () ->
          ignore (Name.generate ~random:unreachable_random ~separator:"-" ~tokens:0 ()));
      expect_invalid_argument "generate ~tokens:(-1) raises without drawing" (fun () ->
          ignore (Name.generate ~random:unreachable_random ~separator:"-" ~tokens:(-1) ()));
      expect_invalid_argument "generate_many ~tokens:0 raises without drawing" (fun () ->
          ignore
            (Name.generate_many ~random:unreachable_random ~count:3 ~separator:"-"
               ~tokens:0 ()));
      expect_invalid_argument
        "generate_many ~count:0 ~tokens:0 still raises (tokens is checked regardless of \
         count)" (fun () ->
          ignore
            (Name.generate_many ~random:unreachable_random ~count:0 ~separator:"-"
               ~tokens:0 ()));
    ] )

let suites = [ exact_output_suite; boundaries_suite ]
