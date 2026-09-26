open Lwt.Infix
module Query = Charamel_lipgloss_terminal
module Color = Charamel_ansi.Color

let color : Color.t Alcotest.testable = Alcotest.of_pp Color.pp

let check_color name expected actual =
  Alcotest.(check (option color)) name expected actual

let check_bool name expected actual = Alcotest.(check bool) name expected actual
let check_string name expected actual = Alcotest.(check string) name expected actual

let sink buffer =
  Lwt_io.make ~mode:Lwt_io.Output (fun bytes offset length ->
      Buffer.add_subbytes buffer (Lwt_bytes.to_bytes bytes) offset length;
      Lwt.return length)

let input_of reply = Lwt_io.of_bytes ~mode:Lwt_io.Input (Lwt_bytes.of_string reply)
let query_bytes = "\027]11;?\007\027[c"

let ask reply =
  let written = Buffer.create 32 in
  let answer =
    Lwt_main.run
      (Query.background_color ~input:(input_of reply) ~output:(sink written) ())
  in
  (answer, Buffer.contents written)

let rgb r g b = Some (Color.Rgb (r, g, b))

let test_query_writes_both_queries () =
  let answer, written = ask "\027]11;rgb:0000/0000/0000\007" in
  check_string "query bytes" query_bytes written;
  check_color "black reply" (rgb 0 0 0) answer

let test_component_scaling () =
  check_color "four digits" (rgb 255 128 0) (fst (ask "\027]11;rgb:ffff/8080/0000\007"));
  check_color "two digits" (rgb 18 52 86) (fst (ask "\027]11;rgb:12/34/56\007"));
  check_color "one digit" (rgb 255 0 17) (fst (ask "\027]11;rgb:f/0/1\007"));
  check_color "hex triple" (rgb 255 0 0) (fst (ask "\027]11;#ff0000\007"))

let test_dark_background () =
  let dark reply =
    Lwt_main.run
      (Query.has_dark_background ~input:(input_of reply) ~output:Lwt_io.null ())
  in
  check_bool "black is dark" true (dark "\027]11;rgb:0000/0000/0000\007");
  check_bool "white is light" false (dark "\027]11;rgb:ffff/ffff/ffff\007");
  check_bool "silence defaults to dark" true (dark "")

let test_no_reply () =
  check_color "empty stream" None (fst (ask ""));
  check_color "unparsable reply" None (fst (ask "\027]11;not-a-colour\007"));
  check_color "other osc stays open" None (fst (ask "\027]10;rgb:0000/0000/0000\007"))

let test_timeout_ends_the_query () =
  let read_end, write_end = Lwt_unix.pipe ~cloexec:true () in
  let input = Lwt_io.of_fd ~mode:Lwt_io.Input read_end in
  let answer =
    Lwt_main.run
      (Lwt.finalize
         (fun () -> Query.background_color ~timeout:0.05 ~input ~output:Lwt_io.null ())
         (fun () ->
           Lwt_unix.close write_end >>= fun () ->
           Lwt.catch (fun () -> Lwt_io.close input) (fun _ -> Lwt.return_unit)))
  in
  check_color "silent pipe times out" None answer

let test_profile_completion () =
  let triple = (Color.Basic 1, Color.Indexed 124, Option.get (Color.rgb 255 0 0)) in
  check_color "truecolor slot" (rgb 255 0 0)
    (Some (Query.complete Charamel_colorprofile.True_color triple));
  check_color "ansi256 slot" (Some (Color.Indexed 124))
    (Some (Query.complete Charamel_colorprofile.Ansi256 triple));
  check_color "no tty is default" (Some Color.Default)
    (Some (Query.complete Charamel_colorprofile.No_tty triple));
  check_color "adaptive picks the dark truecolor" (rgb 255 0 0)
    (Some
       (Query.complete_adaptive Charamel_colorprofile.True_color ~dark:true ~light:triple
          ~night:triple))

let cases =
  [
    Alcotest.test_case "query writes both requests" `Quick test_query_writes_both_queries;
    Alcotest.test_case "reply components scale" `Quick test_component_scaling;
    Alcotest.test_case "dark background decision" `Quick test_dark_background;
    Alcotest.test_case "no reply is no colour" `Quick test_no_reply;
    Alcotest.test_case "timeout ends the query" `Quick test_timeout_ends_the_query;
    Alcotest.test_case "profile completion" `Quick test_profile_completion;
  ]
