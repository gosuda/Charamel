(** Unit tests for {!Melt_phrase.Phrase}: the EOF join on terminals and the fail-closed
    read, both driven through pipes so no terminal is needed. *)

open Lwt.Syntax
module Phrase = Melt_phrase.Phrase

let with_pipe_input contents f =
  let read_end, write_end = Unix.pipe () in
  let out_channel = Unix.out_channel_of_descr write_end in
  if contents <> "" then output_string out_channel contents;
  close_out out_channel;
  let input = Unix.in_channel_of_descr read_end in
  Lwt.finalize
    (fun () -> f input)
    (fun () ->
      close_in_noerr input;
      Lwt.return_unit)

let piped_input_is_returned_whole =
  Alcotest_lwt.test_case "piped input is returned whole" `Quick (fun _switch () ->
      with_pipe_input "abandon abandon\nabandon abandon \n" (fun input ->
          let* result = Phrase.read_stdin_phrase ~is_tty:false ~input () in
          (match result with
          | Ok text ->
              Alcotest.check Alcotest.string "piped input kept"
                "abandon abandon\nabandon abandon \n" text
          | Error _ -> Alcotest.fail "piped input was refused");
          Lwt.return_unit))

let terminal_input_joins_lines =
  Alcotest_lwt.test_case "terminal input joins lines to end-of-file" `Quick
    (fun _switch () ->
      with_pipe_input "abandon abandon\nabandon abandon\n" (fun input ->
          let restored = ref false in
          let echo_off () = fun () -> restored := true in
          let* result = Phrase.read_stdin_phrase ~is_tty:true ~echo_off ~input () in
          (match result with
          | Ok text ->
              Alcotest.check Alcotest.string "lines joined"
                "abandon abandon\nabandon abandon" text;
              Alcotest.check Alcotest.bool "echo restored" true !restored
          | Error _ -> Alcotest.fail "terminal input was refused");
          Lwt.return_unit))

let echo_failure_refuses_the_read =
  Alcotest_lwt.test_case "an echo failure refuses the read" `Quick (fun _switch () ->
      with_pipe_input "unconsumed-recovery-phrase\n" (fun input ->
          let echo_off () = raise (Unix.Unix_error (Unix.ENOTTY, "echo_off", "")) in
          let* result = Phrase.read_stdin_phrase ~is_tty:true ~echo_off ~input () in
          (match result with
          | Error `Cannot_hide_input -> ()
          | Ok _ -> Alcotest.fail "unhidden input was read instead of refused");
          Alcotest.check Alcotest.string "input remains unread"
            "unconsumed-recovery-phrase" (input_line input);
          Lwt.return_unit))

let prompt_failure_does_not_prevent_reading =
  Alcotest_lwt.test_case "failed prompt output still reads and restores echo" `Quick
    (fun _switch () ->
      with_pipe_input "abandon abandon\n" (fun input ->
          let restored = ref false in
          let echo_off () = fun () -> restored := true in
          let prompt _ = raise (Sys_error "stderr is full") in
          let* result =
            Phrase.read_stdin_phrase ~is_tty:true ~echo_off ~input ~prompt ()
          in
          (match result with
          | Ok text -> Alcotest.check Alcotest.string "phrase read" "abandon abandon" text
          | Error _ -> Alcotest.fail "diagnostic failure prevented the read");
          Alcotest.check Alcotest.bool "echo restored" true !restored;
          Lwt.return_unit))

let cases =
  [
    piped_input_is_returned_whole;
    terminal_input_joins_lines;
    echo_failure_refuses_the_read;
    prompt_failure_does_not_prevent_reading;
  ]
