module Spinner = Charamel_bubbles.Spinner

let check_frames () =
  Alcotest.(check (list string))
    "line frames" [ "|"; "/"; "-"; "\\" ] (Spinner.frames Spinner.Line);
  Alcotest.(check int)
    "moon frame count" 8
    (Stdlib.List.length (Spinner.frames Spinner.Moon));
  Alcotest.(check (float 0.000001)) "globe cadence" 0.25 (Spinner.fps Spinner.Globe);
  let names =
    [
      "line";
      "dot";
      "minidot";
      "jump";
      "pulse";
      "points";
      "globe";
      "moon";
      "monkey";
      "meter";
      "hamburger";
      "ellipsis";
    ]
  in
  Alcotest.(check int)
    "all kind names" 12
    (Stdlib.List.length (Stdlib.List.filter_map Spinner.kind_of_string names))

let tick_wraps () =
  let spinner = Spinner.v () in
  let spinner =
    let rec loop count value =
      if count = 0 then value
      else loop (count - 1) (fst (Spinner.update Spinner.Tick value))
    in
    loop 4 spinner
  in
  Alcotest.(check string)
    "four ticks return to first frame" "|"
    (Charamel_ansi.Text.strip (Spinner.view spinner))

let custom_frames () =
  let spinner = Spinner.v ~frames:([ "a"; "b"; "c"; "d" ], 0.0625) () in
  Alcotest.(check (option int))
    "custom kind is not named" None
    (match Spinner.kind spinner with Some _ -> Some 1 | None -> None);
  Alcotest.(check string) "custom first frame" "a" (Spinner.view spinner);
  let spinner, _ = Spinner.update Spinner.Tick spinner in
  Alcotest.(check string) "custom next frame" "b" (Spinner.view spinner);
  let spinner, _ = Spinner.update Spinner.Tick spinner in
  let spinner, _ = Spinner.update Spinner.Tick spinner in
  let spinner, _ = Spinner.update Spinner.Tick spinner in
  Alcotest.(check string) "custom wraps" "a" (Spinner.view spinner)

let cases =
  [
    Alcotest_lwt.test_case_sync "frame tables" `Quick check_frames;
    Alcotest_lwt.test_case_sync "tick wrap" `Quick tick_wraps;
    Alcotest_lwt.test_case_sync "custom frames" `Quick custom_frames;
  ]
