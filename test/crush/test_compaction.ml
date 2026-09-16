module Compaction = Crush_core.Compaction
module Session = Crush_core.Session

let message text =
  Session.Message
    { ms = 0; message = Charm_fantasy.Message.text Charm_fantasy.Message.User text }

let reserve_cases () =
  Alcotest.(check int) "zero window" 0 (Compaction.reserve ~context_window:0);
  Alcotest.(check int) "twenty percent" 2_000 (Compaction.reserve ~context_window:10_000);
  Alcotest.(check int) "absolute cap" 20_000 (Compaction.reserve ~context_window:200_000)

let threshold_cases () =
  Alcotest.(check bool)
    "above reserve" false
    (Compaction.needed ~context_window:100 ~prompt_tokens:79 ~completion_tokens:0
       ~disabled:false);
  Alcotest.(check bool)
    "at reserve" true
    (Compaction.needed ~context_window:100 ~prompt_tokens:80 ~completion_tokens:0
       ~disabled:false);
  Alcotest.(check bool)
    "disabled" false
    (Compaction.needed ~context_window:100 ~prompt_tokens:100 ~completion_tokens:0
       ~disabled:true);
  Alcotest.(check bool)
    "zero window" false
    (Compaction.needed ~context_window:0 ~prompt_tokens:0 ~completion_tokens:0
       ~disabled:false)

let split_cases () =
  let old = message (String.make 1_000 'a') in
  let recent = message "new" in
  let through, kept = Compaction.split [| old; recent |] ~keep_tokens:1 in
  Alcotest.(check int) "prefix index" 0 through;
  Alcotest.(check int) "recent suffix length" 1 (List.length kept);
  let through_all, kept_all = Compaction.split [| message "one" |] ~keep_tokens:10 in
  Alcotest.(check int) "no replaced event" (-1) through_all;
  Alcotest.(check int) "all recent events retained" 1 (List.length kept_all)

let cases =
  [
    Alcotest.test_case "reserve" `Quick reserve_cases;
    Alcotest.test_case "threshold" `Quick threshold_cases;
    Alcotest.test_case "split" `Quick split_cases;
  ]
