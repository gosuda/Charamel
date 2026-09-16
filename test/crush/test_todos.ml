module Todos = Crush_core.Todos

let items () =
  [
    { Todos.content = "write code"; status = Todos.Pending; active_form = "writing code" };
    {
      Todos.content = "run tests";
      status = Todos.In_progress;
      active_form = "running tests";
    };
    { Todos.content = "ship"; status = Todos.Completed; active_form = "shipping" };
  ]

let render_case () =
  let expected = "[ ] write code\n[>] running tests\n[x] ship" in
  Alcotest.(check string) "rendered todos" expected (Todos.render (items ()))

let set_get_case () =
  let state = Todos.create () in
  let source = items () in
  Todos.set state source;
  let fetched = Todos.get state in
  Alcotest.(check int) "item count" 3 (List.length fetched);
  Alcotest.(check string) "first item" "write code" (List.hd fetched).Todos.content;
  Alcotest.(check string)
    "active form" "running tests" (List.nth fetched 1).Todos.active_form

let transitions_case () =
  let state = Todos.create () in
  Todos.set state
    [ { content = "work"; status = Todos.Pending; active_form = "working" } ];
  (match Todos.transition state ~index:0 Todos.In_progress with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "pending item did not start");
  (match Todos.transition state ~index:0 Todos.Completed with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "in-progress item did not complete");
  match Todos.transition state ~index:0 Todos.Pending with
  | Error (`Invalid_transition (Todos.Completed, Todos.Pending)) -> ()
  | Error _ -> Alcotest.fail "wrong transition error"
  | Ok () -> Alcotest.fail "completed item regressed"

let invalid_index_case () =
  let state = Todos.create () in
  match Todos.transition state ~index:4 Todos.Pending with
  | Error (`Invalid_index 4) -> ()
  | Error _ -> Alcotest.fail "wrong invalid index"
  | Ok () -> Alcotest.fail "invalid index accepted"

let codec_case () =
  let source = items () in
  match Jsont.Json.encode Todos.jsont source with
  | Error message -> Alcotest.failf "encoding failed: %s" message
  | Ok json -> (
      match Jsont.Json.decode Todos.jsont json with
      | Error message -> Alcotest.failf "decoding failed: %s" message
      | Ok decoded ->
          Alcotest.(check int)
            "round-trip count" (List.length source) (List.length decoded);
          Alcotest.(check string)
            "round-trip content" "ship" (List.nth decoded 2).Todos.content;
          Alcotest.(check bool)
            "round-trip status" true
            (match (List.nth decoded 1).Todos.status with
            | Todos.In_progress -> true
            | _ -> false))

let invalid_status_case () =
  let json =
    Jsont.Json.list
      [
        Jsont.Json.object'
          [
            Jsont.Json.mem (Jsont.Json.name "content") (Jsont.Json.string "x");
            Jsont.Json.mem (Jsont.Json.name "status") (Jsont.Json.string "unknown");
            Jsont.Json.mem (Jsont.Json.name "active_form") (Jsont.Json.string "x");
          ];
      ]
  in
  match Jsont.Json.decode Todos.jsont json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "unknown todo status accepted"

let cases =
  [
    Alcotest.test_case "render" `Quick render_case;
    Alcotest.test_case "set and get" `Quick set_get_case;
    Alcotest.test_case "lifecycle transitions" `Quick transitions_case;
    Alcotest.test_case "invalid index" `Quick invalid_index_case;
    Alcotest.test_case "JSON round trip" `Quick codec_case;
    Alcotest.test_case "invalid status" `Quick invalid_status_case;
  ]
