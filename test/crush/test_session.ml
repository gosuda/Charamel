module Session = Crush_core.Session
module Jsonx = Crush_core.Jsonx

let model = { Session.provider = "anthropic"; model = "claude-test" }

let temporary_directory prefix =
  let path = Filename.temp_file prefix "" in
  Unix.unlink path;
  Unix.mkdir path 0o700;
  path

let restore_environment name previous =
  match previous with Some value -> Unix.putenv name value | None -> Unix.putenv name ""

let with_environment name value f =
  let previous = Sys.getenv_opt name in
  Unix.putenv name value;
  Fun.protect f ~finally:(fun () -> restore_environment name previous)

let with_store f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun _sw ->
  let directory = temporary_directory "charamel-session-" in
  with_environment "XDG_DATA_HOME" directory (fun () ->
      let result =
        Fun.protect
          (fun () -> f env directory)
          ~finally:(fun () ->
            Eio.Path.rmtree ~missing_ok:true Eio.Path.(env#fs / directory))
      in
      result)

let random_source () =
  let counter = ref 0 in
  fun length ->
    let seed = !counter in
    incr counter;
    String.init length (fun index -> Char.chr ((seed + index) land 0xFF))

let contains_substring text needle =
  let rec loop offset =
    if offset + String.length needle > String.length text then false
    else if String.sub text offset (String.length needle) = needle then true
    else loop (offset + 1)
  in
  loop 0

let text_message text = Charamel_fantasy.Message.text Charamel_fantasy.Message.User text

let json_input =
  Jsont.Json.object'
    [ Jsont.Json.mem (Jsont.Json.name "path") (Jsont.Json.string "README.md") ]

let event_cases () =
  let message =
    Charamel_fantasy.Message.
      {
        role = Charamel_fantasy.Message.Assistant;
        parts =
          [
            Charamel_fantasy.Message.Text "answer";
            Charamel_fantasy.Message.Reasoning
              { text = "thinking"; signature = Some "sig" };
            Charamel_fantasy.Message.File
              { mime = "text/plain"; data = "ZGF0YQ=="; name = Some "note.txt" };
            Charamel_fantasy.Message.Tool_call
              { id = "call-1"; name = "read"; input = json_input };
            Charamel_fantasy.Message.Tool_result
              { id = "call-1"; name = "read"; output = `Media ("image/png", "aGVsbG8=") };
          ];
      }
  in
  [
    Session.Message { ms = 1; message };
    Session.Tool_call { ms = 2; id = "call-1"; name = "read"; input = json_input };
    Session.Tool_result
      {
        ms = 3;
        id = "call-1";
        name = "read";
        output = `Error "not found";
        elapsed_ms = 4;
        artifact = Some "art-01020304";
      };
    Session.Usage
      {
        ms = 5;
        usage =
          {
            Charamel_fantasy.Usage.input = 10;
            output = 20;
            cache_read = 2;
            cache_write = 3;
            reasoning = 4;
          };
        cost_usd = 0.125;
        model;
      };
    Session.Summary { ms = 6; text = "earlier"; through = 1 };
    Session.Permission
      {
        ms = 7;
        tool = "edit";
        action = "edit";
        path = "/tmp/a";
        decision = Session.Allow_session;
      };
    Session.Note { ms = 8; text = "note" };
  ]

let event_round_trip () =
  with_store (fun env _directory ->
      let fs = env#fs in
      let cwd = Eio.Path.native_exn env#cwd in
      let store = Session.store ~fs ~cwd in
      let random = random_source () in
      match Session.create store ~clock:env#clock ~random ~cwd ~model () with
      | Error error -> Alcotest.failf "create failed: %a" Session.pp_error error
      | Ok session ->
          let all_events = event_cases () in
          List.iter
            (fun event ->
              match Session.append session ~clock:env#clock event with
              | Ok () -> ()
              | Error error -> Alcotest.failf "append failed: %a" Session.pp_error error)
            all_events;
          begin match Session.set_title session ~title:"roundtrip" with
          | Error error -> Alcotest.failf "set title failed: %a" Session.pp_error error
          | Ok () -> ()
          end;
          let after =
            Session.Message { ms = 9; message = text_message "after-summary" }
          in
          begin match Session.append session ~clock:env#clock after with
          | Error error ->
              Alcotest.failf "append after summary failed: %a" Session.pp_error error
          | Ok () -> ()
          end;
          begin match Session.open_ store ~id:(Session.id session) with
          | Error error -> Alcotest.failf "open failed: %a" Session.pp_error error
          | Ok reopened ->
              let expected =
                List.map (Jsonx.encode Session.event_jsont) (all_events @ [ after ])
              in
              let actual =
                Array.to_list (Session.events reopened)
                |> List.map (Jsonx.encode Session.event_jsont)
              in
              Alcotest.(check string)
                "title restored from index" "roundtrip" (Session.title reopened);
              Alcotest.(check (list string)) "all event variants" expected actual;
              let usage, cost = Session.usage_total reopened in
              Alcotest.(check int) "usage input" 10 usage.Charamel_fantasy.Usage.input;
              Alcotest.(check int) "usage output" 20 usage.Charamel_fantasy.Usage.output;
              Alcotest.(check (float 1e-12)) "usage cost" 0.125 cost;
              let messages = Session.messages reopened in
              Alcotest.(check int) "summary replay and suffix" 3 (List.length messages);
              begin match List.rev messages with
              | {
                  Charamel_fantasy.Message.parts = [ Charamel_fantasy.Message.Text text ];
                  _;
                }
                :: _ ->
                  Alcotest.(check string) "post-summary message" "after-summary" text
              | _ -> Alcotest.fail "post-summary message was not replayed"
              end
          end)

let index_and_concurrent_append () =
  with_store (fun env _directory ->
      let cwd = Eio.Path.native_exn env#cwd in
      let store = Session.store ~fs:env#fs ~cwd in
      let random = random_source () in
      match Session.create store ~clock:env#clock ~random ~cwd ~model () with
      | Error error -> Alcotest.failf "create failed: %a" Session.pp_error error
      | Ok session ->
          let errors = ref [] in
          Eio.Fiber.List.iter
            (fun index ->
              match
                Session.append session ~clock:env#clock
                  (Session.Message
                     { ms = index; message = text_message (string_of_int index) })
              with
              | Ok () -> ()
              | Error error -> errors := error :: !errors)
            (List.init 24 Fun.id);
          Alcotest.(check int) "concurrent appends complete" 0 (List.length !errors);
          begin match Session.open_ store ~id:(Session.id session) with
          | Error error -> Alcotest.failf "open failed: %a" Session.pp_error error
          | Ok reopened ->
              Alcotest.(check int)
                "all concurrent messages" 24
                (Array.length (Session.events reopened));
              begin match Session.list store with
              | Error error -> Alcotest.failf "list failed: %a" Session.pp_error error
              | Ok [ entry ] ->
                  Alcotest.(check int)
                    "index message count" 24 entry.Session.message_count
              | Ok entries ->
                  Alcotest.failf "expected one index row, got %d" (List.length entries)
              end
          end)

let append_line env session line =
  Eio.Path.save ~append:true ~create:(`If_missing 0o600)
    Eio.Path.(env#fs / Session.path session)
    line

let final_corruption_is_repaired () =
  with_store (fun env _directory ->
      let cwd = Eio.Path.native_exn env#cwd in
      let store = Session.store ~fs:env#fs ~cwd in
      let random = random_source () in
      match Session.create store ~clock:env#clock ~random ~cwd ~model () with
      | Error error -> Alcotest.failf "create failed: %a" Session.pp_error error
      | Ok session ->
          let good = Session.Note { ms = 1; text = "complete" } in
          begin match Session.append session ~clock:env#clock good with
          | Error error -> Alcotest.failf "append failed: %a" Session.pp_error error
          | Ok () -> ()
          end;
          append_line env session "{\"t\":\"note\",\"ms\":\n";
          begin match Session.open_ store ~id:(Session.id session) with
          | Error error ->
              Alcotest.failf "final partial line was not repaired: %a" Session.pp_error
                error
          | Ok reopened ->
              Alcotest.(check int)
                "partial line omitted" 1
                (Array.length (Session.events reopened));
              let contents = Eio.Path.load Eio.Path.(env#fs / Session.path session) in
              let expected =
                Jsonx.encode Session.header_jsont (Session.header session)
                ^ "\n"
                ^ Jsonx.encode Session.event_jsont good
                ^ "\n"
              in
              Alcotest.(check string) "bad suffix removed" expected contents
          end)

let middle_corruption_is_not_repaired () =
  with_store (fun env _directory ->
      let cwd = Eio.Path.native_exn env#cwd in
      let store = Session.store ~fs:env#fs ~cwd in
      let random = random_source () in
      match Session.create store ~clock:env#clock ~random ~cwd ~model () with
      | Error error -> Alcotest.failf "create failed: %a" Session.pp_error error
      | Ok session ->
          let first = Session.Note { ms = 1; text = "first" } in
          let last = Session.Note { ms = 2; text = "last" } in
          let header_line = Jsonx.encode Session.header_jsont (Session.header session) in
          let first_line = Jsonx.encode Session.event_jsont first in
          let last_line = Jsonx.encode Session.event_jsont last in
          Eio.Path.save ~create:(`Or_truncate 0o600)
            Eio.Path.(env#fs / Session.path session)
            (String.concat "\n" [ header_line; first_line; "not-json"; last_line ] ^ "\n");
          begin match Session.open_ store ~id:(Session.id session) with
          | Error (`Session_corrupt (path, line)) ->
              Alcotest.(check string) "corrupt path" (Session.path session) path;
              Alcotest.(check int) "middle line" 3 line
          | Error error ->
              Alcotest.failf "wrong corruption error: %a" Session.pp_error error
          | Ok _ -> Alcotest.fail "middle corruption was silently repaired"
          end;
          let contents = Eio.Path.load Eio.Path.(env#fs / Session.path session) in
          Alcotest.(check bool)
            "middle bad line retained" true
            (contains_substring contents "not-json"))

let header_id_mismatch_is_corrupt () =
  with_store (fun env _directory ->
      let cwd = Eio.Path.native_exn env#cwd in
      let store = Session.store ~fs:env#fs ~cwd in
      let random = random_source () in
      match Session.create store ~clock:env#clock ~random ~cwd ~model () with
      | Error error -> Alcotest.failf "create failed: %a" Session.pp_error error
      | Ok session ->
          let wrong =
            {
              (Session.header session) with
              id = Crush_core.Ulid.v ~now_ms:1 ~random:(fun n -> String.make n '\255');
            }
          in
          Eio.Path.save ~create:(`Or_truncate 0o600)
            Eio.Path.(env#fs / Session.path session)
            (Jsonx.encode Session.header_jsont wrong ^ "\n");
          begin match Session.open_ store ~id:(Session.id session) with
          | Error (`Session_corrupt (path, line)) ->
              Alcotest.(check string) "mismatch path" (Session.path session) path;
              Alcotest.(check int) "mismatch line" 1 line
          | Error error ->
              Alcotest.failf "wrong mismatch error: %a" Session.pp_error error
          | Ok _ -> Alcotest.fail "header id mismatch accepted"
          end)

let permission_boundary () =
  with_store (fun env _directory ->
      if Unix.getuid () = 0 then ()
      else
        let cwd = Eio.Path.native_exn env#cwd in
        let store = Session.store ~fs:env#fs ~cwd in
        let random = random_source () in
        match Session.create store ~clock:env#clock ~random ~cwd ~model () with
        | Error error -> Alcotest.failf "create failed: %a" Session.pp_error error
        | Ok session ->
            let native = Session.path session in
            Unix.chmod native 0o400;
            begin match
              Session.append session ~clock:env#clock
                (Session.Note { ms = 1; text = "blocked" })
            with
            | Error (`Io _) -> ()
            | Error error ->
                Alcotest.failf "wrong permission error: %a" Session.pp_error error
            | Ok () -> Alcotest.fail "session append ignored file permissions"
            end;
            Unix.chmod native 0o600)

let cases =
  [
    Alcotest.test_case "JSONL event variants and replay" `Quick event_round_trip;
    Alcotest.test_case "serialized appends and index are concurrent-safe" `Quick
      index_and_concurrent_append;
    Alcotest.test_case "final malformed line is repaired" `Quick
      final_corruption_is_repaired;
    Alcotest.test_case "middle malformed line is corruption" `Quick
      middle_corruption_is_not_repaired;
    Alcotest.test_case "header id mismatch is corruption" `Quick
      header_id_mismatch_is_corrupt;
    Alcotest.test_case "file permissions are enforced" `Quick permission_boundary;
  ]
