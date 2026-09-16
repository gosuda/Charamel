let key name =
  match Charm_tea.Key.of_string name with
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.failf "invalid test key %s: %s" name message

let with_env f =
  Eio_main.run (fun env ->
      let form_env = Charm_huh.Form.Env.v ~fs:env#fs ~temp_dir:env#fs ~editor:[] in
      f form_env)

let run_app form_env ?timeout form events =
  Charm_tea.Test.run (Charm_huh.Run.app form_env ?timeout form) ~events ~size:(24, 80)

let test_two_group_completion () =
  with_env (fun form_env ->
      let name = Charm_huh.Key.v "name" in
      let language = Charm_huh.Key.v "language" in
      let ok = Charm_huh.Key.v "ok" in
      let form =
        Charm_huh.Form.v
          [
            Charm_huh.Group.v
              [ Charm_huh.Field.input ~title:(Charm_huh.Dyn.const "Name") name ];
            Charm_huh.Group.v
              [
                Charm_huh.Field.select
                  ~title:(Charm_huh.Dyn.const "Language")
                  ~options:
                    (Charm_huh.Dyn.const
                       (Charm_huh.Field.options_of_strings [ "ocaml"; "go"; "rust" ]))
                  language;
                Charm_huh.Field.confirm ~title:(Charm_huh.Dyn.const "Continue?") ok;
              ];
          ]
      in
      let model, _frame =
        run_app form_env form
          [
            `Text "ada";
            `Key (key "enter");
            `Key (key "down");
            `Key (key "enter");
            `Key (key "y");
            `Key (key "enter");
          ]
      in
      match Charm_huh.Form.state model.Charm_huh.Run.form with
      | `Completed results ->
          Alcotest.(check (option string))
            "name" (Some "ada")
            (Charm_huh.Results.get name results);
          Alcotest.(check (option string))
            "language" (Some "go")
            (Charm_huh.Results.get language results);
          Alcotest.(check (option bool))
            "confirmation" (Some true)
            (Charm_huh.Results.get ok results)
      | `Normal -> Alcotest.fail "form did not complete"
      | `Aborted -> Alcotest.fail "form was aborted")

let test_validation_blocks_navigation () =
  with_env (fun form_env ->
      let name = Charm_huh.Key.v "name" in
      let form =
        Charm_huh.Form.v
          [
            Charm_huh.Group.v
              [
                Charm_huh.Field.input ~title:(Charm_huh.Dyn.const "Name")
                  ~validate:Charm_huh.Validate.not_empty name;
              ];
          ]
      in
      let model, frame = run_app form_env form [ `Key (key "enter") ] in
      Alcotest.(check bool)
        "still normal after invalid submit" true
        (match Charm_huh.Form.state model.Charm_huh.Run.form with
        | `Normal -> true
        | _ -> false);
      Alcotest.(check bool)
        "error is rendered" true
        (String.length frame > 0 && String.contains frame '*'))

let test_abort () =
  with_env (fun form_env ->
      let name = Charm_huh.Key.v "name" in
      let form = Charm_huh.Form.v [ Charm_huh.Group.v [ Charm_huh.Field.input name ] ] in
      let model, _ = run_app form_env form [ `Key (key "ctrl+c") ] in
      Alcotest.(check bool)
        "ctrl+c aborts" true
        (match Charm_huh.Form.state model.Charm_huh.Run.form with
        | `Aborted -> true
        | _ -> false))

let test_typed_results_isolation () =
  let first = Charm_huh.Key.v "first" in
  let second = Charm_huh.Key.v "second" in
  let results = Charm_huh.Results.add first "value" Charm_huh.Results.empty in
  Alcotest.(check (option string))
    "first value" (Some "value")
    (Charm_huh.Results.get first results);
  Alcotest.(check (option string))
    "second remains unset" None
    (Charm_huh.Results.get second results)

let test_timeout () =
  with_env (fun form_env ->
      let note = Charm_huh.Field.note ~title:(Charm_huh.Dyn.const "Waiting") () in
      let form = Charm_huh.Form.v [ Charm_huh.Group.v [ note ] ] in
      let model, _ = run_app form_env ~timeout:0.01 form [ `Wait 0.05 ] in
      Alcotest.(check bool) "timeout marks model" true model.Charm_huh.Run.timed_out)

let test_input_completion () =
  with_env (fun form_env ->
      let value = Charm_huh.Key.v "value" in
      let form =
        Charm_huh.Form.v
          [
            Charm_huh.Group.v
              [
                Charm_huh.Field.input ~title:(Charm_huh.Dyn.const "Value")
                  ~suggestions:(Charm_huh.Dyn.const [ "ada"; "alba" ])
                  value;
              ];
          ]
      in
      let model, _ =
        run_app form_env form [ `Text "a"; `Key (key "ctrl+e"); `Key (key "enter") ]
      in
      match Charm_huh.Form.state model.Charm_huh.Run.form with
      | `Completed results ->
          Alcotest.(check (option string))
            "accepted suggestion" (Some "ada")
            (Charm_huh.Results.get value results)
      | `Normal -> Alcotest.fail "completion form did not complete"
      | `Aborted -> Alcotest.fail "completion form was aborted")

let tests =
  [
    Alcotest.test_case "two-group completion" `Quick test_two_group_completion;
    Alcotest.test_case "validation blocks navigation" `Quick
      test_validation_blocks_navigation;
    Alcotest.test_case "ctrl-c abort" `Quick test_abort;
    Alcotest.test_case "typed result isolation" `Quick test_typed_results_isolation;
    Alcotest.test_case "timeout" `Quick test_timeout;
    Alcotest.test_case "input completion" `Quick test_input_completion;
  ]

let () =
  Alcotest.run "huh"
    [
      ("form", tests);
      ("accessible", Test_accessible.tests);
      ("spinner", Test_spinner.tests);
    ]
