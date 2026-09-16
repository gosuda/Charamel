let run_form form make_env_actions =
  Eio_main.run (fun env ->
      let form_env = Charm_huh.Form.Env.v ~fs:env#fs ~temp_dir:env#fs ~editor:[] in
      let stdin = Eio_mock.Flow.make "huh-stdin" in
      Eio_mock.Flow.on_read stdin make_env_actions;
      let output = Buffer.create 256 in
      let reader = Charm_huh.Accessible.reader_of_flow ~stdin ~echo_off:None in
      let results =
        Charm_huh.Form.run_accessible form_env
          ~out:(fun text -> Buffer.add_string output text)
          reader form
      in
      (results, Buffer.contents output))

let test_validation_and_defaults () =
  let name = Charm_huh.Key.v "name" in
  let language = Charm_huh.Key.v "language" in
  let confirmed = Charm_huh.Key.v "confirmed" in
  let form =
    Charm_huh.Form.v
      [
        Charm_huh.Group.v
          [ Charm_huh.Field.input ~title:(Charm_huh.Dyn.const "Name") ~default:"d" name ];
        Charm_huh.Group.v
          [
            Charm_huh.Field.select
              ~title:(Charm_huh.Dyn.const "Language")
              ~options:
                (Charm_huh.Dyn.const
                   (Charm_huh.Field.options_of_strings [ "ocaml"; "go"; "rust" ]))
              language;
            Charm_huh.Field.confirm ~title:(Charm_huh.Dyn.const "Confirm") confirmed;
          ];
      ]
  in
  let results, transcript =
    run_form form
      [ `Return "\n"; `Return "bad\n"; `Return "2\n"; `Return "y\n"; `Raise End_of_file ]
  in
  Alcotest.(check (option string))
    "default input" (Some "d")
    (Charm_huh.Results.get name results);
  Alcotest.(check (option string))
    "selected option" (Some "go")
    (Charm_huh.Results.get language results);
  Alcotest.(check (option bool))
    "confirmation" (Some true)
    (Charm_huh.Results.get confirmed results);
  Alcotest.(check bool)
    "validation transcript" true
    (String.length transcript > 0 && String.contains transcript 'I')

let test_eof_keeps_defaults () =
  let value = Charm_huh.Key.v "value" in
  let form =
    Charm_huh.Form.v
      [
        Charm_huh.Group.v
          [
            Charm_huh.Field.input ~title:(Charm_huh.Dyn.const "Value") ~default:"fallback"
              value;
          ];
      ]
  in
  let results, _ = run_form form [ `Raise End_of_file ] in
  Alcotest.(check (option string))
    "EOF default" (Some "fallback")
    (Charm_huh.Results.get value results)

let tests =
  [
    Alcotest.test_case "validation and defaults" `Quick test_validation_and_defaults;
    Alcotest.test_case "EOF defaults" `Quick test_eof_keeps_defaults;
  ]
