let run_form form make_env_actions =
  Eio_main.run (fun env ->
      let form_env = Charamel_huh.Form.Env.v ~fs:env#fs ~temp_dir:env#fs ~editor:[] in
      let stdin = Eio_mock.Flow.make "huh-stdin" in
      Eio_mock.Flow.on_read stdin make_env_actions;
      let output = Buffer.create 256 in
      let reader = Charamel_huh.Accessible.reader_of_flow ~stdin ~echo_off:None in
      let results =
        Charamel_huh.Form.run_accessible form_env
          ~out:(fun text -> Buffer.add_string output text)
          reader form
      in
      (results, Buffer.contents output))

let test_validation_and_defaults () =
  let name = Charamel_huh.Key.v "name" in
  let language = Charamel_huh.Key.v "language" in
  let confirmed = Charamel_huh.Key.v "confirmed" in
  let form =
    Charamel_huh.Form.v
      [
        Charamel_huh.Group.v
          [
            Charamel_huh.Field.input
              ~title:(Charamel_huh.Dyn.const "Name")
              ~default:"d" name;
          ];
        Charamel_huh.Group.v
          [
            Charamel_huh.Field.select
              ~title:(Charamel_huh.Dyn.const "Language")
              ~options:
                (Charamel_huh.Dyn.const
                   (Charamel_huh.Field.options_of_strings [ "ocaml"; "go"; "rust" ]))
              language;
            Charamel_huh.Field.confirm ~title:(Charamel_huh.Dyn.const "Confirm") confirmed;
          ];
      ]
  in
  let results, transcript =
    run_form form
      [ `Return "\n"; `Return "bad\n"; `Return "2\n"; `Return "y\n"; `Raise End_of_file ]
  in
  Alcotest.(check (option string))
    "default input" (Some "d")
    (Charamel_huh.Results.get name results);
  Alcotest.(check (option string))
    "selected option" (Some "go")
    (Charamel_huh.Results.get language results);
  Alcotest.(check (option bool))
    "confirmation" (Some true)
    (Charamel_huh.Results.get confirmed results);
  Alcotest.(check bool)
    "validation transcript" true
    (String.length transcript > 0 && String.contains transcript 'I')

let test_eof_keeps_defaults () =
  let value = Charamel_huh.Key.v "value" in
  let form =
    Charamel_huh.Form.v
      [
        Charamel_huh.Group.v
          [
            Charamel_huh.Field.input
              ~title:(Charamel_huh.Dyn.const "Value")
              ~default:"fallback" value;
          ];
      ]
  in
  let results, _ = run_form form [ `Raise End_of_file ] in
  Alcotest.(check (option string))
    "EOF default" (Some "fallback")
    (Charamel_huh.Results.get value results)

let tests =
  [
    Alcotest.test_case "validation and defaults" `Quick test_validation_and_defaults;
    Alcotest.test_case "EOF defaults" `Quick test_eof_keeps_defaults;
  ]
