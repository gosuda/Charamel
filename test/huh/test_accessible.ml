open Lwt.Syntax

let scripted_reader lines =
  let queue = ref lines in
  let read_line () =
    match !queue with
    | [] -> Lwt.return_none
    | line :: rest ->
        queue := rest;
        Lwt.return (Some line)
  in
  Charamel_huh.Accessible.{ read_line; read_password = read_line }

let run_form form lines =
  let form_env =
    Charamel_huh.Form.Env.v ~fs_root:(Sys.getcwd ())
      ~temp_dir:(Filename.get_temp_dir_name ())
      ~editor:None ~clock:Charamel_os.Time.lwt
  in
  let output = Buffer.create 256 in
  let reader = scripted_reader lines in
  let write text =
    Buffer.add_string output text;
    Lwt.return_unit
  in
  let* results = Charamel_huh.Form.run_accessible form_env ~out:write reader form in
  Lwt.return (results, Buffer.contents output)

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
  let* results, transcript = run_form form [ "\n"; "bad\n"; "2\n"; "y\n" ] in
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
    (Test_support.contains ~needle:"Invalid: must be a number between 1 and 3"
       ~haystack:transcript);
  Lwt.return_unit

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
  let* results, _ = run_form form [] in
  Alcotest.(check (option string))
    "EOF default" (Some "fallback")
    (Charamel_huh.Results.get value results);
  Lwt.return_unit

let tests =
  [
    Alcotest_lwt.test_case "validation and defaults" `Quick (fun _switch () ->
        test_validation_and_defaults ());
    Alcotest_lwt.test_case "EOF defaults" `Quick (fun _switch () ->
        test_eof_keeps_defaults ());
  ]
