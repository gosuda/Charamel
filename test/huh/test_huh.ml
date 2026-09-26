open Lwt.Syntax

let key name =
  match Charamel_tea.Key.of_string name with
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.failf "invalid test key %s: %s" name message

let with_env f =
  let form_env =
    Charamel_huh.Form.Env.v ~fs_root:(Sys.getcwd ())
      ~temp_dir:(Filename.get_temp_dir_name ())
      ~editor:None ~clock:Charamel_os.Time.lwt
  in
  f form_env

let run_app form_env ?timeout form events =
  Charamel_tea.Test.run
    (Charamel_huh.Run.app form_env ?timeout form)
    ~events ~size:(24, 80)

let test_two_group_completion () =
  with_env (fun form_env ->
      let name = Charamel_huh.Key.v "name" in
      let language = Charamel_huh.Key.v "language" in
      let ok = Charamel_huh.Key.v "ok" in
      let form =
        Charamel_huh.Form.v
          [
            Charamel_huh.Group.v
              [ Charamel_huh.Field.input ~title:(Charamel_huh.Dyn.const "Name") name ];
            Charamel_huh.Group.v
              [
                Charamel_huh.Field.select
                  ~title:(Charamel_huh.Dyn.const "Language")
                  ~options:
                    (Charamel_huh.Dyn.const
                       (Charamel_huh.Field.options_of_strings [ "ocaml"; "go"; "rust" ]))
                  language;
                Charamel_huh.Field.confirm ~title:(Charamel_huh.Dyn.const "Continue?") ok;
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
      match Charamel_huh.Form.state model.Charamel_huh.Run.form with
      | `Completed results ->
          Alcotest.(check (option string))
            "name" (Some "ada")
            (Charamel_huh.Results.get name results);
          Alcotest.(check (option string))
            "language" (Some "go")
            (Charamel_huh.Results.get language results);
          Alcotest.(check (option bool))
            "confirmation" (Some true)
            (Charamel_huh.Results.get ok results)
      | `Normal -> Alcotest.fail "form did not complete"
      | `Aborted -> Alcotest.fail "form was aborted")

let test_validation_blocks_navigation () =
  with_env (fun form_env ->
      let name = Charamel_huh.Key.v "name" in
      let form =
        Charamel_huh.Form.v
          [
            Charamel_huh.Group.v
              [
                Charamel_huh.Field.input
                  ~title:(Charamel_huh.Dyn.const "Name")
                  ~validate:Charamel_huh.Validate.not_empty name;
              ];
          ]
      in
      let model, frame = run_app form_env form [ `Key (key "enter") ] in
      Alcotest.(check bool)
        "still normal after invalid submit" true
        (match Charamel_huh.Form.state model.Charamel_huh.Run.form with
        | `Normal -> true
        | _ -> false);
      Alcotest.(check bool)
        "error is rendered" true
        (String.length frame > 0 && String.contains frame '*'))

let test_abort () =
  with_env (fun form_env ->
      let name = Charamel_huh.Key.v "name" in
      let form =
        Charamel_huh.Form.v [ Charamel_huh.Group.v [ Charamel_huh.Field.input name ] ]
      in
      let model, _ = run_app form_env form [ `Key (key "ctrl+c") ] in
      Alcotest.(check bool)
        "ctrl+c aborts" true
        (match Charamel_huh.Form.state model.Charamel_huh.Run.form with
        | `Aborted -> true
        | _ -> false))

let test_typed_results_isolation () =
  let first = Charamel_huh.Key.v "first" in
  let second = Charamel_huh.Key.v "second" in
  let results = Charamel_huh.Results.add first "value" Charamel_huh.Results.empty in
  Alcotest.(check (option string))
    "first value" (Some "value")
    (Charamel_huh.Results.get first results);
  Alcotest.(check (option string))
    "second remains unset" None
    (Charamel_huh.Results.get second results)

let test_timeout () =
  with_env (fun form_env ->
      let note = Charamel_huh.Field.note ~title:(Charamel_huh.Dyn.const "Waiting") () in
      let form = Charamel_huh.Form.v [ Charamel_huh.Group.v [ note ] ] in
      let model, _ = run_app form_env ~timeout:0.01 form [ `Wait 0.05 ] in
      Alcotest.(check bool) "timeout marks model" true model.Charamel_huh.Run.timed_out)

let test_input_completion () =
  with_env (fun form_env ->
      let value = Charamel_huh.Key.v "value" in
      let form =
        Charamel_huh.Form.v
          [
            Charamel_huh.Group.v
              [
                Charamel_huh.Field.input
                  ~title:(Charamel_huh.Dyn.const "Value")
                  ~suggestions:(Charamel_huh.Dyn.const [ "ada"; "alba" ])
                  value;
              ];
          ]
      in
      let model, _ =
        run_app form_env form [ `Text "a"; `Key (key "ctrl+e"); `Key (key "enter") ]
      in
      match Charamel_huh.Form.state model.Charamel_huh.Run.form with
      | `Completed results ->
          Alcotest.(check (option string))
            "accepted suggestion" (Some "ada")
            (Charamel_huh.Results.get value results)
      | `Normal -> Alcotest.fail "completion form did not complete"
      | `Aborted -> Alcotest.fail "completion form was aborted")

let count needle text =
  let n = String.length needle in
  let rec loop offset acc =
    if offset + n > String.length text then acc
    else if String.sub text offset n = needle then loop (offset + 1) (acc + 1)
    else loop (offset + 1) acc
  in
  loop 0 0

let test_filtering_accessors () =
  let k = Charamel_huh.Key.v "k" in
  let options = Charamel_huh.Dyn.const (Charamel_huh.Field.options_of_strings [ "a" ]) in
  let field = Charamel_huh.Field.select ~filterable:true ~options k in
  Alcotest.(check (option bool))
    "initial" (Some false)
    (Charamel_huh.Field.filtering field);
  let field = Charamel_huh.Field.set_filtering true field in
  Alcotest.(check (option bool)) "set" (Some true) (Charamel_huh.Field.filtering field);
  Alcotest.(check (option bool))
    "input has none" None
    (Charamel_huh.Field.filtering (Charamel_huh.Field.input (Charamel_huh.Key.v "i")))

let test_hovered_and_navigation () =
  with_env (fun form_env ->
      let first = Charamel_huh.Key.v "first" in
      let second = Charamel_huh.Key.v "second" in
      let options =
        Charamel_huh.Dyn.const (Charamel_huh.Field.options_of_strings [ "x"; "y" ])
      in
      let form =
        Charamel_huh.Form.v
          [
            Charamel_huh.Group.v
              [
                Charamel_huh.Field.select ~options first; Charamel_huh.Field.input second;
              ];
          ]
      in
      let model, _ = run_app form_env form [] in
      let form = model.Charamel_huh.Run.form in
      (match Charamel_huh.Form.focused_field form with
      | None -> Alcotest.fail "focused field present"
      | Some field ->
          Alcotest.(check (option string))
            "hovered" (Some "x")
            (Charamel_huh.Field.hovered field);
          Alcotest.(check bool) "key binds" true (Charamel_huh.Form.key_binds form <> []);
          Alcotest.(check bool)
            "help view" true
            (Charamel_bubbles.Help.short_view (Charamel_huh.Form.help form)
               (Charamel_huh.Form.key_binds form)
            <> ""));
      let form, _cmd = Charamel_huh.Form.next_field form in
      Alcotest.(check (option string))
        "focused moved" (Some "second")
        (Option.join
           (Option.map Charamel_huh.Field.key_name (Charamel_huh.Form.focused_field form)));
      let form, _cmd = Charamel_huh.Form.previous_field form in
      Alcotest.(check (option string))
        "focused back" (Some "first")
        (Option.join
           (Option.map Charamel_huh.Field.key_name (Charamel_huh.Form.focused_field form)));
      let form, _cmd = Charamel_huh.Form.next_group form in
      Alcotest.(check bool)
        "single group completes" true
        (match Charamel_huh.Form.state form with `Completed _ -> true | _ -> false))

let test_note_next_labels () =
  with_env (fun form_env ->
      let render field =
        snd (run_app form_env (Charamel_huh.Form.v [ Charamel_huh.Group.v [ field ] ]) [])
      in
      let plain = render (Charamel_huh.Field.note ()) in
      let shown = render (Charamel_huh.Field.note ~show_next:true ()) in
      let named =
        render (Charamel_huh.Field.note ~show_next:true ~next_label:"Continue" ())
      in
      Alcotest.(check bool) "no next label by default" true (count "Next" plain = 0);
      Alcotest.(check bool) "show_next renders Next" true (count "Next" shown > 0);
      Alcotest.(check bool) "custom next label" true (count "Continue" named > 0);
      Alcotest.(check bool) "custom label replaces Next" true (count "Next" named = 0))

let test_placeholder_tracks_results () =
  with_env (fun form_env ->
      let name = Charamel_huh.Key.v "name" in
      let body = Charamel_huh.Key.v "body" in
      let placeholder =
        Charamel_huh.Dyn.Of_results
          (fun results ->
            Option.value ~default:"empty" (Charamel_huh.Results.get name results))
      in
      let form =
        Charamel_huh.Form.v
          [
            Charamel_huh.Group.v
              [ Charamel_huh.Field.input name; Charamel_huh.Field.text ~placeholder body ];
          ]
      in
      let _, frame = run_app form_env form [ `Text "hi"; `Key (key "enter") ] in
      Alcotest.(check bool)
        "placeholder follows typed value" true
        (count "hi" (Charamel_ansi.Text.strip frame) >= 2))

let test_suggestion_style_is_bubbles_default () =
  with_env (fun form_env ->
      let base = Charamel_huh.Theme.charm ~is_dark:true in
      let red = Option.get (Charamel_ansi.Color.rgb 255 0 0) in
      let red_style =
        Charamel_lipgloss.Style.foreground red Charamel_lipgloss.Style.empty
      in
      let text_input =
        {
          base.Charamel_huh.Styles.focused.Charamel_huh.Styles.text_input with
          Charamel_huh.Styles.placeholder = red_style;
        }
      in
      let focused =
        { base.Charamel_huh.Styles.focused with Charamel_huh.Styles.text_input }
      in
      let styles = { base with Charamel_huh.Styles.focused } in
      let theme ~is_dark:_ = styles in
      let value = Charamel_huh.Key.v "value" in
      let form =
        Charamel_huh.Form.v ~theme
          [
            Charamel_huh.Group.v
              [
                Charamel_huh.Field.input
                  ~suggestions:(Charamel_huh.Dyn.const [ "ada" ])
                  value;
              ];
          ]
      in
      let model, _ = run_app form_env form [ `Text "a" ] in
      let frame = Charamel_huh.Form.view model.Charamel_huh.Run.form in
      Alcotest.(check bool)
        "suggestion keeps the bubbles default color" true
        (count "38;5;240" frame > 0);
      Alcotest.(check bool)
        "suggestion ignores the custom placeholder style" true
        (count "38;2;255;0;0" frame = 0))

let test_background_boundary_matches_ansi () =
  with_env (fun form_env ->
      let selected = ref None in
      let theme ~is_dark =
        selected := Some is_dark;
        Charamel_huh.Theme.charm ~is_dark
      in
      let form =
        Charamel_huh.Form.v ~theme
          [ Charamel_huh.Group.v [ Charamel_huh.Field.input (Charamel_huh.Key.v "v") ] ]
      in
      let _ = run_app form_env form [ `Text "\027]11;rgb:8080/8080/8080\007" ] in
      let grey = Option.get (Charamel_ansi.Color.rgb 128 128 128) in
      Alcotest.(check (option bool))
        "luma-128 background agrees with Color.is_dark"
        (Some (Charamel_ansi.Color.is_dark grey))
        !selected)

let test_async_options_deliver () =
  with_env (fun form_env ->
      let name = Charamel_huh.Key.v "name" in
      let pick = Charamel_huh.Key.v "pick" in
      let options =
        Charamel_huh.Dyn.Of_results_async
          (fun _results -> Charamel_huh.Field.options_of_strings [ "a"; "b" ])
      in
      let form =
        Charamel_huh.Form.v
          [
            Charamel_huh.Group.v
              [ Charamel_huh.Field.input name; Charamel_huh.Field.select ~options pick ];
          ]
      in
      let model, _ =
        run_app form_env form
          [ `Key (key "enter"); `Key (key "down"); `Key (key "enter") ]
      in
      match Charamel_huh.Form.state model.Charamel_huh.Run.form with
      | `Completed results ->
          Alcotest.(check (option string))
            "async options committed" (Some "b")
            (Charamel_huh.Results.get pick results)
      | `Normal | `Aborted -> Alcotest.fail "async form did not complete")

let test_char_limit_counts_graphemes () =
  with_env (fun form_env ->
      let value = Charamel_huh.Key.v "value" in
      let form =
        Charamel_huh.Form.v
          [
            Charamel_huh.Group.v
              [
                Charamel_huh.Field.input ~char_limit:4
                  ~default:
                    "\u{0065}\u{0301}\u{0065}\u{0301}\u{0065}\u{0301}\u{0065}\u{0301}"
                  value;
              ];
          ]
      in
      let model, _ = run_app form_env form [ `Key (key "enter") ] in
      Alcotest.(check bool)
        "four clusters satisfy a limit of four" true
        (match Charamel_huh.Form.state model.Charamel_huh.Run.form with
        | `Completed _ -> true
        | `Normal | `Aborted -> false))

let test_filter_folds_unicode_case () =
  with_env (fun form_env ->
      let k = Charamel_huh.Key.v "k" in
      let options =
        Charamel_huh.Dyn.const (Charamel_huh.Field.options_of_strings [ "item"; "other" ])
      in
      let form =
        Charamel_huh.Form.v
          [
            Charamel_huh.Group.v [ Charamel_huh.Field.select ~filterable:true ~options k ];
          ]
      in
      let _, frame = run_app form_env form [ `Key (key "/"); `Text "\u{130}" ] in
      Alcotest.(check bool)
        "U+0130 folds to i and matches item" true
        (count "item" (Charamel_ansi.Text.strip frame) > 0))

let test_view_hook_and_commands () =
  with_env (fun form_env ->
      let hook frame =
        {
          frame with
          Charamel_tea.View.content = frame.Charamel_tea.View.content ^ "|HOOK";
        }
      in
      let value = Charamel_huh.Key.v "value" in
      let nop_cmd = Charamel_tea.Cmd.msg Charamel_huh.Form.nop in
      let form =
        Charamel_huh.Form.v ~view_hook:hook ~submit_cmd:nop_cmd ~cancel_cmd:nop_cmd
          [ Charamel_huh.Group.v [ Charamel_huh.Field.input value ] ]
      in
      let model, frame = run_app form_env form [ `Text "z"; `Key (key "enter") ] in
      Alcotest.(check bool) "view hook applied" true (count "|HOOK" frame > 0);
      Alcotest.(check bool)
        "completes through the submit command" true
        (match Charamel_huh.Form.state model.Charamel_huh.Run.form with
        | `Completed results -> Charamel_huh.Results.get value results = Some "z"
        | `Normal | `Aborted -> false);
      let model, _ = run_app form_env form [ `Key (key "ctrl+c") ] in
      Alcotest.(check bool)
        "aborts through the cancel command" true
        (match Charamel_huh.Form.state model.Charamel_huh.Run.form with
        | `Aborted -> true
        | `Completed _ | `Normal -> false))

let test_accessible_trim_is_unicode () =
  let value = Charamel_huh.Key.v "value" in
  let form =
    Charamel_huh.Form.v
      [
        Charamel_huh.Group.v
          [ Charamel_huh.Field.input ~validate:Charamel_huh.Validate.not_empty value ];
      ]
  in
  with_env (fun form_env ->
      let reader =
        {
          Charamel_huh.Accessible.read_line =
            (fun () -> Lwt.return (Some " \u{3000}x\u{3000} "));
          read_password = (fun () -> Lwt.return None);
        }
      in
      let* results =
        Charamel_huh.Form.run_accessible form_env
          ~out:(fun _ -> Lwt.return_unit)
          reader form
      in
      Alcotest.(check (option string))
        "ideographic spaces trimmed" (Some "x")
        (Charamel_huh.Results.get value results);
      Lwt.return_unit)

let test_timeout_unsupported_in_accessible () =
  let form =
    Charamel_huh.Form.v
      [ Charamel_huh.Group.v [ Charamel_huh.Field.input (Charamel_huh.Key.v "v") ] ]
  in
  with_env (fun form_env ->
      let* result =
        Charamel_huh.Run.run ~timeout:1. ~accessible:true ~env:form_env
          ~clock:Charamel_os.Time.lwt form
      in
      Alcotest.(check bool)
        "accessible timeout is rejected" true
        (match result with Error `Timeout_unsupported -> true | _ -> false);
      Lwt.return_unit)

let test_run_field_completes_on_eof () =
  let value = Charamel_huh.Key.v "value" in
  let field = Charamel_huh.Field.input ~default:"d" value in
  let* result =
    Charamel_huh.Run.run_field ~accessible:true ~clock:Charamel_os.Time.lwt field
  in
  (match result with
  | Ok results ->
      Alcotest.(check (option string))
        "run_field commits the default" (Some "d")
        (Charamel_huh.Results.get value results)
  | Error error -> Alcotest.failf "run_field: %a" Charamel_huh.Run.pp_error error);
  Lwt.return_unit

let tests =
  [
    Alcotest_lwt.test_case_sync "two-group completion" `Quick test_two_group_completion;
    Alcotest_lwt.test_case_sync "validation blocks navigation" `Quick
      test_validation_blocks_navigation;
    Alcotest_lwt.test_case_sync "ctrl-c abort" `Quick test_abort;
    Alcotest_lwt.test_case_sync "typed result isolation" `Quick
      test_typed_results_isolation;
    Alcotest_lwt.test_case_sync "timeout" `Quick test_timeout;
    Alcotest_lwt.test_case_sync "input completion" `Quick test_input_completion;
    Alcotest_lwt.test_case_sync "filtering accessors" `Quick test_filtering_accessors;
    Alcotest_lwt.test_case_sync "hovered and navigation" `Quick
      test_hovered_and_navigation;
    Alcotest_lwt.test_case_sync "note next labels" `Quick test_note_next_labels;
    Alcotest_lwt.test_case_sync "placeholder tracks results" `Quick
      test_placeholder_tracks_results;
    Alcotest_lwt.test_case_sync "suggestion style is bubbles default" `Quick
      test_suggestion_style_is_bubbles_default;
    Alcotest_lwt.test_case_sync "background boundary matches ansi" `Quick
      test_background_boundary_matches_ansi;
    Alcotest_lwt.test_case_sync "char limit counts graphemes" `Quick
      test_char_limit_counts_graphemes;
    Alcotest_lwt.test_case_sync "filter folds unicode case" `Quick
      test_filter_folds_unicode_case;
    Alcotest_lwt.test_case_sync "view hook and commands" `Quick
      test_view_hook_and_commands;
    Alcotest_lwt.test_case_sync "async options deliver" `Quick test_async_options_deliver;
    Alcotest_lwt.test_case "accessible unicode trim" `Quick (fun _switch ->
        test_accessible_trim_is_unicode);
    Alcotest_lwt.test_case "timeout unsupported in accessible" `Quick (fun _switch ->
        test_timeout_unsupported_in_accessible);
    Alcotest_lwt.test_case "run field completes on eof" `Quick (fun _switch ->
        test_run_field_completes_on_eof);
  ]

let () =
  match Sys.getenv_opt "CHARM_TEST_HUH_SPINNER_CHILD" with
  | Some _ -> Test_spinner.run_pty_child ()
  | None ->
      Test_support.run_lwt "huh"
        [
          ("form", tests);
          ("accessible", Test_accessible.tests);
          ("spinner", Test_spinner.tests);
        ]
