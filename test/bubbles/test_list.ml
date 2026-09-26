module BList = Charamel_bubbles.List

let make () =
  let delegate = BList.default_delegate ~title:(fun item -> item) () in
  BList.v ~width:40 ~height:10 ~delegate
    ~filter_value:(fun item -> item)
    [ "foo"; "bar"; "baz" ]

let items () =
  let model = make () in
  Alcotest.(check (list string))
    "all items" [ "foo"; "bar"; "baz" ] (BList.visible_items model);
  let model = BList.set_filter_text "ba" model in
  Alcotest.(check (list string))
    "filtered items" [ "bar"; "baz" ] (BList.visible_items model);
  Alcotest.(check bool) "filtered state" true (BList.is_filtered model)

let selection_and_pages () =
  let model = make () |> BList.select 2 in
  Alcotest.(check (option string))
    "selected item" (Some "baz") (BList.selected_item model);
  let model = BList.cursor_up model in
  Alcotest.(check int) "cursor up" 1 (BList.index model);
  let model = BList.go_to_start model in
  Alcotest.(check int) "start" 0 (BList.index model);
  let model = BList.go_to_end model in
  Alcotest.(check int) "end" 2 (BList.index model)

let filter_states () =
  let model =
    make () |> BList.set_filter_text "ba" |> BList.set_filter_state BList.Unfiltered
  in
  Alcotest.(check (list string))
    "unfiltered state restores all" [ "foo"; "bar"; "baz" ] (BList.visible_items model);
  let model = BList.set_filter_state BList.Filtering model in
  Alcotest.(check bool) "setting filter" true (BList.setting_filter model)

let empty_and_view () =
  let delegate = BList.default_delegate ~title:(fun item -> item) () in
  let model = BList.v ~width:30 ~height:8 ~delegate ~filter_value:(fun item -> item) [] in
  Alcotest.(check (option string)) "empty selection" None (BList.selected_item model);
  let plain = Charamel_ansi.Text.strip (BList.view model) in
  Alcotest.(check bool) "empty status" true (String.length plain > 0)

let char_key c = Charamel_tea.Key.v (Charamel_tea.Key.Char (Uchar.of_char c))

let delegate_update () =
  let base = BList.default_delegate ~title:(fun item -> item) () in
  let down_key = Charamel_tea.Key.v Charamel_tea.Key.Down in
  let delegate =
    {
      base with
      update =
        (fun ctx key item ->
          if key = char_key 'x' then Some ("foo!", BList.Toggle_full_help)
          else if key = char_key 'y' then Some ("ignored", BList.Set_items [ "only" ])
          else if key = char_key 'u' then Some (item, BList.Toggle_full_help)
          else if ctx.BList.selected && key = down_key then Some (item, BList.Go_to_start)
          else None);
    }
  in
  let model =
    BList.v ~width:40 ~height:10 ~delegate
      ~filter_value:(fun item -> item)
      [ "foo"; "bar"; "baz" ]
  in
  (* A key unknown to the builtin keymap is still consumed by the delegate, and
     the returned item replaces the selected one in place: Toggle_full_help
     touches no items, so the ["foo!"; ...] result can only come from the
     replacement itself. *)
  (match BList.key model (char_key 'x') with
  | Some (BList.Delegate_msg (item, BList.Toggle_full_help)) ->
      Alcotest.(check string) "replacement item threaded" "foo!" item;
      let model', _ =
        BList.update (BList.Delegate_msg (item, BList.Toggle_full_help)) model
      in
      Alcotest.(check (list string))
        "replacement applied in place" [ "foo!"; "bar"; "baz" ]
        (BList.visible_items model')
  | _ -> Alcotest.fail "delegate did not consume x");
  (* The inner message runs after the replacement: Set_items then replaces the
     whole list. *)
  (match BList.key model (char_key 'y') with
  | Some (BList.Delegate_msg (item, BList.Set_items [ "only" ])) ->
      let model', _ =
        BList.update (BList.Delegate_msg (item, BList.Set_items [ "only" ])) model
      in
      Alcotest.(check (list string))
        "inner message applied" [ "only" ] (BList.visible_items model')
  | _ -> Alcotest.fail "delegate did not consume y");
  (* The delegate runs before the builtin keymap: 'u' is builtin prev_page. *)
  (match BList.key model (char_key 'u') with
  | Some (BList.Delegate_msg _) -> ()
  | Some BList.Prev_page -> Alcotest.fail "builtin beat the delegate on u"
  | _ -> Alcotest.fail "delegate did not consume u");
  (* Down would be builtin Cursor_down; the delegate intercepts it. *)
  (match BList.key model down_key with
  | Some (BList.Delegate_msg _) -> ()
  | _ -> Alcotest.fail "delegate did not consume Down");
  (* Returning None falls through to the builtin keymap. *)
  (match BList.key model (char_key 'k') with
  | Some BList.Cursor_up -> ()
  | other ->
      Alcotest.failf "fall-through broken: %s"
        (match other with Some _ -> "wrong msg" | None -> "no msg"));
  (* While filtering the delegate is not consulted; text goes to the input. *)
  let filtering = BList.set_filter_state BList.Filtering model in
  (match BList.key filtering (char_key 'x') with
  | Some (BList.Filter_input _) -> ()
  | _ -> Alcotest.fail "delegate consulted while filtering");
  (* No selected item means no delegate consultation. *)
  let empty = BList.v ~width:30 ~height:8 ~delegate ~filter_value:(fun item -> item) [] in
  match BList.key empty (char_key 'x') with
  | Some (BList.Delegate_msg _) -> Alcotest.fail "delegate consulted on empty list"
  | _ -> ()

let input_darkness () =
  let delegate = BList.default_delegate ~title:(fun item -> item) () in
  let build is_dark =
    BList.v ~width:40 ~height:10 ~is_dark ~delegate
      ~filter_value:(fun item -> item)
      [ "foo"; "bar" ]
  in
  Alcotest.(check bool) "light stored" false (BList.is_dark (build false));
  Alcotest.(check bool) "dark stored" true (BList.is_dark (build true));
  (* The blurred filter input renders its value with the palette of the model:
     [set_styles] must rebuild from [m.is_dark], never a hardcoded one. *)
  let palette model =
    model |> BList.set_filter_text "x"
    |> BList.set_filter_state BList.Filter_applied
    |> BList.filter_input |> Charamel_bubbles.Textinput.view
  in
  let light = palette (build false) in
  let dark = palette (build true) in
  Alcotest.(check bool) "palettes differ" true (light <> dark);
  let restyled = build false |> BList.set_styles (BList.default_styles ~is_dark:false) in
  Alcotest.(check string) "set_styles keeps light palette" light (palette restyled)

let view_cursor () =
  let model = make () in
  Alcotest.(check bool) "no cursor while idle" true (BList.view_cursor model = None);
  let filtering = BList.set_filter_state BList.Filtering model in
  match BList.view_cursor filtering with
  | None -> Alcotest.fail "a filtering list asks for a cursor"
  | Some (cursor : Charamel_tea.Cursor.t) ->
      Alcotest.(check int) "the filter line is the first line" 0 cursor.row;
      Alcotest.(check bool) "the column clears the prompt" true (cursor.col > 0)

let cases =
  [
    Alcotest_lwt.test_case_sync "items and filter" `Quick items;
    Alcotest_lwt.test_case_sync "selection and pages" `Quick selection_and_pages;
    Alcotest_lwt.test_case_sync "filter states" `Quick filter_states;
    Alcotest_lwt.test_case_sync "empty and view" `Quick empty_and_view;
    Alcotest_lwt.test_case_sync "delegate update" `Quick delegate_update;
    Alcotest_lwt.test_case_sync "input darkness" `Quick input_darkness;
    Alcotest_lwt.test_case_sync "view cursor" `Quick view_cursor;
  ]
