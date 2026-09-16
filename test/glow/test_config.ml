let env bindings name = List.assoc_opt name bindings
let read files path = List.assoc_opt path files

let precedence () =
  let files =
    [
      ( "/work/glow.json",
        Ok "{\"style\":\"light\",\"width\":41,\"pager\":false,\"all\":false}" );
    ]
  in
  let environment =
    [ ("GLOW_STYLE", "dracula"); ("GLOW_WIDTH", "52"); ("GLOW_PAGER", "true") ]
  in
  match
    Config.load ~explicit:None ~cwd:"/work" ~env:(env environment) ~read:(read files)
  with
  | Error _ -> Alcotest.fail "configuration should decode"
  | Ok (config, Some path) ->
      Alcotest.(check string) "path" "/work/glow.json" path;
      Alcotest.(check string) "style" "dracula" config.Config.style;
      Alcotest.(check int) "width" 52 config.Config.width;
      Alcotest.(check bool) "pager" true config.Config.pager
  | Ok (_, None) -> Alcotest.fail "configuration path missing"

let flags_win () =
  let config =
    Config.apply Config.default
      {
        Config.style = Some "pink";
        width = Some 37;
        pager = Some true;
        tui = None;
        all = None;
        line_numbers = Some true;
        preserve_new_lines = None;
        mouse = None;
      }
  in
  Alcotest.(check string) "style" "pink" config.Config.style;
  Alcotest.(check int) "width" 37 config.Config.width;
  Alcotest.(check bool) "pager" true config.Config.pager;
  Alcotest.(check bool) "line numbers" true config.Config.line_numbers

let booleans () =
  List.iter
    (fun (value, expected) ->
      match Config.parse_bool value with
      | Ok actual -> Alcotest.(check bool) value expected actual
      | Error message -> Alcotest.fail message)
    [
      ("true", true);
      ("YES", true);
      ("1", true);
      ("false", false);
      ("off", false);
      ("0", false);
    ];
  match Config.parse_bool "maybe" with
  | Ok _ -> Alcotest.fail "invalid bool accepted"
  | Error _ -> ()

let suite =
  ( "config",
    [
      Alcotest.test_case "precedence" `Quick precedence;
      Alcotest.test_case "flags win" `Quick flags_win;
      Alcotest.test_case "booleans" `Quick booleans;
    ] )
