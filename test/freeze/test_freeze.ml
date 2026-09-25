open Freeze_core

let path_env = "/usr/bin:/bin"

let minimal_environment ~root =
  let path name = Filename.concat root name in
  [|
    "PATH=" ^ path_env;
    "LANG=C";
    "TERM=xterm-256color";
    "HOME=" ^ path "home";
    "XDG_CONFIG_HOME=" ^ path "xdg-config";
    "XDG_DATA_HOME=" ^ path "xdg-data";
    "XDG_STATE_HOME=" ^ path "xdg-state";
    "XDG_CACHE_HOME=" ^ path "xdg-cache";
    "TMPDIR=" ^ path "tmp";
  |]

let expect_ok = function Ok value -> value | Error message -> Alcotest.fail message

let test_side_expansion () =
  let actual = Config.expand_sides ~scale:2. [ 1.; 2. ] in
  Alcotest.(check (array (float 1e-10)))
    "vertical/horizontal sides" [| 2.; 4.; 2.; 4. |] actual

let test_svg_escapes_and_styles env =
  let language = Option.get (Charamel_highlight.find "ocaml") in
  let config = { Config.default with output = "capture.svg" } in
  let rendered =
    Svg.render ~fs:env#fs ~config ~language:(Some language) ~text:"let x = <&>"
      ~is_ansi:false
    |> expect_ok
  in
  Alcotest.(check bool)
    "SVG root" true
    (String.starts_with ~prefix:"<svg" rendered.Svg.svg);
  Alcotest.(check bool)
    "XML escaped source" true
    (String.contains rendered.Svg.svg '&' && String.contains rendered.Svg.svg ';');
  Alcotest.(check bool) "syntax colour" true (String.contains rendered.Svg.svg '#')

let test_ansi_background env =
  let config = { Config.default with output = "capture.svg" } in
  let rendered =
    Svg.render ~fs:env#fs ~config ~language:None ~text:"\027[48;2;255;0;0mred\027[0m"
      ~is_ansi:true
    |> expect_ok
  in
  Alcotest.(check bool)
    "background rectangle" true
    (String.contains rendered.Svg.svg 'r' && String.contains rendered.Svg.svg '#')

let test_pty_capture () =
  Test_support.with_temp_dir (fun dir ->
      List.iter
        (fun name -> Eio.Path.mkdir ~perm:0o700 Eio.Path.(dir / name))
        [ "home"; "xdg-config"; "xdg-data"; "xdg-state"; "xdg-cache"; "tmp" ];
      let root = Eio.Path.native_exn dir in
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      match
        Pty.execute ~sw ~clock:env#clock ~process_mgr:env#process_mgr
          ~env:(minimal_environment ~root) ~timeout:2. "printf '\\033[31mred\\033[0m'"
      with
      | Ok output -> Alcotest.(check string) "PTY output" "\027[31mred\027[0m" output
      | Error (`Exit (code, output)) -> Alcotest.failf "exit %d: %s" code output
      | Error (`Signaled (signal, output)) -> Alcotest.failf "signal %d: %s" signal output
      | Error (`Timeout output) -> Alcotest.failf "timeout: %s" output
      | Error (`Spawn message) -> Alcotest.fail message
      | Error (`Invalid_command message) -> Alcotest.fail message)

let suites =
  [
    Alcotest.test_case "side expansion" `Quick test_side_expansion;
    Alcotest.test_case "SVG escapes" `Quick (fun () ->
        Eio_main.run test_svg_escapes_and_styles);
    Alcotest.test_case "ANSI background" `Quick (fun () ->
        Eio_main.run test_ansi_background);
    Alcotest.test_case "PTY capture" `Quick test_pty_capture;
  ]

let () = Alcotest.run "freeze" [ ("freeze", suites) ]
