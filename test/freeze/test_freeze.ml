open Freeze_core
open Lwt.Syntax

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

let test_svg_escapes_and_styles () =
  let language = Option.get (Charamel_highlight.find "ocaml") in
  let config = { Config.default with output = "capture.svg" } in
  let rendered =
    Svg.render ~fs_root:"." ~config ~language:(Some language) ~text:"let x = <&>"
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

let test_ansi_background () =
  let config = { Config.default with output = "capture.svg" } in
  let rendered =
    Svg.render ~fs_root:"." ~config ~language:None ~text:"\027[48;2;255;0;0mred\027[0m"
      ~is_ansi:true
    |> expect_ok
  in
  Alcotest.(check bool)
    "background rectangle" true
    (String.contains rendered.Svg.svg 'r' && String.contains rendered.Svg.svg '#')

let render_ansi text =
  let config = { Config.default with output = "capture.svg" } in
  let rendered =
    Svg.render ~fs_root:"." ~config ~language:None ~text ~is_ansi:true |> expect_ok
  in
  rendered.Svg.svg

let test_ansi_underline_color () =
  let svg = render_ansi "\027[4;58;2;0;128;255mblue\027[58;2;255;0;0mred\027[0m" in
  Alcotest.(check bool)
    "underline colour attribute" true
    (Test_support.contains ~needle:"undercolor=\"#0080FF\"" ~haystack:svg);
  Alcotest.(check bool)
    "underline colour splits runs" true
    (Test_support.contains ~needle:"undercolor=\"#FF0000\"" ~haystack:svg);
  let cleared = render_ansi "\027[4;58;2;0;128;255m\027[59mgrey\027[0m" in
  Alcotest.(check bool)
    "underlined run survives the colour reset" true
    (Test_support.contains ~needle:"text-decoration=\"underline\"" ~haystack:cleared);
  Alcotest.(check bool)
    "SGR 59 clears the underline colour" false
    (Test_support.contains ~needle:"undercolor" ~haystack:cleared)

let count_occurrences ~needle ~haystack =
  let n = String.length needle in
  let rec loop at acc =
    if at + n > String.length haystack then acc
    else if String.sub haystack at n = needle then loop (at + n) (acc + 1)
    else loop (at + 1) acc
  in
  loop 0 0

let test_reset_closes_link () =
  let svg =
    render_ansi "\027]8;;http://example.com\027\\\027[4munderlined\027[0m plain"
  in
  Alcotest.(check int)
    "SGR 0 closes the hyperlink" 1
    (count_occurrences ~needle:"href=\"http://example.com\"" ~haystack:svg)

let test_pty_capture () =
  if Sys.win32 then Alcotest.skip ()
  else
    Test_support.with_temp_dir (fun root ->
        List.iter
          (fun name -> Unix.mkdir (Filename.concat root name) 0o700)
          [ "home"; "xdg-config"; "xdg-data"; "xdg-state"; "xdg-cache"; "tmp" ];
        let* result =
          Pty.execute ~env:(minimal_environment ~root) ~timeout:2.
            "printf '\\033[31mred\\033[0m'"
        in
        (match result with
        | Ok output -> Alcotest.(check string) "PTY output" "\027[31mred\027[0m" output
        | Error (`Exit (code, output)) -> Alcotest.failf "exit %d: %s" code output
        | Error (`Signaled (signal, output)) ->
            Alcotest.failf "signal %d: %s" signal output
        | Error (`Timeout output) -> Alcotest.failf "timeout: %s" output
        | Error (`Spawn message) -> Alcotest.fail message
        | Error (`Invalid_command message) -> Alcotest.fail message);
        Lwt.return_unit)

let suites =
  [
    Alcotest_lwt.test_case_sync "side expansion" `Quick test_side_expansion;
    Alcotest_lwt.test_case_sync "SVG escapes" `Quick test_svg_escapes_and_styles;
    Alcotest_lwt.test_case_sync "ANSI background" `Quick test_ansi_background;
    Alcotest_lwt.test_case "PTY capture" `Quick (fun _switch () -> test_pty_capture ());
    Alcotest_lwt.test_case_sync "ANSI underline colour" `Quick test_ansi_underline_color;
    Alcotest_lwt.test_case_sync "SGR 0 closes the hyperlink" `Quick test_reset_closes_link;
  ]

let () = Test_support.run_lwt "freeze" [ ("freeze", suites) ]
