open Charm_colorprofile

let sink_output profile chunks =
  let output = Buffer.create 128 in
  let sink = Eio.Flow.buffer_sink output in
  let writer = Charm_colorprofile.Writer.create ~profile sink in
  List.iter (Charm_colorprofile.Writer.write writer) chunks;
  Buffer.contents output

let writer_case name input expected_truecolor expected_ansi256 expected_ansi
    expected_ascii =
  Alcotest.test_case name `Quick (fun () ->
      let expected profile =
        match profile with
        | Charm_colorprofile.True_color -> expected_truecolor
        | Charm_colorprofile.Ansi256 -> expected_ansi256
        | Charm_colorprofile.Ansi -> expected_ansi
        | Charm_colorprofile.Ascii | Charm_colorprofile.No_tty -> expected_ascii
      in
      List.iter
        (fun profile ->
          Alcotest.check Alcotest.string
            (name ^ " " ^ "whole")
            (expected profile)
            (sink_output profile [ input ]);
          let chunks =
            List.init
              (String.length input + 1)
              (fun split ->
                ( String.sub input 0 split,
                  String.sub input split (String.length input - split) ))
          in
          List.iteri
            (fun split chunks ->
              Alcotest.check Alcotest.string
                (name ^ " split " ^ string_of_int split)
                (expected profile)
                (sink_output profile [ fst chunks; snd chunks ]))
            chunks)
        [ Charm_colorprofile.True_color; Ansi256; Ansi; Ascii; No_tty ])

let writers =
  [
    writer_case "empty" "" "" "" "" "";
    writer_case "no styles" "hello world" "hello world" "hello world" "hello world"
      "hello world";
    writer_case "UTF-8 continuation bytes" "\u{00DC}\u{1F44D}" "\u{00DC}\u{1F44D}"
      "\u{00DC}\u{1F44D}" "\u{00DC}\u{1F44D}" "\u{00DC}\u{1F44D}";
    writer_case "non-SGR sequences remain"
      "\027]8;;https://example.test/\u{00DC}\027\\x\027]8;;\027\\\027[2J"
      "\027]8;;https://example.test/\u{00DC}\027\\x\027]8;;\027\\\027[2J"
      "\027]8;;https://example.test/\u{00DC}\027\\x\027]8;;\027\\\027[2J"
      "\027]8;;https://example.test/\u{00DC}\027\\x\027]8;;\027\\\027[2J"
      "\027]8;;https://example.test/\u{00DC}\027\\x\027]8;;\027\\\027[2J";
    writer_case "simple style attributes" "hello \027[1mworld\027[m"
      "hello \027[1mworld\027[m" "hello \027[1mworld\027[m" "hello \027[1mworld\027[m"
      "hello world";
    writer_case "simple ansi color fg" "hello \027[31mworld\027[m"
      "hello \027[31mworld\027[m" "hello \027[31mworld\027[m" "hello \027[31mworld\027[m"
      "hello world";
    writer_case "default fg" "\027[31mhello \027[39mworld\027[m"
      "\027[31mhello \027[39mworld\027[m" "\027[31mhello \027[39mworld\027[m"
      "\027[31mhello \027[39mworld\027[m" "hello world";
    writer_case "ansi fg and bg" "\027[31;42mhello world\027[m"
      "\027[31;42mhello world\027[m" "\027[31;42mhello world\027[m"
      "\027[31;42mhello world\027[m" "hello world";
    writer_case "bright fg and bg" "\027[91;102mhello world\027[m"
      "\027[91;102mhello world\027[m" "\027[91;102mhello world\027[m"
      "\027[91;102mhello world\027[m" "hello world";
    writer_case "indexed fg" "hello \027[38;5;196mworld\027[m"
      "hello \027[38;5;196mworld\027[m" "hello \027[38;5;196mworld\027[m"
      "hello \027[91mworld\027[m" "hello world";
    writer_case "indexed bg" "\027[48;5;196mhello world\027[m"
      "\027[48;5;196mhello world\027[m" "\027[48;5;196mhello world\027[m"
      "\027[101mhello world\027[m" "hello world";
    writer_case "truecolor" "hello \027[38;2;255;133;55mworld\027[m"
      "hello \027[38;2;255;133;55mworld\027[m" "hello \027[38;5;209mworld\027[m"
      "hello \027[91mworld\027[m" "hello world";
    writer_case "itu truecolor" "hello \027[38:2::255:133:55mworld\027[m"
      "hello \027[38:2::255:133:55mworld\027[m" "hello \027[38;5;209mworld\027[m"
      "hello \027[91mworld\027[m" "hello world";
    writer_case "itu indexed bg" "hello \027[48:5:196mworld\027[m"
      "hello \027[48:5:196mworld\027[m" "hello \027[48;5;196mworld\027[m"
      "hello \027[101mworld\027[m" "hello world";
    writer_case "missing parameter" "\027[31mhello \027[;1mworld"
      "\027[31mhello \027[;1mworld" "\027[31mhello \027[;1mworld"
      "\027[31mhello \027[;1mworld" "hello world";
    writer_case "other attributes" "\027[1;38;5;204mhello \027[38;5;204mworld\027[m"
      "\027[1;38;5;204mhello \027[38;5;204mworld\027[m"
      "\027[1;38;5;204mhello \027[38;5;204mworld\027[m"
      "\027[1;91mhello \027[91mworld\027[m" "hello world";
  ]

let lookup bindings name = List.assoc_opt name bindings

let detect_case name ~is_tty bindings expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check Alcotest.string name expected
        (match Charm_colorprofile.detect ~is_tty ~env:(lookup bindings) with
        | No_tty -> "no-tty"
        | Ascii -> "ascii"
        | Ansi -> "ansi"
        | Ansi256 -> "ansi256"
        | True_color -> "true-color"))

let detect =
  [
    detect_case "not a tty" ~is_tty:false [ ("TERM", "xterm-256color") ] "no-tty";
    detect_case "tty missing term" ~is_tty:true [] "no-tty";
    detect_case "tty force" ~is_tty:false [ ("TTY_FORCE", "1"); ("TERM", "xterm") ] "ansi";
    detect_case "xterm" ~is_tty:true [ ("TERM", "xterm") ] "ansi";
    detect_case "xterm 256" ~is_tty:true [ ("TERM", "xterm-256color") ] "ansi256";
    detect_case "screen" ~is_tty:true [ ("TERM", "screen") ] "ansi256";
    detect_case "screen ignores colorterm" ~is_tty:true
      [ ("TERM", "screen"); ("COLORTERM", "truecolor") ]
      "ansi256";
    detect_case "tmux ignores colorterm" ~is_tty:true
      [ ("TERM", "tmux"); ("COLORTERM", "truecolor") ]
      "ansi256";
    detect_case "colorterm" ~is_tty:true
      [ ("TERM", "xterm"); ("COLORTERM", "truecolor") ]
      "true-color";
    detect_case "direct" ~is_tty:true [ ("TERM", "xterm-direct") ] "true-color";
    detect_case "known truecolor terminal" ~is_tty:true [ ("TERM", "kitty") ] "true-color";
    detect_case "windows terminal" ~is_tty:true
      [ ("TERM", "xterm"); ("WT_SESSION", "1") ]
      "true-color";
    detect_case "empty windows terminal marker" ~is_tty:true
      [ ("TERM", "xterm"); ("WT_SESSION", "") ]
      "true-color";
    detect_case "cloud shell" ~is_tty:true
      [ ("TERM", "xterm"); ("GOOGLE_CLOUD_SHELL", "yes") ]
      "true-color";
    detect_case "no color" ~is_tty:true
      [ ("TERM", "xterm-256color"); ("NO_COLOR", "1") ]
      "ascii";
    detect_case "no color wins force" ~is_tty:true
      [ ("TERM", "xterm"); ("NO_COLOR", "1"); ("CLICOLOR_FORCE", "1") ]
      "ascii";
    detect_case "force without tty" ~is_tty:false [ ("CLICOLOR_FORCE", "1") ] "ansi";
    detect_case "force dumb" ~is_tty:true
      [ ("TERM", "dumb"); ("CLICOLOR_FORCE", "1") ]
      "ansi";
    detect_case "clicolor" ~is_tty:true [ ("TERM", "xterm"); ("CLICOLOR", "1") ] "ansi";
    detect_case "clicolor missing term" ~is_tty:true [ ("CLICOLOR", "1") ] "ansi";
    detect_case "clicolor dumb" ~is_tty:true
      [ ("TERM", "dumb"); ("CLICOLOR", "1") ]
      "no-tty";
    detect_case "false values" ~is_tty:true
      [ ("TERM", "xterm"); ("CLICOLOR", "0"); ("TTY_FORCE", "0") ]
      "ansi";
  ]

let () = Alcotest.run "charm_colorprofile" [ ("writer", writers); ("detect", detect) ]
