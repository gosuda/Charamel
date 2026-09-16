open Charm_ansi

let sub_to_string = function Some v -> string_of_int v | None -> "_"
let param_to_string ps = String.concat ":" (List.map sub_to_string ps)
let params_to_string ps = "[" ^ String.concat ";" (List.map param_to_string ps) ^ "]"

let parser_testable =
  let to_string = function
    | Parser.Print s -> "Print " ^ String.escaped s
    | Parser.Execute c -> "Execute " ^ String.escaped (String.make 1 c)
    | Parser.Csi { params; intermediates; final } ->
        "Csi " ^ params_to_string params ^ " " ^ String.escaped intermediates ^ " "
        ^ String.escaped (String.make 1 final)
    | Parser.Esc { intermediates; final } ->
        "Esc " ^ String.escaped intermediates ^ " " ^ String.escaped (String.make 1 final)
    | Parser.Osc fields ->
        "Osc [" ^ String.concat "; " (List.map String.escaped fields) ^ "]"
    | Parser.Dcs { params; intermediates; final; data } ->
        "Dcs " ^ params_to_string params ^ " " ^ String.escaped intermediates ^ " "
        ^ String.escaped (String.make 1 final)
        ^ " " ^ String.escaped data
    | Parser.Apc s -> "Apc " ^ String.escaped s
    | Parser.Pm s -> "Pm " ^ String.escaped s
    | Parser.Sos s -> "Sos " ^ String.escaped s
  in
  Alcotest.list
    (Alcotest.testable (fun ppf a -> Format.pp_print_string ppf (to_string a)) ( = ))

(* [feed_all s] feeds [s] in one call; [feed_all ~chunk:n s] feeds [s] in
   [n]-byte chunks. Both end with a flush. *)
let feed_all ?(chunk = 0) s =
  let p = Parser.create () in
  let n = String.length s in
  let out = ref [] in
  let i = ref 0 in
  while !i < n do
    let j = if chunk = 0 then n else min (!i + chunk) n in
    out := List.rev_append (Parser.feed p (String.sub s !i (j - !i))) !out;
    i := j
  done;
  List.rev (List.rev_append (Parser.flush p) !out)

let check name input expected =
  Alcotest.check parser_testable name expected (feed_all input)

let print s = Parser.Print s
let execute c = Parser.Execute c
let csi params intermediates final = Parser.Csi { params; intermediates; final }
let esc intermediates final = Parser.Esc { intermediates; final }
let osc fields = Parser.Osc fields

let dcs params intermediates final data =
  Parser.Dcs { params; intermediates; final; data }

let ones n = List.init n (fun _ -> [ Some 1 ])
let nothings n = List.init n (Fun.const None)
let repeat n s = String.concat "" (List.init n (Fun.const s))
let prints s = List.init (String.length s) (fun i -> print (String.sub s i 1))

let decode =
  [
    Alcotest.test_case "decode: single byte" `Quick (fun () ->
        check "single byte" "\x1b" [ execute '\x1b' ]);
    Alcotest.test_case "decode: single byte 2" `Quick (fun () ->
        check "single byte 2" "\x00" [ execute '\x00' ]);
    Alcotest.test_case "decode: ASCII printable" `Quick (fun () ->
        check "ASCII printable" "a" [ print "a" ]);
    Alcotest.test_case "decode: ASCII space" `Quick (fun () ->
        check "ASCII space" " " [ print " " ]);
    Alcotest.test_case "decode: ASCII DEL" `Quick (fun () ->
        check "ASCII DEL" "\x7f" [ execute '\x7f' ]);
    Alcotest.test_case "decode: DEL in the middle of UTF8 string" `Quick (fun () ->
        check "DEL in the middle of UTF8 string" "a\x7fb"
          [ print "a"; execute '\x7f'; print "b" ]);
    Alcotest.test_case "decode: DEL in the middle of DCS" `Quick (fun () ->
        check "DEL in the middle of DCS" "\x1bP1;2+xa\x7fb\x1b\\"
          [ dcs [ [ Some 1 ]; [ Some 2 ] ] "+" 'x' "a\x7fb"; esc "" '\\' ]);
    Alcotest.test_case "decode: ST in the middle of DCS" `Quick (fun () ->
        check "ST in the middle of DCS" "\x1bP1;2+xa\x9cb\x1b\\"
          [ dcs [ [ Some 1 ]; [ Some 2 ] ] "+" 'x' "a"; print "b"; esc "" '\\' ]);
    Alcotest.test_case "decode: CSI style sequence" `Quick (fun () ->
        check "CSI style sequence" "\x1b[1;2;3m"
          [ csi [ [ Some 1 ]; [ Some 2 ]; [ Some 3 ] ] "" 'm' ]);
    Alcotest.test_case "decode: invalid unterminated CSI sequence" `Quick (fun () ->
        check "invalid unterminated CSI sequence" "\x1b[1;2;3" []);
    Alcotest.test_case "decode: set title OSC sequence" `Quick (fun () ->
        check "set title OSC sequence" "\x1b]2;charmbracelet: ~/Source/bubbletea\x07"
          [ osc [ "2"; "charmbracelet: ~/Source/bubbletea" ] ]);
    Alcotest.test_case "decode: set background OSC with 7-bit ST" `Quick (fun () ->
        check "set background OSC with 7-bit ST" "\x1b]11;ff/00/ff\x1b\\"
          [ osc [ "11"; "ff/00/ff" ]; esc "" '\\' ]);
    Alcotest.test_case "decode: set background OSC with ST 8-bit" `Quick (fun () ->
        check "set background OSC with ST 8-bit" "\x1b]11;ff/00/ff\x9c\x1baa\x8fa"
          [ osc [ "11"; "ff/00/ff" ]; esc "" 'a'; print "a"; execute '\x8f'; print "a" ]);
    Alcotest.test_case "decode: set background OSC followed by ESC sequence" `Quick
      (fun () ->
        check "set background OSC followed by ESC sequence" "\x1b]11;ff/00/ff\x1b[1;2;3m"
          [ osc [ "11"; "ff/00/ff" ]; csi [ [ Some 1 ]; [ Some 2 ]; [ Some 3 ] ] "" 'm' ]);
    Alcotest.test_case "decode: set background OSC ESC terminated" `Quick (fun () ->
        check "set background OSC ESC terminated" "\x1b]11;ff/00/ff\x1b"
          [ osc [ "11"; "ff/00/ff" ]; execute '\x1b' ]);
    Alcotest.test_case "decode: multiple sequences" `Quick (fun () ->
        check "multiple sequences"
          "\x1b[1;2;3m\x1b]2;charmbracelet: ~/Source/bubbletea\x07\x1b]11;ff/00/ff\x1b\\"
          [
            csi [ [ Some 1 ]; [ Some 2 ]; [ Some 3 ] ] "" 'm';
            osc [ "2"; "charmbracelet: ~/Source/bubbletea" ];
            osc [ "11"; "ff/00/ff" ];
            esc "" '\\';
          ]);
    Alcotest.test_case "decode: double ESC" `Quick (fun () ->
        check "double ESC" "\x1b\x1b" [ execute '\x1b'; execute '\x1b' ]);
    Alcotest.test_case "decode: double ST" `Quick (fun () ->
        check "double ST" "\x1b\\\x1b\\" [ esc "" '\\'; esc "" '\\' ]);
    Alcotest.test_case "decode: double ST 8-bit" `Quick (fun () ->
        check "double ST 8-bit" "\x9c\x9c" [ execute '\x9c'; execute '\x9c' ]);
    Alcotest.test_case "decode: ASCII printables" `Quick (fun () ->
        check "ASCII printables" "Hello, World!" (prints "Hello, World!"));
    Alcotest.test_case "decode: rune" `Quick (fun () ->
        check "rune" "\xf0\x9f\x91\x8b" [ print "\xf0\x9f\x91\x8b" ]);
    Alcotest.test_case "decode: invalid rune" `Quick (fun () ->
        check "invalid rune" "\xc3" [ print "\xc3" ]);
    Alcotest.test_case "decode: multiple sequences with UTF8 and double ESC" `Quick
      (fun () ->
        check "multiple sequences with UTF8 and double ESC"
          "\xf0\x9f\x91\xa8\xf0\x9f\x8f\xbf\xe2\x80\x8d\xf0\x9f\x8c\xbe\x1b\x1b \
           \x1b[?1:2:3m\xc3\x84abc\x1b\x1bP+q\x1b\\"
          [
            print "\xf0\x9f\x91\xa8";
            print "\xf0\x9f\x8f\xbf";
            print "\xe2\x80\x8d";
            print "\xf0\x9f\x8c\xbe";
            execute '\x1b';
            csi [ [ Some 1; Some 2; Some 3 ] ] "?" 'm';
            print "\xc3\x84";
            print "a";
            print "b";
            print "c";
            execute '\x1b';
            dcs [] "+" 'q' "";
            esc "" '\\';
          ]);
    Alcotest.test_case "decode: style sequences" `Quick (fun () ->
        check "style sequences" "hello, \x1b[1;2;3mworld\x1b[0m!"
          (prints "hello, "
          @ [ csi [ [ Some 1 ]; [ Some 2 ]; [ Some 3 ] ] "" 'm' ]
          @ prints "world"
          @ [ csi [ [ Some 0 ] ] "" 'm' ]
          @ prints "!"));
    Alcotest.test_case "decode: set background OSC with C1" `Quick (fun () ->
        check "set background OSC with C1" "\x1b]11;\x90?\x1b\\"
          [ osc [ "11"; "\x90?" ]; esc "" '\\' ]);
    Alcotest.test_case "decode: unterminated CSI with escape sequence" `Quick (fun () ->
        check "unterminated CSI with escape sequence" "\x1b[1;2;3\x1bOa"
          [ esc "" 'O'; print "a" ]);
    Alcotest.test_case "decode: SS3" `Quick (fun () ->
        check "SS3" "\x1bOa" [ esc "" 'O'; print "a" ]);
    Alcotest.test_case "decode: SS3 8-bit" `Quick (fun () ->
        check "SS3 8-bit" "\x8fa" [ execute '\x8f'; print "a" ]);
    Alcotest.test_case "decode: ESC sequence with intermediate" `Quick (fun () ->
        check "ESC sequence with intermediate" "\x1b Q" [ esc " " 'Q' ]);
    Alcotest.test_case "decode: ESC followed by C0" `Quick (fun () ->
        check "ESC followed by C0" "\x1b[\x00a" [ execute '\x00'; csi [] "" 'a' ]);
    Alcotest.test_case "decode: unterminated DCS sequence" `Quick (fun () ->
        check "unterminated DCS sequence" "\x1bP1;2+xa" []);
    Alcotest.test_case "decode: invalid DCS sequence" `Quick (fun () ->
        check "invalid DCS sequence" "\x1bP\x1b\\ab" []);
    Alcotest.test_case "decode: single param osc" `Quick (fun () ->
        check "single param osc" "\x1b]112\x07" [ osc [ "112" ] ]);
  ]

let dcs_cases =
  [
    Alcotest.test_case "dcs: max_params" `Quick (fun () ->
        check "max_params"
          ("\x1bP" ^ repeat 33 "1;" ^ "p\x1b\\")
          [ dcs (ones 32) "" 'p' ""; esc "" '\\' ]);
    Alcotest.test_case "dcs: reset" `Quick (fun () ->
        check "reset" "\x1b[3;1\x1bP1$tx\x9c" [ dcs [ [ Some 1 ] ] "$" 't' "x" ]);
    Alcotest.test_case "dcs: parse" `Quick (fun () ->
        check "parse" "\x1bP0;1|17/ab\x9c"
          [ dcs [ [ Some 0 ]; [ Some 1 ] ] "" '|' "17/ab" ]);
    Alcotest.test_case "dcs: intermediate_reset_on_exit" `Quick (fun () ->
        check "intermediate_reset_on_exit" "\x1bP=1sZZZ\x1b+\x5c"
          [ dcs [ [ Some 1 ] ] "=" 's' "ZZZ"; esc "+" '\\' ]);
    Alcotest.test_case "dcs: put_utf8" `Quick (fun () ->
        check "put_utf8" "\x1bP+r\xf0\x9f\x98\x83\x1b\\"
          [ dcs [] "+" 'r' "\xf0\x9f\x98\x83"; esc "" '\\' ]);
    Alcotest.test_case "dcs: C1 introducer enters DCS" `Quick (fun () ->
        check "C1 introducer enters DCS" "\x901$r\x9c" [ dcs [ [ Some 1 ] ] "$" 'r' "" ]);
  ]

let csi_cases =
  [
    Alcotest.test_case "csi: no_params" `Quick (fun () ->
        check "no_params" "\x1b[m" [ csi [] "" 'm' ]);
    Alcotest.test_case "csi: C1 CSI after ESC" `Quick (fun () ->
        check "C1 CSI after ESC" "\x1b\x9bh" [ csi [] "" 'h' ]);
    Alcotest.test_case "csi: one_param" `Quick (fun () ->
        check "one_param" "\x1b[7m" [ csi [ [ Some 7 ] ] "" 'm' ]);
    Alcotest.test_case "csi: param_reset" `Quick (fun () ->
        check "param_reset" "\x1b[0mabc\x1b[1;2m"
          ((csi [ [ Some 0 ] ] "" 'm' :: prints "abc")
          @ [ csi [ [ Some 1 ]; [ Some 2 ] ] "" 'm' ]));
    Alcotest.test_case "csi: max_params" `Quick (fun () ->
        check "max_params"
          ("\x1b[" ^ repeat 31 "1;" ^ "p")
          [ csi (ones 31 @ [ [ None ] ]) "" 'p' ]);
    Alcotest.test_case "csi: 33 declared params" `Quick (fun () ->
        check "33 declared params"
          ("\x1b[" ^ repeat 33 "1;" ^ "m")
          [ csi (ones 32) "" 'm' ]);
    Alcotest.test_case "csi: ignore_long" `Quick (fun () ->
        check "ignore_long"
          ("\x1b[" ^ repeat 18 "1;" ^ "p")
          [ csi (ones 18 @ [ [ None ] ]) "" 'p' ]);
    Alcotest.test_case "csi: trailing_semicolon" `Quick (fun () ->
        check "trailing_semicolon" "\x1b[4;m" [ csi [ [ Some 4 ]; [ None ] ] "" 'm' ]);
    Alcotest.test_case "csi: leading_semicolon" `Quick (fun () ->
        check "leading_semicolon" "\x1b[;4m" [ csi [ [ None ]; [ Some 4 ] ] "" 'm' ]);
    Alcotest.test_case "csi: long_param" `Quick (fun () ->
        check "long_param" "\x1b[65535m" [ csi [ [ Some 65535 ] ] "" 'm' ]);
    Alcotest.test_case "csi: reset" `Quick (fun () ->
        check "reset" "\x1b[3;1\x1b[?1049h" [ csi [ [ Some 1049 ] ] "?" 'h' ]);
    Alcotest.test_case "csi: subparams" `Quick (fun () ->
        check "subparams" "\x1b[38:2:255:0:255;1m"
          [ csi [ [ Some 38; Some 2; Some 255; Some 0; Some 255 ]; [ Some 1 ] ] "" 'm' ]);
    Alcotest.test_case "csi: params_buffer_filled_with_subparams" `Quick (fun () ->
        check "params_buffer_filled_with_subparams"
          ("\x1b[" ^ repeat 32 ":" ^ "x\x1b")
          [ csi [ nothings 32 ] "" 'x'; execute '\x1b' ]);
    Alcotest.test_case "csi: dispatch_increment_boundary" `Quick (fun () ->
        check "dispatch_increment_boundary"
          ("\x1b[" ^ repeat 31 "1;" ^ "1m")
          [ csi (ones 32) "" 'm' ]);
  ]

let osc_cases =
  let big = String.make 70000 'a' in
  [
    Alcotest.test_case "osc: empty" `Quick (fun () ->
        check "empty" "\x1b]\x07" [ osc [ "" ] ]);
    Alcotest.test_case "osc: bell_terminated" `Quick (fun () ->
        check "bell_terminated" "\x1b]0;title\x07" [ osc [ "0"; "title" ] ]);
    Alcotest.test_case "osc: utf8" `Quick (fun () ->
        check "utf8" "\x1b]2;h\xc3\xa9llo\x07" [ osc [ "2"; "h\xc3\xa9llo" ] ]);
    Alcotest.test_case "osc: string_terminator" `Quick (fun () ->
        check "string_terminator" "\x1b]8;;u\x9c" [ osc [ "8"; ""; "u" ] ]);
    Alcotest.test_case "osc: c1_collected" `Quick (fun () ->
        check "c1_collected" "\x1b]8;\x9b;\x9c" [ osc [ "8"; "\x9b"; "" ] ]);
    Alcotest.test_case "osc: exceed_max_buffer_size" `Quick (fun () ->
        check "exceed_max_buffer_size"
          ("\x1b]52;c;" ^ big ^ "\x07")
          [ osc [ "52"; "c"; String.make 65531 'a' ] ]);
  ]

let cancel =
  [
    Alcotest.test_case "cancel: CAN in OSC" `Quick (fun () ->
        check "CAN in OSC" "\x1b]11;ff\x18abc" (prints "abc"));
    Alcotest.test_case "cancel: SUB in OSC" `Quick (fun () ->
        check "SUB in OSC" "\x1b]11;ff\x1aabc" (prints "abc"));
    Alcotest.test_case "cancel: CAN in DCS" `Quick (fun () ->
        check "CAN in DCS" "\x1bP1;2|x\x18abc" (prints "abc"));
    Alcotest.test_case "cancel: SUB in SOS" `Quick (fun () ->
        check "SUB in SOS" "\x1bXx\x1ay" [ print "y" ]);
    Alcotest.test_case "cancel: CAN in CSI executes" `Quick (fun () ->
        check "CAN in CSI executes" "\x1b[1\x18m" [ execute '\x18'; print "m" ]);
    Alcotest.test_case "cancel: ESC abandons CSI" `Quick (fun () ->
        check "ESC abandons CSI" "\x1b[1;2\x1bOa" [ esc "" 'O'; print "a" ]);
    Alcotest.test_case "cancel: CAN after ESC executes" `Quick (fun () ->
        check "CAN after ESC executes" "\x1b\x18" [ execute '\x18' ]);
  ]

let limits =
  [
    Alcotest.test_case "limits: extra prefixes ignored" `Quick (fun () ->
        check "extra prefixes ignored" "\x1b[?<>=1p" [ csi [ [ Some 1 ] ] "?" 'p' ]);
    Alcotest.test_case "limits: intermediates capped at two" `Quick (fun () ->
        check "intermediates capped at two" "\x1b[ $%q" [ csi [] " $" 'q' ]);
    Alcotest.test_case "limits: dcs payload cap" `Quick (fun () ->
        check "dcs payload cap"
          ("\x1bP+r" ^ String.make 70000 'x' ^ "\x1b\\")
          [ dcs [] "+" 'r' (String.make 65536 'x'); esc "" '\\' ]);
    Alcotest.test_case "limits: capped payload cancels on flush" `Quick (fun () ->
        let p = Parser.create () in
        let a = Parser.feed p ("\x1b]" ^ String.make 70000 'a') in
        Alcotest.check parser_testable "no action while consuming" [] a;
        Alcotest.check parser_testable "flush" [] (Parser.flush p);
        Alcotest.check parser_testable "ground after flush"
          [ print "b" ]
          (Parser.feed p "b"));
    Alcotest.test_case "limits: oversized OSC swallows CSI-looking tail" `Quick (fun () ->
        check "oversized OSC swallows CSI-looking tail"
          ("\x1b]52;" ^ String.make 70000 'x' ^ "\x9b31mINJECTED\x07after")
          (osc [ "52"; String.make 65533 'x' ] :: prints "after"));
    Alcotest.test_case "limits: oversized DCS swallows CSI-looking tail" `Quick (fun () ->
        check "oversized DCS swallows CSI-looking tail"
          ("\x1bP+r" ^ String.make 70000 'y' ^ "\x9b31mINJ\x1b\\" ^ "ok")
          (dcs [] "+" 'r' (String.make 65536 'y') :: esc "" '\\' :: prints "ok"));
  ]

let utf8 =
  [
    Alcotest.test_case "utf8: rune split across feeds" `Quick (fun () ->
        Alcotest.check parser_testable "rune split across feeds"
          [ print "\xf0\x9f\x91\x8b" ]
          (feed_all ~chunk:2 "\xf0\x9f\x91\x8b"));
    Alcotest.test_case "utf8: invalid continuation completes as replacement" `Quick
      (fun () ->
        check "invalid continuation completes as replacement" "\xc3\x41"
          [ print "\xef\xbf\xbd" ]);
    Alcotest.test_case "utf8: ESC swallowed mid rune" `Quick (fun () ->
        check "ESC swallowed mid rune" "\xc3\x1b" [ print "\xef\xbf\xbd" ]);
    Alcotest.test_case "utf8: rune after ESC prints" `Quick (fun () ->
        check "rune after ESC prints" "\x1b\xc3\xa9" [ print "\xc3\xa9" ]);
    Alcotest.test_case "utf8: overlong lead ignored" `Quick (fun () ->
        check "overlong lead ignored" "\xc0" []);
    Alcotest.test_case "utf8: surrogate rejected" `Quick (fun () ->
        check "surrogate rejected" "\xed\xa0\x80" [ print "\xef\xbf\xbd" ]);
    Alcotest.test_case "utf8: beyond U+10FFFF rejected" `Quick (fun () ->
        check "beyond U+10FFFF rejected" "\xf4\x90\x80\x80" [ print "\xef\xbf\xbd" ]);
    Alcotest.test_case "utf8: U+10FFFF accepted" `Quick (fun () ->
        check "U+10FFFF accepted" "\xf4\x8f\xbf\xbf" [ print "\xf4\x8f\xbf\xbf" ]);
    Alcotest.test_case "utf8: bare continuation byte is a C1 control" `Quick (fun () ->
        check "bare continuation byte is a C1 control" "\x80" [ execute '\x80' ]);
    Alcotest.test_case "utf8: bare 0xa0 ignored" `Quick (fun () ->
        check "bare 0xa0 ignored" "\xa0" []);
  ]

let composite =
  "\x1b[1;2;3mhello\x1b]8;;https://charm.example\x1b\\\x1bP1;2+q5448\x1b\\\xc3\xa9\n\
   \x1b]0;t\x07"

let composite_expected =
  (csi [ [ Some 1 ]; [ Some 2 ]; [ Some 3 ] ] "" 'm' :: prints "hello")
  @ [
      osc [ "8"; ""; "https://charm.example" ];
      esc "" '\\';
      dcs [ [ Some 1 ]; [ Some 2 ] ] "+" 'q' "5448";
      esc "" '\\';
      print "\xc3\xa9";
      execute '\n';
      osc [ "0"; "t" ];
    ]

let partition =
  [
    Alcotest.test_case "partition: chunks equal whole" `Quick (fun () ->
        let whole = feed_all composite in
        List.iter
          (fun c ->
            Alcotest.check parser_testable
              ("chunk " ^ string_of_int c)
              whole (feed_all ~chunk:c composite))
          [ 1; 2; 3; 5; 7; 8117 ]);
    Alcotest.test_case "partition: mid sequence split" `Quick (fun () ->
        Alcotest.check parser_testable "mid sequence split" composite_expected
          (feed_all ~chunk:4 composite));
    Alcotest.test_case "partition: family split across feeds" `Quick (fun () ->
        let p = Parser.create () in
        let a = Parser.feed p "\x1b[1;2" in
        let b = Parser.feed p ";3mok" in
        Alcotest.check parser_testable "first part" [] a;
        Alcotest.check parser_testable "second part"
          (csi [ [ Some 1 ]; [ Some 2 ]; [ Some 3 ] ] "" 'm' :: prints "ok")
          b);
    Alcotest.test_case "partition: string split across feeds" `Quick (fun () ->
        let p = Parser.create () in
        let a = Parser.feed p "\x1b]2;ti" in
        let b = Parser.feed p "tle\x07x" in
        Alcotest.check parser_testable "first part" [] a;
        Alcotest.check parser_testable "second part" [ osc [ "2"; "title" ]; print "x" ] b);
  ]

let flush =
  [
    Alcotest.test_case "flush: bare ESC executes" `Quick (fun () ->
        check "bare ESC executes" "\x1b" [ execute '\x1b' ]);
    Alcotest.test_case "flush: partial rune prints raw bytes" `Quick (fun () ->
        check "partial rune prints raw bytes" "\xe4\xb8" [ print "\xe4\xb8" ]);
    Alcotest.test_case "flush: partial escape intermediate cancels" `Quick (fun () ->
        check "partial escape intermediate cancels" "\x1b " []);
    Alcotest.test_case "flush: partial CSI cancels" `Quick (fun () ->
        check "partial CSI cancels" "\x1b[1;2" []);
    Alcotest.test_case "flush: partial OSC cancels" `Quick (fun () ->
        check "partial OSC cancels" "\x1b]2;ti" []);
    Alcotest.test_case "flush: ground afterwards" `Quick (fun () ->
        let p = Parser.create () in
        let a = Parser.feed p "\x1b[" in
        Alcotest.check parser_testable "partial" [] a;
        Alcotest.check parser_testable "flush" [] (Parser.flush p);
        Alcotest.check parser_testable "ground" [ print "a" ] (Parser.feed p "a"));
  ]

let sos_pm_apc =
  [
    Alcotest.test_case "sos_pm_apc: SOS dispatch" `Quick (fun () ->
        check "SOS dispatch" "\x1bXhello\x1b\\" [ Parser.Sos "hello" ]);
    Alcotest.test_case "sos_pm_apc: PM dispatch with C1 ST" `Quick (fun () ->
        check "PM dispatch with C1 ST" "\x1b^note\x9c" [ Parser.Pm "note" ]);
    Alcotest.test_case "sos_pm_apc: APC dispatch" `Quick (fun () ->
        check "APC dispatch" "\x1b_apc\x1b\\" [ Parser.Apc "apc" ]);
    Alcotest.test_case "sos_pm_apc: UTF-8 lead abandons the string" `Quick (fun () ->
        check "UTF-8 lead abandons the string" "\x1bXab\xc3\xa9\x1b\\"
          [ print "\xc3\xa9"; esc "" '\\' ]);
  ]

let cases =
  List.concat
    [
      decode;
      dcs_cases;
      csi_cases;
      osc_cases;
      cancel;
      limits;
      utf8;
      partition;
      flush;
      sos_pm_apc;
    ]
