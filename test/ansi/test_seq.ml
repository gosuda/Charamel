module Seq = Charm_ansi.Seq
module Link = Charm_ansi.Link

let wire name expected actual =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check Alcotest.string name expected (actual ()))

let rejects name run =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.match_raises name (function Invalid_argument _ -> true | _ -> false) run)

let cases =
  [
    wire "cursor home" "\x1b[H" (fun () -> Seq.cup ~row:1 ~col:1);
    wire "cursor position" "\x1b[24;80H" (fun () -> Seq.cup ~row:24 ~col:80);
    wire "cursor first row" "\x1b[1;40H" (fun () -> Seq.cup ~row:1 ~col:40);
    wire "cursor first column" "\x1b[3;1H" (fun () -> Seq.cup ~row:3 ~col:1);
    wire "cursor omitted row" "\x1b[;5H" (fun () -> Seq.cup ~row:0 ~col:5);
    wire "cursor relative movement" "\x1b[A\x1b[3B\x1b[12C\x1b[7D" (fun () ->
        Seq.cuu 1 ^ Seq.cud 3 ^ Seq.cuf 12 ^ Seq.cub 7);
    wire "cursor save and restore" "\x1b7\x1b8" (fun () ->
        Seq.save_cursor ^ Seq.restore_cursor);
    wire "erase line operands" "\x1b[K\x1b[1K\x1b[2K" (fun () ->
        Seq.el `To_end ^ Seq.el `To_start ^ Seq.el `All);
    wire "erase display operands" "\x1b[J\x1b[1J\x1b[2J\x1b[3J" (fun () ->
        Seq.ed `Below ^ Seq.ed `Above ^ Seq.ed `All ^ Seq.ed `Scrollback);
    wire "alternate screen and cursor modes" "\x1b[?1049h\x1b[?25l" (fun () ->
        Seq.decset Seq.alt_screen ^ Seq.decrst Seq.cursor_visible);
    wire "synchronized frame" "\x1b[?2026hframe\x1b[?2026l" (fun () ->
        Seq.decset Seq.sync_output ^ "frame" ^ Seq.decrst Seq.sync_output);
    wire "mouse click mode" "\x1b[?1000h\x1b[?1006h" (fun () -> Seq.mouse_on ~mode:`Click);
    wire "mouse motion mode" "\x1b[?1002h\x1b[?1006h" (fun () ->
        Seq.mouse_on ~mode:`Motion);
    wire "mouse all motion mode" "\x1b[?1003h\x1b[?1006h" (fun () ->
        Seq.mouse_on ~mode:`All);
    wire "mouse teardown" "\x1b[?1000l\x1b[?1002l\x1b[?1003l\x1b[?1016l\x1b[?1006l"
      (fun () -> Seq.mouse_off);
    wire "title" "\x1b]2;charm\x07" (fun () -> Seq.title "charm");
    wire "Unicode title" "\x1b]2;\u{00DC}\u{1F44D}\x07" (fun () ->
        Seq.title "\u{00DC}\u{1F44D}");
    wire "clipboard text" "\x1b]52;c;aGVsbG8=\x07" (fun () -> Seq.clipboard_osc52 "hello");
    wire "clipboard opaque bytes" "\x1b]52;c;AP+c\x07" (fun () ->
        Seq.clipboard_osc52 "\x00\xff\x9c");
    wire "clipboard clear" "\x1b]52;c;\x07" (fun () -> Seq.clipboard_osc52 "");
    wire "notification" "\x1b]9;done\x07" (fun () -> Seq.notify_osc9 "done");
    rejects "title rejects escape" (fun () -> ignore (Seq.title "a\x1b[31mb"));
    rejects "notification rejects C1 scalar" (fun () ->
        ignore (Seq.notify_osc9 "x\u{009C}y"));
    rejects "title rejects malformed bytes" (fun () -> ignore (Seq.title "x\x9cy"));
    wire "color queries" "\x1b]11;?\x07\x1b]10;?\x07\x1b]12;?\x07" (fun () ->
        Seq.bg_query ^ Seq.fg_query ^ Seq.cursor_color_query);
    wire "device queries" "\x1b[c\x1b[>q" (fun () -> Seq.da1 ^ Seq.xtversion);
    wire "kitty keyboard stack" "\x1b[>1u\x1b[>15u\x1b[>u\x1b[<u" (fun () ->
        Seq.kitty_push 1 ^ Seq.kitty_push 15 ^ Seq.kitty_push 0 ^ Seq.kitty_pop);
    wire "hyperlink parameters" "\x1b]8;id=1:other=2;https://example.test\x07" (fun () ->
        Link.osc8
          (Some { url = "https://example.test"; params = [ ("id", "1"); ("other", "2") ] }));
    wire "hyperlink Unicode and URL semicolon"
      "\x1b]8;;https://example.test/\u{00DC};a\x07" (fun () ->
        Link.osc8 (Some { url = "https://example.test/\u{00DC};a"; params = [] }));
    wire "hyperlink close" "\x1b]8;;\x07" (fun () -> Link.osc8 None);
    rejects "hyperlink rejects empty URL" (fun () ->
        ignore (Link.osc8 (Some { url = ""; params = [] })));
    rejects "hyperlink validates raw record" (fun () ->
        ignore (Link.osc8 (Some { url = "x\x07y"; params = [] })));
    rejects "hyperlink rejects parameter delimiter" (fun () ->
        ignore
          (Link.osc8 (Some { url = "https://example.test"; params = [ ("id", "x:y") ] })));
    rejects "hyperlink rejects parameter assignment" (fun () ->
        ignore
          (Link.osc8 (Some { url = "https://example.test"; params = [ ("a=b", "x") ] })));
  ]
