let wire name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check Alcotest.string name expected (Explain.explain input))

let cases =
  [
    (* Print, Execute and SGR truecolor: the three sequence families the app-level
       contract names as canonical [explain] output shapes. *)
    wire "print text" "hello" "Print \"hello\"\n";
    wire "execute lf" "\n" "Execute LF\n";
    wire "sgr truecolor" "\x1b[38;2;255;0;0m"
      "CSI 38;2;255;0;0 m  SGR: foreground color: rgb 255 0 0\n";
    (* Control mnemonics. *)
    wire "execute cr" "\r" "Execute CR\n";
    wire "execute tab" "\t" "Execute TAB\n";
    wire "lone esc flushes" "\x1b" "Execute ESC\n";
    wire "text then pending esc" "hi\x1b" "Print \"hi\"\nExecute ESC\n";
    (* SGR: 38/48/58 colors, the 4:x underline family, and reset. *)
    wire "sgr indexed background" "\x1b[48;5;209m"
      "CSI 48;5;209 m  SGR: background color: indexed 209\n";
    wire "sgr underline curly" "\x1b[4:3m" "CSI 4:3 m  SGR: curly underline\n";
    wire "sgr reset" "\x1b[m" "CSI m  SGR: reset style\n";
    wire "sgr compound" "\x1b[1;38:5:209m"
      "CSI 1;38:5:209 m  SGR: bold, foreground color: indexed 209\n";
    (* Cursor movement and erase. *)
    wire "cursor up" "\x1b[3A" "CSI 3 A  Cursor up 3\n";
    wire "cursor home" "\x1b[H" "CSI H  Set cursor position row=1 col=1\n";
    wire "cursor position" "\x1b[10;20H"
      "CSI 10;20 H  Set cursor position row=10 col=20\n";
    wire "cursor style" "\x1b[3 q" "CSI 3 q  Set cursor style: blinking underline\n";
    wire "save cursor" "\x1b7" "ESC 7  Save cursor\n";
    wire "restore cursor" "\x1b8" "ESC 8  Restore cursor\n";
    wire "erase display below" "\x1b[J" "CSI J  Erase screen below cursor\n";
    wire "erase entire display" "\x1b[2J" "CSI 2 J  Erase entire screen\n";
    wire "erase line right" "\x1b[K" "CSI K  Erase line right of cursor\n";
    wire "insert lines" "\x1b[3L" "CSI 3 L  Insert 3 blank line(s)\n";
    wire "delete lines" "\x1b[2M" "CSI 2 M  Delete 2 line(s)\n";
    wire "scroll region" "\x1b[10;20r"
      "CSI 10;20 r  Set scrolling region top=10 bottom=20\n";
    (* DEC private modes. *)
    wire "enable alternate screen" "\x1b[?1049h"
      "CSI ?1049 h  Enable private mode alternate screen (1049)\n";
    wire "disable cursor visibility" "\x1b[?25l"
      "CSI ?25 l  Disable private mode cursor visibility (25)\n";
    wire "enable mouse sgr pixels" "\x1b[?1016h"
      "CSI ?1016 h  Enable private mode mouse SGR pixel encoding (1016)\n";
    wire "request synchronized output" "\x1b[?2026$p"
      "CSI ?2026$p  Request private mode synchronized output (2026)\n";
    (* OSC: hyperlink (8), title, clipboard, notify. *)
    wire "hyperlink" "\x1b]8;;https://example.test\x07"
      "OSC 8;;https://example.test  Set hyperlink to \"https://example.test\"\n";
    wire "hyperlink with parameters" "\x1b]8;id=1:other=2;https://example.test\x07"
      "OSC 8;id=1:other=2;https://example.test  Set hyperlink to \
       \"https://example.test\" (id=1, other=2)\n";
    wire "hyperlink close" "\x1b]8;;\x07" "OSC 8;;  Close hyperlink\n";
    wire "set window title" "\x1b]2;hello\x07"
      "OSC 2;hello  Set window title to \"hello\"\n";
    wire "set clipboard" "\x1b]52;c;aGVsbG8=\x07"
      "OSC 52;c;aGVsbG8=  Set system clipboard to base64 \"aGVsbG8=\"\n";
    wire "request clipboard" "\x1b]52;c;?\x07" "OSC 52;c;?  Request system clipboard\n";
    wire "notify" "\x1b]9;done\x07" "OSC 9;done  Show desktop notification \"done\"\n";
    wire "request foreground color" "\x1b]10;?\x07" "OSC 10;?  Request foreground color\n";
    (* DCS: XTGETTCAP and the terminal name/version reply. *)
    wire "termcap request" "\x1bP+q544e\x1b\\"
      "DCS +q \"544e\"  Request termcap entry for TN\nESC \\  String terminator\n";
    wire "terminal version reply" "\x1bP>|XTerm(370)\x1b\\"
      "DCS > | \"XTerm(370)\"  Terminal name and version: XTerm(370)\n\
       ESC \\  String terminator\n";
    wire "request terminal version" "\x1b[>q"
      "CSI > q  Request terminal name and version\n";
    wire "request device attributes" "\x1b[c" "CSI c  Request primary device attributes\n";
    (* Kitty keyboard protocol. *)
    wire "kitty push" "\x1b[>5u"
      "CSI >5 u  Push Kitty keyboard flags: disambiguate escape codes, report alternate \
       keys\n";
    wire "kitty pop" "\x1b[<2u" "CSI <2 u  Pop 2 Kitty keyboard flag(s)\n";
    wire "kitty request" "\x1b[?u" "CSI ? u  Request Kitty keyboard flags\n";
    wire "kitty set" "\x1b[=5;1u"
      "CSI =5;1 u  Set Kitty keyboard flags to disambiguate escape codes, report \
       alternate keys (replacing existing flags)\n";
    wire "kitty disable" "\x1b[>0u" "CSI >0 u  Disable Kitty keyboard\n";
    (* Mouse SGR reports. *)
    wire "mouse press" "\x1b[<0;10;20M"
      "CSI <0;10;20 M  Mouse press button=left x=10 y=20\n";
    wire "mouse release" "\x1b[<0;10;20m"
      "CSI <0;10;20 m  Mouse release button=left x=10 y=20\n";
    wire "mouse wheel" "\x1b[<64;5;5M"
      "CSI <64;5;5 M  Mouse press button=wheel up x=5 y=5\n";
    wire "mouse with modifiers" "\x1b[<20;3;4M"
      "CSI <20;3;4 M  Mouse press button=left modifiers=shift+ctrl x=3 y=4\n";
    (* Mixed text and control interleaving. *)
    wire "text interleaved with sgr" "hi\x1b[1mbold\x1b[0m"
      "Print \"hi\"\nCSI 1 m  SGR: bold\nPrint \"bold\"\nCSI 0 m  SGR: reset style\n";
    (* Unrecognized sequences preserve the reconstructed raw form with no fabricated
       description. *)
    wire "unrecognized csi" "\x1b[5x" "CSI 5 x  Unknown\n";
    wire "unrecognized osc" "\x1b]999;foo\x07" "OSC 999;foo  Unknown\n";
    wire "unrecognized apc" "\x1b_data\x1b\\" "APC \"data\"\n";
  ]
