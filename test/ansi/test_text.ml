module Text = Charamel_ansi.Text

type truncate_case = {
  name : string;
  input : string;
  extra : string;
  width : int;
  expect_right : string;
  expect_left : string;
}

(* The shared table in .references/x/ansi/truncate_test.go drives both
   Truncate and TruncateLeft.  [extra] is the Go table's tail/prefix column. *)
let truncate_vectors =
  [
    {
      name = "empty";
      input = "";
      extra = "";
      width = 0;
      expect_right = "";
      expect_left = "";
    };
    {
      name = "truncate_length_0";
      input = "foo";
      extra = "";
      width = 0;
      expect_right = "";
      expect_left = "foo";
    };
    {
      name = "equalascii";
      input = "one";
      extra = ".";
      width = 3;
      expect_right = "one";
      expect_left = "";
    };
    {
      name = "equalemoji";
      input = "on👋";
      extra = ".";
      width = 3;
      expect_right = "on.";
      expect_left = ".👋";
    };
    {
      name = "simple_multiple_words";
      input = "a couple of words";
      extra = "";
      width = 6;
      expect_right = "a coup";
      expect_left = "le of words";
    };
    {
      name = "equalcontrolemoji";
      input = "one\x1b[0m";
      extra = ".";
      width = 3;
      expect_right = "one\x1b[0m";
      expect_left = "\x1b[0m";
    };
    {
      name = "truncate_tail_greater";
      input = "foo";
      extra = "...";
      width = 5;
      expect_right = "foo";
      expect_left = "";
    };
    {
      name = "simple";
      input = "foobar";
      extra = "";
      width = 3;
      expect_right = "foo";
      expect_left = "bar";
    };
    {
      name = "passthrough";
      input = "foobar";
      extra = "";
      width = 10;
      expect_right = "foobar";
      expect_left = "";
    };
    {
      name = "ascii";
      input = "hello";
      extra = "";
      width = 3;
      expect_right = "hel";
      expect_left = "lo";
    };
    {
      name = "emoji";
      input = "👋";
      extra = "";
      width = 2;
      expect_right = "👋";
      expect_left = "";
    };
    {
      name = "wideemoji";
      input = "🫧";
      extra = "";
      width = 2;
      expect_right = "🫧";
      expect_left = "";
    };
    {
      name = "controlemoji";
      input = "\x1b[31mhello 👋abc\x1b[0m";
      extra = "";
      width = 8;
      expect_right = "\x1b[31mhello 👋\x1b[0m";
      expect_left = "\x1b[31mabc\x1b[0m";
    };
    {
      name = "osc8";
      input = "\x1b]8;;https://charm.sh\x1b\\Charmbracelet 🫧\x1b]8;;\x1b\\";
      extra = "";
      width = 5;
      expect_right = "\x1b]8;;https://charm.sh\x1b\\Charm\x1b]8;;\x1b\\";
      expect_left = "\x1b]8;;https://charm.sh\x1b\\bracelet 🫧\x1b]8;;\x1b\\";
    };
    {
      name = "osc8_8bit";
      input = "\x9d8;;https://charm.sh\x9cCharmbracelet 🫧\x9d8;;\x9c";
      extra = "";
      width = 5;
      expect_right = "\x9d8;;https://charm.sh\x9cCharm\x9d8;;\x9c";
      expect_left = "\x9d8;;https://charm.sh\x9cbracelet 🫧\x9d8;;\x9c";
    };
    {
      name = "style_tail";
      input = "\x1b[38;5;219mHiya!";
      extra = "…";
      width = 3;
      expect_right = "\x1b[38;5;219mHi…";
      expect_left = "\x1b[38;5;219m…a!";
    };
    {
      name = "double_style_tail";
      input = "\x1b[38;5;219mHiya!\x1b[38;5;219mHello";
      extra = "…";
      width = 7;
      expect_right = "\x1b[38;5;219mHiya!\x1b[38;5;219mH…";
      expect_left = "\x1b[38;5;219m\x1b[38;5;219m…llo";
    };
    {
      name = "noop";
      input = "\x1b[7m--";
      extra = "";
      width = 2;
      expect_right = "\x1b[7m--";
      expect_left = "\x1b[7m";
    };
    {
      name = "double_width";
      input = "\x1b[38;2;249;38;114m你好\x1b[0m";
      extra = "";
      width = 3;
      expect_right = "\x1b[38;2;249;38;114m你\x1b[0m";
      expect_left = "\x1b[38;2;249;38;114m好\x1b[0m";
    };
    {
      name = "double_width_rune";
      input = "你";
      extra = "";
      width = 1;
      expect_right = "";
      expect_left = "你";
    };
    {
      name = "double_width_runes";
      input = "你好";
      extra = "";
      width = 2;
      expect_right = "你";
      expect_left = "好";
    };
    {
      name = "spaces_only";
      input = "    ";
      extra = "…";
      width = 2;
      expect_right = " …";
      expect_left = "…  ";
    };
    {
      name = "longer_tail";
      input = "foo";
      extra = "...";
      width = 2;
      expect_right = "";
      expect_left = "...o";
    };
    {
      name = "same_tail_width";
      input = "foo";
      extra = "...";
      width = 3;
      expect_right = "foo";
      expect_left = "";
    };
    {
      name = "same_tail_width_control";
      input = "\x1b[31mfoo\x1b[0m";
      extra = "...";
      width = 3;
      expect_right = "\x1b[31mfoo\x1b[0m";
      expect_left = "\x1b[31m\x1b[0m";
    };
    {
      name = "same_width";
      input = "foo";
      extra = "";
      width = 3;
      expect_right = "foo";
      expect_left = "";
    };
    {
      name = "truncate_with_tail";
      input = "foobar";
      extra = ".";
      width = 4;
      expect_right = "foo.";
      expect_left = ".ar";
    };
    {
      name = "style";
      input = "I really \x1b[38;2;249;38;114mlove\x1b[0m Go!";
      extra = "";
      width = 8;
      expect_right = "I really\x1b[38;2;249;38;114m\x1b[0m";
      expect_left = " \x1b[38;2;249;38;114mlove\x1b[0m Go!";
    };
    {
      name = "dcs";
      input =
        "\x1bPq#0;2;0;0;0#1;2;100;100;0#2;2;0;100;0#1~~@@vv@@~~@@~~$#2??}}GG}}??}}??-#1!14@\x1b\\foobar";
      extra = "…";
      width = 4;
      expect_right =
        "\x1bPq#0;2;0;0;0#1;2;100;100;0#2;2;0;100;0#1~~@@vv@@~~@@~~$#2??}}GG}}??}}??-#1!14@\x1b\\foo…";
      expect_left =
        "\x1bPq#0;2;0;0;0#1;2;100;100;0#2;2;0;100;0#1~~@@vv@@~~@@~~$#2??}}GG}}??}}??-#1!14@\x1b\\…ar";
    };
    {
      name = "emoji_tail";
      input = "\x1b[36mHello there!\x1b[m";
      extra = "😃";
      width = 8;
      expect_right = "\x1b[36mHello 😃\x1b[m";
      expect_left = "\x1b[36m😃ere!\x1b[m";
    };
    {
      name = "unicode";
      input = "\x1b[35mClaire‘s Boutique\x1b[0m";
      extra = "";
      width = 8;
      expect_right = "\x1b[35mClaire‘s\x1b[0m";
      expect_left = "\x1b[35m Boutique\x1b[0m";
    };
    {
      name = "wide_chars";
      input = "こんにちは";
      extra = "…";
      width = 7;
      expect_right = "こんに…";
      expect_left = "…ちは";
    };
    {
      name = "style_wide_chars";
      input = "\x1b[35mこんにちは\x1b[m";
      extra = "…";
      width = 7;
      expect_right = "\x1b[35mこんに…\x1b[m";
      expect_left = "\x1b[35m…ちは\x1b[m";
    };
    {
      name = "osc8_lf";
      input = "สวัสดีสวัสดี\x1b]8;;https://example.com\x1b\\\nสวัสดีสวัสดี\x1b]8;;\x1b\\";
      extra = "…";
      width = 9;
      expect_right = "สวัสดีสวัสดี\x1b]8;;https://example.com\x1b\\\n…\x1b]8;;\x1b\\";
      expect_left = "\x1b]8;;https://example.com\x1b\\…วัสดีสวัสดี\x1b]8;;\x1b\\";
    };
    {
      name = "simple_japanese_text_prefix_suffix";
      input = "耐許ヱヨカハ調出あゆ監";
      extra = "…";
      width = 13;
      expect_right = "耐許ヱヨカハ…";
      expect_left = "…調出あゆ監";
    };
    {
      name = "simple_japanese_text";
      input = "耐許ヱヨカハ調出あゆ監";
      extra = "";
      width = 14;
      expect_right = "耐許ヱヨカハ調";
      expect_left = "出あゆ監";
    };
    {
      name = "new_line_inside_and_outside_range";
      input = "\n\nsomething\nin\nthe\nway\n\n";
      extra = "-";
      width = 10;
      expect_right = "\n\nsomething\n-";
      expect_left = "-n\nthe\nway\n\n";
    };
    {
      name = "multi_width_graphemes_with_newlines_japanese_text";
      input =
        {|耐許ヱヨカハ調出あゆ監件び理別よン國給災レホチ権輝モエフ会割もフ響3現エツ文時しだびほ経機ムイメフ敗文ヨク現義なさド請情ゆじょて憶主管州けでふく。排ゃわつげ美刊ヱミ出見ツ南者オ抜豆ハトロネ論索モネニイ任償スヲ話破リヤヨ秒止口イセソス止央のさ食周健でてつだ官送ト読聴遊容ひるべ。際ぐドらづ市居ネムヤ研校35岩6繹ごわク報拐イ革深52球ゃレスご究東スラ衝3間ラ録占たス。

禁にンご忘康ざほぎル騰般ねど事超スんいう真表何カモ自浩ヲシミ図客線るふ静王ぱーま写村月掛焼詐面ぞゃ。昇強ごントほ価保キ族85岡モテ恋困ひりこな刊並せご出来ぼぎむう点目ヲウ止環公ニレ事応タス必書タメムノ当84無信升ちひょ。価ーぐ中客テサ告覧ヨトハ極整
ラ得95稿はかラせ江利ス宏丸霊ミ考整ス静将ず業巨職ノラホ収嗅ざな。|};
      extra = "";
      width = 14;
      expect_right = "耐許ヱヨカハ調";
      expect_left =
        {|出あゆ監件び理別よン國給災レホチ権輝モエフ会割もフ響3現エツ文時しだびほ経機ムイメフ敗文ヨク現義なさド請情ゆじょて憶主管州けでふく。排ゃわつげ美刊ヱミ出見ツ南者オ抜豆ハトロネ論索モネニイ任償スヲ話破リヤヨ秒止口イセソス止央のさ食周健でてつだ官送ト読聴遊容ひるべ。際ぐドらづ市居ネムヤ研校35岩6繹ごわク報拐イ革深52球ゃレスご究東スラ衝3間ラ録占たス。

禁にンご忘康ざほぎル騰般ねど事超スんいう真表何カモ自浩ヲシミ図客線るふ静王ぱーま写村月掛焼詐面ぞゃ。昇強ごントほ価保キ族85岡モテ恋困ひりこな刊並せご出来ぼぎむう点目ヲウ止環公ニレ事応タス必書タメムノ当84無信升ちひょ。価ーぐ中客テサ告覧ヨトハ極整
ラ得95稿はかラせ江利ス宏丸霊ミ考整ス静将ず業巨職ノラホ収嗅ざな。|};
    };
  ]

let truncate_case c =
  Alcotest.test_case ("truncate/" ^ c.name) `Quick (fun () ->
      Alcotest.(check string)
        "right" c.expect_right
        (Text.truncate ~tail:c.extra ~width:c.width c.input);
      Alcotest.(check string)
        "left" c.expect_left
        (Text.truncate_left ~prefix:c.extra ~width:c.width c.input))

type wrap_case = { name : string; input : string; expected : string; width : int }

type hardwrap_case = {
  name : string;
  input : string;
  expected : string;
  width : int;
  preserve_space : bool;
}

let hardwrap_vectors =
  [
    { name = "empty_string"; input = ""; width = 0; expected = ""; preserve_space = true };
    {
      name = "passthrough";
      input = "foobar\n ";
      width = 0;
      expected = "foobar\n ";
      preserve_space = true;
    };
    { name = "pass"; input = "foo"; width = 4; expected = "foo"; preserve_space = true };
    {
      name = "simple";
      input = "foobarfoo";
      width = 4;
      expected = "foob\narfo\no";
      preserve_space = true;
    };
    {
      name = "lf";
      input = "f\no\nobar";
      width = 3;
      expected = "f\no\noba\nr";
      preserve_space = true;
    };
    {
      name = "lf_space";
      input = "foo bar\n  baz";
      width = 3;
      expected = "foo\n ba\nr\n  b\naz";
      preserve_space = true;
    };
    {
      name = "tab";
      input = "foo\tbar";
      width = 3;
      expected = "foo\n\tbar";
      preserve_space = true;
    };
    {
      name = "unicode_space";
      input = "foo\u{00A0}bar";
      width = 3;
      expected = "foo\nbar";
      preserve_space = false;
    };
    {
      name = "style_nochange";
      input =
        "\x1b[38;2;249;38;114mfoo\x1b[0m\x1b[38;2;248;248;242m \
         \x1b[0m\x1b[38;2;230;219;116mbar\x1b[0m";
      width = 7;
      expected =
        "\x1b[38;2;249;38;114mfoo\x1b[0m\x1b[38;2;248;248;242m \
         \x1b[0m\x1b[38;2;230;219;116mbar\x1b[0m";
      preserve_space = true;
    };
    {
      name = "style";
      input =
        "\x1b[38;2;249;38;114m(\x1b[0m\x1b[38;2;248;248;242mjust another \
         test\x1b[38;2;249;38;114m)\x1b[0m";
      width = 3;
      expected =
        "\x1b[38;2;249;38;114m(\x1b[0m\x1b[38;2;248;248;242mju\n\
         st \n\
         ano\n\
         the\n\
         r t\n\
         est\x1b[38;2;249;38;114m\n\
         )\x1b[0m";
      preserve_space = true;
    };
    {
      name = "style_lf";
      input = "I really \x1b[38;2;249;38;114mlove\x1b[0m Go!";
      width = 8;
      expected = "I really\n\x1b[38;2;249;38;114mlove\x1b[0m Go!";
      preserve_space = false;
    };
    {
      name = "style_emoji";
      input = "I really \x1b[38;2;249;38;114mlove u🫧\x1b[0m";
      width = 8;
      expected = "I really\n\x1b[38;2;249;38;114mlove u🫧\x1b[0m";
      preserve_space = false;
    };
    {
      name = "hyperlink";
      input = "I really \x1b]8;;https://example.com/\x1b\\love\x1b]8;;\x1b\\ Go!";
      width = 10;
      expected = "I really \x1b]8;;https://example.com/\x1b\\l\nove\x1b]8;;\x1b\\ Go!";
      preserve_space = false;
    };
    {
      name = "dcs";
      input =
        "\x1bPq#0;2;0;0;0#1;2;100;100;0#2;2;0;100;0#1~~@@vv@@~~@@~~$#2??}}GG}}??}}??-#1!14@\x1b\\foobar";
      width = 3;
      expected =
        "\x1bPq#0;2;0;0;0#1;2;100;100;0#2;2;0;100;0#1~~@@vv@@~~@@~~$#2??}}GG}}??}}??-#1!14@\x1b\\foo\n\
         bar";
      preserve_space = false;
    };
    {
      name = "begin_with_space";
      input = " foo";
      width = 4;
      expected = " foo";
      preserve_space = false;
    };
    {
      name = "style_dont_affect_wrap";
      input =
        "\x1b[38;2;249;38;114mfoo\x1b[0m\x1b[38;2;248;248;242m \
         \x1b[0m\x1b[38;2;230;219;116mbar\x1b[0m";
      width = 7;
      expected =
        "\x1b[38;2;249;38;114mfoo\x1b[0m\x1b[38;2;248;248;242m \
         \x1b[0m\x1b[38;2;230;219;116mbar\x1b[0m";
      preserve_space = false;
    };
    {
      name = "preserve_style";
      input =
        "\x1b[38;2;249;38;114m(\x1b[0m\x1b[38;2;248;248;242mjust another \
         test\x1b[38;2;249;38;114m)\x1b[0m";
      width = 3;
      expected =
        "\x1b[38;2;249;38;114m(\x1b[0m\x1b[38;2;248;248;242mju\n\
         st \n\
         ano\n\
         the\n\
         r t\n\
         est\x1b[38;2;249;38;114m\n\
         )\x1b[0m";
      preserve_space = false;
    };
    {
      name = "emoji";
      input = "foo🫧foobar";
      width = 4;
      expected = "foo\n🫧fo\nobar";
      preserve_space = false;
    };
    {
      name = "osc8_wrap";
      input = "สวัสดีสวัสดี\x1b]8;;https://example.com\x1b\\สวัสดีสวัสดี\x1b]8;;\x1b\\";
      width = 8;
      expected = "สวัสดีสวัสดี\x1b]8;;https://example.com\x1b\\\nสวัสดีสวัสดี\x1b]8;;\x1b\\";
      preserve_space = false;
    };
    {
      name = "column";
      input = "VERTICAL";
      width = 1;
      expected = "V\nE\nR\nT\nI\nC\nA\nL";
      preserve_space = false;
    };
    {
      name = "over_wide_cluster_owns_its_line";
      input = "漢x";
      width = 1;
      expected = "漢\nx";
      preserve_space = false;
    };
    {
      name = "over_wide_cluster_breaks_to_its_own_line";
      input = "a漢";
      width = 1;
      expected = "a\n漢";
      preserve_space = false;
    };
  ]

let hardwrap_case c =
  Alcotest.test_case ("hardwrap/" ^ c.name) `Quick (fun () ->
      Alcotest.(check string)
        "wrapped" c.expected
        (Text.hardwrap ~preserve_space:c.preserve_space ~width:c.width c.input))

type wordwrap_case = {
  name : string;
  input : string;
  expected : string;
  width : int;
  breakpoints : string;
}

let wordwrap_vectors =
  [
    { name = "empty_string"; input = ""; width = 0; expected = ""; breakpoints = "" };
    {
      name = "passthrough";
      input = "foobar\n ";
      width = 0;
      expected = "foobar\n ";
      breakpoints = "";
    };
    { name = "pass"; input = "foo"; width = 3; expected = "foo"; breakpoints = "" };
    {
      name = "toolong";
      input = "foobarfoo";
      width = 4;
      expected = "foobarfoo";
      breakpoints = "";
    };
    {
      name = "white_space";
      input = "foo bar foo";
      width = 4;
      expected = "foo\nbar\nfoo";
      breakpoints = "";
    };
    {
      name = "broken_at_spaces";
      input = "foo bars foobars";
      width = 4;
      expected = "foo\nbars\nfoobars";
      breakpoints = "";
    };
    {
      name = "hyphen";
      input = "foo-foobar";
      width = 4;
      expected = "foo-\nfoobar";
      breakpoints = "-";
    };
    {
      name = "emoji_breakpoint";
      input = "foo😃 foobar";
      width = 4;
      expected = "foo😃\nfoobar";
      breakpoints = "😃";
    };
    {
      name = "wide_emoji_breakpoint";
      input = "foo🫧 foobar";
      width = 4;
      expected = "foo🫧\nfoobar";
      breakpoints = "🫧";
    };
    {
      name = "space_breakpoint";
      input = "foo --bar";
      width = 9;
      expected = "foo --bar";
      breakpoints = "-";
    };
    {
      name = "simple";
      input = "foo bars foobars";
      width = 4;
      expected = "foo\nbars\nfoobars";
      breakpoints = "";
    };
    {
      name = "limit";
      input = "foo bar";
      width = 5;
      expected = "foo\nbar";
      breakpoints = "";
    };
    {
      name = "remove_white_spaces";
      input = "foo    \nb   ar   ";
      width = 4;
      expected = "foo\nb\nar";
      breakpoints = "";
    };
    {
      name = "white_space_trail_width";
      input = "foo\nb\t a\n bar";
      width = 4;
      expected = "foo\nb\t a\n bar";
      breakpoints = "";
    };
    {
      name = "explicit_line_break";
      input = "foo bar foo\n";
      width = 4;
      expected = "foo\nbar\nfoo\n";
      breakpoints = "";
    };
    {
      name = "explicit_breaks";
      input = "\nfoo bar\n\n\nfoo\n";
      width = 4;
      expected = "\nfoo\nbar\n\n\nfoo\n";
      breakpoints = "";
    };
    {
      name = "example";
      input = " This is a list: \n\n\t* foo\n\t* bar\n\n\n\t* foo  \nbar    ";
      width = 6;
      expected = " This\nis a\nlist: \n\n\t* foo\n\t* bar\n\n\n\t* foo\nbar";
      breakpoints = "";
    };
    {
      name = "style_code_dont_affect_length";
      input =
        "\x1b[38;2;249;38;114mfoo\x1b[0m\x1b[38;2;248;248;242m \
         \x1b[0m\x1b[38;2;230;219;116mbar\x1b[0m";
      width = 7;
      expected =
        "\x1b[38;2;249;38;114mfoo\x1b[0m\x1b[38;2;248;248;242m \
         \x1b[0m\x1b[38;2;230;219;116mbar\x1b[0m";
      breakpoints = "";
    };
    {
      name = "style_code_dont_get_wrapped";
      input =
        "\x1b[38;2;249;38;114m(\x1b[0m\x1b[38;2;248;248;242mjust another \
         test\x1b[38;2;249;38;114m)\x1b[0m";
      width = 3;
      expected =
        "\x1b[38;2;249;38;114m(\x1b[0m\x1b[38;2;248;248;242mjust\n\
         another\n\
         test\x1b[38;2;249;38;114m)\x1b[0m";
      breakpoints = "";
    };
    {
      name = "osc8_wrap";
      input = "สวัสดีสวัสดี\x1b]8;;https://example.com\x1b\\ สวัสดีสวัสดี\x1b]8;;\x1b\\";
      width = 8;
      expected = "สวัสดีสวัสดี\x1b]8;;https://example.com\x1b\\\nสวัสดีสวัสดี\x1b]8;;\x1b\\";
      breakpoints = "";
    };
  ]

let wordwrap_case c =
  Alcotest.test_case ("wordwrap/" ^ c.name) `Quick (fun () ->
      Alcotest.(check string)
        "wrapped" c.expected
        (Text.wordwrap ~breakpoints:c.breakpoints ~width:c.width c.input))

(* The combined table in wrap_test.go has a different shape from the hard and
   word wrapping tables.  The two Japanese rows are kept verbatim as the
   high-width grapheme regression corpus. *)
let wrap_vectors =
  [
    {
      name = "simple";
      input = "I really \x1b[38;2;249;38;114mlove\x1b[0m Go!";
      width = 8;
      expected = "I really\n\x1b[38;2;249;38;114mlove\x1b[0m Go!";
    };
    { name = "passthrough"; input = "hello world"; width = 11; expected = "hello world" };
    { name = "asian"; input = "こんにち"; width = 7; expected = "こんに\nち" };
    { name = "emoji"; input = "😃👰🏻‍♀️🫧"; width = 2; expected = "😃\n👰🏻‍♀️\n🫧" };
    {
      name = "long_style";
      input = "\x1b[38;2;249;38;114ma really long string\x1b[0m";
      width = 10;
      expected = "\x1b[38;2;249;38;114ma really\nlong\nstring\x1b[0m";
    };
    {
      name = "long_style_nbsp";
      input = "\x1b[38;2;249;38;114ma really\u{00A0}long string\x1b[0m";
      width = 10;
      expected = "\x1b[38;2;249;38;114ma\nreally\u{00A0}lon\ng string\x1b[0m";
    };
    {
      name = "longer";
      input = "the quick brown foxxxxxxxxxxxxxxxx jumped over the lazy dog.";
      width = 16;
      expected = "the quick brown\nfoxxxxxxxxxxxxxx\nxx jumped over\nthe lazy dog.";
    };
    {
      name = "longer_asian";
      input = "猴 猴 猴猴 猴猴猴猴猴猴猴猴猴 猴猴猴 猴猴 猴’ 猴猴 猴.";
      width = 16;
      expected = "猴 猴 猴猴\n猴猴猴猴猴猴猴猴\n猴 猴猴猴 猴猴\n猴’ 猴猴 猴.";
    };
    {
      name = "long_input";
      input =
        "Rotated keys for \
         a-good-offensive-cheat-code-incorporated/animal-like-law-on-the-rocks.";
      width = 76;
      expected =
        "Rotated keys for a-good-offensive-cheat-code-incorporated/animal-like-law-\n\
         on-the-rocks.";
    };
    {
      name = "long_input2";
      input =
        "Rotated keys for \
         a-good-offensive-cheat-code-incorporated/crypto-line-operating-system.";
      width = 76;
      expected =
        "Rotated keys for a-good-offensive-cheat-code-incorporated/crypto-line-\n\
         operating-system.";
    };
    {
      name = "hyphen_breakpoint";
      input = "a-good-offensive-cheat-code";
      width = 10;
      expected = "a-good-\noffensive-\ncheat-code";
    };
    {
      name = "exact";
      input = "\x1b[91mfoo\x1b[0";
      width = 3;
      expected = "\x1b[91mfoo\x1b[0";
    };
    { name = "extra_space"; input = "foo "; width = 3; expected = "foo" };
    {
      name = "extra_space_style";
      input = "\x1b[mfoo \x1b[m";
      width = 3;
      expected = "\x1b[mfoo\x1b[m";
    };
    {
      name = "paragraph_styles";
      input =
        "Lorem ipsum dolor \x1b[1msit\x1b[m amet, consectetur adipiscing elit, sed do \
         eiusmod tempor incididunt ut labore et dolore magna aliqua. \x1b[31mUt \
         enim\x1b[m ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut \
         aliquip ex ea \x1b[38;5;200mcommodo consequat\x1b[m. Duis aute irure dolor in \
         reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. \
         \x1b[1;2;33mExcepteur sint occaecat cupidatat non proident, sunt in culpa qui \
         officia deserunt mollit anim id est laborum.\x1b[m";
      width = 30;
      expected =
        "Lorem ipsum dolor \x1b[1msit\x1b[m amet,\n\
         consectetur adipiscing elit,\n\
         sed do eiusmod tempor\n\
         incididunt ut labore et dolore\n\
         magna aliqua. \x1b[31mUt enim\x1b[m ad minim\n\
         veniam, quis nostrud\n\
         exercitation ullamco laboris\n\
         nisi ut aliquip ex ea \x1b[38;5;200mcommodo\n\
         consequat\x1b[m. Duis aute irure\n\
         dolor in reprehenderit in\n\
         voluptate velit esse cillum\n\
         dolore eu fugiat nulla\n\
         pariatur. \x1b[1;2;33mExcepteur sint\n\
         occaecat cupidatat non\n\
         proident, sunt in culpa qui\n\
         officia deserunt mollit anim\n\
         id est laborum.\x1b[m";
    };
    {
      name = "multi_byte_spaces";
      input =
        "A\u{202F}B\u{202F}C\u{202F}DA\u{205F}\u{205F}B\u{205F}C\u{205F}DA\u{3000}B\u{3000}C\u{3000}D";
      width = 7;
      expected =
        "A\u{202F}B\u{202F}C\nDA\u{205F}\u{205F}B\u{205F}C\nDA\u{3000}B\nC\u{3000}D";
    };
    { name = "hyphen_break"; input = "foo-bar"; width = 5; expected = "foo-\nbar" };
    {
      name = "double_space";
      input = "f  bar foobaz";
      width = 6;
      expected = "f  bar\nfoobaz";
    };
    { name = "passthrough_lines"; input = "foobar\n "; width = 0; expected = "foobar\n " };
    { name = "pass"; input = "foo"; width = 3; expected = "foo" };
    { name = "toolong"; input = "foobarfoo"; width = 4; expected = "foob\narfo\no" };
    { name = "white_space"; input = "foo bar foo"; width = 4; expected = "foo\nbar\nfoo" };
    {
      name = "broken_at_spaces";
      input = "foo bars foobars";
      width = 4;
      expected = "foo\nbars\nfoob\nars";
    };
    { name = "hyphen"; input = "foob-foobar"; width = 4; expected = "foob\n-foo\nbar" };
    {
      name = "wide_emoji_breakpoint";
      input = "foo🫧 foobar";
      width = 4;
      expected = "foo\n🫧\nfoob\nar";
    };
    { name = "space_breakpoint"; input = "foo --bar"; width = 9; expected = "foo --bar" };
    {
      name = "simple_words";
      input = "foo bars foobars";
      width = 4;
      expected = "foo\nbars\nfoob\nars";
    };
    { name = "limit"; input = "foo bar"; width = 5; expected = "foo\nbar" };
    {
      name = "remove_white_spaces";
      input = "foo    \nb   ar   ";
      width = 4;
      expected = "foo\nb\nar";
    };
    {
      name = "white_space_trail_width";
      input = "foo\nb\t a\n bar";
      width = 4;
      expected = "foo\nb\t a\n bar";
    };
    {
      name = "explicit_line_break";
      input = "foo bar foo\n";
      width = 4;
      expected = "foo\nbar\nfoo\n";
    };
    {
      name = "explicit_breaks";
      input = "\nfoo bar\n\n\nfoo\n";
      width = 4;
      expected = "\nfoo\nbar\n\n\nfoo\n";
    };
    {
      name = "example";
      input = " This is a list: \n\n\t* foo\n\t* bar\n\n\n\t* foo  \nbar    ";
      width = 6;
      expected = " This\nis a\nlist: \n\n\t* foo\n\t* bar\n\n\n\t* foo\nbar";
    };
    {
      name = "style_code_dont_affect_length";
      input =
        "\x1b[38;2;249;38;114mfoo\x1b[0m\x1b[38;2;248;248;242m \
         \x1b[0m\x1b[38;2;230;219;116mbar\x1b[0m";
      width = 7;
      expected =
        "\x1b[38;2;249;38;114mfoo\x1b[0m\x1b[38;2;248;248;242m \
         \x1b[0m\x1b[38;2;230;219;116mbar\x1b[0m";
    };
    {
      name = "style_code_dont_get_wrapped";
      input =
        "\x1b[38;2;249;38;114m(\x1b[0m\x1b[38;2;248;248;242mjust another \
         test\x1b[38;2;249;38;114m)\x1b[0m";
      width = 7;
      expected =
        "\x1b[38;2;249;38;114m(\x1b[0m\x1b[38;2;248;248;242mjust\n\
         another\n\
         test\x1b[38;2;249;38;114m)\x1b[0m";
    };
    {
      name = "osc8_wrap";
      input = "สวัสดีสวัสดี\x1b]8;;https://example.com\x1b\\ สวัสดีสวัสดี\x1b]8;;\x1b\\";
      width = 8;
      expected = "สวัสดีสวัสดี\x1b]8;;https://example.com\x1b\\\nสวัสดีสวัสดี\x1b]8;;\x1b\\";
    };
    { name = "tab"; input = "foo\tbar"; width = 3; expected = "foo\nbar" };
    {
      name = "narrow_nbsp";
      input = "0\u{202F}1\u{202F}2\u{202F}3\u{202F}4";
      width = 7;
      expected = "0\u{202F}1\u{202F}2\u{202F}3\n4";
    };
    (* Go's row says U+2029 occupies zero cells.  Uucp.Break.tty_width_hint
       explicitly classifies Zp as one cell (uucp__break.ml:67), so this is
       the intentional frozen-width divergence from wrap_test.go:228-230. *)
    {
      name = "paragraph_separator_uucp_width_one";
      input = "0\u{2029}1\u{2029}2\u{2029}3\u{2029}4";
      width = 7;
      expected = "0\u{2029}1\u{2029}2\u{2029}3\n4";
    };
    {
      name = "medium_mathematical_space";
      input = "0\u{205F}1\u{205F}2\u{205F}3\u{205F}4";
      width = 7;
      expected = "0\u{205F}1\u{205F}2\u{205F}3\n4";
    };
    {
      name = "ideographic_space";
      input = "0\u{3000}1\u{3000}2\u{3000}3\u{3000}";
      width = 7;
      expected = "0\u{3000}1\u{3000}2\n3\u{3000}";
    };
    {
      name = "japanese_with_white_spaces_narrow";
      input =
        {|耐許ヱヨカハ調出あゆ監件び理別よン國給災レホチ権輝モエフ会割もフ響3現エツ文時しだびほ経機ムイメフ敗文ヨク現義なさド請情ゆじょて憶主管州けでふく。排ゃわつげ美刊ヱミ出見ツ南者オ抜豆ハトロネ論索モネニイ任償スヲ話破リヤヨ秒止口イセソス止央のさ食周健でてつだ官送ト読聴遊容ひるべ。際ぐドらづ市居ネムヤ研校35岩6繹ごわク報拐イ革深52球ゃレスご究東スラ衝3間ラ録占たス。
禁にンご忘康ざほぎル騰般ねど事超スんいう真表何カモ自浩ヲシミ図客線るふ静王ぱーま写村月掛焼詐面ぞゃ。昇強ごントほ価保キ族85岡モテ恋困ひりこな刊並せご出来ぼぎむう点目ヲウ止環公ニレ事応タス必書タメムノ当84無信升ちひょ。価ーぐ中客テサ告覧ヨトハ極整ラ得95稿はかラせ江利ス宏丸霊ミ考整ス静将ず業巨職ノラホ収嗅ざな。|};
      width = 13;
      expected =
        {|耐許ヱヨカハ
調出あゆ監件
び理別よン國
給災レホチ権
輝モエフ会割
もフ響3現エツ
文時しだびほ
経機ムイメフ
敗文ヨク現義
なさド請情ゆ
じょて憶主管
州けでふく。
排ゃわつげ美
刊ヱミ出見ツ
南者オ抜豆ハ
トロネ論索モ
ネニイ任償ス
ヲ話破リヤヨ
秒止口イセソ
ス止央のさ食
周健でてつだ
官送ト読聴遊
容ひるべ。際
ぐドらづ市居
ネムヤ研校35
岩6繹ごわク報
拐イ革深52球
ゃレスご究東
スラ衝3間ラ録
占たス。
禁にンご忘康
ざほぎル騰般
ねど事超スん
いう真表何カ
モ自浩ヲシミ
図客線るふ静
王ぱーま写村
月掛焼詐面ぞ
ゃ。昇強ごン
トほ価保キ族8
5岡モテ恋困ひ
りこな刊並せ
ご出来ぼぎむ
う点目ヲウ止
環公ニレ事応
タス必書タメ
ムノ当84無信
升ちひょ。価
ーぐ中客テサ
告覧ヨトハ極
整ラ得95稿は
かラせ江利ス
宏丸霊ミ考整
ス静将ず業巨
職ノラホ収嗅
ざな。|};
    };
    {
      name = "japanese_with_white_spaces_wide";
      input =
        {|耐許ヱヨカハ調出あゆ監件び理別よン國給災レホチ権輝モエフ会割もフ響3現エツ文時しだびほ経機ムイメフ敗文ヨク現義なさド請情ゆじょて憶主管州けでふく。排ゃわつげ美刊ヱミ出見ツ南者オ抜豆ハトロネ論索モネニイ任償スヲ話破リヤヨ秒止口イセソス止央のさ食周健でてつだ官送ト読聴遊容ひるべ。際ぐドらづ市居ネムヤ研校35岩6繹ごわク報拐イ革深52球ゃレスご究東スラ衝3間ラ録占たス。
禁にンご忘康ざほぎル騰般ねど事超スんいう真表何カモ自浩ヲシミ図客線るふ静王ぱーま写村月掛焼詐面ぞゃ。昇強ごントほ価保キ族85岡モテ恋困ひりこな刊並せご出来ぼぎむう点目ヲウ止環公ニレ事応タス必書タメムノ当84無信升ちひょ。価ーぐ中客テサ告覧ヨトハ極整ラ得95稿はかラせ江利ス宏丸霊ミ考整ス静将ず業巨職ノラホ収嗅ざな。|};
      width = 30;
      expected =
        {|耐許ヱヨカハ調出あゆ監件び理別
よン國給災レホチ権輝モエフ会割
もフ響3現エツ文時しだびほ経機
ムイメフ敗文ヨク現義なさド請情
ゆじょて憶主管州けでふく。排ゃ
わつげ美刊ヱミ出見ツ南者オ抜豆
ハトロネ論索モネニイ任償スヲ話
破リヤヨ秒止口イセソス止央のさ
食周健でてつだ官送ト読聴遊容ひ
るべ。際ぐドらづ市居ネムヤ研校
35岩6繹ごわク報拐イ革深52球ゃ
レスご究東スラ衝3間ラ録占たス
。
禁にンご忘康ざほぎル騰般ねど事
超スんいう真表何カモ自浩ヲシミ
図客線るふ静王ぱーま写村月掛焼
詐面ぞゃ。昇強ごントほ価保キ族
85岡モテ恋困ひりこな刊並せご出
来ぼぎむう点目ヲウ止環公ニレ事
応タス必書タメムノ当84無信升ち
ひょ。価ーぐ中客テサ告覧ヨトハ
極整ラ得95稿はかラせ江利ス宏丸
霊ミ考整ス静将ず業巨職ノラホ収
嗅ざな。|};
    };
    {
      name = "trailing_space_drops_when_it_overflows";
      input = "ab ";
      expected = "ab";
      width = 2;
    };
    {
      name = "trailing_space_survives_inside_the_box";
      input = "ab ";
      expected = "ab ";
      width = 3;
    };
    {
      name = "space_run_drops_at_a_line_break";
      input = "ab  cd";
      expected = "ab\ncd";
      width = 3;
    };
    {
      name = "over_wide_cluster_owns_its_line";
      input = "漢x";
      expected = "漢\nx";
      width = 1;
    };
    {
      name = "overfull_separator_drops_before_a_break";
      input = "foo -bar";
      expected = "foo\n-ba\nr";
      width = 3;
    };
  ]

let wrap_case (c : wrap_case) =
  Alcotest.test_case ("wrap/" ^ c.name) `Quick (fun () ->
      Alcotest.(check string) "wrapped" c.expected (Text.wrap ~width:c.width c.input))

type width_case = { name : string; input : string; stripped : string; width : int }

let width_vectors =
  [
    { name = "empty"; input = ""; stripped = ""; width = 0 };
    { name = "ascii"; input = "hello"; stripped = "hello"; width = 5 };
    { name = "emoji"; input = "👋"; stripped = "👋"; width = 2 };
    { name = "wideemoji"; input = "🫧"; stripped = "🫧"; width = 2 };
    { name = "combining"; input = "a\u{0300}"; stripped = "a\u{0300}"; width = 1 };
    { name = "control"; input = "\x1b[31mhello\x1b[0m"; stripped = "hello"; width = 5 };
    { name = "csi8"; input = "\x9b38;5;1mhello\x9bm"; stripped = "hello"; width = 5 };
    {
      name = "osc";
      input = "\x9d2;charmbracelet: ~/Source/bubbletea\x9c";
      stripped = "";
      width = 0;
    };
    { name = "controlemoji"; input = "\x1b[31m👋\x1b[0m"; stripped = "👋"; width = 2 };
    { name = "oscwideemoji"; input = "\x1b]2;title👨‍👩‍👦\x07"; stripped = ""; width = 0 };
    {
      name = "sgr_wide_emoji_family";
      input = "\x1b[31m👨‍👩‍👦\x1b[m";
      stripped = "👨‍👩‍👦";
      width = 2;
    };
    {
      name = "osc8eastasianlink";
      input = "\x9d8;id=1;https://example.com/\x9c打豆豆\x9d8;id=1;\x07";
      stripped = "打豆豆";
      width = 6;
    };
    {
      name = "dcsarabic";
      input = "\x1bP?123$pسلام\x1b\\اهلا";
      stripped = "اهلا";
      width = 4;
    };
    { name = "newline"; input = "hello\nworld"; stripped = "hello\nworld"; width = 10 };
    { name = "tab"; input = "hello\tworld"; stripped = "hello\tworld"; width = 10 };
    {
      name = "controlnewline";
      input = "\x1b[31mhello\x1b[0m\nworld";
      stripped = "hello\nworld";
      width = 10;
    };
    { name = "style"; input = "\x1b[38;2;249;38;114mfoo"; stripped = "foo"; width = 3 };
    { name = "unicode"; input = "\x1b[35m“box”\x1b[0m"; stripped = "“box”"; width = 5 };
    {
      name = "just_unicode";
      input = "Claire’s Boutique";
      stripped = "Claire’s Boutique";
      width = 17;
    };
    {
      name = "unclosed_ansi";
      input = "Hey, \x1b[7m\n猴";
      stripped = "Hey, \n猴";
      width = 7;
    };
    { name = "double_asian_runes"; input = " 你\x1b[8m好."; stripped = " 你好."; width = 6 };
    { name = "flag"; input = "🇸🇦"; stripped = "🇸🇦"; width = 2 };
    { name = "half_width_and_ascii"; input = "(ﾟ"; stripped = "(ﾟ"; width = 1 };
    { name = "latin_acute"; input = "é"; stripped = "é"; width = 1 };
    {
      name = "u2029_uucp_width_one";
      input = "\u{2029}";
      stripped = "\u{2029}";
      width = 1;
    };
  ]

let width_case c =
  Alcotest.test_case ("width/" ^ c.name) `Quick (fun () ->
      Alcotest.(check string) "strip" c.stripped (Text.strip c.input);
      Alcotest.(check int) "width" c.width (Text.width c.input))

let cut_vectors =
  [
    ("simple_string", "This is a long string", 2, 6, "is i");
    ( "with_ansi",
      "I really \x1b[38;2;249;38;114mlove\x1b[0m Go!",
      4,
      25,
      "ally \x1b[38;2;249;38;114mlove\x1b[0m Go!" );
    ( "left_is_0",
      "Foo \x1b[38;2;249;38;114mbar\x1b[0mbaz",
      0,
      5,
      "Foo \x1b[38;2;249;38;114mb\x1b[0m" );
    ("right_is_0", "\x1b[7mHello\x1b[m", 3, 0, "");
    ("right_less_than_left", "\x1b[7mHello\x1b[m", 3, 2, "");
    ("cut_size_is_0", "\x1b[7mHello\x1b[m", 2, 2, "");
    ( "maintains_open_ansi",
      "\x1b[38;5;212;48;5;63mHello, Artichoke!\x1b[m",
      7,
      16,
      "\x1b[38;5;212;48;5;63mArtichoke\x1b[m" );
    ( "multiline",
      "\n\
       \x1b[38;2;98;98;98m\n\
       if [ -f RE\n\
       ADME.md ]; then\x1b[m\n\
       \x1b[38;2;98;98;98m    echo oi\x1b[m\n\
       \x1b[38;2;98;98;98mfi\x1b[m\n",
      8,
      13,
      "\x1b[38;2;98;98;98mRE\nADM\x1b[m\x1b[38;2;98;98;98m\x1b[m\x1b[38;2;98;98;98m\x1b[m"
    );
  ]

let cut_case (name, input, left, right, expected) =
  Alcotest.test_case ("cut/" ^ name) `Quick (fun () ->
      Alcotest.(check string) "cut" expected (Text.cut ~left ~right input))

let escape_state_regressions =
  [
    Alcotest.test_case "truncate/preserves SGR around tail" `Quick (fun () ->
        let input = "\x1b[31mhello\x1b[0m" in
        Alcotest.(check string)
          "tail" "\x1b[31mhel…\x1b[0m"
          (Text.truncate ~tail:"…" ~width:4 input));
    Alcotest.test_case "truncate_left/preserves SGR around prefix" `Quick (fun () ->
        let input = "\x1b[31mhello\x1b[0m" in
        Alcotest.(check string)
          "prefix" "\x1b[31m…o\x1b[0m"
          (Text.truncate_left ~prefix:"…" ~width:4 input));
    Alcotest.test_case "truncate/preserves OSC8 around tail" `Quick (fun () ->
        let input = "\x1b]8;;https://example.com\x1b\\hello\x1b]8;;\x1b\\" in
        Alcotest.(check string)
          "tail" "\x1b]8;;https://example.com\x1b\\he…\x1b]8;;\x1b\\"
          (Text.truncate ~tail:"…" ~width:3 input));
    Alcotest.test_case "truncate_left/preserves OSC8 around prefix" `Quick (fun () ->
        let input = "\x1b]8;;https://example.com\x1b\\hello\x1b]8;;\x1b\\" in
        Alcotest.(check string)
          "prefix" "\x1b]8;;https://example.com\x1b\\…lo\x1b]8;;\x1b\\"
          (Text.truncate_left ~prefix:"…" ~width:3 input));
  ]

let unicode_regressions =
  [
    Alcotest.test_case "strip/keeps precomposed Unicode" `Quick (fun () ->
        Alcotest.(check string) "precomposed" "Ü" (Text.strip "Ü");
        Alcotest.(check int) "precomposed width" 1 (Text.width "Ü"));
    Alcotest.test_case "strip/keeps emoji unchanged" `Quick (fun () ->
        Alcotest.(check string) "emoji" "👋🫧" (Text.strip "👋🫧"));
    Alcotest.test_case "strip/keeps non-ASCII OSC payload out" `Quick (fun () ->
        Alcotest.(check string) "payload" "visible" (Text.strip "\x1b]2;題名👨‍👩‍👦\x07visible"));
    Alcotest.test_case "strip/retains C0 controls" `Quick (fun () ->
        let controls = "\x00\x01\x07\x08\x09\x0a\x0b\x0c\x0d\x1f" in
        Alcotest.(check string) "controls" controls (Text.strip controls));
    Alcotest.test_case "width/retains zero-width tab" `Quick (fun () ->
        Alcotest.(check string) "tab" "a\tb" (Text.strip "a\tb");
        Alcotest.(check int) "tab width" 2 (Text.width "a\tb"));
    Alcotest.test_case "truncate/does not split combining cluster" `Quick (fun () ->
        let input = "e\u{0301}x" in
        Alcotest.(check string) "right" "e\u{0301}" (Text.truncate ~width:1 input);
        Alcotest.(check string) "left" "x" (Text.truncate_left ~width:1 input);
        Alcotest.(check string) "cut" "e\u{0301}" (Text.cut ~left:0 ~right:1 input));
    Alcotest.test_case "truncate/does not split emoji cluster" `Quick (fun () ->
        let input = "👋x" in
        Alcotest.(check string) "right" "👋" (Text.truncate ~width:2 input);
        Alcotest.(check string) "left" "x" (Text.truncate_left ~width:2 input);
        Alcotest.(check string) "cut" "👋" (Text.cut ~left:0 ~right:2 input));
    Alcotest.test_case "truncate/does not split wide cluster" `Quick (fun () ->
        let input = "你x" in
        Alcotest.(check string) "right" "你" (Text.truncate ~width:2 input);
        Alcotest.(check string) "left" "x" (Text.truncate_left ~width:2 input);
        Alcotest.(check string) "cut" "你" (Text.cut ~left:0 ~right:2 input));
    Alcotest.test_case "wordwrap/keeps long word" `Quick (fun () ->
        let input = "abcdefgh" in
        Alcotest.(check string) "wordwrap" input (Text.wordwrap ~width:4 input);
        Alcotest.(check string) "wrap" "abcd\nefgh" (Text.wrap ~width:4 input));
    Alcotest.test_case "pad_right/escape state and cell width" `Quick (fun () ->
        Alcotest.(check string) "ascii" "foo  " (Text.pad_right ~width:5 "foo");
        Alcotest.(check string) "wide" "你   " (Text.pad_right ~width:5 "你");
        Alcotest.(check string)
          "combining" "e\u{0301}  "
          (Text.pad_right ~width:3 "e\u{0301}");
        Alcotest.(check string)
          "open SGR" "\x1b[31mfoo  "
          (Text.pad_right ~width:5 "\x1b[31mfoo"));
  ]

let cases : unit Alcotest.test_case list =
  List.map truncate_case truncate_vectors
  @ List.map hardwrap_case hardwrap_vectors
  @ List.map wordwrap_case wordwrap_vectors
  @ List.map wrap_case wrap_vectors
  @ List.map width_case width_vectors
  @ List.map cut_case cut_vectors @ escape_state_regressions @ unicode_regressions
