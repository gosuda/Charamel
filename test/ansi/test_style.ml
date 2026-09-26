module Color = Charamel_ansi.Color

let st ?(fg = Color.Default) ?(bg = Color.Default) ?(uc = Color.Default) ?(bold = false)
    ?(faint = false) ?(italic = false) ?(underline = Charamel_ansi.Style.No_underline)
    ?(blink = false) ?(reverse = false) ?(conceal = false) ?(strike = false) () :
    Charamel_ansi.Style.t =
  {
    Charamel_ansi.Style.fg;
    bg;
    underline_color = uc;
    bold;
    faint;
    italic;
    underline;
    blink;
    reverse;
    conceal;
    strike;
  }

let renders name expected style =
  Alcotest.check Alcotest.string name expected (Charamel_ansi.Style.to_sgr style)

let transitions name expected ~from to_ =
  Alcotest.check Alcotest.string name expected (Charamel_ansi.Style.transition ~from to_)

let default_resets =
  Alcotest.test_case "default resets" `Quick (fun () ->
      renders "default is reset" "\x1b[m" (st ()))

let color_params =
  Alcotest.test_case "color params" `Quick (fun () ->
      renders "fg rgb" "\x1b[38;2;255;0;0m" (st ~fg:(Color.Rgb (255, 0, 0)) ());
      renders "fg basic dark" "\x1b[33m" (st ~fg:(Color.Basic 3) ());
      renders "fg basic bright" "\x1b[93m" (st ~fg:(Color.Basic 11) ());
      renders "fg indexed" "\x1b[38;5;209m" (st ~fg:(Color.Indexed 209) ());
      renders "bg basic dark" "\x1b[44m" (st ~bg:(Color.Basic 4) ());
      renders "bg basic bright" "\x1b[104m" (st ~bg:(Color.Basic 12) ());
      renders "bg indexed" "\x1b[48;5;209m" (st ~bg:(Color.Indexed 209) ());
      renders "underline color basic uses extended form" "\x1b[58;5;5m"
        (st ~uc:(Color.Basic 5) ());
      renders "underline color indexed" "\x1b[58;5;42m" (st ~uc:(Color.Indexed 42) ());
      renders "underline color rgb" "\x1b[58;2;1;2;3m" (st ~uc:(Color.Rgb (1, 2, 3)) ()))

let attribute_params =
  Alcotest.test_case "attribute params" `Quick (fun () ->
      renders "bold" "\x1b[1m" (st ~bold:true ());
      renders "faint" "\x1b[2m" (st ~faint:true ());
      renders "italic" "\x1b[3m" (st ~italic:true ());
      renders "blink" "\x1b[5m" (st ~blink:true ());
      renders "reverse" "\x1b[7m" (st ~reverse:true ());
      renders "conceal" "\x1b[8m" (st ~conceal:true ());
      renders "strike" "\x1b[9m" (st ~strike:true ());
      renders "single underline" "\x1b[4m" (st ~underline:Charamel_ansi.Style.Single ());
      renders "double underline" "\x1b[4:2m" (st ~underline:Charamel_ansi.Style.Double ());
      renders "curly underline" "\x1b[4:3m" (st ~underline:Charamel_ansi.Style.Curly ());
      renders "dotted underline" "\x1b[4:4m" (st ~underline:Charamel_ansi.Style.Dotted ());
      renders "dashed underline" "\x1b[4:5m" (st ~underline:Charamel_ansi.Style.Dashed ());
      renders "italic and strike" "\x1b[3;9m" (st ~italic:true ~strike:true ());
      renders "bold italic and underline" "\x1b[1;3;4m"
        (st ~bold:true ~italic:true ~underline:Charamel_ansi.Style.Single ());
      renders "full style" "\x1b[1;2;3;5;7;8;9;4:3;38;2;255;133;85;48;5;209;58;2;1;2;3m"
        (st
           ~fg:(Color.Rgb (255, 133, 85))
           ~bg:(Color.Indexed 209)
           ~uc:(Color.Rgb (1, 2, 3))
           ~bold:true ~faint:true ~italic:true ~underline:Charamel_ansi.Style.Curly
           ~blink:true ~reverse:true ~conceal:true ~strike:true ()))

let color_transitions =
  Alcotest.test_case "color transitions" `Quick (fun () ->
      let red = Color.Rgb (255, 0, 0) in
      let blue = Color.Rgb (0, 0, 255) in
      let green = Color.Rgb (0, 255, 0) in
      let yellow = Color.Rgb (255, 255, 0) in
      let cyan = Color.Rgb (0, 255, 255) in
      transitions "add foreground" "\x1b[38;2;255;0;0m" ~from:(st ()) (st ~fg:red ());
      transitions "change foreground" "\x1b[38;2;0;0;255m" ~from:(st ~fg:red ())
        (st ~fg:blue ());
      transitions "same foreground" "" ~from:(st ~fg:red ()) (st ~fg:red ());
      transitions "remove foreground" "\x1b[m" ~from:(st ~fg:red ()) (st ());
      transitions "add background" "\x1b[48;2;0;0;255m" ~from:(st ()) (st ~bg:blue ());
      transitions "change background" "\x1b[48;2;255;0;0m" ~from:(st ~bg:blue ())
        (st ~bg:red ());
      transitions "same background" "" ~from:(st ~bg:blue ()) (st ~bg:blue ());
      transitions "remove background" "\x1b[m" ~from:(st ~bg:blue ()) (st ());
      transitions "add underline color" "\x1b[58;2;255;0;0m" ~from:(st ()) (st ~uc:red ());
      transitions "change underline color" "\x1b[58;2;0;0;255m" ~from:(st ~uc:red ())
        (st ~uc:blue ());
      transitions "same underline color" "" ~from:(st ~uc:red ()) (st ~uc:red ());
      transitions "remove underline color" "\x1b[m" ~from:(st ~uc:red ()) (st ());
      transitions "remove all colors" "\x1b[39;49;59m"
        ~from:
          (st ~fg:red ~bg:blue ~uc:green ~bold:true ~italic:true
             ~underline:Charamel_ansi.Style.Single ())
        (st ~bold:true ~italic:true ~underline:Charamel_ansi.Style.Single ());
      transitions "change all colors" "\x1b[38;2;0;255;0;48;2;255;255;0;58;2;0;255;255m"
        ~from:
          (st ~fg:red ~bg:blue ~uc:green ~bold:true ~italic:true
             ~underline:Charamel_ansi.Style.Single ())
        (st ~fg:green ~bg:yellow ~uc:cyan ~bold:true ~italic:true
           ~underline:Charamel_ansi.Style.Single ());
      transitions "only attributes change" "\x1b[3m"
        ~from:(st ~fg:red ~bg:blue ~uc:green ~bold:true ())
        (st ~fg:red ~bg:blue ~uc:green ~bold:true ~italic:true ()))

let intensity_transitions =
  Alcotest.test_case "intensity transitions" `Quick (fun () ->
      transitions "add bold" "\x1b[1m" ~from:(st ()) (st ~bold:true ());
      transitions "remove bold" "\x1b[m" ~from:(st ~bold:true ()) (st ());
      transitions "keep bold" "" ~from:(st ~bold:true ()) (st ~bold:true ());
      transitions "add faint" "\x1b[2m" ~from:(st ()) (st ~faint:true ());
      transitions "remove faint" "\x1b[m" ~from:(st ~faint:true ()) (st ());
      transitions "keep faint" "" ~from:(st ~faint:true ()) (st ~faint:true ());
      transitions "bold to faint" "\x1b[22;2m" ~from:(st ~bold:true ())
        (st ~faint:true ());
      transitions "faint to bold" "\x1b[22;1m" ~from:(st ~faint:true ())
        (st ~bold:true ());
      transitions "bold to bold and faint" "\x1b[2m" ~from:(st ~bold:true ())
        (st ~bold:true ~faint:true ());
      transitions "bold and faint to bold" "\x1b[22;1m"
        ~from:(st ~bold:true ~faint:true ())
        (st ~bold:true ());
      transitions "bold and faint to faint" "\x1b[22;2m"
        ~from:(st ~bold:true ~faint:true ())
        (st ~faint:true ());
      transitions "faint to bold and faint" "\x1b[1m" ~from:(st ~faint:true ())
        (st ~bold:true ~faint:true ());
      transitions "nothing to bold and faint" "\x1b[1;2m" ~from:(st ())
        (st ~bold:true ~faint:true ()))

let underline_transitions =
  Alcotest.test_case "underline transitions" `Quick (fun () ->
      let s = Charamel_ansi.Style.Single in
      let d = Charamel_ansi.Style.Double in
      let c = Charamel_ansi.Style.Curly in
      let no = Charamel_ansi.Style.No_underline in
      transitions "add single underline" "\x1b[4m" ~from:(st ()) (st ~underline:s ());
      transitions "add double underline" "\x1b[4:2m" ~from:(st ()) (st ~underline:d ());
      transitions "add curly underline" "\x1b[4:3m" ~from:(st ()) (st ~underline:c ());
      transitions "change single to double" "\x1b[4:2m" ~from:(st ~underline:s ())
        (st ~underline:d ());
      transitions "change single to curly" "\x1b[4:3m" ~from:(st ~underline:s ())
        (st ~underline:c ());
      transitions "change double to curly" "\x1b[4:3m" ~from:(st ~underline:d ())
        (st ~underline:c ());
      transitions "keep underline" "" ~from:(st ~underline:s ()) (st ~underline:s ());
      transitions "remove single underline" "\x1b[m" ~from:(st ~underline:s ()) (st ());
      transitions "remove curly underline" "\x1b[m" ~from:(st ~underline:c ()) (st ());
      transitions "remove underline with attributes" "\x1b[24m"
        ~from:(st ~underline:s ~italic:true ())
        (st ~italic:true ());
      transitions "underline colors independent of underline style" "\x1b[4:2m"
        ~from:(st ~underline:no ~uc:(Color.Rgb (1, 1, 1)) ())
        (st ~underline:d ~uc:(Color.Rgb (1, 1, 1)) ()))

let attribute_transitions =
  Alcotest.test_case "attribute transitions" `Quick (fun () ->
      transitions "add italic" "\x1b[3m" ~from:(st ()) (st ~italic:true ());
      transitions "remove italic" "\x1b[m" ~from:(st ~italic:true ()) (st ());
      transitions "keep italic" "" ~from:(st ~italic:true ()) (st ~italic:true ());
      transitions "add blink" "\x1b[5m" ~from:(st ()) (st ~blink:true ());
      transitions "remove blink" "\x1b[m" ~from:(st ~blink:true ()) (st ());
      transitions "keep blink" "" ~from:(st ~blink:true ()) (st ~blink:true ());
      transitions "add reverse" "\x1b[7m" ~from:(st ()) (st ~reverse:true ());
      transitions "remove reverse" "\x1b[m" ~from:(st ~reverse:true ()) (st ());
      transitions "keep reverse" "" ~from:(st ~reverse:true ()) (st ~reverse:true ());
      transitions "add conceal" "\x1b[8m" ~from:(st ()) (st ~conceal:true ());
      transitions "remove conceal" "\x1b[m" ~from:(st ~conceal:true ()) (st ());
      transitions "add strike" "\x1b[9m" ~from:(st ()) (st ~strike:true ());
      transitions "remove strike" "\x1b[m" ~from:(st ~strike:true ()) (st ());
      transitions "bold and italic to italic" "\x1b[22m"
        ~from:(st ~bold:true ~italic:true ())
        (st ~italic:true ());
      transitions "bold faint and italic to bold and italic" "\x1b[22;1m"
        ~from:(st ~bold:true ~faint:true ~italic:true ())
        (st ~bold:true ~italic:true ());
      transitions "bold to bold faint and italic" "\x1b[2;3m" ~from:(st ~bold:true ())
        (st ~bold:true ~faint:true ~italic:true ());
      transitions "nothing to bold faint and italic" "\x1b[1;2;3m" ~from:(st ())
        (st ~bold:true ~faint:true ~italic:true ());
      transitions "bold italic to bold reverse" "\x1b[23;7m"
        ~from:(st ~bold:true ~italic:true ())
        (st ~bold:true ~reverse:true ());
      transitions "swap italic and strike" "\x1b[23;9m" ~from:(st ~italic:true ())
        (st ~strike:true ());
      transitions "bold and italic to bold faint and italic" "\x1b[2m"
        ~from:(st ~bold:true ~italic:true ())
        (st ~bold:true ~faint:true ~italic:true ());
      transitions "remove reverse to non-zero target" "\x1b[27m"
        ~from:(st ~reverse:true ~italic:true ())
        (st ~italic:true ());
      transitions "complete style reset" "\x1b[m"
        ~from:
          (st
             ~fg:(Color.Rgb (255, 133, 85))
             ~bg:(Color.Indexed 209)
             ~uc:(Color.Rgb (1, 2, 3))
             ~bold:true ~faint:true ~italic:true ~underline:Charamel_ansi.Style.Curly
             ~blink:true ~reverse:true ~conceal:true ~strike:true ())
        (st ());
      transitions "no changes with all properties" ""
        ~from:
          (st
             ~fg:(Color.Rgb (255, 133, 85))
             ~bg:(Color.Indexed 209)
             ~uc:(Color.Rgb (1, 2, 3))
             ~bold:true ~faint:true ~italic:true ~underline:Charamel_ansi.Style.Curly
             ~blink:true ~reverse:true ~conceal:true ~strike:true ())
        (st
           ~fg:(Color.Rgb (255, 133, 85))
           ~bg:(Color.Indexed 209)
           ~uc:(Color.Rgb (1, 2, 3))
           ~bold:true ~faint:true ~italic:true ~underline:Charamel_ansi.Style.Curly
           ~blink:true ~reverse:true ~conceal:true ~strike:true ()))

let style_is =
  Alcotest.testable
    (fun ppf s -> Fmt.string ppf (Charamel_ansi.Style.to_sgr s))
    Charamel_ansi.Style.equal

let folds name expected ~params start =
  Alcotest.check style_is name expected (Charamel_ansi.Style.of_sgr ~params start)

let d = Charamel_ansi.Style.default
let single = Charamel_ansi.Style.Single
let curly = Charamel_ansi.Style.Curly

let sgr_folding =
  Alcotest.test_case "sgr folding" `Quick (fun () ->
      folds "attributes"
        (st ~bold:true ~faint:true ~italic:true ~blink:true ~reverse:true ~conceal:true
           ~strike:true ())
        ~params:
          [
            [ Some 1 ];
            [ Some 2 ];
            [ Some 3 ];
            [ Some 5 ];
            [ Some 7 ];
            [ Some 8 ];
            [ Some 9 ];
          ]
        d;
      folds "blink six is blink" (st ~blink:true ()) ~params:[ [ Some 6 ] ] d;
      folds "empty parameter resets" (st ~italic:true ())
        ~params:[ [ Some 1 ]; []; [ Some 3 ] ]
        d;
      folds "none parameter resets" (st ~strike:true ())
        ~params:[ [ Some 7 ]; [ None ]; [ Some 9 ] ]
        d;
      folds "zero parameter resets" (st ~reverse:true ())
        ~params:[ [ Some 5 ]; [ Some 0 ]; [ Some 7 ] ]
        d;
      folds "intensity reset clears bold and faint" (st ~italic:true ())
        ~params:[ [ Some 1 ]; [ Some 2 ]; [ Some 3 ]; [ Some 22 ] ]
        d;
      folds "attribute resets" (st ())
        ~params:
          [ [ Some 23 ]; [ Some 24 ]; [ Some 25 ]; [ Some 27 ]; [ Some 28 ]; [ Some 29 ] ]
        (st ~italic:true ~underline:curly ~blink:true ~reverse:true ~conceal:true
           ~strike:true ());
      folds "bare underline is single" (st ~underline:single ()) ~params:[ [ Some 4 ] ] d;
      folds "underline colon double"
        (st ~underline:Charamel_ansi.Style.Double ())
        ~params:[ [ Some 4; Some 2 ] ]
        d;
      folds "underline colon curly" (st ~underline:curly ())
        ~params:[ [ Some 4; Some 3 ] ]
        d;
      folds "underline colon dotted"
        (st ~underline:Charamel_ansi.Style.Dotted ())
        ~params:[ [ Some 4; Some 4 ] ]
        d;
      folds "underline colon dashed"
        (st ~underline:Charamel_ansi.Style.Dashed ())
        ~params:[ [ Some 4; Some 5 ] ]
        d;
      folds "underline colon one is single" (st ~underline:single ())
        ~params:[ [ Some 4; Some 1 ] ]
        d;
      folds "underline colon omitted subparameter is single" (st ~underline:single ())
        ~params:[ [ Some 4; None ] ]
        d;
      folds "underline colon zero" (st ())
        ~params:[ [ Some 4; Some 0 ] ]
        (st ~underline:curly ());
      folds "underline colon unknown is single" (st ~underline:single ())
        ~params:[ [ Some 4; Some 9 ] ]
        d;
      folds "basic foreground" (st ~fg:(Color.Basic 1) ()) ~params:[ [ Some 31 ] ] d;
      folds "basic background" (st ~bg:(Color.Basic 4) ()) ~params:[ [ Some 44 ] ] d;
      folds "bright foreground" (st ~fg:(Color.Basic 10) ()) ~params:[ [ Some 92 ] ] d;
      folds "bright background" (st ~bg:(Color.Basic 11) ()) ~params:[ [ Some 103 ] ] d;
      folds "slot resets" (st ~bold:true ())
        ~params:[ [ Some 39 ]; [ Some 49 ]; [ Some 59 ] ]
        (st
           ~fg:(Color.Rgb (1, 2, 3))
           ~bg:(Color.Indexed 9) ~uc:(Color.Basic 2) ~bold:true ());
      folds "semicolon indexed foreground"
        (st ~fg:(Color.Indexed 209) ())
        ~params:[ [ Some 38 ]; [ Some 5 ]; [ Some 209 ] ]
        d;
      folds "semicolon indexed leaves later parameters"
        (st ~fg:(Color.Indexed 209) ~bold:true ())
        ~params:[ [ Some 38 ]; [ Some 5 ]; [ Some 209 ]; [ Some 1 ] ]
        d;
      folds "semicolon indexed background"
        (st ~bg:(Color.Indexed 42) ())
        ~params:[ [ Some 48 ]; [ Some 5 ]; [ Some 42 ] ]
        d;
      folds "semicolon indexed underline color"
        (st ~uc:(Color.Indexed 100) ())
        ~params:[ [ Some 58 ]; [ Some 5 ]; [ Some 100 ] ]
        d;
      folds "colon indexed foreground"
        (st ~fg:(Color.Indexed 196) ())
        ~params:[ [ Some 38; Some 5; Some 196 ] ]
        d;
      folds "colon rgb background"
        (st ~bg:(Color.Rgb (1, 2, 3)) ())
        ~params:[ [ Some 48; Some 2; Some 1; Some 2; Some 3 ] ]
        d;
      folds "colon rgb with colorspace"
        (st ~fg:(Color.Rgb (10, 20, 30)) ())
        ~params:[ [ Some 38; Some 2; Some 1; Some 10; Some 20; Some 30 ] ]
        d;
      folds "colon rgb omitted subparameters are zero"
        (st ~fg:(Color.Rgb (0, 0, 0)) ())
        ~params:[ [ Some 38; Some 2; None; None; None ] ]
        d;
      folds "semicolon rgb foreground"
        (st ~fg:(Color.Rgb (255, 0, 128)) ())
        ~params:[ [ Some 38 ]; [ Some 2 ]; [ Some 255 ]; [ Some 0 ]; [ Some 128 ] ]
        d;
      folds "index outside the palette selects default" (st ())
        ~params:[ [ Some 38 ]; [ Some 5 ]; [ Some 300 ] ]
        d;
      folds "component outside the palette selects default" (st ())
        ~params:[ [ Some 38 ]; [ Some 2 ]; [ Some 300 ]; [ Some 0 ]; [ Some 0 ] ]
        d;
      folds "dangling mode five continues as a parameter" (st ~blink:true ())
        ~params:[ [ Some 38 ]; [ Some 5 ] ] d;
      folds "unknown parameter is ignored" (st ~bold:true ()) ~params:[ [ Some 99 ] ]
        (st ~bold:true ());
      folds "empty parameter list keeps the style" (st ~bold:true ()) ~params:[]
        (st ~bold:true ()))

let cases =
  [
    default_resets;
    color_params;
    attribute_params;
    color_transitions;
    intensity_transitions;
    underline_transitions;
    attribute_transitions;
    sgr_folding;
  ]
