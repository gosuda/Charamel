open Spec

let spec =
  make_spec ~names:[ "toml" ] ~constants:[ "true"; "false" ] ~line_comment:[ "#" ]
    ~strings:
      [
        ("\"\"\"", "\"\"\"", true);
        ("'''", "'''", false);
        ("\"", "\"", true);
        ("'", "'", false);
      ]
    ~number:
      (Re.alt
         [
           Re.seq
             [
               Re.repn (Re.set "0123456789") 4 (Some 4);
               Re.str "-";
               Re.repn (Re.set "0123456789") 2 (Some 2);
               Re.str "-";
               Re.repn (Re.set "0123456789") 2 (Some 2);
             ];
           float_number;
           hex_literal ();
           Re.seq [ Re.str "0o"; Re.rep1 (Re.set "01234567") ];
           Re.seq [ Re.str "0b"; Re.rep1 (Re.set "01") ];
           integer_number;
         ])
    ~ident:identifier_dash ~operators:[ "="; "." ]
    ~attribute:
      (Some
         (Re.alt
            [
              Re.seq
                [
                  Re.str "[[";
                  Re.rep
                    (Re.set
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.- ");
                  Re.str "]]";
                ];
              Re.seq
                [
                  Re.str "[";
                  Re.rep
                    (Re.set
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.- ");
                  Re.str "]";
                ];
            ]))
    ()
