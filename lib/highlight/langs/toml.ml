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
      (alt
         [
           seq
             [
               Re.repn (set "0123456789") 4 (Some 4);
               str "-";
               Re.repn (set "0123456789") 2 (Some 2);
               str "-";
               Re.repn (set "0123456789") 2 (Some 2);
             ];
           float_number;
           seq [ str "0x"; rep1 (set "0123456789abcdefABCDEF") ];
           seq [ str "0o"; rep1 (set "01234567") ];
           seq [ str "0b"; rep1 (set "01") ];
           integer_number;
         ])
    ~ident:identifier_dash ~operators:[ "="; "." ]
    ~attribute:
      (Some
         (alt
            [
              seq
                [
                  str "[[";
                  rep
                    (set
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.- ");
                  str "]]";
                ];
              seq
                [
                  str "[";
                  rep
                    (set
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.- ");
                  str "]";
                ];
            ]))
    ()
