open Spec

let spec =
  make_spec ~names:[ "yaml"; "yml" ]
    ~constants:[ "true"; "false"; "null"; "yes"; "no"; "on"; "off" ]
    ~line_comment:[ "#" ]
    ~strings:[ ("\"", "\"", true); ("'", "'", false) ]
    ~number:decimal_signed ~ident:identifier_dash ~operators:[ "-" ]
    ~attribute:
      (Some
         (alt
            [
              seq [ set "&*"; identifier_dash ];
              seq [ str "!!"; identifier ];
              seq [ str "!"; identifier ];
              seq
                [
                  str "%";
                  rep1
                    (set
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-");
                ];
            ]))
    ~case_sensitive:false ()
