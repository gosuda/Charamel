open Spec

let spec =
  make_spec ~names:[ "yaml"; "yml" ]
    ~constants:[ "true"; "false"; "null"; "yes"; "no"; "on"; "off" ]
    ~line_comment:[ "#" ]
    ~strings:[ ("\"", "\"", true); ("'", "'", false) ]
    ~number:decimal_signed ~ident:identifier_dash ~operators:[ "-" ]
    ~attribute:
      (Some
         (Re.alt
            [
              Re.seq [ Re.set "&*"; identifier_dash ];
              Re.seq [ Re.str "!!"; identifier ];
              Re.seq [ Re.str "!"; identifier ];
              Re.seq
                [
                  Re.str "%";
                  Re.rep1
                    (Re.set
                       "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-");
                ];
            ]))
    ~case_sensitive:false ()
