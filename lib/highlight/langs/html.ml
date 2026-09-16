open Spec

let spec =
  make_spec ~names:[ "html"; "htm" ] ~keywords:[ "<!DOCTYPE"; "<!doctype" ]
    ~strings:[ ("\"", "\"", true); ("'", "'", true) ]
    ~block_comment:[ ("<!--", "-->") ]
    ~number:integer_number ~ident:identifier_dash
    ~attribute:
      (Some
         (alt
            [
              seq [ str "</"; identifier_dash ];
              seq [ str "<"; identifier_dash ];
              seq
                [
                  str "&";
                  rep1 (set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
                  str ";";
                ];
              seq [ str "&#"; rep1 (set "0123456789"); str ";" ];
            ]))
    ()
