open Spec

let spec =
  make_spec ~names:[ "html"; "htm" ] ~keywords:[ "<!DOCTYPE"; "<!doctype" ]
    ~strings:[ ("\"", "\"", true); ("'", "'", true) ]
    ~block_comment:[ ("<!--", "-->") ]
    ~number:integer_number ~ident:identifier_dash
    ~attribute:
      (Some
         (Re.alt
            [
              Re.seq [ Re.str "</"; identifier_dash ];
              Re.seq [ Re.str "<"; identifier_dash ];
              Re.seq
                [
                  Re.str "&";
                  Re.rep1 (Re.set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
                  Re.str ";";
                ];
              Re.seq [ Re.str "&#"; Re.rep1 (Re.set "0123456789"); Re.str ";" ];
            ]))
    ()
