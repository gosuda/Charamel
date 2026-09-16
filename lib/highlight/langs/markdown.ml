open Spec

let spec =
  make_spec ~names:[ "markdown"; "md"; "mkd" ]
    ~raw_strings:[ ("```", "```") ]
    ~number:Re.empty ~ident:Re.empty
    ~attribute:(Some (seq [ Re.bol; rep1 (set "#"); str " "; rep (not_chars "\n") ]))
    ()
