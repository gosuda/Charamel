open Spec

let spec =
  make_spec ~names:[ "markdown"; "md"; "mkd" ]
    ~raw_strings:[ ("```", "```") ]
    ~number:Re.empty ~ident:Re.empty
    ~attribute:
      (Some (Re.seq [ Re.bol; Re.rep1 (Re.set "#"); Re.str " "; Re.rep (not_chars "\n") ]))
    ()
