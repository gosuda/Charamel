open Spec

let spec =
  make_spec ~names:[ "json" ] ~constants:[ "true"; "false"; "null" ]
    ~strings:[ ("\"", "\"", true) ]
    ~number:json_number ~ident:identifier_dollar ~operators:[] ()
