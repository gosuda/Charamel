open Spec

let spec =
  make_spec ~names:[ "css" ]
    ~keywords:
      [
        "@media";
        "@import";
        "@charset";
        "@font-face";
        "@keyframes";
        "@supports";
        "@page";
        "@namespace";
        "@document";
        "@viewport";
        "!important";
        "from";
        "to";
      ]
    ~builtins:
      [
        "red";
        "green";
        "blue";
        "white";
        "black";
        "gray";
        "grey";
        "silver";
        "maroon";
        "olive";
        "lime";
        "aqua";
        "teal";
        "navy";
        "fuchsia";
        "purple";
        "orange";
        "gold";
        "yellow";
        "pink";
        "brown";
        "transparent";
        "currentcolor";
        "inherit";
        "initial";
        "unset";
      ]
    ~block_comment:[ ("/*", "*/") ]
    ~strings:[ ("\"", "\"", true); ("'", "'", true) ]
    ~number:css_number ~ident:identifier_dash
    ~operators:
      [
        "::";
        "=>";
        "==";
        "!=";
        "<=";
        ">=";
        "+=";
        "-=";
        "*=";
        "/=";
        "=";
        "+";
        "-";
        "*";
        "/";
      ]
    ~attribute:
      (Some
         (Re.alt
            [
              Re.seq [ Re.str "."; identifier_dash ];
              Re.seq [ Re.str "::"; identifier ];
              Re.seq [ Re.str ":"; identifier ];
            ]))
    ~case_sensitive:false ()
