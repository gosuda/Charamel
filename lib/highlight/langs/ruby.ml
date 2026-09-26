open Spec

let spec =
  make_spec ~names:[ "ruby"; "rb" ]
    ~keywords:
      [
        "BEGIN";
        "END";
        "alias";
        "and";
        "begin";
        "break";
        "case";
        "class";
        "def";
        "defined?";
        "do";
        "else";
        "elsif";
        "end";
        "ensure";
        "for";
        "if";
        "in";
        "module";
        "next";
        "not";
        "or";
        "redo";
        "rescue";
        "retry";
        "return";
        "self";
        "super";
        "then";
        "undef";
        "unless";
        "until";
        "when";
        "while";
        "yield";
      ]
    ~types:
      [
        "Integer";
        "Float";
        "String";
        "Symbol";
        "Array";
        "Hash";
        "Struct";
        "Proc";
        "Lambda";
        "Range";
        "Exception";
      ]
    ~builtins:
      [
        "puts";
        "print";
        "p";
        "require";
        "require_relative";
        "attr_accessor";
        "attr_reader";
        "attr_writer";
        "new";
        "lambda";
        "proc";
        "raise";
        "loop";
        "each";
        "map";
        "select";
        "reject";
        "reduce";
        "inject";
        "times";
        "upto";
        "downto";
      ]
    ~constants:[ "true"; "false"; "nil" ] ~line_comment:[ "#" ]
    ~strings:[ ("\"", "\"", true); ("'", "'", true) ]
    ~raw_strings:
      [
        ("%w[", "]");
        ("%w(", ")");
        ("%w{", "}");
        ("%i[", "]");
        ("%i(", ")");
        ("%i{", "}");
        ("%q[", "]");
        ("%q(", ")");
        ("%q{", "}");
        ("%Q[", "]");
        ("%Q(", ")");
        ("%Q{", "}");
      ]
    ~number:
      (Re.alt
         [
           hex_literal ();
           Re.seq [ Re.str "0b"; Re.rep1 (Re.set "01") ];
           Re.seq [ Re.str "0o"; Re.rep1 (Re.set "01234567") ];
           decimal;
         ])
    ~ident:identifier
    ~operators:
      [
        "<=>";
        "!~";
        "=~";
        "**";
        "<<";
        ">>";
        "..";
        "...";
        "&&";
        "||";
        "==";
        "!=";
        "<=";
        ">=";
        "=>";
        "::";
        "&.";
        "+=";
        "-=";
        "*=";
        "/=";
        "%=";
        "+";
        "-";
        "*";
        "/";
        "%";
        "<";
        ">";
        "=";
        "!";
        "&";
        "|";
        "^";
        "~";
        "?";
        ":";
        ".";
      ]
    ~attribute:
      (Some
         (Re.alt
            [
              Re.seq [ Re.str "@@"; identifier ];
              Re.seq [ Re.str "@"; identifier ];
              Re.seq [ Re.str "$"; identifier ];
              Re.seq [ Re.str "$"; Re.set "0123456789@*_?!" ];
            ]))
    ()
