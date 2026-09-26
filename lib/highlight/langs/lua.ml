open Spec

let spec =
  make_spec ~names:[ "lua" ]
    ~keywords:
      [
        "and";
        "break";
        "do";
        "else";
        "elseif";
        "end";
        "for";
        "function";
        "goto";
        "if";
        "in";
        "local";
        "not";
        "or";
        "repeat";
        "return";
        "then";
        "until";
        "while";
      ]
    ~builtins:
      [
        "print";
        "type";
        "pairs";
        "ipairs";
        "tostring";
        "tonumber";
        "require";
        "setmetatable";
        "getmetatable";
        "pcall";
        "xpcall";
        "select";
        "rawget";
        "rawset";
        "rawlen";
        "rawequal";
        "error";
        "assert";
        "next";
        "load";
        "dofile";
        "collectgarbage";
      ]
    ~constants:[ "true"; "false"; "nil"; "_G"; "_VERSION" ]
    ~line_comment:[ "--" ]
    ~block_comment:
      [
        ("--[====[", "]====]");
        ("--[===[", "]===]");
        ("--[==[", "]==]");
        ("--[=[", "]=]");
        ("--[[", "]]");
      ]
    ~strings:[ ("\"", "\"", true); ("'", "'", true) ]
    ~raw_strings:
      [
        ("[====[", "]====]");
        ("[===[", "]===]");
        ("[==[", "]==]");
        ("[=[", "]=]");
        ("[[", "]]");
      ]
    ~number:
      (Re.alt
         [
           Re.seq
             [
               Re.str "0x";
               Re.rep1 (Re.set "0123456789abcdefABCDEF");
               Re.opt (Re.seq [ Re.str "."; Re.rep1 (Re.set "0123456789abcdefABCDEF") ]);
             ];
           float_number;
         ])
    ~ident:identifier
    ~operators:
      [
        "...";
        "..";
        "==";
        "~=";
        "<=";
        ">=";
        "::";
        "+";
        "-";
        "*";
        "/";
        "%";
        "^";
        "#";
        "<";
        ">";
        "=";
        ".";
        ":";
      ]
    ()
