open Spec

let spec =
  make_spec ~names:[ "ocaml"; "ml"; "mli" ]
    ~keywords:
      [
        "and";
        "as";
        "assert";
        "asr";
        "begin";
        "class";
        "constraint";
        "do";
        "done";
        "downto";
        "else";
        "end";
        "exception";
        "external";
        "for";
        "fun";
        "function";
        "functor";
        "if";
        "in";
        "include";
        "inherit";
        "initializer";
        "land";
        "lazy";
        "let";
        "lor";
        "lsl";
        "lsr";
        "lxor";
        "match";
        "method";
        "mod";
        "module";
        "mutable";
        "new";
        "nonrec";
        "object";
        "of";
        "open";
        "or";
        "private";
        "rec";
        "sig";
        "struct";
        "then";
        "to";
        "type";
        "val";
        "virtual";
        "when";
        "while";
        "with";
      ]
    ~types:
      [
        "int";
        "float";
        "bool";
        "char";
        "string";
        "unit";
        "list";
        "array";
        "option";
        "bytes";
        "nativeint";
        "int32";
        "int64";
        "exn";
        "lazy_t";
        "result";
      ]
    ~builtins:
      [
        "print_endline";
        "print_string";
        "print_int";
        "print_float";
        "print_newline";
        "prerr_endline";
        "prerr_string";
        "read_line";
        "read_int";
        "string_of_int";
        "int_of_string";
        "string_of_float";
        "float_of_int";
        "string_of_bool";
        "bool_of_string";
        "fst";
        "snd";
        "ignore";
        "not";
        "ref";
        "incr";
        "decr";
        "failwith";
        "invalid_arg";
        "exit";
        "compare";
        "min";
        "max";
        "abs";
        "abs_float";
        "succ";
        "pred";
        "sqrt";
        "exp";
        "log";
        "log10";
        "sin";
        "cos";
        "tan";
        "atan";
        "floor";
        "ceil";
        "truncate";
        "char_of_int";
        "int_of_char";
      ]
    ~constants:[ "true"; "false"; "None"; "Some"; "Ok"; "Error" ]
    ~block_comment:[ ("(*", "*)") ]
    ~strings:[ ("\"", "\"", true) ]
    ~raw_strings:[ ("{|", "|}") ]
    ~number:
      (alt
         [
           seq [ str "0x"; rep1 (set "_0123456789abcdefABCDEF") ];
           seq [ str "0X"; rep1 (set "_0123456789abcdefABCDEF") ];
           seq [ str "0o"; rep1 (set "_01234567") ];
           seq [ str "0O"; rep1 (set "_01234567") ];
           seq [ str "0b"; rep1 (set "_01") ];
           seq [ str "0B"; rep1 (set "_01") ];
           seq [ decimal; opt (set "lLn") ];
         ])
    ~ident:
      (seq
         [
           set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";
           rep (set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'");
         ])
    ~operators:
      [
        ";;";
        "->";
        "=>";
        "::";
        ":=";
        "<>";
        "<=";
        ">=";
        "&&";
        "||";
        "**";
        "*.";
        "+.";
        "-.";
        "/.";
        "^";
        "+";
        "-";
        "*";
        "/";
        "%";
        "=";
        "<";
        ">";
        "@";
        "!";
        "&";
        "|";
        "$";
        "~";
        ".";
        "#";
      ]
    ~attribute:
      (Some
         (seq
            [
              alt [ str "[@@@"; str "[@@"; str "[@" ];
              set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";
              rep (set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'");
            ]))
    ()
