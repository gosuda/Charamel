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
      (Re.alt
         [
           Re.seq [ Re.str "0x"; Re.rep1 (Re.set "_0123456789abcdefABCDEF") ];
           Re.seq [ Re.str "0X"; Re.rep1 (Re.set "_0123456789abcdefABCDEF") ];
           Re.seq [ Re.str "0o"; Re.rep1 (Re.set "_01234567") ];
           Re.seq [ Re.str "0O"; Re.rep1 (Re.set "_01234567") ];
           Re.seq [ Re.str "0b"; Re.rep1 (Re.set "_01") ];
           Re.seq [ Re.str "0B"; Re.rep1 (Re.set "_01") ];
           Re.seq [ decimal; Re.opt (Re.set "lLn") ];
         ])
    ~ident:
      (Re.seq
         [
           Re.set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";
           Re.rep
             (Re.set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'");
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
         (Re.seq
            [
              Re.alt [ Re.str "[@@@"; Re.str "[@@"; Re.str "[@" ];
              Re.set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";
              Re.rep
                (Re.set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'");
            ]))
    ()
