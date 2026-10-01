type kind =
  | Keyword
  | Type
  | Builtin
  | Constant
  | String
  | Number
  | Comment
  | Operator
  | Punct
  | Ident
  | Attribute
  | Text

type t = {
  names : string list;
  keywords : string list;
  types : string list;
  builtins : string list;
  constants : string list;
  line_comment : string list;
  block_comment : (string * string) list;
  strings : (string * string * bool) list;
  raw_strings : (string * string) list;
  number : Re.t;
  ident : Re.t;
  operators : string list;
  attribute : Re.t option;
  case_sensitive : bool;
}

let make_spec ~names ?(keywords = []) ?(types = []) ?(builtins = []) ?(constants = [])
    ?(line_comment = []) ?(block_comment = []) ?(strings = []) ?(raw_strings = [])
    ?(operators = []) ?(attribute = None) ?(case_sensitive = true) ~number ~ident () =
  {
    names;
    keywords;
    types;
    builtins;
    constants;
    line_comment;
    block_comment;
    strings;
    raw_strings;
    number;
    ident;
    operators;
    attribute;
    case_sensitive;
  }

let not_chars chars = Re.diff Re.any (Re.set chars)

let hex_literal ?suffix () =
  Re.seq
    ([ Re.str "0x"; Re.rep1 (Re.set "0123456789abcdefABCDEF") ]
    @ match suffix with None -> [] | Some suffix -> [ Re.opt suffix ])

let decimal =
  Re.seq
    [
      Re.set "0123456789";
      Re.rep (Re.set "_0123456789");
      Re.opt (Re.seq [ Re.str "."; Re.rep (Re.set "_0123456789") ]);
      Re.opt
        (Re.seq
           [
             Re.set "eE";
             Re.opt (Re.set "+-");
             Re.set "0123456789";
             Re.rep (Re.set "_0123456789");
           ]);
    ]

let decimal_signed = Re.seq [ Re.opt (Re.set "+-"); decimal ]

let float_number =
  Re.alt
    [
      Re.seq
        [
          Re.set "0123456789";
          Re.rep (Re.set "_0123456789");
          Re.str ".";
          Re.rep (Re.set "_0123456789");
          Re.opt
            (Re.seq [ Re.set "eE"; Re.opt (Re.set "+-"); Re.rep1 (Re.set "_0123456789") ]);
        ];
      decimal;
    ]

let integer_number = Re.seq [ Re.set "0123456789"; Re.rep (Re.set "_0123456789") ]

let c_number =
  Re.alt
    [
      Re.seq
        [
          Re.str "0x"; Re.rep1 (Re.set "_0123456789abcdefABCDEF"); Re.opt (Re.set "uUlL");
        ];
      Re.seq
        [
          Re.str "0X"; Re.rep1 (Re.set "_0123456789abcdefABCDEF"); Re.opt (Re.set "uUlL");
        ];
      Re.seq [ Re.str "0b"; Re.rep1 (Re.set "_01"); Re.opt (Re.set "uUlL") ];
      Re.seq [ Re.str "0B"; Re.rep1 (Re.set "_01"); Re.opt (Re.set "uUlL") ];
      Re.seq [ Re.str "0o"; Re.rep1 (Re.set "_01234567"); Re.opt (Re.set "uUlL") ];
      Re.seq [ Re.str "0O"; Re.rep1 (Re.set "_01234567"); Re.opt (Re.set "uUlL") ];
      Re.seq [ decimal; Re.opt (Re.set "fFlLuU") ];
    ]

let python_number =
  Re.alt
    [
      Re.seq [ Re.str "0x"; Re.rep1 (Re.set "_0123456789abcdefABCDEF") ];
      Re.seq [ Re.str "0X"; Re.rep1 (Re.set "_0123456789abcdefABCDEF") ];
      Re.seq [ Re.str "0o"; Re.rep1 (Re.set "_01234567") ];
      Re.seq [ Re.str "0O"; Re.rep1 (Re.set "_01234567") ];
      Re.seq [ Re.str "0b"; Re.rep1 (Re.set "_01") ];
      Re.seq [ Re.str "0B"; Re.rep1 (Re.set "_01") ];
      Re.seq [ float_number; Re.opt (Re.set "jJ") ];
      Re.seq [ integer_number; Re.opt (Re.set "jJ") ];
    ]

let rust_number =
  Re.alt
    [
      Re.seq
        [
          Re.str "0x";
          Re.rep1 (Re.set "_0123456789abcdefABCDEF");
          Re.opt
            (Re.alt
               [
                 Re.str "u8";
                 Re.str "u16";
                 Re.str "u32";
                 Re.str "u64";
                 Re.str "u128";
                 Re.str "usize";
                 Re.str "i8";
                 Re.str "i16";
                 Re.str "i32";
                 Re.str "i64";
                 Re.str "i128";
                 Re.str "isize";
               ]);
        ];
      Re.seq [ Re.str "0o"; Re.rep1 (Re.set "_01234567") ];
      Re.seq [ Re.str "0b"; Re.rep1 (Re.set "_01") ];
      Re.seq [ float_number; Re.opt (Re.alt [ Re.str "f32"; Re.str "f64" ]) ];
      Re.seq
        [
          integer_number;
          Re.opt
            (Re.alt
               [
                 Re.str "u8";
                 Re.str "u16";
                 Re.str "u32";
                 Re.str "u64";
                 Re.str "u128";
                 Re.str "usize";
                 Re.str "i8";
                 Re.str "i16";
                 Re.str "i32";
                 Re.str "i64";
                 Re.str "i128";
                 Re.str "isize";
               ]);
        ];
    ]

let json_number =
  Re.seq
    [
      Re.opt (Re.set "-");
      Re.alt [ Re.str "0"; Re.seq [ Re.set "123456789"; Re.rep (Re.set "0123456789") ] ];
      Re.opt (Re.seq [ Re.str "."; Re.rep1 (Re.set "0123456789") ]);
      Re.opt (Re.seq [ Re.set "eE"; Re.opt (Re.set "+-"); Re.rep1 (Re.set "0123456789") ]);
    ]

let css_number =
  Re.alt
    [
      Re.seq [ Re.str "#"; Re.rep1 (Re.set "0123456789abcdefABCDEF") ];
      Re.seq
        [
          decimal;
          Re.opt
            (Re.alt
               [
                 Re.str "vmin";
                 Re.str "vmax";
                 Re.str "rem";
                 Re.str "px";
                 Re.str "em";
                 Re.str "ex";
                 Re.str "ch";
                 Re.str "vw";
                 Re.str "vh";
                 Re.str "cm";
                 Re.str "mm";
                 Re.str "in";
                 Re.str "pt";
                 Re.str "pc";
                 Re.str "deg";
                 Re.str "grad";
                 Re.str "rad";
                 Re.str "turn";
                 Re.str "Hz";
                 Re.str "kHz";
                 Re.str "ms";
                 Re.str "s";
                 Re.str "%";
               ]);
        ];
    ]

let ident = Re.set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
let ident_tail = Re.set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"
let identifier = Re.seq [ ident; Re.rep ident_tail ]

let identifier_dash =
  Re.seq
    [
      ident;
      Re.rep (Re.set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-");
    ]

let identifier_dollar =
  Re.seq
    [
      Re.set "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";
      Re.rep (Re.set "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_");
    ]
