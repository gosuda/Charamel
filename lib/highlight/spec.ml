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

let seq = Re.seq
let alt = Re.alt
let str = Re.str
let set = Re.set
let rep = Re.rep
let rep1 = Re.rep1
let opt = Re.opt
let not_chars chars = Re.diff Re.any (Re.set chars)

let decimal =
  seq
    [
      set "0123456789";
      rep (set "_0123456789");
      opt (seq [ str "."; rep (set "_0123456789") ]);
      opt (seq [ set "eE"; opt (set "+-"); set "0123456789"; rep (set "_0123456789") ]);
    ]

let decimal_signed = seq [ opt (set "+-"); decimal ]

let float_number =
  alt
    [
      seq
        [
          set "0123456789";
          rep (set "_0123456789");
          str ".";
          rep (set "_0123456789");
          opt (seq [ set "eE"; opt (set "+-"); rep1 (set "_0123456789") ]);
        ];
      decimal;
    ]

let integer_number = seq [ set "0123456789"; rep (set "_0123456789") ]

let c_number =
  alt
    [
      seq [ str "0x"; rep1 (set "_0123456789abcdefABCDEF"); opt (set "uUlL") ];
      seq [ str "0X"; rep1 (set "_0123456789abcdefABCDEF"); opt (set "uUlL") ];
      seq [ str "0b"; rep1 (set "_01"); opt (set "uUlL") ];
      seq [ str "0B"; rep1 (set "_01"); opt (set "uUlL") ];
      seq [ str "0o"; rep1 (set "_01234567"); opt (set "uUlL") ];
      seq [ str "0O"; rep1 (set "_01234567"); opt (set "uUlL") ];
      seq [ decimal; opt (set "fFlLuU") ];
    ]

let python_number =
  alt
    [
      seq [ str "0x"; rep1 (set "_0123456789abcdefABCDEF") ];
      seq [ str "0X"; rep1 (set "_0123456789abcdefABCDEF") ];
      seq [ str "0o"; rep1 (set "_01234567") ];
      seq [ str "0O"; rep1 (set "_01234567") ];
      seq [ str "0b"; rep1 (set "_01") ];
      seq [ str "0B"; rep1 (set "_01") ];
      seq [ float_number; opt (set "jJ") ];
      seq [ integer_number; opt (set "jJ") ];
    ]

let rust_number =
  alt
    [
      seq
        [
          str "0x";
          rep1 (set "_0123456789abcdefABCDEF");
          opt
            (alt
               [
                 str "u8";
                 str "u16";
                 str "u32";
                 str "u64";
                 str "u128";
                 str "usize";
                 str "i8";
                 str "i16";
                 str "i32";
                 str "i64";
                 str "i128";
                 str "isize";
               ]);
        ];
      seq [ str "0o"; rep1 (set "_01234567") ];
      seq [ str "0b"; rep1 (set "_01") ];
      seq [ float_number; opt (alt [ str "f32"; str "f64" ]) ];
      seq
        [
          integer_number;
          opt
            (alt
               [
                 str "u8";
                 str "u16";
                 str "u32";
                 str "u64";
                 str "u128";
                 str "usize";
                 str "i8";
                 str "i16";
                 str "i32";
                 str "i64";
                 str "i128";
                 str "isize";
               ]);
        ];
    ]

let json_number =
  seq
    [
      opt (set "-");
      alt [ str "0"; seq [ set "123456789"; rep (set "0123456789") ] ];
      opt (seq [ str "."; rep1 (set "0123456789") ]);
      opt (seq [ set "eE"; opt (set "+-"); rep1 (set "0123456789") ]);
    ]

let css_number =
  alt
    [
      seq [ str "#"; rep1 (set "0123456789abcdefABCDEF") ];
      seq
        [
          decimal;
          opt
            (alt
               [
                 str "vmin";
                 str "vmax";
                 str "rem";
                 str "px";
                 str "em";
                 str "ex";
                 str "ch";
                 str "vw";
                 str "vh";
                 str "cm";
                 str "mm";
                 str "in";
                 str "pt";
                 str "pc";
                 str "deg";
                 str "grad";
                 str "rad";
                 str "turn";
                 str "Hz";
                 str "kHz";
                 str "ms";
                 str "s";
                 str "%";
               ]);
        ];
    ]

let ident = set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_"
let ident_tail = set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_"
let identifier = seq [ ident; rep ident_tail ]

let identifier_dash =
  seq
    [
      ident; rep (set "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_-");
    ]

let identifier_dollar =
  seq
    [
      set "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_";
      rep (set "$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_");
    ]
