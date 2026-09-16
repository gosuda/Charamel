type code =
  | Char of Uchar.t
  | Enter
  | Tab
  | Backspace
  | Escape
  | Space
  | Insert
  | Delete
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | F of int
  | Kp_0
  | Kp_1
  | Kp_2
  | Kp_3
  | Kp_4
  | Kp_5
  | Kp_6
  | Kp_7
  | Kp_8
  | Kp_9
  | Kp_decimal
  | Kp_divide
  | Kp_multiply
  | Kp_subtract
  | Kp_add
  | Kp_enter
  | Kp_equal
  | Kp_begin
  | Caps_lock
  | Scroll_lock
  | Num_lock
  | Print_screen
  | Pause
  | Menu
  | Media_play
  | Media_pause
  | Media_play_pause
  | Media_stop
  | Media_next
  | Media_prev
  | Media_record
  | Media_fast_forward
  | Media_rewind
  | Volume_up
  | Volume_down
  | Volume_mute
  | Left_shift
  | Left_ctrl
  | Left_alt
  | Left_super
  | Left_hyper
  | Left_meta
  | Right_shift
  | Right_ctrl
  | Right_alt
  | Right_super
  | Right_hyper
  | Right_meta
  | Iso_level3_shift
  | Iso_level5_shift

type mods = {
  shift : bool;
  alt : bool;
  ctrl : bool;
  meta : bool;
  super : bool;
  hyper : bool;
  caps_lock : bool;
  num_lock : bool;
}

type event = Press | Repeat | Release

type t = {
  code : code;
  mods : mods;
  text : string;
  shifted : Uchar.t option;
  base : Uchar.t option;
  event : event;
}

let no_mods =
  {
    shift = false;
    alt = false;
    ctrl = false;
    meta = false;
    super = false;
    hyper = false;
    caps_lock = false;
    num_lock = false;
  }

let normalize_mods code mods =
  let own_ctrl = match code with Left_ctrl | Right_ctrl -> true | _ -> false in
  let own_alt = match code with Left_alt | Right_alt -> true | _ -> false in
  let own_shift = match code with Left_shift | Right_shift -> true | _ -> false in
  let own_meta = match code with Left_meta | Right_meta -> true | _ -> false in
  let own_super = match code with Left_super | Right_super -> true | _ -> false in
  let own_hyper = match code with Left_hyper | Right_hyper -> true | _ -> false in
  {
    ctrl = mods.ctrl && not own_ctrl;
    alt = mods.alt && not own_alt;
    shift = mods.shift && not own_shift;
    meta = mods.meta && not own_meta;
    super = mods.super && not own_super;
    hyper = mods.hyper && not own_hyper;
    caps_lock = mods.caps_lock && code <> Caps_lock;
    num_lock = mods.num_lock && code <> Num_lock;
  }

let v ?(mods = no_mods) code =
  let code =
    match code with
    | Char scalar when Uchar.equal scalar (Uchar.of_char ' ') -> Space
    | code -> code
  in
  {
    code;
    mods = normalize_mods code mods;
    text = "";
    shifted = None;
    base = None;
    event = Press;
  }

(* Every non-[Char] code paired with its canonical lower-case name. [to_string] looks up
   a code here; [of_string] builds the reverse table from the same list, so the two can
   never drift apart. *)
let named_codes =
  [
    (Enter, "enter");
    (Tab, "tab");
    (Backspace, "backspace");
    (Escape, "escape");
    (Space, "space");
    (Insert, "insert");
    (Delete, "delete");
    (Up, "up");
    (Down, "down");
    (Left, "left");
    (Right, "right");
    (Home, "home");
    (End, "end");
    (Page_up, "page_up");
    (Page_down, "page_down");
    (Kp_0, "kp_0");
    (Kp_1, "kp_1");
    (Kp_2, "kp_2");
    (Kp_3, "kp_3");
    (Kp_4, "kp_4");
    (Kp_5, "kp_5");
    (Kp_6, "kp_6");
    (Kp_7, "kp_7");
    (Kp_8, "kp_8");
    (Kp_9, "kp_9");
    (Kp_decimal, "kp_decimal");
    (Kp_divide, "kp_divide");
    (Kp_multiply, "kp_multiply");
    (Kp_subtract, "kp_subtract");
    (Kp_add, "kp_add");
    (Kp_enter, "kp_enter");
    (Kp_equal, "kp_equal");
    (Kp_begin, "kp_begin");
    (Caps_lock, "caps_lock");
    (Scroll_lock, "scroll_lock");
    (Num_lock, "num_lock");
    (Print_screen, "print_screen");
    (Pause, "pause");
    (Menu, "menu");
    (Media_play, "media_play");
    (Media_pause, "media_pause");
    (Media_play_pause, "media_play_pause");
    (Media_stop, "media_stop");
    (Media_next, "media_next");
    (Media_prev, "media_prev");
    (Media_record, "media_record");
    (Media_fast_forward, "media_fast_forward");
    (Media_rewind, "media_rewind");
    (Volume_up, "volume_up");
    (Volume_down, "volume_down");
    (Volume_mute, "volume_mute");
    (Left_shift, "left_shift");
    (Left_ctrl, "left_ctrl");
    (Left_alt, "left_alt");
    (Left_super, "left_super");
    (Left_hyper, "left_hyper");
    (Left_meta, "left_meta");
    (Right_shift, "right_shift");
    (Right_ctrl, "right_ctrl");
    (Right_alt, "right_alt");
    (Right_super, "right_super");
    (Right_hyper, "right_hyper");
    (Right_meta, "right_meta");
    (Iso_level3_shift, "iso_level3_shift");
    (Iso_level5_shift, "iso_level5_shift");
  ]

let name_of_code =
  let tbl = Hashtbl.create 128 in
  List.iter (fun (c, n) -> Hashtbl.replace tbl c n) named_codes;
  fun c -> Hashtbl.find_opt tbl c

let code_of_name =
  let tbl = Hashtbl.create 128 in
  List.iter (fun (code, name) -> Hashtbl.replace tbl name code) named_codes;
  for n = 1 to 63 do
    Hashtbl.replace tbl (Fmt.str "f%d" n) (F n)
  done;
  Hashtbl.replace tbl "esc" Escape;
  Hashtbl.replace tbl "pgup" Page_up;
  Hashtbl.replace tbl "pgdown" Page_down;
  Hashtbl.replace tbl "plus" (Char (Uchar.of_char '+'));
  fun name -> Hashtbl.find_opt tbl name

let code_name = function
  | F n -> Fmt.str "f%d" n
  | Char u when Uchar.equal u (Uchar.of_char ' ') -> "space"
  | Char u ->
      let b = Buffer.create 4 in
      Buffer.add_utf_8_uchar b u;
      Buffer.contents b
  | c -> ( match name_of_code c with Some n -> n | None -> assert false)

let is_ctrl_mod_code = function Left_ctrl | Right_ctrl -> true | _ -> false
let is_alt_mod_code = function Left_alt | Right_alt -> true | _ -> false
let is_shift_mod_code = function Left_shift | Right_shift -> true | _ -> false
let is_meta_mod_code = function Left_meta | Right_meta -> true | _ -> false
let is_super_mod_code = function Left_super | Right_super -> true | _ -> false
let is_hyper_mod_code = function Left_hyper | Right_hyper -> true | _ -> false

let to_string k =
  let b = Buffer.create 16 in
  let add cond suffix pred =
    if cond && not (pred k.code) then Buffer.add_string b suffix
  in
  add k.mods.ctrl "ctrl+" is_ctrl_mod_code;
  add k.mods.alt "alt+" is_alt_mod_code;
  add k.mods.shift "shift+" is_shift_mod_code;
  add k.mods.meta "meta+" is_meta_mod_code;
  add k.mods.super "super+" is_super_mod_code;
  add k.mods.hyper "hyper+" is_hyper_mod_code;
  if k.mods.caps_lock then Buffer.add_string b "caps_lock+";
  if k.mods.num_lock then Buffer.add_string b "num_lock+";
  let has_prefix = Buffer.length b > 0 in
  let name =
    match k.code with
    | Char u when has_prefix && Uchar.equal u (Uchar.of_char '+') -> "plus"
    | code -> code_name code
  in
  Buffer.add_string b name;
  Buffer.contents b

(* A [part] is a single Unicode scalar iff decoding it consumes every byte. Malformed
   UTF-8 or more than one scalar is not a key name. *)
let single_scalar part =
  match String.length part with
  | 0 -> None
  | _ ->
      let d = String.get_utf_8_uchar part 0 in
      if Uchar.utf_decode_is_valid d && Uchar.utf_decode_length d = String.length part
      then Some (Uchar.utf_decode_uchar d)
      else None

let modifier_of_name = function
  | "ctrl" -> Some (fun m -> { m with ctrl = true })
  | "alt" -> Some (fun m -> { m with alt = true })
  | "shift" -> Some (fun m -> { m with shift = true })
  | "meta" -> Some (fun m -> { m with meta = true })
  | "super" -> Some (fun m -> { m with super = true })
  | "hyper" -> Some (fun m -> { m with hyper = true })
  | "caps_lock" -> Some (fun m -> { m with caps_lock = true })
  | "num_lock" -> Some (fun m -> { m with num_lock = true })
  | _ -> None

let classify_code part =
  match code_of_name part with
  | Some code -> `Code code
  | None -> (
      match single_scalar part with
      | Some scalar -> `Code (Char scalar)
      | None -> `Error part)

let classify_parts parts =
  let rec loop = function
    | [] -> []
    | [ part ] -> [ classify_code part ]
    | part :: rest -> (
        match modifier_of_name part with
        | Some update -> `Mod update :: loop rest
        | None -> `Error part :: loop rest)
  in
  loop parts

let of_string s =
  if s = "" then Error (`Msg "Key.of_string: empty string")
  else if s = "+" then Ok (v (Char (Uchar.of_char '+')))
  else
    let classified = classify_parts (String.split_on_char '+' s) in
    let bad_part =
      List.find_map (function `Error part -> Some part | _ -> None) classified
    in
    match bad_part with
    | Some part ->
        Error (`Msg (Fmt.str "Key.of_string: unknown key part %S in %S" part s))
    | None -> (
        match
          List.filter_map (function `Code code -> Some code | _ -> None) classified
        with
        | [] -> Error (`Msg (Fmt.str "Key.of_string: no key name in %S" s))
        | [ code ] ->
            let mods =
              List.fold_left
                (fun mods part ->
                  match part with `Mod update -> update mods | _ -> mods)
                no_mods classified
            in
            Ok (v ~mods code)
        | _ :: _ :: _ ->
            Error (`Msg (Fmt.str "Key.of_string: more than one key name in %S" s)))

let matches k1 k2 = k1.code = k2.code && k1.mods = k2.mods
let pp ppf k = Fmt.string ppf (to_string k)
