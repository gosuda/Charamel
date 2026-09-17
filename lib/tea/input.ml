let cap_bytes = 65_536
let byte s i = Char.code s.[i]
let esc = Char.chr 0x1b
let bel = Char.chr 0x07
let can = Char.chr 0x18
let sub = Char.chr 0x1a
let st = Char.chr 0x9c

let append_string a b =
  if a = "" then b
  else if b = "" then a
  else
    let out = Bytes.create (String.length a + String.length b) in
    Bytes.blit_string a 0 out 0 (String.length a);
    Bytes.blit_string b 0 out (String.length a) (String.length b);
    Bytes.unsafe_to_string out

let drop_prefix s n =
  if n >= String.length s then "" else String.sub s n (String.length s - n)

let valid_scalar n = n >= 0 && n <= 0x10ffff && not (n >= 0xd800 && n <= 0xdfff)
let uchar_or_replacement n = if valid_scalar n then Uchar.of_int n else Uchar.rep

let decode_utf8_scalar s i =
  let len = String.length s in
  let continuation j = j < len && byte s j land 0xc0 = 0x80 in
  let b0 = byte s i in
  if b0 < 0x80 then (Some (Uchar.of_int b0), 1)
  else if b0 land 0xe0 = 0xc0 then
    if b0 >= 0xc2 && continuation (i + 1) then
      let cp = ((b0 land 0x1f) lsl 6) lor (byte s (i + 1) land 0x3f) in
      (Some (Uchar.of_int cp), 2)
    else (None, 1)
  else if b0 land 0xf0 = 0xe0 then
    if continuation (i + 1) && continuation (i + 2) then
      let cp =
        ((b0 land 0x0f) lsl 12)
        lor ((byte s (i + 1) land 0x3f) lsl 6)
        lor (byte s (i + 2) land 0x3f)
      in
      if cp < 0x800 || (cp >= 0xd800 && cp <= 0xdfff) then (None, 1)
      else (Some (Uchar.of_int cp), 3)
    else (None, 1)
  else if b0 land 0xf8 = 0xf0 && b0 <= 0xf4 then
    if continuation (i + 1) && continuation (i + 2) && continuation (i + 3) then
      let cp =
        ((b0 land 0x07) lsl 18)
        lor ((byte s (i + 1) land 0x3f) lsl 12)
        lor ((byte s (i + 2) land 0x3f) lsl 6)
        lor (byte s (i + 3) land 0x3f)
      in
      if cp < 0x10000 || cp > 0x10ffff then (None, 1) else (Some (Uchar.of_int cp), 4)
    else (None, 1)
  else (None, 1)

let utf8_of_uchar u =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b u;
  Buffer.contents b

let utf8_expected b =
  if b < 0x80 then 1
  else if b land 0xe0 = 0xc0 then 2
  else if b land 0xf0 = 0xe0 then 3
  else if b land 0xf8 = 0xf0 && b <= 0xf4 then 4
  else 1

let is_print_scalar n =
  if not (valid_scalar n) then false
  else if n = 0x20 then true
  else
    match Uucp.Gc.general_category (Uchar.of_int n) with
    | `Lu | `Ll | `Lt | `Lm | `Lo | `Mn | `Mc | `Me | `Nd | `Nl | `No | `Pc | `Pd | `Ps
    | `Pe | `Pi | `Pf | `Po | `Sm | `Sc | `Sk | `So ->
        true
    | `Cc | `Cf | `Cn | `Co | `Cs | `Zl | `Zp | `Zs -> false

let simple_case_map mapping u =
  match mapping u with `Self -> u | `Uchars (x :: _) -> x | `Uchars [] -> u

let to_lower_scalar = simple_case_map Uucp.Case.Map.to_lower
let to_upper_scalar = simple_case_map Uucp.Case.Map.to_upper

let no_mods =
  {
    Key.shift = false;
    alt = false;
    ctrl = false;
    meta = false;
    super = false;
    hyper = false;
    caps_lock = false;
    num_lock = false;
  }

let mods_of ?(shift = false) ?(alt = false) ?(ctrl = false) ?(meta = false)
    ?(super = false) ?(hyper = false) ?(caps_lock = false) ?(num_lock = false) () =
  { Key.shift; alt; ctrl; meta; super; hyper; caps_lock; num_lock }

let mods_or (a : Key.mods) (b : Key.mods) =
  {
    Key.shift = a.Key.shift || b.Key.shift;
    alt = a.Key.alt || b.Key.alt;
    ctrl = a.Key.ctrl || b.Key.ctrl;
    meta = a.Key.meta || b.Key.meta;
    super = a.Key.super || b.Key.super;
    hyper = a.Key.hyper || b.Key.hyper;
    caps_lock = a.Key.caps_lock || b.Key.caps_lock;
    num_lock = a.Key.num_lock || b.Key.num_lock;
  }

let mods_of_kitty_bits bits =
  {
    Key.shift = bits land 1 <> 0;
    alt = bits land 2 <> 0;
    ctrl = bits land 4 <> 0;
    super = bits land 8 <> 0;
    hyper = bits land 16 <> 0;
    meta = bits land 32 <> 0;
    caps_lock = bits land 64 <> 0;
    num_lock = bits land 128 <> 0;
  }

let mods_of_legacy_bits bits =
  {
    Key.shift = bits land 1 <> 0;
    alt = bits land 2 <> 0;
    ctrl = bits land 4 <> 0;
    meta = bits land 8 <> 0;
    hyper = bits land 16 <> 0;
    super = bits land 32 <> 0;
    caps_lock = bits land 64 <> 0;
    num_lock = bits land 128 <> 0;
  }

let beyond_shift (m : Key.mods) =
  m.Key.alt || m.Key.ctrl || m.Key.meta || m.Key.super || m.Key.hyper

let printable_scalar = function
  | Some scalar when is_print_scalar (Uchar.to_int scalar) -> Some scalar
  | _ -> None

let make_key ?(mods = no_mods) ?(text = "") ?shifted ?base ?(event = Key.Press) code =
  { Key.code; mods; text; shifted; base; event }

let add_alt (k : Key.t) = { k with Key.mods = { k.Key.mods with alt = true }; text = "" }

let parse_decimal s start stop =
  if start >= stop then None
  else
    let value = ref 0 in
    let valid = ref true in
    for i = start to stop - 1 do
      let d = byte s i - Char.code '0' in
      if d < 0 || d > 9 || !value > 1_000_000_000 then valid := false
      else value := (!value * 10) + d
    done;
    if !valid then Some !value else None

type parameter = int option list

type csi = {
  prefix : char option;
  params : parameter list;
  intermediates : string;
  final : char option;
  next : int;
  urxvt : bool;
  invalid_params : bool;
}

let parse_params s start stop =
  let fields_rev = ref [] in
  let current_rev = ref [] in
  let component_start = ref None in
  let field_active = ref false in
  let component_required = ref false in
  let total = ref 0 in
  let add_component at =
    let value = Option.bind !component_start (fun from -> parse_decimal s from at) in
    if !total < 32 then current_rev := value :: !current_rev;
    incr total;
    component_start := None;
    component_required := false
  in
  for i = start to stop - 1 do
    match s.[i] with
    | '0' .. '9' ->
        field_active := true;
        component_required := true;
        if Option.is_none !component_start then component_start := Some i
    | ':' ->
        field_active := true;
        add_component i;
        component_required := true
    | ';' ->
        field_active := true;
        add_component i;
        if !current_rev <> [] then fields_rev := List.rev !current_rev :: !fields_rev;
        current_rev := [];
        component_required := true
    | _ -> ()
  done;
  if !field_active then begin
    if Option.is_some !component_start || !component_required then add_component stop;
    if !current_rev <> [] then fields_rev := List.rev !current_rev :: !fields_rev
  end;
  List.rev !fields_rev

let parse_csi_header s start =
  let len = String.length s in
  let i = ref start in
  let prefix =
    if !i < len && s.[!i] >= '<' && s.[!i] <= '?' then begin
      let value = Some s.[!i] in
      incr i;
      value
    end
    else None
  in
  let params_start = !i in
  let invalid_params = ref false in
  while !i < len && byte s !i >= 0x30 && byte s !i <= 0x3f do
    if byte s !i >= 0x3c then invalid_params := true;
    incr i
  done;
  let params_stop = !i in
  let intermediates_start = !i in
  while !i < len && byte s !i >= 0x20 && byte s !i <= 0x2f do
    incr i
  done;
  let intermediates =
    String.sub s intermediates_start (min 2 (!i - intermediates_start))
  in
  let params = parse_params s params_start params_stop in
  if !i >= len then
    if intermediates <> "" && params_stop > params_start then
      let last = intermediates.[String.length intermediates - 1] in
      if last = '$' || last = '^' || last = '@' then
        Some
          {
            prefix;
            params;
            intermediates;
            final = None;
            next = !i;
            urxvt = true;
            invalid_params = !invalid_params;
          }
      else None
    else None
  else if byte s !i >= 0x40 && byte s !i <= 0x7e then
    Some
      {
        prefix;
        params;
        intermediates;
        final = Some s.[!i];
        next = !i + 1;
        urxvt = false;
        invalid_params = !invalid_params;
      }
  else
    Some
      {
        prefix;
        params;
        intermediates;
        final = None;
        next = !i;
        urxvt = false;
        invalid_params = !invalid_params;
      }

let nth_opt xs n =
  let rec loop i = function
    | [] -> None
    | x :: _ when i = 0 -> Some x
    | _ :: rest -> loop (i - 1) rest
  in
  loop n xs

let param_value ?(default = 0) = function Some (Some n :: _) -> n | _ -> default

let param_is_plain = function
  | None | Some [] | Some [ _ ] -> true
  | Some (_ :: _ :: _) -> false

let has_component = function Some (_ :: _) -> true | _ -> false

let code_of_tilde = function
  | 1 | 7 -> Some Key.Home
  | 2 -> Some Key.Insert
  | 3 -> Some Key.Delete
  | 4 | 8 -> Some Key.End
  | 5 -> Some Key.Page_up
  | 6 -> Some Key.Page_down
  | 11 -> Some (Key.F 1)
  | 12 -> Some (Key.F 2)
  | 13 -> Some (Key.F 3)
  | 14 -> Some (Key.F 4)
  | 15 -> Some (Key.F 5)
  | 17 -> Some (Key.F 6)
  | 18 -> Some (Key.F 7)
  | 19 -> Some (Key.F 8)
  | 20 -> Some (Key.F 9)
  | 21 -> Some (Key.F 10)
  | 23 -> Some (Key.F 11)
  | 24 -> Some (Key.F 12)
  | 25 -> Some (Key.F 13)
  | 26 -> Some (Key.F 14)
  | 28 -> Some (Key.F 15)
  | 29 -> Some (Key.F 16)
  | 31 -> Some (Key.F 17)
  | 32 -> Some (Key.F 18)
  | 33 -> Some (Key.F 19)
  | 34 -> Some (Key.F 20)
  | _ -> None

let code_of_simple = function
  | 'A' | 'a' -> Some Key.Up
  | 'B' | 'b' -> Some Key.Down
  | 'C' | 'c' -> Some Key.Right
  | 'D' | 'd' -> Some Key.Left
  | 'E' -> Some Key.Kp_begin
  | 'F' -> Some Key.End
  | 'H' -> Some Key.Home
  | 'P' -> Some (Key.F 1)
  | 'Q' -> Some (Key.F 2)
  | 'R' -> Some (Key.F 3)
  | 'S' -> Some (Key.F 4)
  | 'Z' -> Some Key.Tab
  | _ -> None

let legacy_key_for_csi csi =
  let final = match csi.final with Some c -> c | None -> '~' in
  let first = param_value (nth_opt csi.params 0) in
  let modifiers =
    match nth_opt csi.params 1 with
    | Some (Some n :: _) when n > 0 -> mods_of_legacy_bits (n - 1)
    | _ -> no_mods
  in
  let code = if final = '~' then code_of_tilde first else code_of_simple final in
  Option.bind code (fun code ->
      let valid_shape =
        List.length csi.params <= 2
        && ((final = '~' && param_is_plain (nth_opt csi.params 0))
           || final <> '~'
              && (first = 0 || first = 1)
              && param_is_plain (nth_opt csi.params 0))
      in
      if not valid_shape then None
      else
        let modifiers =
          if csi.urxvt then
            match csi.intermediates with
            | "$" -> mods_or modifiers (mods_of ~shift:true ())
            | "^" -> mods_or modifiers (mods_of ~ctrl:true ())
            | "@" -> mods_or modifiers (mods_of ~shift:true ~ctrl:true ())
            | _ -> modifiers
          else if final >= 'a' && final <= 'd' then
            mods_or modifiers (mods_of ~shift:true ())
          else modifiers
        in
        Some (make_key ~mods:modifiers code))

let kitty_code cp =
  match cp with
  | 8 -> Key.Backspace
  | 9 -> Key.Tab
  | 13 -> Key.Enter
  | 27 -> Key.Escape
  | 127 -> Key.Backspace
  | 57344 -> Key.Escape
  | 57345 -> Key.Enter
  | 57346 -> Key.Tab
  | 57347 -> Key.Backspace
  | 57348 -> Key.Insert
  | 57349 -> Key.Delete
  | 57350 -> Key.Left
  | 57351 -> Key.Right
  | 57352 -> Key.Up
  | 57353 -> Key.Down
  | 57354 -> Key.Page_up
  | 57355 -> Key.Page_down
  | 57356 -> Key.Home
  | 57357 -> Key.End
  | 57358 -> Key.Caps_lock
  | 57359 -> Key.Scroll_lock
  | 57360 -> Key.Num_lock
  | 57361 -> Key.Print_screen
  | 57362 -> Key.Pause
  | 57363 -> Key.Menu
  | n when n >= 57364 && n <= 57398 -> Key.F (n - 57364 + 1)
  | 57399 -> Key.Kp_0
  | 57400 -> Key.Kp_1
  | 57401 -> Key.Kp_2
  | 57402 -> Key.Kp_3
  | 57403 -> Key.Kp_4
  | 57404 -> Key.Kp_5
  | 57405 -> Key.Kp_6
  | 57406 -> Key.Kp_7
  | 57407 -> Key.Kp_8
  | 57408 -> Key.Kp_9
  | 57409 -> Key.Kp_decimal
  | 57410 -> Key.Kp_divide
  | 57411 -> Key.Kp_multiply
  | 57412 -> Key.Kp_subtract
  | 57413 -> Key.Kp_add
  | 57414 -> Key.Kp_enter
  | 57415 -> Key.Kp_equal
  | 57416 -> Key.Char (Uchar.of_char ',')
  | 57417 -> Key.Left
  | 57418 -> Key.Right
  | 57419 -> Key.Up
  | 57420 -> Key.Down
  | 57421 -> Key.Page_up
  | 57422 -> Key.Page_down
  | 57423 -> Key.Home
  | 57424 -> Key.End
  | 57425 -> Key.Insert
  | 57426 -> Key.Delete
  | 57427 -> Key.Kp_begin
  | 57428 -> Key.Media_play
  | 57429 -> Key.Media_pause
  | 57430 -> Key.Media_play_pause
  | 57431 -> Key.Media_rewind
  | 57432 -> Key.Media_stop
  | 57433 -> Key.Media_fast_forward
  | 57434 -> Key.Media_rewind
  | 57435 -> Key.Media_next
  | 57436 -> Key.Media_prev
  | 57437 -> Key.Media_record
  | 57438 -> Key.Volume_down
  | 57439 -> Key.Volume_up
  | 57440 -> Key.Volume_mute
  | 57441 -> Key.Left_shift
  | 57442 -> Key.Left_ctrl
  | 57443 -> Key.Left_alt
  | 57444 -> Key.Left_super
  | 57445 -> Key.Left_hyper
  | 57446 -> Key.Left_meta
  | 57447 -> Key.Right_shift
  | 57448 -> Key.Right_ctrl
  | 57449 -> Key.Right_alt
  | 57450 -> Key.Right_super
  | 57451 -> Key.Right_hyper
  | 57452 -> Key.Right_meta
  | 57453 -> Key.Iso_level3_shift
  | 57454 -> Key.Iso_level5_shift
  | n when valid_scalar n -> Key.Char (Uchar.of_int n)
  | _ -> Key.Char Uchar.rep

let scalar_of_field field index =
  match nth_opt field index with
  | Some (Some n) -> Some (uchar_or_replacement n)
  | _ -> None

let text_of_field field =
  let out = Buffer.create 8 in
  List.iter
    (function
      | Some n when n > 0 ->
          Buffer.add_string out (utf8_of_uchar (uchar_or_replacement n))
      | _ -> ())
    field;
  Buffer.contents out

let kitty_key params =
  let first = nth_opt params 0 in
  let code = kitty_code (param_value ~default:1 first) in
  let shifted =
    printable_scalar (Option.bind first (fun field -> scalar_of_field field 1))
  in
  let base =
    printable_scalar (Option.bind first (fun field -> scalar_of_field field 2))
  in
  let second = nth_opt params 1 in
  let modifier_value = param_value ~default:1 second in
  let mods =
    if modifier_value > 1 then mods_of_kitty_bits (modifier_value - 1) else no_mods
  in
  let event =
    match Option.bind second (fun field -> scalar_of_field field 1) with
    | Some u when Uchar.to_int u = 2 -> Key.Repeat
    | Some u when Uchar.to_int u = 3 -> Key.Release
    | _ -> Key.Press
  in
  let payload =
    match nth_opt params 2 with Some field -> text_of_field field | None -> ""
  in
  let text_mods = { mods with num_lock = false } in
  let printable =
    (not text_mods.Key.alt) && (not text_mods.Key.ctrl) && (not text_mods.Key.meta)
    && (not text_mods.Key.super) && not text_mods.Key.hyper
  in
  let text =
    if beyond_shift text_mods then ""
    else if payload <> "" then payload
    else
      match code with
      | Key.Kp_0 when printable -> "0"
      | Key.Kp_1 when printable -> "1"
      | Key.Kp_2 when printable -> "2"
      | Key.Kp_3 when printable -> "3"
      | Key.Kp_4 when printable -> "4"
      | Key.Kp_5 when printable -> "5"
      | Key.Kp_6 when printable -> "6"
      | Key.Kp_7 when printable -> "7"
      | Key.Kp_8 when printable -> "8"
      | Key.Kp_9 when printable -> "9"
      | Key.Kp_equal when printable -> "="
      | Key.Kp_multiply when printable -> "*"
      | Key.Kp_add when printable -> "+"
      | Key.Kp_subtract when printable -> "-"
      | Key.Kp_decimal when printable -> "."
      | Key.Kp_divide when printable -> "/"
      | Key.Char u when printable ->
          if mods = no_mods then utf8_of_uchar u
          else if Option.is_some shifted then utf8_of_uchar (Option.get shifted)
          else if mods.Key.shift || mods.Key.caps_lock then
            utf8_of_uchar (to_upper_scalar u)
          else utf8_of_uchar (to_lower_scalar u)
      | _ -> ""
  in
  make_key ~mods ~text ?shifted ?base ~event code

let kitty_extended params k =
  match nth_opt params 1 with
  | Some field -> (
      match scalar_of_field field 1 with
      | Some u when Uchar.to_int u = 2 -> { k with Key.event = Key.Repeat }
      | Some u when Uchar.to_int u = 3 -> { k with Key.event = Key.Release }
      | _ -> k)
  | None -> k

let mouse_button bits =
  let mods =
    mods_of ~shift:(bits land 4 <> 0) ~alt:(bits land 8 <> 0) ~ctrl:(bits land 16 <> 0) ()
  in
  let button_index = bits land 3 in
  let button, wheel =
    if bits land 128 <> 0 then
      ( (match button_index with
        | 0 -> Mouse.Backward
        | 1 -> Mouse.Forward
        | 2 -> Mouse.Button_10
        | _ -> Mouse.Button_11),
        false )
    else if bits land 64 <> 0 then
      ( (match button_index with
        | 0 -> Mouse.Wheel_up
        | 1 -> Mouse.Wheel_down
        | 2 -> Mouse.Wheel_left
        | _ -> Mouse.Wheel_right),
        true )
    else
      ( (match button_index with
        | 0 -> Mouse.Left
        | 1 -> Mouse.Middle
        | 2 -> Mouse.Right
        | _ -> Mouse.None_),
        false )
  in
  let motion = bits land 32 <> 0 && not wheel in
  (mods, button, wheel, motion, button_index = 3)

let mouse_event ~x ~y ~release bits =
  let mods, button, wheel, motion, x10_release = mouse_button bits in
  let action =
    if wheel then Mouse.Press
    else if motion then Mouse.Motion
    else if release || x10_release then Mouse.Release
    else Mouse.Press
  in
  Event.Mouse { Mouse.x; y; button; action; mods }

let find_substring s needle from =
  let n = String.length needle in
  let last = String.length s - n in
  let rec loop i =
    if i > last then None else if String.sub s i n = needle then Some i else loop (i + 1)
  in
  if n = 0 then Some from else if from > last then None else loop from

type termination = Terminated of int * int | Cancelled of int * int | Need_termination

let find_termination ~accept_bel s start =
  let len = String.length s in
  let rec loop i =
    if i >= len then Need_termination
    else
      match s.[i] with
      | c when c = can || c = sub -> Cancelled (i, 1)
      | c when c = st -> Terminated (i, 1)
      | c when accept_bel && c = bel -> Terminated (i, 1)
      | c when c = esc ->
          if i + 1 >= len then Need_termination
          else if s.[i + 1] = '\\' then Terminated (i, 2)
          else loop (i + 1)
      | _ -> loop (i + 1)
  in
  loop start

let parse_hex_digit c =
  if c >= '0' && c <= '9' then Some (Char.code c - Char.code '0')
  else if c >= 'a' && c <= 'f' then Some (10 + Char.code c - Char.code 'a')
  else if c >= 'A' && c <= 'F' then Some (10 + Char.code c - Char.code 'A')
  else None

let parse_hex_component s =
  if s = "" || String.length s > 4 then None
  else
    let value = ref 0 in
    let valid = ref true in
    String.iter
      (fun c ->
        match parse_hex_digit c with
        | Some d -> value := (!value lsl 4) lor d
        | None -> valid := false)
      s;
    if not !valid then None
    else
      Some
        (if String.length s = 1 then !value * 0x11
         else !value lsr ((4 * String.length s) - 8))

let parse_color s =
  if String.length s >= 4 && String.sub s 0 4 = "rgb:" then
    match String.split_on_char '/' (String.sub s 4 (String.length s - 4)) with
    | [ r; g; b ] -> (
        match (parse_hex_component r, parse_hex_component g, parse_hex_component b) with
        | Some r, Some g, Some b -> Charm_ansi.Color.rgb r g b
        | _ -> None)
    | _ -> None
  else Charm_ansi.Color.of_hex s

let parse_osc s start =
  match find_termination ~accept_bel:true s start with
  | Need_termination -> `Need
  | Cancelled (at, term_len) ->
      let next = at + term_len in
      `Done (next, [ Event.Unknown (String.sub s 0 next) ])
  | Terminated (at, term_len) -> (
      let next = at + term_len in
      let raw = String.sub s 0 next in
      let command_end =
        let i = ref start in
        while !i < at && s.[!i] >= '0' && s.[!i] <= '9' do
          incr i
        done;
        !i
      in
      if command_end = start || command_end >= at || s.[command_end] <> ';' then
        `Done (next, [ Event.Unknown raw ])
      else
        let payload = String.sub s (command_end + 1) (at - command_end - 1) in
        let event =
          match parse_decimal s start command_end with
          | Some 10 ->
              Option.map (fun c -> Event.Foreground_color c) (parse_color payload)
          | Some 11 ->
              Option.map (fun c -> Event.Background_color c) (parse_color payload)
          | Some 12 -> Option.map (fun c -> Event.Cursor_color c) (parse_color payload)
          | _ -> None
        in
        match event with
        | Some event -> `Done (next, [ event ])
        | None -> `Done (next, [ Event.Unknown raw ]))

type dcs_header = {
  prefix : char option;
  intermediates : string;
  final : char;
  data : int;
}

let parse_dcs_header s start stop =
  let i = ref start in
  let prefix =
    if !i < stop && s.[!i] >= '<' && s.[!i] <= '?' then begin
      let value = Some s.[!i] in
      incr i;
      value
    end
    else None
  in
  while !i < stop && byte s !i >= 0x30 && byte s !i <= 0x3f do
    incr i
  done;
  let inter_start = !i in
  while !i < stop && byte s !i >= 0x20 && byte s !i <= 0x2f do
    incr i
  done;
  if !i >= stop || byte s !i < 0x40 || byte s !i > 0x7e then None
  else
    Some
      {
        prefix;
        intermediates = String.sub s inter_start (min 2 (!i - inter_start));
        final = s.[!i];
        data = !i + 1;
      }

let parse_dcs s start =
  match find_termination ~accept_bel:false s start with
  | Need_termination -> `Need
  | Cancelled (at, term_len) ->
      let next = at + term_len in
      `Done (next, [ Event.Unknown (String.sub s 0 next) ])
  | Terminated (at, term_len) -> (
      let next = at + term_len in
      let raw = String.sub s 0 next in
      match parse_dcs_header s start at with
      | Some h when h.prefix = Some '>' && h.intermediates = "" && h.final = '|' ->
          `Done (next, [ Event.Terminal_version (String.sub s h.data (at - h.data)) ])
      | _ -> `Done (next, [ Event.Unknown raw ]))

let parse_st_unknown s start =
  match find_termination ~accept_bel:false s start with
  | Need_termination -> `Need
  | Cancelled (at, term_len) ->
      let next = at + term_len in
      `Done (next, [ Event.Unknown (String.sub s 0 next) ])
  | Terminated (at, term_len) ->
      let next = at + term_len in
      `Done (next, [ Event.Unknown (String.sub s 0 next) ])

let key_for_ss3 final modifier =
  let code =
    match final with
    | 'a' -> Some Key.Up
    | 'b' -> Some Key.Down
    | 'c' -> Some Key.Right
    | 'd' -> Some Key.Left
    | 'A' -> Some Key.Up
    | 'B' -> Some Key.Down
    | 'C' -> Some Key.Right
    | 'D' -> Some Key.Left
    | 'E' -> Some Key.Kp_begin
    | 'F' -> Some Key.End
    | 'H' -> Some Key.Home
    | 'P' -> Some (Key.F 1)
    | 'Q' -> Some (Key.F 2)
    | 'R' -> Some (Key.F 3)
    | 'S' -> Some (Key.F 4)
    | 'M' -> Some Key.Kp_enter
    | 'X' -> Some Key.Kp_equal
    | c when c >= 'j' && c <= 'y' ->
        let keypad =
          [
            Key.Kp_multiply;
            Key.Kp_add;
            Key.Char (Uchar.of_char ',');
            Key.Kp_subtract;
            Key.Kp_decimal;
            Key.Kp_divide;
            Key.Kp_0;
            Key.Kp_1;
            Key.Kp_2;
            Key.Kp_3;
            Key.Kp_4;
            Key.Kp_5;
            Key.Kp_6;
            Key.Kp_7;
            Key.Kp_8;
            Key.Kp_9;
          ]
        in
        Some (List.nth keypad (Char.code c - Char.code 'j'))
    | _ -> None
  in
  Option.map
    (fun code ->
      let mods = mods_of_legacy_bits (max 0 (modifier - 1)) in
      let mods =
        if final >= 'a' && final <= 'd' then mods_or mods (mods_of ~ctrl:true ())
        else mods
      in
      make_key ~mods code)
    code

let parse_ss3 s start =
  let len = String.length s in
  let i = ref start in
  let modifier = ref 0 in
  while !i < len && s.[!i] >= '0' && s.[!i] <= '9' do
    modifier := (!modifier * 10) + byte s !i - Char.code '0';
    incr i
  done;
  if !i >= len then `Need
  else if byte s !i >= 0x21 && byte s !i <= 0x7e then
    match key_for_ss3 s.[!i] !modifier with
    | Some key -> `Done (!i + 1, [ Event.Key key ])
    | None -> `Done (!i + 1, [ Event.Unknown (String.sub s 0 (!i + 1)) ])
  else `Done (!i, [ Event.Unknown (String.sub s 0 !i) ])

let parse_x10 s at =
  if String.length s < at + 3 then `Need
  else
    let bits = byte s at - 32 in
    let x = byte s (at + 1) - 33 in
    let y = byte s (at + 2) - 33 in
    `Done (at + 3, [ mouse_event ~x ~y ~release:false bits ])

let xterm_modify_other_keys params =
  if List.length params <> 3 then None
  else
    let modifier = param_value ~default:1 (nth_opt params 1) in
    let cp = param_value ~default:0 (nth_opt params 2) in
    let mods = mods_of_legacy_bits (max 0 (modifier - 1)) in
    let code, text =
      match cp with
      | 8 -> (Key.Backspace, "")
      | 9 -> (Key.Tab, "")
      | 13 -> (Key.Enter, "")
      | 27 -> (Key.Escape, "")
      | 32 -> (Key.Space, " ")
      | 127 -> (Key.Backspace, "")
      | n when valid_scalar n ->
          let u = Uchar.of_int n in
          let text =
            if beyond_shift mods then ""
            else if mods.Key.shift then utf8_of_uchar (to_upper_scalar u)
            else utf8_of_uchar u
          in
          (Key.Char u, text)
      | _ -> (Key.Char Uchar.rep, "")
    in
    Some (make_key ~mods ~text code)

let valid_mouse_params params =
  List.length params = 3
  && List.for_all
       (fun index ->
         match nth_opt params index with Some (Some _ :: _) -> true | _ -> false)
       [ 0; 1; 2 ]

let parse_csi_events s (csi : csi) =
  let raw = String.sub s 0 csi.next in
  let unknown () = [ Event.Unknown raw ] in
  let legacy () =
    match legacy_key_for_csi csi with
    | Some key -> [ Event.Key (kitty_extended csi.params key) ]
    | None -> unknown ()
  in
  if csi.invalid_params then unknown ()
  else
    match (csi.prefix, csi.intermediates, csi.final, csi.urxvt) with
    | Some '?', "$", Some 'y', false | None, "$", Some 'y', false ->
        let mode = param_value ~default:(-1) (nth_opt csi.params 0) in
        let value = param_value (nth_opt csi.params 1) in
        if mode < 0 || not (has_component (nth_opt csi.params 1)) then unknown ()
        else [ Event.Mode_report { mode; value } ]
    | Some '?', "", Some 'c', false | Some '>', "", Some 'c', false -> unknown ()
    | Some '?', "", Some 'u', false ->
        [ Event.Kitty_flags (param_value ~default:0 (nth_opt csi.params 0)) ]
    | Some '?', "", Some 'R', false ->
        let row = param_value ~default:1 (nth_opt csi.params 0) in
        let col = param_value ~default:1 (nth_opt csi.params 1) in
        if has_component (nth_opt csi.params 1) then
          [ Event.Cursor_position { row = row - 1; col = col - 1 } ]
        else unknown ()
    | Some '<', "", Some 'M', false ->
        if not (valid_mouse_params csi.params) then unknown ()
        else
          let bits = param_value (nth_opt csi.params 0) in
          let x = param_value ~default:1 (nth_opt csi.params 1) - 1 in
          let y = param_value ~default:1 (nth_opt csi.params 2) - 1 in
          [ mouse_event ~x ~y ~release:false bits ]
    | Some '<', "", Some 'm', false ->
        if not (valid_mouse_params csi.params) then unknown ()
        else
          let bits = param_value (nth_opt csi.params 0) in
          let x = param_value ~default:1 (nth_opt csi.params 1) - 1 in
          let y = param_value ~default:1 (nth_opt csi.params 2) - 1 in
          [ mouse_event ~x ~y ~release:true bits ]
    | Some '>', "", Some 'm', false -> unknown ()
    | None, "", Some 'I', false when csi.params = [] -> [ Event.Focus ]
    | None, "", Some 'O', false when csi.params = [] -> [ Event.Blur ]
    | None, "", Some 'R', false ->
        if csi.params = [] then [ Event.Key (make_key (Key.F 3)) ]
        else if
          List.length csi.params = 2
          && param_is_plain (nth_opt csi.params 0)
          && param_is_plain (nth_opt csi.params 1)
        then
          let row = param_value ~default:1 (nth_opt csi.params 0) in
          let col = param_value ~default:1 (nth_opt csi.params 1) in
          let cursor = Event.Cursor_position { row = row - 1; col = col - 1 } in
          if row = 1 && col >= 1 && col - 1 <= 15 then
            [
              Event.Key (make_key ~mods:(mods_of_legacy_bits (col - 1)) (Key.F 3)); cursor;
            ]
          else [ cursor ]
        else unknown ()
    | ( None,
        "",
        Some
          ( 'a' | 'b' | 'c' | 'd' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'H' | 'P' | 'Q'
          | 'S' | 'Z' ),
        false ) ->
        legacy ()
    | None, "", Some 'M', false -> []
    | None, "", Some 'u', false ->
        if csi.params = [] then unknown () else [ Event.Key (kitty_key csi.params) ]
    | None, "", Some '~', false ->
        let n = param_value ~default:(-1) (nth_opt csi.params 0) in
        if n = 200 && List.length csi.params = 1 && param_is_plain (nth_opt csi.params 0)
        then []
        else if
          n = 201 && List.length csi.params = 1 && param_is_plain (nth_opt csi.params 0)
        then unknown ()
        else if n = 27 then
          match xterm_modify_other_keys csi.params with
          | Some key -> [ Event.Key key ]
          | None -> unknown ()
        else legacy ()
    | None, "$", None, true | None, "^", None, true | None, "@", None, true -> legacy ()
    | _ -> unknown ()

type decoded = Need | Done of int * Event.t list

let parse_csi s start =
  match parse_csi_header s start with
  | None -> Need
  | Some csi when csi.final = Some 'M' && csi.prefix = None && not csi.urxvt -> (
      match parse_x10 s csi.next with
      | `Need -> Need
      | `Done (next, events) -> Done (next, events))
  | Some csi when Option.is_none csi.final && not csi.urxvt -> Need
  | Some csi ->
      let events = parse_csi_events s csi in
      let consumed = match csi.final with Some _ -> csi.next | None -> csi.next in
      Done (consumed, events)

let decode_plain s =
  let b = byte s 0 in
  if b <= 0x1f || b = 0x7f then
    Done
      ( 1,
        [
          Event.Key
            (match b with
            | 0x00 -> make_key ~mods:(mods_of ~ctrl:true ()) Key.Space
            | 0x08 ->
                make_key ~mods:(mods_of ~ctrl:true ()) (Key.Char (Uchar.of_char 'h'))
            | 0x09 -> make_key Key.Tab
            | 0x0d -> make_key Key.Enter
            | 0x1b -> make_key Key.Escape
            | n when n >= 0x01 && n <= 0x1a ->
                make_key ~mods:(mods_of ~ctrl:true ())
                  (Key.Char (Uchar.of_int (b + 0x60)))
            | n when n >= 0x1c && n <= 0x1f ->
                make_key ~mods:(mods_of ~ctrl:true ())
                  (Key.Char (Uchar.of_int (b + 0x40)))
            | 0x7f -> make_key Key.Backspace
            | _ -> make_key (Key.Char (Uchar.of_int b)));
        ] )
  else if b = 0x20 then Done (1, [ Event.Key (make_key ~text:" " Key.Space) ])
  else if b >= 0x80 && b <= 0x9f then
    Done
      ( 1,
        [
          Event.Key
            (make_key
               ~mods:(mods_of ~alt:true ~ctrl:true ())
               (Key.Char (Uchar.of_int (b - 0x40))));
        ] )
  else if b < 0x80 then
    let u = Uchar.of_int b in
    if b >= Char.code 'A' && b <= Char.code 'Z' then
      Done
        ( 1,
          [
            Event.Key
              (make_key ~mods:(mods_of ~shift:true ()) ~text:(String.sub s 0 1) ~shifted:u
                 (Key.Char (to_lower_scalar u)));
          ] )
    else Done (1, [ Event.Key (make_key ~text:(String.sub s 0 1) (Key.Char u)) ])
  else
    let needed = utf8_expected b in
    if String.length s < needed then Need
    else
      match decode_utf8_scalar s 0 with
      | None, _ -> Done (1, [ Event.Unknown (String.sub s 0 1) ])
      | Some u, scalar_len ->
          let look = min 256 (String.length s) in
          let text =
            match Charm_ansi.Width.graphemes (String.sub s 0 look) with
            | first :: _ when String.length first >= scalar_len -> first
            | _ -> utf8_of_uchar u
          in
          Done (String.length text, [ Event.Key (make_key ~text (Key.Char u)) ])

let rec decode_one s =
  if s = "" then Need
  else
    match byte s 0 with
    | 0x1b -> (
        if String.length s = 1 then Need
        else
          match s.[1] with
          | '[' -> parse_csi s 2
          | 'O' -> (
              match parse_ss3 s 2 with
              | `Need -> Need
              | `Done (n, events) -> Done (n, events))
          | ']' -> (
              match parse_osc s 2 with
              | `Need -> Need
              | `Done (n, events) -> Done (n, events))
          | 'P' -> (
              match parse_dcs s 2 with
              | `Need -> Need
              | `Done (n, events) -> Done (n, events))
          | '_' | '^' | 'X' -> (
              match parse_st_unknown s 2 with
              | `Need -> Need
              | `Done (n, events) -> Done (n, events))
          | _ -> (
              if s.[1] = esc then Done (2, [ Event.Key (add_alt (make_key Key.Escape)) ])
              else
                match decode_one (String.sub s 1 (String.length s - 1)) with
                | Done (n, [ Event.Key key ]) -> Done (n + 1, [ Event.Key (add_alt key) ])
                | Done _ -> Done (1, [ Event.Key (make_key Key.Escape) ])
                | Need -> Need))
    | 0x9b -> parse_csi s 1
    | 0x8f -> (
        match parse_ss3 s 1 with `Need -> Need | `Done (n, events) -> Done (n, events))
    | 0x9d -> (
        match parse_osc s 1 with `Need -> Need | `Done (n, events) -> Done (n, events))
    | 0x90 -> (
        match parse_dcs s 1 with `Need -> Need | `Done (n, events) -> Done (n, events))
    | 0x9f | 0x9e | 0x98 -> (
        match parse_st_unknown s 1 with
        | `Need -> Need
        | `Done (n, events) -> Done (n, events))
    | _ -> decode_plain s

let paste_end_7 = "\027[201~"
let paste_end_8 = "\155201~"

type t = { mutable pending : string; mutable paste : bool }

let create () = { pending = ""; paste = false }
let pending_escape t = (not t.paste) && t.pending = String.make 1 esc

let rec drain_paste t output =
  let first = find_substring t.pending paste_end_7 0 in
  let second = find_substring t.pending paste_end_8 0 in
  let marker =
    match (first, second) with
    | None, None -> None
    | Some at, None -> Some (at, String.length paste_end_7)
    | None, Some at -> Some (at, String.length paste_end_8)
    | Some a, Some b ->
        if a <= b then Some (a, String.length paste_end_7)
        else Some (b, String.length paste_end_8)
  in
  match marker with
  | Some (at, marker_length) ->
      let payload = String.sub t.pending 0 at in
      let rec emit offset remaining =
        if remaining = 0 then
          begin if offset = 0 then output := Event.Paste "" :: !output
          end
        else begin
          let n = min cap_bytes remaining in
          output := Event.Paste (String.sub payload offset n) :: !output;
          emit (offset + n) (remaining - n)
        end
      in
      emit 0 (String.length payload);
      t.pending <- drop_prefix t.pending (at + marker_length);
      t.paste <- false
  | None ->
      let length = String.length t.pending in
      if length > cap_bytes then begin
        let n = min cap_bytes (length - 5) in
        output := Event.Paste (String.sub t.pending 0 n) :: !output;
        t.pending <- drop_prefix t.pending n;
        drain_paste t output
      end

let emit_unknown_chunks output s =
  let rec loop offset remaining =
    if remaining > 0 then begin
      let n = min cap_bytes remaining in
      output := Event.Unknown (String.sub s offset n) :: !output;
      loop (offset + n) (remaining - n)
    end
  in
  loop 0 (String.length s)

let rec drain t output ~force =
  if t.paste then begin
    drain_paste t output;
    if not t.paste then drain t output ~force
  end
  else if t.pending = "" then ()
  else if String.length t.pending > cap_bytes && not force then drain t output ~force:true
  else
    match decode_one t.pending with
    | Need when force ->
        if t.pending = String.make 1 esc then
          output := Event.Key (make_key Key.Escape) :: !output
        else if String.length t.pending > cap_bytes then
          emit_unknown_chunks output t.pending
        else output := Event.Unknown t.pending :: !output;
        t.pending <- ""
    | Need -> ()
    | Done (n, events) ->
        let consumed = String.sub t.pending 0 n in
        t.pending <- drop_prefix t.pending n;
        output := List.rev_append events !output;
        if consumed = "\027[200~" || consumed = "\155200~" then begin
          t.paste <- true;
          drain_paste t output;
          if not t.paste then drain t output ~force
        end
        else drain t output ~force

let feed t chunk =
  let output = ref [] in
  let rec append offset =
    if offset < String.length chunk then begin
      let n = min 4096 (String.length chunk - offset) in
      t.pending <- append_string t.pending (String.sub chunk offset n);
      drain t output ~force:false;
      append (offset + n)
    end
  in
  append 0;
  List.rev !output

let flush t =
  if t.paste then []
  else
    let output = ref [] in
    drain t output ~force:true;
    List.rev !output
