open Lwt.Infix

type input_record = Os_platform.input_record =
  | Key_event of key_event
  | Buffer_size of { rows : int; cols : int }
  | Focus of bool
  | Ignored

and key_event = Os_platform.key_event = {
  down : bool;
  repeat : int;
  virtual_key : int;
  wide_char : int;
  control_key_state : int;
}

let shift_pressed = 0x0010
let left_ctrl_pressed = 0x0008
let right_ctrl_pressed = 0x0004
let left_alt_pressed = 0x0002
let right_alt_pressed = 0x0001
let high_surrogate_lo = 0xd800
let high_surrogate_hi = 0xdbff
let low_surrogate_lo = 0xdc00
let low_surrogate_hi = 0xdfff
let repeat_limit = 256

let modifier_state state =
  let shift = state land shift_pressed <> 0 in
  let alt = state land (left_alt_pressed lor right_alt_pressed) <> 0 in
  let ctrl = state land (left_ctrl_pressed lor right_ctrl_pressed) <> 0 in
  1 + (if shift then 1 else 0) + (if alt then 2 else 0) + if ctrl then 4 else 0

let vk_back = 0x08
let vk_tab = 0x09
let vk_return = 0x0d
let vk_escape = 0x1b
let vk_space = 0x20
let vk_prior = 0x21
let vk_next = 0x22
let vk_end = 0x23
let vk_home = 0x24
let vk_left = 0x25
let vk_up = 0x26
let vk_right = 0x27
let vk_down = 0x28
let vk_insert = 0x2d
let vk_delete = 0x2e
let vk_f1 = 0x70

let sequences =
  [
    (vk_back, `character '\x7f');
    (vk_tab, `character '\t');
    (vk_return, `character '\r');
    (vk_escape, `character '\x1b');
    (vk_space, `character ' ');
    (vk_prior, `number 5);
    (vk_next, `number 6);
    (vk_end, `letter 'F');
    (vk_home, `letter 'H');
    (vk_left, `letter 'D');
    (vk_up, `letter 'A');
    (vk_right, `letter 'C');
    (vk_down, `letter 'B');
    (vk_insert, `number 2);
    (vk_delete, `number 3);
    (vk_f1, `function_key 'P');
    (vk_f1 + 1, `function_key 'Q');
    (vk_f1 + 2, `function_key 'R');
    (vk_f1 + 3, `function_key 'S');
    (vk_f1 + 4, `number 15);
    (vk_f1 + 5, `number 17);
    (vk_f1 + 6, `number 18);
    (vk_f1 + 7, `number 19);
    (vk_f1 + 8, `number 20);
    (vk_f1 + 9, `number 21);
    (vk_f1 + 10, `number 23);
    (vk_f1 + 11, `number 24);
  ]

let decimal = string_of_int

let sequence_text form modifiers =
  match form with
  | `character character -> String.make 1 character
  | `letter letter ->
      if modifiers = 1 then "\x1b[" ^ String.make 1 letter
      else "\x1b[1;" ^ decimal modifiers ^ String.make 1 letter
  | `function_key letter ->
      if modifiers = 1 then "\x1bO" ^ String.make 1 letter
      else "\x1b[1;" ^ decimal modifiers ^ String.make 1 letter
  | `number number ->
      if modifiers = 1 then "\x1b[" ^ decimal number ^ "~"
      else "\x1b[" ^ decimal number ^ ";" ^ decimal modifiers ^ "~"

let utf8 scalar =
  let bytes = Bytes.create 4 in
  let length =
    if scalar < 0x80 then (
      Bytes.set_uint8 bytes 0 scalar;
      1)
    else if scalar < 0x800 then (
      Bytes.set_uint8 bytes 0 (0xc0 lor (scalar lsr 6));
      Bytes.set_uint8 bytes 1 (0x80 lor (scalar land 0x3f));
      2)
    else if scalar < 0x10000 then (
      Bytes.set_uint8 bytes 0 (0xe0 lor (scalar lsr 12));
      Bytes.set_uint8 bytes 1 (0x80 lor ((scalar lsr 6) land 0x3f));
      Bytes.set_uint8 bytes 2 (0x80 lor (scalar land 0x3f));
      3)
    else (
      Bytes.set_uint8 bytes 0 (0xf0 lor (scalar lsr 18));
      Bytes.set_uint8 bytes 1 (0x80 lor ((scalar lsr 12) land 0x3f));
      Bytes.set_uint8 bytes 2 (0x80 lor ((scalar lsr 6) land 0x3f));
      Bytes.set_uint8 bytes 3 (0x80 lor (scalar land 0x3f));
      4)
  in
  Bytes.sub_string bytes 0 length

let repeat times =
  if times < 1 then 1 else if times > repeat_limit then repeat_limit else times

let emit buffer times text =
  for _ = 1 to times do
    Buffer.add_string buffer text
  done

let combine high low =
  ((high - high_surrogate_lo) lsl 10) + (low - low_surrogate_lo) + 0x10000

let is_high unit = unit >= high_surrogate_lo && unit <= high_surrogate_hi
let is_low unit = unit >= low_surrogate_lo && unit <= low_surrogate_hi

(* A key-down carrying a character contributes that character, joining the high surrogate
   left over by the previous record; [pending] is that leftover, and a pair reports the
   repeat count of the record that completes it. A key-down with no character contributes the
   VT bytes of its [VK_*] code, when this module knows that code, and a key-up contributes
   nothing. *)
let encode_key buffer pending (event : key_event) =
  if not event.down then pending
  else
    let unit = event.wide_char in
    if unit <> 0 && is_high unit then Some unit
    else if unit <> 0 && is_low unit then (
      (match pending with
      | Some high -> emit buffer (repeat event.repeat) (utf8 (combine high unit))
      | None -> ());
      None)
    else if unit <> 0 then (
      emit buffer (repeat event.repeat) (utf8 unit);
      None)
    else
      match List.assoc_opt event.virtual_key sequences with
      | Some form ->
          let modifiers = modifier_state event.control_key_state in
          emit buffer (repeat event.repeat) (sequence_text form modifiers);
          None
      | None -> pending

let encode_record buffer pending = function
  | Key_event event -> encode_key buffer pending event
  | Buffer_size { rows; cols } ->
      Buffer.add_string buffer ("\x1b[8;" ^ decimal rows ^ ";" ^ decimal cols ^ "t");
      pending
  | Focus focused ->
      Buffer.add_string buffer (if focused then "\x1b[I" else "\x1b[O");
      pending
  | Ignored -> pending

let encode records =
  let buffer = Buffer.create 128 in
  let pending = List.fold_left (encode_record buffer) None records in
  ignore pending;
  Bytes.of_string (Buffer.contents buffer)

let records = Os_platform.Console.read_records

type console_input =
  | Channel of Lwt_io.input_channel
  | Records
  | Reader of (unit -> string option Lwt.t)
  | Queue of string list ref
  | Blocked

let read_chunk = 64
let of_channel channel = Channel channel
let of_console_records () = Records
let of_reader reader = Reader reader
let of_queue chunks = Queue (ref chunks)
let blocked () = Blocked

(* [Lwt_io.read ~count] answers as soon as anything has arrived, which is what a keypress
   needs, and reports a closed channel as the empty string — the same convention this module
   gives every source. *)
let read source =
  match source with
  | Channel channel -> Lwt_io.read ~count:read_chunk channel
  | Records -> records () >|= fun events -> Bytes.to_string (encode events)
  | Reader reader -> ( reader () >|= function Some text -> text | None -> "")
  | Queue chunks -> (
      match !chunks with
      | first :: rest ->
          chunks := rest;
          Lwt.return first
      | [] -> Lwt.return "")
  | Blocked -> fst (Lwt.wait ())
