open Ctypes
open Unsigned

type handle = nativeint
type mode = int

type event =
  | Key_event of {
      down : bool;
      repeat : int;
      virtual_key : int;
      scan_code : int;
      wide_char : int;
      control_key_state : int;
    }
  | Buffer_resize of { rows : int; cols : int }
  | Focus_event of { focused : bool }
  | Other_event

let kernel32 = Dl.dlopen ~filename:"kernel32.dll" ~flags:[]
let win32 name typ = Foreign.foreign ~from:kernel32 name typ
let c_get_std_handle = win32 "GetStdHandle" (uint32_t @-> returning nativeint)

let c_get_console_mode =
  win32 "GetConsoleMode" (nativeint @-> ptr uint32_t @-> returning int)

let c_set_console_mode = win32 "SetConsoleMode" (nativeint @-> uint32_t @-> returning int)

let c_screen_buffer_info =
  win32 "GetConsoleScreenBufferInfo" (nativeint @-> ptr void @-> returning int)

let c_get_console_cp = win32 "GetConsoleCP" (void @-> returning uint32_t)
let c_get_console_output_cp = win32 "GetConsoleOutputCP" (void @-> returning uint32_t)
let c_set_console_cp = win32 "SetConsoleCP" (uint32_t @-> returning int)
let c_set_console_output_cp = win32 "SetConsoleOutputCP" (uint32_t @-> returning int)

let c_event_count =
  win32 "GetNumberOfConsoleInputEvents" (nativeint @-> ptr uint32_t @-> returning int)

let c_read_console_input =
  win32 "ReadConsoleInputW"
    (nativeint @-> ptr void @-> uint32_t @-> ptr uint32_t @-> returning int)

let c_flush_console_input = win32 "FlushConsoleInputBuffer" (nativeint @-> returning int)

let c_create_file_w =
  win32 "CreateFileW"
    (ptr uint16_t @-> uint32_t @-> uint32_t @-> ptr void @-> uint32_t @-> uint32_t
   @-> nativeint @-> returning nativeint)

let u32 value = UInt32.of_int value
let std_input_handle = u32 (-10)
let std_output_handle = u32 (-11)
let std_error_handle = u32 (-12)
let invalid_handle = Nativeint.minus_one
let no_handle = Nativeint.zero
let generic_read = u32 0x80000000
let generic_write = u32 0x40000000
let share_read_write = u32 0x3
let open_existing = u32 3
let attribute_normal = u32 0x80

let wide_of_string text =
  let length = String.length text in
  let buffer = allocate_n uint16_t ~count:(length + 1) in
  for index = 0 to length - 1 do
    buffer +@ index <-@ UInt16.of_int (Char.code text.[index])
  done;
  buffer +@ length <-@ UInt16.zero;
  buffer

let open_console name =
  let access = UInt32.logor generic_read generic_write in
  c_create_file_w (wide_of_string name) access share_read_write null open_existing
    attribute_normal no_handle

let is_missing handle =
  Nativeint.equal handle invalid_handle || Nativeint.equal handle no_handle

let std_handle which name =
  let handle = c_get_std_handle which in
  if is_missing handle then open_console name else handle

let std_input () = std_handle std_input_handle "CONIN$"
let std_output () = std_handle std_output_handle "CONOUT$"
let std_error () = std_handle std_error_handle "CONOUT$"

let get_console_mode handle =
  let word = allocate_n uint32_t ~count:1 in
  if c_get_console_mode handle word = 0 then None else Some (UInt32.to_int !@word)

let set_console_mode handle mode = c_set_console_mode handle (u32 mode) <> 0
let byte buffer index = Char.code !@(buffer +@ index)
let le16u buffer index = byte buffer index lor (byte buffer (index + 1) lsl 8)

let le16 buffer index =
  let value = le16u buffer index in
  if value land 0x8000 <> 0 then value - 0x10000 else value

let le32u buffer index =
  let value =
    byte buffer index
    lor (byte buffer (index + 1) lsl 8)
    lor (byte buffer (index + 2) lsl 16)
    lor (byte buffer (index + 3) lsl 24)
  in
  if value < 0 then value + 0x1_0000_0000 else value

let console_screen_size handle =
  (* CONSOLE_SCREEN_BUFFER_INFO packs to 22 bytes: dwSize and dwCursorPosition at 0 and 4,
     wAttributes at 8, srWindow as Left, Top, Right, Bottom at 10 through 16, then
     dwMaximumWindowSize at 18. *)
  let info = allocate_n char ~count:24 in
  if c_screen_buffer_info handle (coerce (ptr char) (ptr void) info) = 0 then None
  else
    let cols = le16 info 14 - le16 info 10 + 1 in
    let rows = le16 info 16 - le16 info 12 + 1 in
    if rows <= 0 || cols <= 0 then None else Some (rows, cols)

let get_console_cp () = UInt32.to_int (c_get_console_cp ())
let get_console_output_cp () = UInt32.to_int (c_get_console_output_cp ())
let set_console_cp cp = c_set_console_cp (u32 cp) <> 0
let set_console_output_cp cp = c_set_console_output_cp (u32 cp) <> 0
let input_record_size = 20
let key_event_record = 1
let window_buffer_size_record = 3
let focus_event_record = 16

let count_input_events handle =
  let word = allocate_n uint32_t ~count:1 in
  if c_event_count handle word = 0 then 0 else UInt32.to_int !@word

let decode buffer =
  match le16u buffer 0 with
  | code when code = key_event_record ->
      Key_event
        {
          down = le32u buffer 4 <> 0;
          repeat = le16u buffer 8;
          virtual_key = le16u buffer 10;
          scan_code = le16u buffer 12;
          wide_char = le16u buffer 14;
          control_key_state = le32u buffer 16;
        }
  | code when code = window_buffer_size_record ->
      Buffer_resize { rows = le16 buffer 6; cols = le16 buffer 4 }
  | code when code = focus_event_record -> Focus_event { focused = le32u buffer 4 <> 0 }
  | _ -> Other_event

let events_of_buffer buffer count =
  List.init count (fun index -> decode (buffer +@ (index * input_record_size)))

let read_console_input handle ~max_events =
  if max_events <= 0 then []
  else
    let buffer = allocate_n char ~count:(input_record_size * max_events) in
    let read = allocate_n uint32_t ~count:1 in
    let void = coerce (ptr char) (ptr void) buffer in
    if c_read_console_input handle void (u32 max_events) read = 0 then []
    else events_of_buffer buffer (UInt32.to_int !@read)

let flush_console_input handle = ignore (c_flush_console_input handle)
