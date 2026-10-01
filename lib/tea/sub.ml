type 'msg t =
  | None_
  | Batch of 'msg t list
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Key of (Key.t -> 'msg)
  | Key_release of (Key.t -> 'msg)
  | Mouse of (Mouse.t -> 'msg)
  | Paste of (string -> 'msg)
  | Focus of ([ `Focused | `Blurred ] -> 'msg)
  | Resize of (rows:int -> cols:int -> 'msg)
  | Every of float * (Mtime.t -> 'msg)
  | Terminal of (Event.t -> 'msg)
  | Stream of 'msg Lwt_stream.t
  | Resume of (unit -> 'msg)

let none = None_
let batch subs = Batch subs
let map f sub = Map (f, sub)
let key handler = Key handler
let key_release handler = Key_release handler
let mouse handler = Mouse handler
let paste handler = Paste handler
let focus handler = Focus handler
let resize handler = Resize handler
let every seconds handler = Every (seconds, handler)
let terminal handler = Terminal handler
let stream source = Stream source
let resume handler = Resume handler
