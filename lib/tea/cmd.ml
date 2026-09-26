type 'msg t =
  | None_
  | Batch of 'msg t list
  | Seq of 'msg t list
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Msg of 'msg
  | Perform of (unit -> 'msg)
  | Await of 'msg Lwt.t
  | After of float * (unit -> 'msg)
  | Quit
  | Interrupt
  | Suspend
  | Exec of { argv : string list; on_exit : int -> 'msg }
  | Print of string
  | Set_clipboard of string
  | Query of
      [ `Background
      | `Foreground
      | `Cursor_color
      | `Terminal_version
      | `Kitty_flags
      | `Cursor_position ]
  | Window_size

let none = None_
let batch cmds = Batch cmds
let seq cmds = Seq cmds
let map f cmd = Map (f, cmd)
let msg m = Msg m
let perform thunk = Perform thunk
let await promise = Await promise
let after seconds thunk = After (seconds, thunk)
let quit = Quit
let interrupt = Interrupt
let suspend = Suspend
let exec ~argv on_exit = Exec { argv; on_exit }
let print s = Print s
let set_clipboard s = Set_clipboard s
let query kind = Query kind
let window_size = Window_size
