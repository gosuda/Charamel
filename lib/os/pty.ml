type t = Os_platform.Pty.t
type error = Os_platform.Pty.error

let create = Os_platform.Pty.create
let slave_path = Os_platform.Pty.slave_path
let exec = Os_platform.Pty.exec
let read = Os_platform.Pty.read
let write = Os_platform.Pty.write
let size = Os_platform.Pty.size
let resize = Os_platform.Pty.resize
let terminate = Os_platform.Pty.terminate
let close = Os_platform.Pty.close
