type t = Os_platform.Process.t
type redir = Os_platform.Process.redir

let spawn = Os_platform.Process.spawn
let pid = Os_platform.Process.pid
let stdin_w = Os_platform.Process.stdin_w
let stdout_r = Os_platform.Process.stdout_r
let stderr_r = Os_platform.Process.stderr_r
let await = Os_platform.Process.await
let terminate = Os_platform.Process.terminate
let kill_tree = Os_platform.Process.kill_tree
let alive = Os_platform.Process.alive
