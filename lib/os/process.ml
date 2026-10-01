type t = Os_platform.Process.t
type redir = Os_platform.Process.redir

let spawn = Os_platform.Process.spawn
let pid = Os_platform.Process.pid
let stdin_w = Os_platform.Process.stdin_w
let stdout_r = Os_platform.Process.stdout_r
let stderr_r = Os_platform.Process.stderr_r
let await = Os_platform.Process.await
let signal = Os_platform.Process.signal
let terminate = Os_platform.Process.terminate
let kill_tree = Os_platform.Process.kill_tree
let alive = Os_platform.Process.alive
let grace = 2.

let terminate_tree t =
  if Sys.win32 then terminate t
  else
    try Unix.kill (0 - pid t) Sys.sigterm
    with Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), _, _) -> ()

let group_alive t =
  if Sys.win32 then alive t
  else
    match Unix.kill (0 - pid t) 0 with
    | () -> true
    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> false
    | exception Unix.Unix_error (Unix.EPERM, _, _) -> true

(* The group is the authority, not the leader: a grandchild that outlived it still holds
   the pipes a caller is waiting to see closed. When the group is gone nothing is
   signalled, so a recycled process id is never reached. [Lwt.protected] keeps the race
   from cancelling the one shared exit promise. *)
let force_remainder t =
  if group_alive t then kill_tree t;
  Lwt.return_unit

let stop ?(grace = grace) t =
  terminate_tree t;
  Lwt.bind
    (Lwt.choose [ Lwt.map (fun _ -> ()) (Lwt.protected (await t)); Lwt_unix.sleep grace ])
    (fun () -> force_remainder t)

let stop_after_grace ?(grace = grace) t =
  terminate_tree t;
  Lwt.bind (Lwt_unix.sleep grace) (fun () -> force_remainder t)
