(** PTY-backed command execution. *)

type error =
  [ `Invalid_command of string
  | `Spawn of string
  | `Exit of int * string
  | `Signaled of int * string
  | `Timeout of string ]
(** Failures returned by [execute]. Captured output is retained for exit, signal, and
    deadline failures. *)

val execute :
  env:string array ->
  ?width:int ->
  ?height:int ->
  timeout:float ->
  string ->
  (string, error) result Lwt.t
(** [execute ~env ~timeout command] is the result of running [command] under [/bin/sh]
    attached to a real PTY. [env] is the base child process environment. Existing [TERM],
    [COLUMNS], and [LINES] entries are replaced with terminal and window-size values.
    Captured output is retained for exit, signal, and deadline failures. Terminal size
    defaults to the current stdout PTY and then 80x24. *)
