(** PNG conversion through the installed resvg command. *)

val convert :
  sw:Eio.Switch.t ->
  process_mgr:_ Eio_unix.Process.mgr ->
  svg:string ->
  output:string ->
  (unit, string) result
(** [convert ~sw ~process_mgr ~svg ~output] sends [svg] to [resvg] and writes [output]. It
    returns ["resvg not found on PATH"] when that executable is unavailable, and never
    silently writes SVG bytes to a PNG path. *)
