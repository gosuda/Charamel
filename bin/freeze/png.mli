(** PNG conversion through the installed resvg command. *)

val convert : svg:string -> output:string -> (unit, string) result Lwt.t
(** [convert ~svg ~output] sends [svg] to [resvg] and writes [output]. It returns
    ["resvg not found on PATH"] when that executable is unavailable, and never silently
    writes SVG bytes to a PNG path. *)
