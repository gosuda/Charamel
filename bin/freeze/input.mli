(** Input loading and line-oriented transformations. *)

type source =
  | Stdin
  | File of string
  | Execute of string  (** The supported input modes. *)

type loaded = { text : string; path : string option }
(** Input bytes together with an optional source path. *)

val read :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  stdin:_ Eio.Flow.source ->
  source ->
  (loaded, string) result
(** [read ~fs ~stdin source] reads a file or stdin. [Execute] is rejected here because PTY
    capture belongs to {!Pty}. *)

val cut_lines : lines:int list -> string -> string
(** [cut_lines ~lines text] selects the inclusive zero-based range. An empty range keeps
    [text]; a negative end selects through the last line. *)

val language : override:string -> path:string option -> Charm_highlight.spec option
(** [language ~override ~path] resolves an explicit language first, then a source path
    extension. *)

val is_ansi : language:string -> string -> bool
(** [is_ansi ~language text] is true for explicit [ansi] or any text containing terminal
    escape sequences. *)

val wrap : width:int -> string -> string
(** [wrap ~width text] delegates to the ANSI-aware wrapping algorithm. *)
