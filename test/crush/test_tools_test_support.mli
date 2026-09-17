(** Shared filesystem and tool execution helpers for Crush tests. *)

val contains : string -> string -> bool
(** [contains needle haystack] is [true] if [haystack] contains [needle]. *)

val temp_root : string -> string
(** [temp_root prefix] is a unique temporary path using [prefix]. *)

val make_context :
  ?allowed_tools:string list ->
  Eio_unix.Stdenv.base ->
  Eio.Switch.t ->
  string ->
  Crush_core.Tool.ctx
(** [make_context ?allowed_tools env sw root] is a tool context rooted at [root].
    [allowed_tools] defaults to ["read"; "write"; "edit"]. *)

val with_context :
  temp_prefix:string ->
  ?allowed_tools:string list ->
  (Eio_unix.Stdenv.base -> string -> Crush_core.Tool.ctx -> 'a) ->
  'a
(** [with_context ~temp_prefix ?allowed_tools f] runs [f] with a fresh tool context.
    [allowed_tools] defaults to ["read"; "write"; "edit"]. *)

val write_file : Eio_unix.Stdenv.base -> string -> string -> unit
(** [write_file env path content] writes [content] to [path]. *)

val load_file : Eio_unix.Stdenv.base -> string -> string
(** [load_file env path] is the content of [path]. *)

val json_object : (string * Jsont.json) list -> Jsont.json
(** [json_object fields] is a JSON object containing [fields]. *)

val run_tool :
  Crush_core.Tool.t -> Crush_core.Tool.ctx -> Jsont.json -> Crush_core.Tool.output
(** [run_tool tool context value] is the output from running [tool] on [value]. *)
