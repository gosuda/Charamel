(** Shared test helpers: substring search, a bounded CLI process harness, temporary
    fixture directories, and the fixed CJK regression corpus. *)

val contains : needle:string -> haystack:string -> bool
(** [contains ~needle ~haystack] is [true] when [needle] occurs in [haystack]. An empty
    [needle] is contained in every string. Both arguments are labeled in one order. *)

val run_cli :
  exe:string ->
  ?env:string array ->
  ?cwd:string ->
  ?timeout:float ->
  ?stdin:string ->
  string list ->
  int * string * string
(** [run_cli ~exe ?env ?cwd ?timeout ?stdin args] spawns [exe] with [args], writes [stdin]
    (default [""]) to the child standard input, and returns [(status, stdout, stderr)]
    after a bounded wait of [?timeout] seconds (default [10.]). [status] is the exit code,
    or [128 + signal] when the child dies on a signal. A child that exceeds the bound is
    killed. *)

val with_temp_dir : (Eio.Fs.dir_ty Eio.Path.t -> 'a) -> 'a
(** [with_temp_dir f] creates a fresh temporary directory outside the source tree, calls
    [f] with its path, removes the directory tree afterwards, and returns the result of
    [f]. Do not nest calls: subdivide one directory instead. *)

val corpus : (string * int * int) list
(** [corpus] is the fixed CJK regression corpus. Each row is
    [(text, cell width, grapheme count)] as measured by [Charamel_ansi.Width]. *)

val cjk_gen : string QCheck2.Gen.t
(** [cjk_gen] generates strings that mix the corpus classes with ASCII text. *)
