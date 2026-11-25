(** Shared test helpers: substring search, the [set /p] prompts of a [cmd] fixture, a
    bounded CLI process harness, temporary fixture directories, the Lwt test entry point,
    and the fixed CJK regression corpus.

    Every helper here returns a promise and runs on the caller's reactor. The one
    [Lwt_main.run] of a test binary is {!val:run_lwt}: a helper that entered it would nest
    a second reactor inside the suite that is already driving it, which [Lwt_main.run]
    refuses. *)

val contains : needle:string -> haystack:string -> bool
(** [contains ~needle ~haystack] is [true] when [needle] occurs in [haystack]. An empty
    [needle] is contained in every string. Both arguments are labeled in one order. *)

val set_prompts : string -> string list
(** [set_prompts script] is the prompt text of every [set /p x=] in the [cmd] [script], in
    order, each read up to the [)] that closes its group. [cmd] drops a redirection from
    that text but keeps the blank in front of it, so [x=err 1>&2] prints ["err "]: a
    Windows fixture prints exactly its prompt only when the prompt here is bare. *)

val child_environment :
  win32:bool -> getenv:(string -> string option) -> string array -> string array
(** [child_environment ~win32 ~getenv env] completes a child environment for the platform
    under test. On POSIX [env] is returned as it stands. On Windows the loader resolves
    the runtime DLLs through [PATH], and [CreateProcess] needs [SystemRoot], [SystemDrive]
    and [PATHEXT], so those four variables are taken from [getenv] and any caller entry
    for them is dropped; every other entry is kept, so HOME and XDG isolation is
    unaffected. A name [getenv] does not answer keeps the caller entry. *)

val run_cli :
  exe:string ->
  ?env:string array ->
  ?cwd:string ->
  ?timeout:float ->
  ?stdin:string ->
  string list ->
  (int * string * string) Lwt.t
(** [run_cli ~exe ?env ?cwd ?timeout ?stdin args] spawns [exe] with [args], writes [stdin]
    (default [""]) to the child standard input, and returns [(status, stdout, stderr)]
    after a bounded wait of [?timeout] seconds (default [10.]). [status] is the exit code,
    or [128 + signal] when the child dies on a signal. A child that exceeds the bound is
    killed and whatever it had already written is still reported. *)

val with_temp_dir : (string -> 'a Lwt.t) -> 'a Lwt.t
(** [with_temp_dir f] creates a fresh temporary directory outside the source tree, calls
    [f] with its path, removes the directory tree afterwards, and returns the promise [f]
    returned. Do not nest calls: subdivide one directory instead. *)

val run_lwt : string -> unit Alcotest_lwt.test list -> unit
(** [run_lwt name suites] runs the Alcotest [suites] for [name] on one Lwt reactor and
    exits the process with Alcotest's status. It is the single [Lwt_main.run] entry point
    of a test binary; a synchronous suite keeps using [Alcotest.run]. *)

val corpus : (string * int * int) list
(** [corpus] is the fixed CJK regression corpus. Each row is
    [(text, cell width, grapheme count)] as measured by [Charamel_ansi.Width]. *)

val cjk_gen : string QCheck2.Gen.t
(** [cjk_gen] generates strings that mix the corpus classes with ASCII text. *)
