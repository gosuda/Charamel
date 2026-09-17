(** Standard-input/output and terminal routing for gum commands.

    Selection and transformed command results go to stdout. Interactive views use stderr,
    or a separately opened [/dev/tty] when stdin is a pipe. *)

exception No_tty
(** [No_tty] means that no terminal input can be acquired for an interactive command. *)

val stdin_is_empty : Eio_unix.Stdenv.base -> bool
(** [stdin_is_empty env] is [true] for a terminal or an empty non-FIFO stdin; a FIFO is
    considered non-empty without consuming it. *)

val read_stdin :
  ?strip_ansi:bool ->
  ?single_line:bool ->
  Eio_unix.Stdenv.base ->
  (string, [ `Empty | `Read of string ]) result
(** [read_stdin ?strip_ansi ?single_line env] reads stdin with a 64 MiB bound, trims the
    result, and optionally strips ANSI sequences. [single_line] defaults to [false]; when
    true only the first line is read. Empty text is [Error `Empty]. *)

val split : delimiter:string -> string -> string list
(** [split ~delimiter text] splits [text] on the complete delimiter. An empty delimiter
    returns [text] as one item. *)

val stdout_is_tty : Eio_unix.Stdenv.base -> bool
(** [stdout_is_tty env] reports whether stdout is a terminal. *)

val stderr_is_tty : Eio_unix.Stdenv.base -> bool
(** [stderr_is_tty env] reports whether stderr is a terminal. *)

val stdin_is_tty : Eio_unix.Stdenv.base -> bool
(** [stdin_is_tty env] reports whether stdin is a terminal. *)

val println : Eio_unix.Stdenv.base -> string -> unit
(** [println env text] writes [text] and a newline to stdout through a profile-aware color
    writer. *)

val print_raw : Eio_unix.Stdenv.base -> string -> unit
(** [print_raw env text] writes [text] and a newline to stdout without changing bytes. *)

val list_files : Eio_unix.Stdenv.base -> string list
(** [list_files env] recursively lists relative files below the current directory,
    skipping entries rooted at [.git], [node_modules], or a hidden path component. *)

val ui_terminal : ?sw:Eio.Switch.t -> Eio_unix.Stdenv.base -> Charamel_tea.Terminal.t
(** [ui_terminal ?sw env] routes interactive rendering to stderr. With piped stdin, it
    opens [/dev/tty] for keyboard input when stderr is a terminal. [sw] owns that
    descriptor and must outlive the returned terminal; callers that omit it receive
    [No_tty] for the [/dev/tty] path. It raises [No_tty] when no usable terminal exists.
*)
