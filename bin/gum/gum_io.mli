(** Standard-input/output and terminal routing for gum commands.

    Selection and transformed command results go to stdout. Interactive views use stderr,
    or a separately opened [/dev/tty] when stdin is a pipe. *)

exception No_tty
(** [No_tty] means that no terminal input can be acquired for an interactive command. *)

val stdin_is_empty : Charamel_cli.Env.t -> bool Lwt.t
(** [stdin_is_empty env] is [true] for a terminal or an empty non-FIFO stdin; a FIFO is
    considered non-empty without consuming it. *)

val read_stdin :
  ?strip_ansi:bool ->
  ?single_line:bool ->
  Charamel_cli.Env.t ->
  (string, [ `Empty | `Read of string ]) result Lwt.t
(** [read_stdin ?strip_ansi ?single_line env] reads stdin with a 64 MiB bound, trims the
    result, and optionally strips ANSI sequences. [single_line] defaults to [false]; when
    true only the first line is read. Empty text is [Error `Empty]. *)

val split : delimiter:string -> string -> string list
(** [split ~delimiter text] splits [text] on the complete delimiter. An empty delimiter
    returns [text] as one item. *)

val stdout_is_tty : Charamel_cli.Env.t -> bool
(** [stdout_is_tty env] reports whether the process's own stdout is a terminal. *)

val stderr_is_tty : Charamel_cli.Env.t -> bool
(** [stderr_is_tty env] reports whether the process's own stderr is a terminal. *)

val stdin_is_tty : Charamel_cli.Env.t -> bool
(** [stdin_is_tty env] reports whether the process's own stdin is a terminal. *)

val println : Charamel_cli.Env.t -> string -> unit Lwt.t
(** [println env text] writes [text] and a newline to stdout through a profile-aware color
    writer. *)

val print_raw : Charamel_cli.Env.t -> string -> unit Lwt.t
(** [print_raw env text] writes [text] and a newline to stdout without changing bytes. *)

val list_files : Charamel_cli.Env.t -> string list
(** [list_files env] recursively lists relative files below the current directory,
    skipping entries rooted at [.git], [node_modules], or a hidden path component. *)

val ui_terminal : Charamel_cli.Env.t -> Charamel_tea.Terminal.t
(** [ui_terminal env] routes interactive rendering to stderr. With piped stdin, it opens
    the process's controlling terminal for keyboard input when stderr is a terminal. It
    raises [No_tty] when no usable terminal exists. *)

val place_cursor :
  frame:(string -> string) -> Charamel_tea.Cursor.t option -> Charamel_tea.Cursor.t option
(** [place_cursor ~frame cursor] moves a component's cursor request to where the frame
    shows that component. [frame] is the function that turns the component's own text into
    the command's full view text; the origin is measured by wrapping a one-cell marker, so
    headers, borders, margins and padding all count. Shape, blink and color pass through
    unchanged. [None] stays [None], which hides the hardware cursor. *)
