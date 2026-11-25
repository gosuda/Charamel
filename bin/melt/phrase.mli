(** Read a recovery phrase from standard input.

    A pasted phrase may span several lines, so the terminal branch reads to end-of-file
    and joins the lines; {!Melt_core.Mnemonic} splits on any ASCII whitespace. Echo is
    turned off for the whole read, and a terminal whose echo cannot be turned off is
    refused rather than read visibly. *)

type error = [ `Cannot_hide_input ]
(** [error] is raised when console echo cannot be disabled. *)

val read_stdin_phrase :
  ?is_tty:bool ->
  ?echo_off:(unit -> unit -> unit) ->
  ?input:in_channel ->
  unit ->
  (string, error) result Lwt.t
(** [read_stdin_phrase ()] reads the recovery phrase.

    When [is_tty] (default: {!Charamel_os.Tty.is_tty_stdin}) is false, the whole of
    [input] (default: [stdin]) is returned, as from a pipe or a file. When true, echo is
    disabled through [echo_off] (default: {!Charamel_os.Tty.echo_off}) for the duration of
    the read, every line of [input] is read to end-of-file, and the lines are joined with
    ["\n"]. When [echo_off] raises [Unix.Unix_error], the result is
    [Error `Cannot_hide_input] and [input] is never read. *)
