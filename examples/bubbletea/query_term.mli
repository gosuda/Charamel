(** A text input that writes a typed escape sequence to the terminal.

    Upstream: [.references/bubbletea/examples/query-term/main.go]. Type an escape sequence
    with Go-style escapes, such as backslash-e followed by the CSI cursor report, and
    press [enter] to unescape it and write the bytes to the terminal. [ctrl+c] quits. An
    input that does not start with ESC shows
    ["Error: sequence is not an ANSI escape sequence"]; a malformed escape shows
    ["Error: invalid syntax"]. Upstream prints every runtime message it receives above the
    view; this port ignores those messages instead, because the printed form depends on Go
    type names. Upstream unquotes through [strconv.Unquote]; this port implements the same
    escape set for the bare input. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
