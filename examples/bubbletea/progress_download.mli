(** A progress bar that tracks a file download.

    Upstream: [.references/bubbletea/examples/progress-download/main.go] and
    [.references/bubbletea/examples/progress-download/tui.go]. The program fetches one URL
    and writes the body to the file named by the URL base name. The bar shows the ratio of
    the bytes received to the content length. The bar uses the upstream blend colors and a
    spring. Any key quits. The program quits by itself 0.75 seconds after the ratio
    reaches 1.0. [main] reads ["-url"] or ["--url"] from the command line and falls back
    to the upstream glow tarball URL, because this runtime has no [flag] package. Upstream
    prints the pre-flight failures to stdout and exits before the TUI starts. The port
    keeps that behavior for the fetch, the content length, and the file errors. Upstream
    posts progress from a goroutine. The port pushes the messages from an [Lwt.async] task
    into a [Sub.stream]. The smoke passes a stubbed download, so it never touches the
    network. *)

val main : unit -> unit Lwt.t
(** [main ()] runs the example against the local terminal. *)

val smoke : unit -> (string * string) list
(** [smoke ()] is the scripted [(needle, frame)] pairs of the example. *)
