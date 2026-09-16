(** Structured terminal application execution.

    Runs commands, subscriptions and rendering within one resource lifetime. *)

type error = [ `Interrupted | `Killed | `Exn of exn * Printexc.raw_backtrace ]

type 'msg script_event =
  [ `Key of Key.t
  | `Text of string
  | `Resize of int * int
  | `Msg of 'msg
  | `Wait of float ]

val run_core :
  terminal:Terminal.t ->
  fps:int ->
  filter:('model -> 'msg -> 'msg option) ->
  clock:_ Eio.Time.clock ->
  now:(unit -> Mtime.t) ->
  exec:(string list -> int) ->
  suspend:(unit -> unit) ->
  signals:bool ->
  ?script:'msg script_event list ->
  ('model, 'msg) App.t ->
  ('model * string, error) result
(** [run_core ~terminal ~fps ~filter ~clock ~now ~exec ~suspend ~signals app] is the final
    model and plain frame produced by [app]. Optional [script] defaults to terminal input.
    A script uses the same event queue, commands and subscriptions. The queue holds 256
    events and blocks producers when full, bounding input bursts without dropping
    messages. [exec] and [suspend] run with the terminal released. *)

val run :
  ?terminal:Terminal.t ->
  ?fps:int ->
  ?filter:('model -> 'msg -> 'msg option) ->
  clock:_ Eio.Time.clock ->
  ('model, 'msg) App.t ->
  Eio_unix.Stdenv.base ->
  ('model, error) result
(** [run ~clock app env] is the final model or the execution error. [terminal] defaults to
    the local terminal, [fps] to 60, and [filter] to accepting messages. Positive frame
    rates are capped at 120. External cancellation propagates after terminal restoration.
*)
