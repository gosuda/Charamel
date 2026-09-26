(** Structured terminal application execution.

    Runs commands, subscriptions and rendering within one lifetime on the caller's Lwt
    reactor. *)

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
  clock:Charamel_os.Time.clock ->
  now:(unit -> Mtime.t) ->
  exec:(string list -> int Lwt.t) ->
  suspend:(unit -> unit) ->
  signals:bool ->
  ?script:'msg script_event list ->
  ('model, 'msg) App.t ->
  ('model * string, error) result Lwt.t
(** [run_core ~terminal ~fps ~filter ~clock ~now ~exec ~suspend ~signals app] is the final
    model and plain frame produced by [app]. Optional [script] defaults to terminal input.
    A script uses the same event queue, commands and subscriptions. The queue holds 256
    events and blocks producers when full, bounding input bursts without dropping
    messages. [exec] and [suspend] run with the terminal released; {!Cmd.perform} and
    {!Cmd.after} thunks run on the task that dispatched the command. *)

val run :
  ?terminal:Terminal.t ->
  ?fps:int ->
  ?filter:('model -> 'msg -> 'msg option) ->
  clock:Charamel_os.Time.clock ->
  ('model, 'msg) App.t ->
  ('model, error) result Lwt.t
(** [run ~clock app] is the final model or the execution error. [terminal] defaults to the
    local terminal, [fps] to 60, and [filter] to accepting messages. Positive frame rates
    are capped at 120. Cancellation propagates after terminal restoration. *)
