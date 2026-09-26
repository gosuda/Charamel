(** Structured terminal application execution.

    Runs commands, subscriptions and rendering within one lifetime on the caller's Lwt
    reactor. *)

type error = [ `Interrupted | `Exn of exn * Printexc.raw_backtrace ]

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
  ?paint:bool ->
  ?profile:Charamel_colorprofile.t ->
  ?script:'msg script_event list ->
  ('model, 'msg) App.t ->
  ('model * string, error) result Lwt.t
(** [run_core ~terminal ~fps ~filter ~clock ~now ~exec ~suspend ~signals app] is the final
    model and plain frame produced by [app]. Optional [script] defaults to terminal input.
    A script uses the same event queue, commands and subscriptions. The queue holds 256
    events and blocks producers when full, bounding input bursts without dropping
    messages. [exec] and [suspend] run with the terminal released; {!Cmd.perform} and
    {!Cmd.after} thunks run on the task that dispatched the command.

    Optional [paint], which defaults to [true], selects the renderer: [false] never enters
    raw mode, never paints, and discards the bytes of {!Cmd.raw}, {!Cmd.set_clipboard} and
    {!Cmd.query}, while {!Cmd.print} output still reaches [terminal]'s output. Optional
    [profile] overrides the color profile that would otherwise be detected from
    [terminal]. *)

val run :
  ?terminal:Terminal.t ->
  ?fps:int ->
  ?filter:('model -> 'msg -> 'msg option) ->
  ?renderer:[ `Terminal | `None ] ->
  ?color_profile:Charamel_colorprofile.t ->
  clock:Charamel_os.Time.clock ->
  ('model, 'msg) App.t ->
  ('model, error) result Lwt.t
(** [run ~clock app] is the final model or the execution error. [terminal] defaults to the
    local terminal, [fps] to 60, and [filter] to accepting messages. Positive frame rates
    are capped at 120. [renderer] defaults to [`Terminal]; [`None] runs the program with
    no renderer at all, as [run_core] describes. [color_profile] overrides detection.
    Cancellation propagates after terminal restoration. *)
