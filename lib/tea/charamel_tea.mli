(** Typed terminal applications with structured commands and subscriptions.

    An application is an {!type:app} record over a model and a message type. {!run} drives
    it against a terminal until a command stops it. Commands describe one-off work the
    runtime performs on the application's behalf. Subscriptions describe the external
    events the application currently wants delivered as messages. Building a command or a
    subscription performs no effect. The runtime interprets both.

    The runtime owns the terminal for the duration of {!run}. It puts the local terminal
    in raw mode, reconciles terminal modes against each rendered {!View.t}, coalesces
    frames to the requested frame rate and restores the terminal before returning, whether
    the run ends by a command, by an exception or by cancellation from an enclosing
    switch.

    The first event a running application receives is {!constructor:Event.Profile},
    followed by an initial {!constructor:Event.Resize}. [ctrl+c] is delivered as an
    ordinary key press. The application decides whether it ends the run, by returning
    {!Cmd.interrupt} or {!Cmd.quit}. *)

module Key = Key
module Mouse = Mouse
module Event = Event
module Cursor = Cursor
module View = View

module Cmd : sig
  type 'msg t
  (** The type for a command producing messages of type ['msg]. A command is a value. It
      runs only when [update] or [init] returns it. *)

  val none : 'msg t
  (** [none] does nothing. *)

  val batch : 'msg t list -> 'msg t
  (** [batch cmds] runs every command in [cmds] concurrently. Each member delivers its
      messages as it completes, in no fixed order. *)

  val seq : 'msg t list -> 'msg t
  (** [seq cmds] runs every command in [cmds] in order. The next member starts only after
      the previous one has delivered. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f cmd] is [cmd] with every message it produces passed through [f]. Control
      effects of [cmd], such as quitting, interrupting, suspending, executing a process,
      printing, setting the clipboard or querying the terminal, are unchanged by [f]. *)

  val msg : 'msg -> 'msg t
  (** [msg m] delivers [m] on the next loop iteration. *)

  val perform : (unit -> 'msg) -> 'msg t
  (** [perform thunk] runs [thunk] in its own fiber once the command is dispatched and
      delivers its result. [thunk] is not called when the command is built. An exception
      raised by [thunk] ends the run. The terminal is restored and {!run} returns [`Exn]
      with that exception and its backtrace. *)

  val after : float -> (unit -> 'msg) -> 'msg t
  (** [after seconds thunk] runs [thunk] once, [seconds] after the command is dispatched,
      and delivers its result. [thunk] is not called when the command is built. An
      exception raised by [thunk] ends the run as for {!perform}. *)

  val quit : 'msg t
  (** [quit] stops the run loop. Messages already queued are applied before {!run} returns
      [Ok] with the final model. *)

  val interrupt : 'msg t
  (** [interrupt] stops the run loop. {!run} returns [Error `Interrupted]. *)

  val suspend : 'msg t
  (** [suspend] restores the terminal, suspends the process and, once the process resumes,
      puts the terminal back in raw mode and redraws the view in full. Input and rendering
      are paused while suspended. *)

  val exec : argv:string list -> (int -> 'msg) -> 'msg t
  (** [exec ~argv on_exit] releases the terminal, runs the process named by [argv]
      attached to the terminal, restores the terminal, redraws the view in full and
      delivers [on_exit code] where [code] is the process's exit code. Input and rendering
      are paused while the process runs. *)

  val print : string -> 'msg t
  (** [print s] writes [s] on its own lines above the view when the view is inline, that
      is when {!View.t}'s [alt_screen] is [false]. When the view is on the alternate
      screen [s] is queued until the run exits. Queued output is written even when the run
      ends by an exception or by cancellation. *)

  val set_clipboard : string -> 'msg t
  (** [set_clipboard s] sets the terminal's clipboard to [s] through OSC 52. *)

  val query :
    [ `Background
    | `Foreground
    | `Cursor_color
    | `Terminal_version
    | `Kitty_flags
    | `Cursor_position ] ->
    'msg t
  (** [query kind] asks the terminal for [kind]. The run never waits for the answer. If
      the terminal replies, the reply arrives as the matching {!Event.t} constructor
      through {!Sub.terminal}. A terminal that does not support the query sends nothing.
  *)

  val window_size : 'msg t
  (** [window_size] re-delivers the current terminal size through {!Sub.resize}. *)
end

module Sub : sig
  type 'msg t
  (** The type for a subscription producing messages of type ['msg]. A subscription is a
      value. After every [update] the runtime compares the subscription set
      [subscriptions] returns against the previous one and starts or stops event sources
      accordingly. Handlers are always taken from the most recently returned set. *)

  val none : 'msg t
  (** [none] expresses no interest in external events. *)

  val batch : 'msg t list -> 'msg t
  (** [batch subs] subscribes to every event source in [subs]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f sub] is [sub] with every message it produces passed through [f]. *)

  val key : (Key.t -> 'msg) -> 'msg t
  (** [key handler] delivers key presses and key repeats to [handler]. Key releases are
      not delivered here. *)

  val key_release : (Key.t -> 'msg) -> 'msg t
  (** [key_release handler] delivers key releases to [handler]. Releases arrive only from
      terminals that report them, which requires the [report_events] flag of {!View.t}'s
      [keyboard] field. *)

  val mouse : (Mouse.t -> 'msg) -> 'msg t
  (** [mouse handler] delivers mouse events to [handler]. Events arrive only while
      {!View.t}'s [mouse] field is not [Mouse_off]. *)

  val paste : (string -> 'msg) -> 'msg t
  (** [paste handler] delivers the payload of each bracketed paste to [handler]. Payloads
      arrive only while {!View.t}'s [bracketed_paste] field is [true]. *)

  val focus : ([ `Focused | `Blurred ] -> 'msg) -> 'msg t
  (** [focus handler] delivers terminal focus and blur reports to [handler]. Reports
      arrive only while {!View.t}'s [report_focus] field is [true]. *)

  val resize : (rows:int -> cols:int -> 'msg) -> 'msg t
  (** [resize handler] delivers the terminal size to [handler] on every size change, once
      at startup and on every {!Cmd.window_size}. *)

  val every : float -> (Mtime.t -> 'msg) -> 'msg t
  (** [every seconds handler] delivers the tick time to [handler] once every [seconds].
      Timers are keyed by interval. All subscriptions in the tree sharing the same
      [seconds] share one timer, so they tick together, and a timer stops when no
      subscription in the tree names its interval any more. *)

  val terminal : (Event.t -> 'msg) -> 'msg t
  (** [terminal handler] delivers terminal reports to [handler]. These are the replies to
      {!Cmd.query} and any input the runtime could not classify, delivered as
      {!constructor:Event.Unknown} with the raw bytes. *)
end

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> View.t;
  subscriptions : 'model -> 'msg Sub.t;
}
(** The type for an application over ['model] and ['msg].

    [init ()] is the starting model and its first command. [update m t] is the model after
    message [m] is applied to [t] and the command that follows from it. [view t] is the
    frame [t] renders as, together with the terminal modes it requires. [subscriptions t]
    is the set of external events [t] is currently interested in. [update] and [view] run
    on the run loop's fiber. An exception raised by either ends the run with [`Exn]. *)

module Terminal : sig
  type t
  (** The type for a terminal transport. A transport supplies the byte source the runtime
      decodes input from, the byte sink it renders to, the terminal size, the environment
      used for color profile detection and whether the transport is a terminal device.
      Constructing a transport changes no terminal state. *)

  val local : ?output:[ `Stdout | `Stderr ] -> Eio_unix.Stdenv.base -> t
  (** [local ~output env] is the transport for the calling process's terminal. It reads
      from standard input and writes to [output]. [output] defaults to [`Stdout]. The
      transport counts as a terminal device only when both standard input and [output] are
      terminals. Raw mode is entered on standard input when {!run} starts and restored
      when it ends. On a transport that is not a terminal device no raw mode is entered,
      and the size falls back to the [COLUMNS] and [LINES] environment variables, then to
      80 columns by 24 rows, when either variable is missing, not an integer or not
      positive. Color profile detection reads the process environment. *)

  val custom :
    input:_ Eio.Flow.source ->
    output:_ Eio.Flow.sink ->
    size:(unit -> int * int) ->
    on_resize:(unit -> unit) Eio.Stream.t option ->
    env:(string -> string option) ->
    is_tty:bool ->
    t
  (** [custom ~input ~output ~size ~on_resize ~env ~is_tty] is a transport over
      caller-supplied flows, such as an SSH channel. Input is decoded from [input] and
      frames are written to [output]. [size ()] is the current [(rows, columns)] and is
      consulted at startup and after each notification read from [on_resize]. [on_resize]
      is [None] when the transport has no resize notifications of its own. [env name] is
      the value of environment variable [name] as seen by the transport's terminal, and
      drives color profile detection. [is_tty] states whether the far end is a terminal
      device. No raw mode is negotiated on a custom transport. The caller owns that on its
      side of the connection. *)
end

type error = [ `Interrupted | `Killed | `Exn of exn * Printexc.raw_backtrace ]
(** The type for run failures. [`Interrupted] is the result of {!Cmd.interrupt}. [`Killed]
    is the result of the run being stopped from outside the application rather than by one
    of its commands. [`Exn (e, bt)] is the exception [e] raised by [update], [view] or a
    command, with the backtrace [bt] captured at the raise. In every case the terminal has
    already been restored when {!run} returns. *)

val run :
  ?terminal:Terminal.t ->
  ?fps:int ->
  ?filter:('model -> 'msg -> 'msg option) ->
  clock:_ Eio.Time.clock ->
  ('model, 'msg) app ->
  Eio_unix.Stdenv.base ->
  ('model, error) result
(** [run ~terminal ~fps ~filter ~clock app env] runs [app] until a command stops it and is
    the final model, or the error that ended the run. [terminal] defaults to
    [Terminal.local env]. [fps] is the maximum number of frames rendered per second and
    defaults to [60]. Values above [120] are treated as [120]. [filter] runs on every
    message before [update] with the current model. It defaults to accepting every
    message. Returning [None] drops the message. [clock] times frames, {!Cmd.after} and
    {!Sub.every}. Terminal state is restored before [run] returns, on a normal stop, on an
    error and on cancellation. Cancellation from an enclosing switch propagates as
    [Eio.Cancel.Cancelled] after the terminal is restored. It never yields [Ok].

    @raise Invalid_argument if [fps] is not positive. *)

module Test : sig
  val run :
    ('model, 'msg) app ->
    events:
      [ `Key of Key.t
      | `Text of string
      | `Resize of int * int
      | `Msg of 'msg
      | `Wait of float ]
      list ->
    size:int * int ->
    'model * string
  (** [run app ~events ~size] runs [app] against a mock terminal of [size] rows by
      columns, delivers [events] in order and is the final model paired with the content
      of the final view as plain text with every escape sequence stripped. The view is
      recorded when the model changes, so it says what the application asked to show,
      never what reached the terminal. The same run loop, commands and subscriptions as
      {!Charamel_tea.run} are used. [`Key k] delivers [k]. [`Text s] is the key presses
      that type [s]. [`Resize (rows, cols)] resizes the terminal. [`Msg m] delivers [m] to
      [update]. [`Wait seconds] lets [seconds] of simulated time pass, so timers and
      {!Cmd.after} fire, without waiting in real time. After the last event the run is
      stopped as by {!Cmd.quit}. *)
end
