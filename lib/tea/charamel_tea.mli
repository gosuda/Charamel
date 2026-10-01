(** Typed terminal applications with structured commands and subscriptions.

    An application is an {!type:app} record over a model and a message type. {!run} drives
    it against a terminal until a command stops it. Commands describe one-off work the
    runtime performs on the application's behalf. Subscriptions describe the external
    events the application currently wants delivered as messages. Building a command or a
    subscription performs no effect. The runtime interprets both.

    The runtime owns the terminal for the duration of {!run}. It puts the local terminal
    in raw mode, reconciles terminal modes against each rendered {!View.t}, coalesces
    frames to the requested frame rate and restores the terminal before returning, whether
    the run ends by a command, by an exception or by cancelling the promise it returns.

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
  (** [perform thunk] runs [thunk] in its own task once the command is dispatched and
      delivers its result. [thunk] is not called when the command is built. An exception
      raised by [thunk] ends the run. The terminal is restored and {!run} returns [`Exn]
      with that exception and its backtrace. *)

  val await : 'msg Lwt.t -> 'msg t
  (** [await promise] delivers the value [promise] resolves to. The run loop keeps
      rendering while [promise] is pending, and an error it fails with ends the run as for
      {!perform}. Cancelling the command leaves [promise] running. *)

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
      are paused while the process runs. The child stays in this process's own foreground
      process group — an editor must receive the terminal's signals — so this is the one
      place the runtime spawns without a group of its own; {!Charamel_os.Process} owns
      background process groups. *)

  val print : string -> 'msg t
  (** [print s] writes [s] on its own lines above the view when the view is inline, that
      is when {!View.t}'s [alt_screen] is [false]. When the view is on the alternate
      screen [s] is queued until the run exits. Queued output is written even when the run
      ends by an exception or by cancellation. *)

  val set_clipboard : ?selection:[ `System | `Primary ] -> string -> 'msg t
  (** [set_clipboard ~selection s] sets the terminal's clipboard ([`System], the default)
      or primary selection ([`Primary]) to [s] through OSC 52. *)

  val read_clipboard : [ `System | `Primary ] -> 'msg t
  (** [read_clipboard selection] asks the terminal for its clipboard ([`System]) or
      primary selection ([`Primary]) through an OSC 52 query. The reply arrives as
      {!constructor:Event.Clipboard} through {!Sub.terminal}; a terminal that disallows
      clipboard reads sends nothing. *)

  val raw : string -> 'msg t
  (** [raw bytes] writes [bytes] verbatim to the terminal. The renderer records none of
      the state those bytes change, so this is the hatch for control sequences the library
      does not model. *)

  val query :
    [ `Background
    | `Foreground
    | `Cursor_color
    | `Terminal_version
    | `Kitty_flags
    | `Cursor_position
    | `Capability of string ] ->
    'msg t
  (** [query kind] asks the terminal for [kind], as an OSC 10/11/12, XTVERSION,
      Kitty-flags, cursor-position (DCR) or XTGETTCAP request. The run never waits for the
      answer. If the terminal replies, the reply arrives as the matching {!Event.t}
      constructor through {!Sub.terminal}. A terminal that does not support the query
      sends nothing. *)

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

  val stream : 'msg Lwt_stream.t -> 'msg t
  (** [stream source] delivers every message [source] yields, reading each distinct stream
      with exactly one task keyed by physical equality on the stream. A stream that ends
      cleanly simply stops delivering. Messages are queued like any other input, so a fast
      producer meets back-pressure from the runtime's bounded queue. *)

  val resume : (unit -> 'msg) -> 'msg t
  (** [resume handler] delivers [handler ()] once each time the program resumes: after a
      {!val:Cmd.suspend} the shell continues, after a {!val:Cmd.exec} child exits, and
      after an external [SIGCONT]. Nothing is delivered on Windows, which has no job
      control, nor on a transport that is not the local terminal. *)
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
    on the run loop's task. An exception raised by either ends the run with [`Exn]. *)

module Terminal : sig
  type t
  (** The type for a terminal transport. A transport supplies the byte source the runtime
      decodes input from, the byte sink it renders to, the terminal size, the environment
      used for color profile detection and whether the transport is a terminal device.
      Constructing a transport changes no terminal state. *)

  val local : ?output:[ `Stdout | `Stderr ] -> unit -> t
  (** [local ~output ()] is the transport for the calling process's terminal. It reads
      from standard input — the Windows console record queue there, the byte stream
      elsewhere — and writes to [output], which defaults to [`Stdout]. The transport
      counts as a terminal device only when both standard input and [output] are
      terminals. Raw mode is entered on standard input when {!run} starts and restored
      when it ends. On a transport that is not a terminal device no raw mode is entered,
      and the size falls back to the [COLUMNS] and [LINES] environment variables, then to
      80 columns by 24 rows, when either variable is missing, not an integer or not
      positive. Color profile detection reads the process environment. *)

  val custom :
    input:Charamel_os.Console_input.console_input ->
    output:Lwt_io.output_channel ->
    size:(unit -> int * int) ->
    on_resize:(unit -> unit) Lwt_stream.t option ->
    env:(string -> string option) ->
    is_tty:bool ->
    t
  (** [custom ~input ~output ~size ~on_resize ~env ~is_tty] is a transport over
      caller-supplied channels, such as an SSH channel. Input is decoded from [input] and
      frames are written to [output]. [size ()] is the current [(rows, columns)] and is
      consulted at startup and after each notification read from [on_resize]. [on_resize]
      is [None] when the transport has no resize notifications of its own. [env name] is
      the value of environment variable [name] as seen by the transport's terminal, and
      drives color profile detection. [is_tty] states whether the far end is a terminal
      device. No raw mode is negotiated on a custom transport, and {!Cmd.exec} runs a
      child on the local process's own descriptors, so a command that execs on a remote
      transport inherits this process's terminal, not the far end's; the caller owns that
      on its side of the connection, or replaces it with {!val:custom_with_exec}. *)

  val custom_with_exec :
    input:Charamel_os.Console_input.console_input ->
    output:Lwt_io.output_channel ->
    size:(unit -> int * int) ->
    on_resize:(unit -> unit) Lwt_stream.t option ->
    env:(string -> string option) ->
    is_tty:bool ->
    exec:(string list -> int Lwt.t) ->
    t
  (** [custom_with_exec ~input ~output ~size ~on_resize ~env ~is_tty ~exec] is
      {!val:custom} whose {!Cmd.exec} runs through [exec]: the runtime releases the
      transport, awaits [exec argv], restores the transport, and delivers the exit code
      [exec] returns to the program's [on_exit] handler. *)
end

type error = [ `Interrupted | `Exn of exn * Printexc.raw_backtrace ]
(** The type for run failures. [`Interrupted] is the result of {!Cmd.interrupt}, of
    [ctrl+c], or of a [SIGINT] or [SIGTERM] the runtime was asked to honor. [`Exn (e, bt)]
    is the exception [e] raised by [update], [view] or a command, with the backtrace [bt]
    captured at the raise. In every case the terminal has already been restored when
    {!run} returns. *)

val run :
  ?terminal:Terminal.t ->
  ?fps:int ->
  ?filter:('model -> 'msg -> 'msg option) ->
  ?renderer:[ `Terminal | `None ] ->
  ?color_profile:Charamel_colorprofile.t ->
  clock:Charamel_os.Time.clock ->
  ('model, 'msg) app ->
  ('model, error) result Lwt.t
(** [run ~terminal ~fps ~filter ~clock app] runs [app] until a command stops it and is the
    final model, or the error that ended the run. [terminal] defaults to
    [Terminal.local ()]. [fps] is the maximum number of frames rendered per second and
    defaults to [60]. Values above [120] are treated as [120]. [filter] runs on every
    message before [update] with the current model. It defaults to accepting every
    message. Returning [None] drops the message. [clock] times frames, {!Cmd.after} and
    {!Sub.every}. [renderer] defaults to [`Terminal]; [`None] runs the program with no
    renderer, which never enters raw mode and never paints, so only {!Cmd.print} output
    reaches the terminal — the shape a daemon that also serves a TUI wants.
    [color_profile] overrides the profile detected from [terminal]. Terminal state is
    restored before the returned promise resolves, on a normal stop, on an error and on
    cancellation. Cancelling the promise propagates as [Lwt.Canceled] after the terminal
    is restored.

    @raise Invalid_argument if [fps] is not positive. *)

module Test : sig
  val run :
    ?output:Lwt_io.output_channel ->
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
      never what reached the terminal. [output] receives the bytes the runtime paints; it
      defaults to a sink that discards them. The same run loop, commands and subscriptions
      as {!Charamel_tea.run} are used, driven on a simulated clock without an event loop,
      so the call is synchronous. [`Key k] delivers [k]. [`Text s] is the key presses that
      type [s]. [`Resize (rows, cols)] resizes the terminal. [`Msg m] delivers [m] to
      [update]. [`Wait seconds] lets [seconds] of simulated time pass, so timers and
      {!Cmd.after} fire, without waiting in real time. After the last event the run is
      stopped as by {!Cmd.quit}. *)
end
