(** Shared helpers for the example scripts and entry points.

    Each example module uses these helpers to run its application against the local
    terminal and to drive it through {!Charamel_tea.Test.run} on the simulated clock. *)

type 'msg event =
  [ `Key of Charamel_tea.Key.t
  | `Text of string
  | `Resize of int * int
  | `Msg of 'msg
  | `Wait of float ]
(** The type for one scripted event, as {!Charamel_tea.Test.run} accepts it. *)

val key : string -> [> `Key of Charamel_tea.Key.t ]
(** [key name] is the scripted press of the key that {!Charamel_tea.Key.of_string} parses
    from [name].

    @raise Invalid_argument if [name] does not parse. *)

val frame :
  ?size:int * int -> ('model, 'msg) Charamel_tea.app -> 'msg event list -> string
(** [frame ?size app events] runs [app] on the simulated clock, delivers [events] in order
    and is the final view as plain text. [size] is [(rows, cols)] and defaults to
    [(24, 80)]. *)

val expect :
  ?size:int * int ->
  ('model, 'msg) Charamel_tea.app ->
  'msg event list ->
  string list ->
  (string * string) list
(** [expect ?size app events needles] is one [(needle, frame)] pair for each element of
    [needles], where [frame] is [frame ?size app events]. A smoke passes when every
    [needle] occurs in its [frame]. To assert several states, call [expect] once per
    state, each with the script that reaches it. *)

val run :
  ?renderer:[ `Terminal | `None ] ->
  ?color_profile:Charamel_colorprofile.t ->
  ?fps:int ->
  ?filter:('model -> 'msg -> 'msg option) ->
  ('model, 'msg) Charamel_tea.app ->
  'model option Lwt.t
(** [run app] runs [app] against the local terminal on the real clock. The result is
    [Some model] with the final model after a normal stop and [None] after an interrupt.

    @raise exn the exception that ended the run, with its backtrace. *)

val run_ : ('model, 'msg) Charamel_tea.app -> unit Lwt.t
(** [run_ app] is [run app] with the final model discarded. *)
