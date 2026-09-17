(** Typed multi-field forms.

    [v] is pure. Call [init] once with explicit environment capabilities before sending
    messages to the form. *)

type t
type msg

module Env : module type of Env

val v :
  ?theme:Theme.t ->
  ?keymap:Keymap.t ->
  ?layout:Layout.t ->
  ?width:int ->
  ?height:int ->
  ?show_help:bool ->
  ?show_errors:bool ->
  Group.t list ->
  t
(** [v groups] creates a pure form. Width defaults to [80], height to fit. *)

val init : Env.t -> t -> t * msg Charamel_tea.Cmd.t
(** [init env form] attaches explicit capabilities, evaluates dynamic fields, focuses the
    first visible field, and requests the terminal background. *)

val update : msg -> t -> t * msg Charamel_tea.Cmd.t
(** [update message form] applies one event. It raises [Invalid_argument] if [init] has
    not attached an explicit [Env.t]. *)

val view : t -> string
val key : Charamel_tea.Key.t -> msg option
val paste : string -> msg
val subscriptions : t -> msg Charamel_tea.Sub.t
val state : t -> [ `Normal | `Completed of Results.t | `Aborted ]
val set_size : rows:int -> cols:int -> t -> t
val results : t -> Results.t
val errors : t -> string list
val set_dark : bool -> t -> t

val run_accessible : Env.t -> out:(string -> unit) -> Accessible.reader -> t -> Results.t
(** [run_accessible] walks visible fields as plain prompts, committing every default and
    supplied value. *)
