(** Typed multi-field forms.

    [v] is pure. Call [init] once with explicit environment capabilities before sending
    messages to the form. *)

type t
type msg

val nop : msg
(** [nop] is an inert message that [update] ignores. It lets callers build commands for
    [v]'s [submit_cmd] and [cancel_cmd] without fabricating state transitions. *)

module Env : module type of Env

val v :
  ?theme:Theme.t ->
  ?keymap:Keymap.t ->
  ?layout:Layout.t ->
  ?width:int ->
  ?height:int ->
  ?show_help:bool ->
  ?show_errors:bool ->
  ?view_hook:(Charamel_tea.View.t -> Charamel_tea.View.t) ->
  ?submit_cmd:msg Charamel_tea.Cmd.t ->
  ?cancel_cmd:msg Charamel_tea.Cmd.t ->
  Group.t list ->
  t
(** [v groups] creates a pure form. Width defaults to [80], height to fit. [view_hook]
    post-processes every rendered frame; [submit_cmd] and [cancel_cmd] are dispatched once
    when the form completes or aborts. *)

val init : Env.t -> t -> t * msg Charamel_tea.Cmd.t
(** [init env form] attaches explicit capabilities, evaluates dynamic fields, focuses the
    first visible field, and requests the terminal background. *)

val update : msg -> t -> t * msg Charamel_tea.Cmd.t
(** [update message form] applies one event. It raises [Invalid_argument] if [init] has
    not attached an explicit [Env.t]. *)

val view : t -> string

val cursor : t -> Charamel_tea.Cursor.t option
(** [cursor t] is the hardware cursor request of the focused field, with coordinates
    relative to {!val:view}: the row counts the rendered lines above the field and the
    column is its left offset within them. [None] when no field is active, the focused
    field has no text entry under focus, or scrolling hides the field. The caller adds the
    component origin. *)

val key : Charamel_tea.Key.t -> msg option
val paste : string -> msg
val subscriptions : t -> msg Charamel_tea.Sub.t
val state : t -> [ `Normal | `Completed of Results.t | `Aborted ]
val set_size : rows:int -> cols:int -> t -> t
val results : t -> Results.t
val errors : t -> string list
val set_dark : bool -> t -> t

val key_binds : t -> Charamel_bubbles.Key_binding.t list
(** [key_binds t] are the enabled bindings of the focused field of the selected group. *)

val focused_field : t -> Field_impl.t option
(** [focused_field t] is the field with focus in the selected group. *)

val help : t -> Charamel_bubbles.Help.t
(** [help t] is the help view model of the selected group's focused field bindings. *)

val next_group : t -> t * msg Charamel_tea.Cmd.t
(** [next_group t] moves to the next visible group, completing the form when none. *)

val previous_group : t -> t * msg Charamel_tea.Cmd.t
(** [previous_group t] moves to the previous visible group. *)

val next_field : t -> t * msg Charamel_tea.Cmd.t
(** [next_field t] moves focus to the next field of the selected group. *)

val previous_field : t -> t * msg Charamel_tea.Cmd.t
(** [previous_field t] moves focus to the previous field of the selected group. *)

val view_hook : t -> (Charamel_tea.View.t -> Charamel_tea.View.t) option
(** [view_hook t] is the frame post-processing hook given to [v]. *)

val submit_cmd : t -> msg Charamel_tea.Cmd.t option
(** [submit_cmd t] is the command dispatched on completion. *)

val cancel_cmd : t -> msg Charamel_tea.Cmd.t option
(** [cancel_cmd t] is the command dispatched on abort. *)

val run_accessible :
  Env.t -> out:(string -> unit Lwt.t) -> Accessible.reader -> t -> Results.t Lwt.t
(** [run_accessible] walks visible fields as plain prompts, committing every default and
    supplied value. *)
