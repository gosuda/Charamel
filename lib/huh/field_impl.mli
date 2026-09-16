(** Existential field implementations and the public typed field constructors.

    The existential wrapper keeps each field's message and value type private while
    preserving type-safe command and result dispatch through [Type.Id]. *)

type position = { is_first : bool; is_last : bool }

type ctx = {
  styles : Styles.t;
  keymap : Keymap.t;
  width : int;
  height : int;
  position : position;
  results : Results.t;
  env : Env.t;
}

type outcome = Stay | Next | Prev | Submit
type t

val init : t -> ctx -> t * Field_msg.t Charm_tea.Cmd.t
(** [init field ctx] initializes effectful child components and returns their command. *)

val reevaluate : t -> ctx -> t
(** [reevaluate field ctx] recomputes dynamic properties when results changed. *)

val step_key : t -> ctx -> Charm_tea.Key.t -> t * Field_msg.t Charm_tea.Cmd.t * outcome
(** [step_key field ctx key] handles one key and reports navigation separately. *)

val step_msg : t -> ctx -> Field_msg.t -> t * Field_msg.t Charm_tea.Cmd.t
(** [step_msg field ctx message] dispatches a message only when its typed id matches. *)

val step_paste : t -> ctx -> string -> t
(** [step_paste field ctx text] inserts paste text for editable fields. *)

val subscriptions : t -> ctx -> Field_msg.t Charm_tea.Sub.t
(** [subscriptions field ctx] maps component subscriptions into existential messages. *)

val view : t -> ctx -> focused:bool -> string
(** [view field ctx ~focused] renders the field. *)

val focus : t -> ctx -> t * Field_msg.t Charm_tea.Cmd.t
(** [focus field ctx] focuses the child editor, when the field has one. *)

val blur : t -> ctx -> t
(** [blur field ctx] removes focus and stores validation errors. *)

val error : t -> string option
(** [error field] is its current validation error. *)

val skip : t -> ctx -> bool
(** [skip field ctx] reports whether the field is excluded from navigation. *)

val zoom : t -> bool
(** [zoom field] is [true] when the field owns the group's full viewport. *)

val key_name : t -> string option
(** [key_name field] is its typed result key, or [None] for a note. *)

val key_binds : t -> ctx -> Charm_bubbles.Key_binding.t list
(** [key_binds field ctx] returns enabled bindings for the current position. *)

val run_accessible : t -> ctx -> out:(string -> unit) -> Accessible.reader -> t
(** [run_accessible field ctx ~out reader] performs this field's line-oriented prompt. *)

val commit : t -> Results.t -> Results.t
(** [commit field results] adds the field's current value when it has a key. *)

type 'a option_ = { key : string; value : 'a }
(** One selection option presented by select and multi-select fields. *)

module Field : sig
  type nonrec t = t
  type nonrec 'a option_ = 'a option_

  val option_ : key:string -> 'a -> 'a option_
  val options_of_strings : string list -> string option_ list

  val input :
    ?title:string Dyn.t ->
    ?description:string Dyn.t ->
    ?placeholder:string ->
    ?prompt:string ->
    ?char_limit:int ->
    ?suggestions:string list Dyn.t ->
    ?echo:[ `Normal | `Password | `None ] ->
    ?inline:bool ->
    ?default:string ->
    ?validate:(string -> (unit, string) result) ->
    string Key.t ->
    t

  val text :
    ?title:string Dyn.t ->
    ?description:string Dyn.t ->
    ?placeholder:string ->
    ?lines:int ->
    ?char_limit:int ->
    ?show_line_numbers:bool ->
    ?editor:bool ->
    ?editor_extension:string ->
    ?default:string ->
    ?validate:(string -> (unit, string) result) ->
    string Key.t ->
    t

  val select :
    ?title:string Dyn.t ->
    ?description:string Dyn.t ->
    ?height:int ->
    ?inline:bool ->
    ?filterable:bool ->
    ?default:string ->
    ?validate:('a -> (unit, string) result) ->
    options:'a option_ list Dyn.t ->
    'a Key.t ->
    t

  val multi_select :
    ?title:string Dyn.t ->
    ?description:string Dyn.t ->
    ?height:int ->
    ?limit:int ->
    ?filterable:bool ->
    ?default:string list ->
    ?validate:('a list -> (unit, string) result) ->
    options:'a option_ list Dyn.t ->
    'a list Key.t ->
    t

  val confirm :
    ?title:string Dyn.t ->
    ?description:string Dyn.t ->
    ?affirmative:string ->
    ?negative:string option ->
    ?inline:bool ->
    ?default:bool ->
    ?validate:(bool -> (unit, string) result) ->
    bool Key.t ->
    t

  val note :
    ?title:string Dyn.t ->
    ?description:string Dyn.t ->
    ?height:int ->
    ?next:string option ->
    unit ->
    t

  val file :
    ?title:string Dyn.t ->
    ?description:string Dyn.t ->
    ?dir:string ->
    ?show_hidden:bool ->
    ?show_size:bool ->
    ?show_permissions:bool ->
    ?allowed:string list ->
    ?files:bool ->
    ?dirs:bool ->
    ?height:int ->
    ?validate:(string -> (unit, string) result) ->
    string Key.t ->
    t

  val key_name : t -> string option
end
