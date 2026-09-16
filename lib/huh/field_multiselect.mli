(** Multiple-choice field constructor. *)

type t = Field_impl.t
type 'a option_ = 'a Field_impl.Field.option_

val make :
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
(** [make ~options key] constructs a multiple-choice field. *)
