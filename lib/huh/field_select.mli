(** Single-choice field constructor. *)

type t = Field_impl.t
type 'a option_ = 'a Field_impl.Field.option_

val make :
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
(** [make ~options key] constructs a single-choice field. *)
