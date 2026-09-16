(** Boolean confirmation field constructor. *)

type t = Field_impl.t

val make :
  ?title:string Dyn.t ->
  ?description:string Dyn.t ->
  ?affirmative:string ->
  ?negative:string option ->
  ?inline:bool ->
  ?default:bool ->
  ?validate:(bool -> (unit, string) result) ->
  bool Key.t ->
  t
(** [make key] constructs a confirmation field. *)
