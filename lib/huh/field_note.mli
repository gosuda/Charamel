(** Informational note field constructor. *)

type t = Field_impl.t

val make :
  ?title:string Dyn.t ->
  ?description:string Dyn.t ->
  ?height:int ->
  ?next:string option ->
  unit ->
  t
(** [make ()] constructs a note, which has no result key. *)
