(** Text input field constructor.

    The implementation is represented by the existential field wrapper. *)

type t = Field_impl.t

val make :
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
(** [make key] constructs a single-line input field. *)
