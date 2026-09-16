(** Multi-line text field constructor. *)

type t = Field_impl.t

val make :
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
(** [make key] constructs a multi-line text field. *)
