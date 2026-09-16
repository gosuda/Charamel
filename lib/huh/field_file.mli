(** File chooser field constructor. *)

type t = Field_impl.t

val make :
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
(** [make key] constructs a file chooser with explicit filesystem capabilities supplied by
    [Form.Env] during initialization. *)
