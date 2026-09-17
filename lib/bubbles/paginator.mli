(** Pagination state and page indicators. *)

type kind = Arabic | Dots
type keymap = { prev_page : Key_binding.t; next_page : Key_binding.t }

val default_keymap : keymap

type msg = Prev_page | Next_page
type t

val v :
  ?kind:kind ->
  ?per_page:int ->
  ?total_pages:int ->
  ?active_dot:string ->
  ?inactive_dot:string ->
  ?arabic_format:(int -> int -> string) ->
  ?keymap:keymap ->
  unit ->
  t

val update : msg -> t -> t * msg Charamel_tea.Cmd.t
val view : t -> string
val key : t -> Charamel_tea.Key.t -> msg option
val subscriptions : t -> msg Charamel_tea.Sub.t
val page : t -> int
val set_page : int -> t -> t
val per_page : t -> int
val set_per_page : int -> t -> t
val total_pages : t -> int
val set_total_pages : items:int -> t -> t
val items_on_page : total:int -> t -> int
val slice_bounds : length:int -> t -> int * int
val prev_page : t -> t
val next_page : t -> t
val on_first_page : t -> bool
val on_last_page : t -> bool
val set_kind : kind -> t -> t
val set_active_dot : string -> t -> t
val set_inactive_dot : string -> t -> t
