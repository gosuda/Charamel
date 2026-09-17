(** Animated progress bars with spring easing and optional colour gradients. *)

type msg = Frame
type t

val v :
  ?width:int ->
  ?colors:Charamel_ansi.Color.t list ->
  ?scaled:bool ->
  ?color_func:(total:float -> current:float -> Charamel_ansi.Color.t) ->
  ?full:string ->
  ?empty:string ->
  ?show_percentage:bool ->
  ?percent_format:(float -> string) ->
  ?percentage_style:Charamel_lipgloss.Style.t ->
  ?spring:float * float ->
  unit ->
  t

val update : msg -> t -> t * msg Charamel_tea.Cmd.t
val view : t -> string
val view_as : float -> t -> string
val key : t -> Charamel_tea.Key.t -> msg option
val subscriptions : t -> msg Charamel_tea.Sub.t
val percent : t -> float
val set_percent : float -> t -> t
val incr_percent : float -> t -> t
val decr_percent : float -> t -> t
val is_animating : t -> bool
val width : t -> int
val set_width : int -> t -> t
val set_spring_options : frequency:float -> damping:float -> t -> t
val set_show_percentage : bool -> t -> t
