(** Terminal colors.

    [t] is a color at full fidelity. [Default] is the terminal's configured foreground or
    background rather than a specific color. The conversion functions downsample the rest
    to what an xterm 256-color or 16-color terminal can show, reproducing the formulas of
    the upstream x/ansi implementation, including the HSLuv distance that breaks ties
    between the color cube and the grey ramp. *)

(** The type for terminal colors. Values whose components are built directly outside these
    ranges are tolerated: the conversion functions and the SGR emitters of {!module:Style}
    handle them as if clamped into range. *)
type t =
  | Default  (** The terminal's configured color of a slot. *)
  | Basic of int  (** An entry of the 16-color ANSI palette, 0 to 15. *)
  | Indexed of int  (** An entry of the xterm 256-color palette, 0 to 255. *)
  | Rgb of int * int * int  (** A 24-bit color, each component 0 to 255. *)

val basic : int -> t option
(** [basic n] is [Basic n] when [n] is between 0 and 15, and [None] otherwise. *)

val indexed : int -> t option
(** [indexed n] is [Indexed n] when [n] is between 0 and 255, and [None] otherwise. *)

val rgb : int -> int -> int -> t option
(** [rgb r g b] is [Rgb (r, g, b)] when every component is between 0 and 255, and [None]
    otherwise. *)

val of_hex : string -> t option
(** [of_hex s] is the color written in [s] as ["#rgb"] or ["#rrggbb"], case insensitive,
    or [None] when [s] has another shape. *)

val to_ansi256 : t -> t
(** [to_ansi256 t] is the entry of the xterm 256-color palette that shows [t] closest.
    [Indexed] colors are returned unchanged and [Default] stays [Default]. *)

val to_ansi16 : t -> t
(** [to_ansi16 t] is the entry of the 16-color ANSI palette that shows [t] closest.
    [to_ansi16] reduces [t] with [to_ansi256] first and maps the result through the
    256-to-16 table. [Basic] colors are returned unchanged and [Default] stays [Default].
*)

val to_rgb : t -> (int * int * int) option
(** [to_rgb t] is the red, green and blue components of [t], each 0 to 255, resolved
    through the xterm palette. [Basic] colors and [Indexed] 0 to 15 use the 16-color ANSI
    palette; [Indexed] 16 to 231 use the corners of the 6x6x6 color cube, whose step
    values are 0, 95, 135, 175, 215 and 255; [Indexed] 232 to 255 use the grey ramp, whose
    steps run from 8 to 238. [Default] has no components, so [to_rgb] is [None]. An index
    or a component built outside its range is resolved as if clamped into range, the
    policy of the conversion functions and of the SGR emitters of {!module:Style}. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] are the same color. *)

val pp : t Fmt.t
(** [pp] formats a color for logs and error messages. *)
