(** Gradients interpolated in CIE L*a*b*.

    Blending happens on the perceptual lightness and chroma axes, so a red-to-blue ramp
    passes through purple without the dark mid-point an sRGB interpolation produces.
    Components are quantized through the 16-bit color representation upstream uses, which
    fixes the byte value of every stop and every intermediate. *)

val blend1d : steps:int -> Charamel_ansi.Color.t list -> Charamel_ansi.Color.t list
(** [blend1d ~steps stops] is [steps] colors distributed over the gradient defined by
    [stops]. Segments between consecutive stops receive an equal number of steps, with the
    leftover steps given to the earlier segments, so a stop is always hit exactly. [steps]
    below [0] is treated as [0]; [steps] at most the number of stops returns that many
    leading stops unchanged; [Charamel_ansi.Color.Default] stops are dropped after that
    shortcut, so an all-[Default] or empty [stops] yields [[]] unless a single stop
    survives, which yields [steps] copies of it. *)

val blend2d :
  width:int ->
  height:int ->
  angle:float ->
  Charamel_ansi.Color.t list ->
  Charamel_ansi.Color.t list
(** [blend2d ~width ~height ~angle stops] is a row-major gradient of [width * height]
    colors: {!val:blend1d} over the larger dimension, sampled along a line through the
    grid center rotated by [angle] degrees, where [0] runs left to right. Each sample is
    clamped to the gradient's extent. [width] or [height] below [1] is treated as [1] and
    [angle] is reduced modulo [360]. *)
