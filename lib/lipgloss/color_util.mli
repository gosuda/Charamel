(** Color arithmetic: alpha channels, hue rotations, and profile-keyed selection.

    These utilities work on component values. A palette entry is resolved through
    {!val:Charamel_ansi.Color.to_rgb} first, so the result of a computation is a truecolor
    value.

    Two darkness tests exist on purpose. {!val:Charamel_ansi.Color.is_dark} is the ITU-R
    BT.601 luma rule that decides whether default text is readable on a background color.
    {!val:is_dark} here is the HSL lightness test upstream lipgloss uses for theme
    decisions, and it is the one a terminal background query feeds. *)

type rgba = { r : int; g : int; b : int; alpha : float }
(** The type for colors with an alpha channel: [r], [g] and [b] are 0 to 255 and [alpha]
    runs from [0.0] (transparent) to [1.0] (opaque). *)

val rgba : ?alpha:float -> r:int -> g:int -> b:int -> unit -> rgba
(** [rgba ?alpha ~r ~g ~b ()] is the color with those components, each clamped to 0 to
    255, and the given [alpha], clamped to 0.0 to 1.0. [alpha] defaults to [1.0]. *)

val alpha : ?scale:float -> Charamel_ansi.Color.t -> rgba
(** [alpha ?scale c] is [c] carrying an alpha channel of [scale], clamped to 0.0 to 1.0;
    [scale] defaults to [1.0]. [Charamel_ansi.Color.Default] has no components and
    resolves to black, as upstream [NoColor] does. *)

val opaque : Charamel_ansi.Color.t -> Charamel_ansi.Color.t
(** [opaque c] is [c]. A {!Charamel_ansi.Color.t} has no alpha channel, so it is always
    fully opaque; the name records the [ensureNotTransparent] rule the blending code
    depends on, which in upstream replaces an alpha of [0] with [1]. *)

val to_color : rgba -> Charamel_ansi.Color.t
(** [to_color v] drops [v.alpha] and yields [Charamel_ansi.Color.Rgb] of its components.
*)

val complementary : Charamel_ansi.Color.t -> Charamel_ansi.Color.t
(** [complementary c] is [c] with its hue rotated 180 degrees in HSV, saturation and value
    preserved, and the result clamped to the renderable range. *)

val darken : Charamel_ansi.Color.t -> float -> Charamel_ansi.Color.t
(** [darken c percent] multiplies every component of [c] by [1.0 - percent] with [percent]
    clamped to 0.0 to 1.0, truncating each product to an integer. *)

val lighten : Charamel_ansi.Color.t -> float -> Charamel_ansi.Color.t
(** [lighten c percent] adds [255.0 * percent], with [percent] clamped to 0.0 to 1.0, to
    every component of [c], truncating each sum and capping it at 255. *)

val is_dark : Charamel_ansi.Color.t -> bool
(** [is_dark c] is [true] when the HSL lightness of [c] is below [0.5].
    [Charamel_ansi.Color.Default] has no components and is dark. *)

type triple = Charamel_ansi.Color.t * Charamel_ansi.Color.t * Charamel_ansi.Color.t
(** The type for one color per output capability: [ansi], [ansi256] and [truecolor]. *)

val complete : Charamel_colorprofile.t -> triple -> Charamel_ansi.Color.t
(** [complete profile (ansi, ansi256, truecolor)] is the slot that matches [profile].
    [No_tty] and [Ascii] can show no color, so they yield [Charamel_ansi.Color.Default].
    No degradation between slots happens: the caller chose each slot. *)

val complete_adaptive :
  Charamel_colorprofile.t ->
  dark:bool ->
  light:triple ->
  night:triple ->
  Charamel_ansi.Color.t
(** [complete_adaptive profile ~dark ~light ~night] selects the [light] or [night] triple
    by [dark] and then resolves it with {!val:complete}, covering the upstream
    [CompleteAdaptiveColor] matrix in one call. *)
