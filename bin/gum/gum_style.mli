(** Typed style-part flags used by gum commands.

    [t] stores already validated command-line values. Commands use {!term} to declare
    their style flags and {!to_style} at the rendering boundary. *)

type t
(** The type for one immutable group of style properties. *)

val empty : t
(** [empty] contains gum's neutral defaults: no colors, no border, left alignment, zero
    geometry, zero margin and padding, and disabled attributes. *)

val defaults :
  ?foreground:string ->
  ?background:string ->
  ?border:string ->
  ?border_foreground:string ->
  ?border_background:string ->
  ?align:string ->
  ?height:int ->
  ?width:int ->
  ?margin:string ->
  ?padding:string ->
  ?bold:bool ->
  ?faint:bool ->
  ?italic:bool ->
  ?strikethrough:bool ->
  ?underline:bool ->
  unit ->
  t
(** [defaults ?foreground ?background ?border ?border_foreground ?border_background ?align
     ?height ?width ?margin ?padding ?bold ?faint ?italic ?strikethrough ?underline ()]
    constructs a validated style group. Defaults are empty colors, border [none],
    alignment [left], zero geometry, ["0 0"] margin and padding, and [false] attributes.
    Invalid color, border, alignment, or padding values raise [Invalid_argument] because
    these values are programmer-owned defaults; command-line values are usage errors. *)

val term :
  cmd:string ->
  ?hidden:bool ->
  ?prefix:string ->
  ?env_prefix:string ->
  defaults:t ->
  unit ->
  t Cmdliner.Term.t
(** [term ~cmd ?hidden ?prefix ?env_prefix ~defaults ()] registers the fifteen style
    options for one style part. [prefix] is prepended to each option name (for example
    ["cursor."]); [env_prefix], when supplied, independently selects the
    environment-variable prefix. The built-in compatibility mapping keeps
    [selected-indicator] on [GUM_*_SELECTED_PREFIX_*] and [match-highlight] on
    [GUM_*_MATCH_HIGH_*]. [hidden] defaults to [true] and only affects Cmdliner
    documentation. *)

val to_style : t -> Charm_lipgloss.Style.t
(** [to_style t] converts all validated properties into an immutable Lipgloss style in the
    contract's order. *)

val inline : t -> Charm_lipgloss.Style.t
(** [inline t] is [to_style t] with Lipgloss inline rendering enabled. *)

val foreground : t -> Charm_ansi.Color.t option
(** [foreground t] is the configured foreground color, if any. *)
