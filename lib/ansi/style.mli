(** SGR (Select Graphic Rendition) attribute sets.

    [t] is an immutable set of text attributes: the three color slots, the intensity,
    emphasis, and decoration flags, and the underline style. [to_sgr] renders the full
    sequence and [transition] renders the minimal sequence that moves the terminal from
    one attribute set to another, following the upstream ultraviolet rendering model. *)

(** The kind of underline the SGR 4 family draws. *)
type underline =
  | No_underline
  | Single  (** SGR 4. *)
  | Double  (** SGR 4:2. *)
  | Curly  (** SGR 4:3. *)
  | Dotted  (** SGR 4:4. *)
  | Dashed  (** SGR 4:5. *)

type t = {
  fg : Color.t;  (** The foreground color slot. *)
  bg : Color.t;  (** The background color slot. *)
  underline_color : Color.t;  (** The color of the underline itself. *)
  bold : bool;
  faint : bool;
  italic : bool;
  underline : underline;
  blink : bool;
  reverse : bool;
  conceal : bool;
  strike : bool;
}
(** The type for SGR attribute sets. A [Default] color slot leaves the terminal's
    configured color for that slot. *)

val default : t
(** [default] is the attribute set a terminal starts with: default colors and no
    attributes. *)

val equal : t -> t -> bool
(** [equal a b] is [true] when [a] and [b] set the same attributes. *)

val to_sgr : t -> string
(** [to_sgr t] is the SGR sequence that sets [t] starting from the terminal's initial
    state. [to_sgr default] resets all attributes. Color components built directly outside
    their range are clamped into range, and an out-of-range [Color.Basic] slot is emitted
    as its clamped value. *)

val transition : from:t -> t -> string
(** [transition ~from t] is the shortest SGR sequence that changes the terminal from
    [from] to [t]. Attributes [from] and [t] share are not re-sent, and equal styles give
    the empty string. The intensity reset SGR 22 clears bold and faint together, so the
    sequence re-asserts the one that survives. *)

val of_sgr : params:int option list list -> t -> t
(** [of_sgr ~params t] is the attribute set that [t] becomes when the SGR parameters in
    [params] are applied from left to right. Each element of [params] is one parameter,
    and each subparameter of that parameter is one option: [None] is an omitted
    subparameter, and a parameter with no subparameter is a single-element list. An empty
    parameter, [[None]], or [[Some 0]] resets the attribute set to {!default} before the
    remaining parameters apply. SGR 4 selects the underline style named by its
    subparameter and defaults to [Single]. SGR 38, 48, and 58 select the foreground,
    background, and underline colors in extended form, accepting modes 5 and 2 in the
    colon form and in the semicolon form. In the semicolon form the mode and component
    values are read from the following parameters, which are consumed. An index or
    component outside its palette range selects [Color.Default] for that slot. Parameters
    this decoder does not recognize are ignored. *)
