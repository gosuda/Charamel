(** Shared command-line flag converters.

    [Gum_flag] keeps command-specific environment names and typed parsing in one place.
    Every parser is suitable for direct use in a Cmdliner term. *)

val env : cmd:string -> string -> Cmdliner.Cmd.Env.info
(** [env ~cmd name] describes the environment variable [GUM_<CMD>_<NAME>], with dots and
    dashes converted to underscores. *)

val key : cmd:string -> string -> Charamel_tea.Key.t
(** [key ~cmd name] parses a key name and raises [Invalid_argument] with a diagnostic
    naming [cmd] when [name] is invalid. *)

val is_key : Charamel_tea.Key.t -> Charamel_tea.Key.t -> bool
(** [is_key actual expected] is [true] when [actual] matches [expected]. *)

val any_key : Charamel_tea.Key.t -> Charamel_tea.Key.t list -> bool
(** [any_key actual expected] is [true] when [actual] matches one of [expected]. *)

val key_name : Charamel_tea.Key.t -> string
(** [key_name key] is the name of [key], spelled as a [GUM_<CMD>_*] key value spells it.
*)

val is_quit : Charamel_tea.Key.t -> bool
(** [is_quit key] is [true] for [q] or [esc], the keys every interactive command leaves
    with an empty result. *)

val is_abort : Charamel_tea.Key.t -> bool
(** [is_abort key] is [true] for [ctrl+c], which ends the command as an interrupt. *)

val is_submit : Charamel_tea.Key.t -> bool
(** [is_submit key] is [true] for [enter] or [ctrl+q], the keys that accept the current
    selection. *)

val string_arg :
  cmd:string -> string -> default:string -> doc:string -> string Cmdliner.Term.t
(** [string_arg ~cmd name ~default ~doc] parses a string option with command-scoped
    environment fallback [GUM_<CMD>_<NAME>]. *)

val int_arg : cmd:string -> string -> default:int -> doc:string -> int Cmdliner.Term.t
(** [int_arg ~cmd name ~default ~doc] parses an integer option with command-scoped
    environment fallback [GUM_<CMD>_<NAME>]. *)

val negatable :
  cmd:string ->
  ?env:bool ->
  ?env_name:string ->
  ?short:char ->
  default:bool ->
  doc:string ->
  string ->
  bool Cmdliner.Term.t
(** [negatable ~cmd ?env ?env_name ?short ~default ~doc name] accepts both [--name] and
    [--no-name], plus [short] as a positive short alias. The last occurrence wins. When
    [env] is true (the default), the environment variable [GUM_<CMD>_<NAME>] (or the
    custom [env_name]) supplies a boolean only when no flag occurs. *)

val flag :
  cmd:string ->
  ?env:bool ->
  ?short:char ->
  ?default:bool ->
  doc:string ->
  string ->
  bool Cmdliner.Term.t
(** [flag ~cmd ?env ?short ?default ~doc name] parses a positive boolean flag. [default]
    defaults to [false]. The environment fallback is used when [env] is true (the
    default). *)

val seconds : cmd:string -> doc:string -> string -> float option Cmdliner.Term.t
(** [seconds ~cmd ~doc name] parses a duration in seconds. Accepted values have [s], [m],
    or [ms] suffixes, or are bare seconds; zero and omission produce [None]. The
    environment variable is [GUM_<CMD>_<NAME>]. *)

val delimiter :
  cmd:string -> default:string -> doc:string -> string -> string Cmdliner.Term.t
(** [delimiter ~cmd ~default ~doc name] parses a delimiter and decodes the escapes [\\n],
    [\\t], and [\\0]. *)

val parse_padding : string -> (Charamel_lipgloss.Sides.t, [ `Msg of string ]) result
(** [parse_padding text] parses one, two, three, or four integer side values. One value
    applies to every side; two are vertical and horizontal; three are top, horizontal, and
    bottom; four are top, right, bottom, and left. Spaces and commas separate values.
    Malformed input is a typed [Error], never a silent zero padding. *)

val parsed_padding : string -> Charamel_lipgloss.Sides.t
(** [parsed_padding text] parses padding and raises [Invalid_argument] with the parser
    diagnostic when [text] is malformed. *)

val validated_padding_term :
  ?doc:string ->
  ?pp:(Stdlib.Format.formatter -> string -> unit) ->
  cmd:string ->
  unit ->
  string Cmdliner.Term.t
(** [validated_padding_term ?doc ?pp ~cmd ()] is a command-line padding term that
    preserves the raw value after validating one to four integer values. [doc] defaults to
    ["Padding as one to four integers."]. [pp] defaults to printing the raw value. *)

val align : string -> Charamel_lipgloss.Position.t option
(** [align text] maps [left] and [top] to {!Charamel_lipgloss.Position.left}, [center] and
    [middle] to [center], and [right] and [bottom] to [right]. *)

val border : string -> Charamel_lipgloss.Border.t option
(** [border text] parses a named Lipgloss border. Unknown names return [None]. *)

val color : string -> (Charamel_ansi.Color.t option, [ `Msg of string ]) result
(** [color text] parses an empty color as [Ok None], a decimal palette index, or a
    [#rgb]/[#rrggbb] value. Invalid input returns a usage message. *)

val enum : docv:string -> (string * 'a) list -> 'a Cmdliner.Arg.conv
(** [enum ~docv choices] is a Cmdliner converter accepting exactly one key in [choices],
    with a diagnostic that names the accepted values. *)
