(** ANSI Markdown rendering with typed themes.

    [render] parses CommonMark and renders it with a built-in or supplied theme. *)

module Color = Charm_ansi.Color
(** Terminal colors used by themes. *)

module Theme : sig
  type block = {
    prefix : string;
    suffix : string;
    indent : int;
    margin : int;
    color : Color.t option;
    background : Color.t option;
    bold : bool;
    italic : bool;
    underline : bool;
    faint : bool;
    strike : bool;
    block_prefix : string;
    block_suffix : string;
  }
  (** The style and layout of one Markdown element. [indent] and [margin] are
      terminal-cell counts. A zero value leaves that dimension unset. *)

  type t = {
    document : block;
    block_quote : block;
    paragraph : block;
    list : block * int;
    heading : block;
    h1 : block;
    h2 : block;
    h3 : block;
    h4 : block;
    h5 : block;
    h6 : block;
    text : block;
    strong : block;
    emph : block;
    strike : block;
    code : block;
    code_block : block * Charm_highlight.Theme.t;
    hr : block;
    link : block;
    link_text : block;
    image : block;
    image_text : block;
    table : block * string;
    task_ticked : string;
    task_unticked : string;
    html_block : block;
    html_span : block;
    definition_term : block;
    definition_description : block;
    item : string;
    enumeration : string;
  }
  (** A complete style sheet. The second component of [list] is the indentation added at
      each nested list level. The second component of [table] is the column separator. *)

  val dark : t
  (** [dark] is the default dark terminal theme. *)

  val light : t
  (** [light] is the light terminal theme. *)

  val dracula : t
  (** [dracula] is the Dracula theme. *)

  val tokyo_night : t
  (** [tokyo_night] is the Tokyo Night theme. *)

  val pink : t
  (** [pink] is the pink theme. *)

  val ascii : t
  (** [ascii] is the ASCII-oriented theme. *)

  val notty : t
  (** [notty] is the no-terminal theme. *)

  val auto : is_dark:bool -> t
  (** [auto ~is_dark] selects [dark] when [is_dark] is true and [light] otherwise. *)
end

type error = [ `Markdown of string ]
(** Errors raised while parsing Markdown. *)

val render :
  ?width:int ->
  ?theme:Theme.t ->
  ?base_url:string ->
  ?preserve_newlines:bool ->
  ?emoji:bool ->
  ?table_wrap:bool ->
  string ->
  string
(** [render ?width ?theme ?base_url ?preserve_newlines ?emoji ?table_wrap markdown] parses
    [markdown] and returns styled terminal text. [width] defaults to [80]; [width = 0]
    disables wrapping. [theme] defaults to [Theme.dark]. [base_url] defaults to the empty
    string and resolves relative links and images. [preserve_newlines], [emoji], and
    [table_wrap] default to [false], [false], and [true], respectively. *)
