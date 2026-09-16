(** OSC 8 hyperlinks.

    Links associate a URL and optional parameters with terminal text. *)

type t = { url : string; params : (string * string) list }

val osc8 : t option -> string
(** [osc8 link] is the OSC 8 sequence for [link]. [None] closes the current hyperlink.
    Parameter names and values cannot contain field separators.

    @raise Invalid_argument
      if the URL is empty, any text is malformed UTF-8 or contains a terminal control, or
      a parameter contains a separator. *)
