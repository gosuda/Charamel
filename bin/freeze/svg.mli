(** Deterministic SVG construction for code and terminal captures. *)

type rendered = { svg : string; width : float; height : float }
(** An SVG document and its pixel dimensions. *)

val render :
  fs_root:string ->
  config:Config.t ->
  language:Charamel_highlight.spec option ->
  text:string ->
  is_ansi:bool ->
  (rendered, string) result
(** [render ~fs_root ~config ~language ~text ~is_ansi] highlights source text when
    [is_ansi] is false, preserves ANSI SGR runs otherwise, and returns escaped SVG with
    configured dimensions, window decoration, line selection, margins, padding, wrapping,
    shadows, borders, and optional font embedding read from [fs_root].

    OSC 8 hyperlinks stay clickable only for [http], [https], and [mailto] targets,
    matched case-insensitively on the scheme before the first colon. Any other target, a
    relative reference, an empty target, or a target that is not well-formed UTF-8 renders
    its label as plain text. An SGR reset closes the hyperlink. *)
