(** Deterministic SVG construction for code and terminal captures. *)

type rendered = { svg : string; width : float; height : float }
(** An SVG document and its pixel dimensions. *)

val render :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  config:Config.t ->
  language:Charamel_highlight.spec option ->
  text:string ->
  is_ansi:bool ->
  (rendered, string) result
(** [render ~fs ~config ~language ~text ~is_ansi] highlights source text when [is_ansi] is
    false, preserves ANSI SGR runs otherwise, and returns escaped SVG with configured
    dimensions, window decoration, line selection, margins, padding, wrapping, shadows,
    borders, and optional font embedding. *)
