(** Token styles derived from the bundled Charm and Chroma palettes.

    Palette values are grounded in the MIT-licensed Glamour Chroma tables under
    [.references/glamour/styles/] and the corresponding standard Chroma palettes. *)

type t = Spec.kind -> Charamel_ansi.Style.t
(** A token-kind to terminal-style mapping. *)

val charm : is_dark:bool -> t
(** [charm ~is_dark] is Glamour's Charm palette for a dark or light terminal. *)

val dracula : t
(** [dracula] is the Dracula Chroma palette. *)

val github : is_dark:bool -> t
(** [github ~is_dark] is the GitHub or GitHub Dark Chroma palette. *)

val monokai : t
(** [monokai] is the Monokai Chroma palette. *)

val nord : t
(** [nord] is the Nord Chroma palette. *)

val solarized : is_dark:bool -> t
(** [solarized ~is_dark] is Solarized Dark or Solarized Light. *)
