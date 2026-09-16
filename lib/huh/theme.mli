(** Named Huh palettes.

    A theme is re-evaluated for the terminal's light/dark background report. *)

type t = is_dark:bool -> Styles.t

val base : t
val charm : t
val dracula : t
val base16 : t
val catppuccin : t
