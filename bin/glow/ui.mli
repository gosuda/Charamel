(** Glow's terminal browser and document pager. *)

val run :
  Eio_unix.Stdenv.base ->
  config:Config.t ->
  location:Source.location ->
  (unit, string) result
(** [run env ~config ~location] runs the file browser or pager on a real terminal. The
    browser discovers Markdown files once, while the pager reflows on every resize event
    and never polls the filesystem. *)

val split_words : string -> string list
(** [split_words command] splits a pager or editor command without invoking a shell. *)

type test_event =
  [ `Key of Charamel_tea.Key.t | `Text of string | `Resize of int * int | `Wait of float ]
(** Script events accepted by {!scripted}. *)

val scripted :
  config:Config.t -> content:string -> events:test_event list -> size:int * int -> string
(** [scripted ~config ~content ~events ~size] runs the pager through the real Tea test
    runtime and returns its final frame without terminal controls. *)
