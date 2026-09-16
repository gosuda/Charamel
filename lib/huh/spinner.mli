(** Running a cancellable action with a terminal spinner.

    Accessible mode prints the title and runs the action without taking terminal
    ownership. Interactive mode runs the action in a child fiber while a [Charm_tea]
    application renders frames. Ctrl-C is a user interruption; cancellation from the
    enclosing switch remains cancellation. *)

type 'e error = [ `Interrupted | `Failed of 'e ]
(** The result of {!run}: user interruption or the action's own error. *)

val run :
  ?title:string ->
  ?style:Charm_bubbles.Spinner.kind ->
  ?accessible:bool ->
  ?theme:(is_dark:bool -> Charm_lipgloss.Style.t * Charm_lipgloss.Style.t) ->
  clock:_ Eio.Time.clock ->
  (unit -> ('a, 'e) result) ->
  Eio_unix.Stdenv.base ->
  ('a, 'e error) result
(** [run ?title ?style ?accessible ?theme ~clock action base] runs [action] and returns
    its value, wrapping an action error as [`Failed]. Defaults are title [Loading...], the
    [Dot] spinner and the standard Huh spinner/title palette. Accessible mode is forced
    for a non-tty input or TERM=dumb, and otherwise can be requested explicitly. *)
