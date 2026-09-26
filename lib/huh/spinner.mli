(** Running a cancellable action with a terminal spinner.

    Accessible mode prints the title and runs the action without taking terminal
    ownership. Interactive mode runs the action in a child fiber while a [Charamel_tea]
    application renders frames. Ctrl-C is a user interruption; cancellation from the
    enclosing switch remains cancellation. *)

type 'e error = [ `Interrupted | `Failed of 'e ]
(** The result of {!run}: user interruption or the action's own error. *)

val run :
  ?title:string ->
  ?style:Charamel_bubbles.Spinner.kind ->
  ?accessible:bool ->
  ?theme:(is_dark:bool -> Charamel_lipgloss.Style.t * Charamel_lipgloss.Style.t) ->
  clock:Charamel_os.Time.clock ->
  (unit -> ('a, 'e) result) ->
  ('a, 'e error) result Lwt.t
(** [run ?title ?style ?accessible ?theme ~clock action] runs [action] and returns its
    value, wrapping an action error as [`Failed]. Defaults are title [Loading...], the
    [Dot] spinner and the standard Huh spinner/title palette. Accessible mode is forced
    for a non-tty input or TERM=dumb, and otherwise can be requested explicitly. In
    interactive mode the action runs on a system thread while the application renders
    frames; an exception raised by the action propagates out of the returned promise. *)
