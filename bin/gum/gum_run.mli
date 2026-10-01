(** Common Charamel Tea execution wrapper for interactive gum commands. *)

type outcome =
  | Submitted
  | Quit
  | Aborted  (** The terminal outcome selected by a component. *)

val run :
  ?timeout:float ->
  Charamel_cli.Env.t ->
  ('model, 'msg) Charamel_tea.app ->
  finished:('model -> outcome) ->
  'model Lwt.t
(** [run ?timeout env app ~finished] runs [app] on {!Gum_io.ui_terminal} and returns its
    final model when [finished model] is [Submitted] or [Quit]. [Aborted] and Ctrl-C exit
    with status 130. A positive [timeout] maps expiry to status 124 and the diagnostic
    ["timed out"]. A Tea exception is raised again after the terminal has been restored.
    [Gum_io.No_tty] is propagated for the command to turn into its command-specific
    diagnostic. *)

val run_tui :
  name:string ->
  ?timeout:float ->
  Charamel_cli.Env.t ->
  ('model, 'msg) Charamel_tea.app ->
  finished:('model -> outcome) ->
  'model Lwt.t
(** [run_tui ~name ?timeout env app ~finished] runs [app] through {!run} and reports
    ["<name>: requires a terminal"] when no terminal is available. It preserves the
    outcome and timeout exit behavior of {!run}. *)
