(** Scripted applications for deterministic Tea tests.

    The test driver runs the production program loop over a mock terminal. *)

type 'msg script_event = 'msg Program.script_event
(** The type for scripted terminal and application events. *)

val run :
  ?output:Lwt_io.output_channel ->
  ('model, 'msg) App.t ->
  events:'msg script_event list ->
  size:int * int ->
  'model * string
(** [run ~output app ~events ~size] runs [app] with the scripted [events] and terminal
    [size]. The result is the final model and the content of the final view with terminal
    control sequences removed. The view is recorded when the model changes, so it says
    what the application asked to show, never what reached the terminal. [output] receives
    what the runtime actually paints; it defaults to a sink that discards it, so a test
    that cares about the bytes can capture them. [Wait] advances the simulated clock.
    Errors from the application are raised after the terminal is given up. *)
