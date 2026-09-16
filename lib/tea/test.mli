(** Scripted applications for deterministic Tea tests.

    The test driver runs the production program loop over a mock terminal. *)

type 'msg script_event = 'msg Program.script_event
(** The type for scripted terminal and application events. *)

val run :
  ('model, 'msg) App.t ->
  events:'msg script_event list ->
  size:int * int ->
  'model * string
(** [run app ~events ~size] runs [app] with the scripted [events] and terminal [size]. The
    result is the final model and the last rendered frame with terminal control sequences
    removed. [Wait] advances the mock clock. Errors from the application are raised after
    terminal cleanup. *)
