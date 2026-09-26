(** Deferred-terminal and usage state shared by the streaming codecs.

    A provider sends the finish reason before the final usage snapshot, so the terminal
    part is held back until the stream ends and released together with the last usage.
    [terminal] answers a mid-stream failure with the held usage and an error terminal;
    [release] delivers the deferred terminal once. Both clear the held state, so a second
    call emits nothing. *)

type t = {
  mutable pending_finish : Stream_part.t option;
  mutable usage : Usage.t option;
  mutable finished : bool;
}
(** The type for deferred-terminal state: [pending_finish] is the terminal part held back,
    [usage] the snapshot not yet emitted, and [finished] whether a terminal part was
    already delivered. *)

val create : unit -> t
(** [create ()] is state with no pending terminal, no held usage, and not finished. *)

val usage_events : t -> Stream_part.t list
(** [usage_events st] is the held usage as one [Usage] part, or [[]] when none is held. *)

val terminal : t -> string -> Stream_part.t list
(** [terminal st msg] marks [st] finished, drops any pending terminal, and returns the
    held usage followed by [Finish (`Error msg)]. *)

val release : t -> Stream_part.t list
(** [release st] delivers the pending terminal once: the held usage followed by it. It is
    [[]] when no terminal is pending. *)
