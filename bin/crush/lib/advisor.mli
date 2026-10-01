(** Read-only turn advisor.

    An advisor reviews a completed turn with the configured model and emits a typed
    verdict. Repeated identical verdicts quarantine the advisor until it is reset. *)

type verdict = { severity : [ `Nit | `Concern | `Blocker ]; guidance : string }
(** The type for an advisor verdict. A blocker is actionable steering text. *)

val verdict_jsont : verdict Jsont.t
(** [verdict_jsont] decodes and encodes advisor verdict objects. *)

type t
(** The type for advisor state. *)

val create : Config.advisor -> t
(** [create config] is advisor state configured by [config]. *)

val review :
  t ->
  sw:Lwt_switch.t ->
  clock:Charamel_os.Time.clock ->
  Models.resolved ->
  context:string ->
  last_turn:Charamel_fantasy.Message.t list ->
  (verdict option, [ `Provider of string ]) result Lwt.t
(** [review t ~sw ~clock model ~context ~last_turn] asks [model] to review [last_turn]
    with [context]. An unparsable answer is ignored. Two consecutive identical verdict
    fingerprints quarantine the advisor. Turning [sw] off ends the stream early and is
    reported as a provider failure. *)

val steering_message : verdict -> Charamel_fantasy.Message.t
(** [steering_message verdict] is a user message carrying a blocker steering instruction
    for [verdict]. *)

val reset : t -> unit
(** [reset t] clears quarantine and repeated-verdict state. *)
