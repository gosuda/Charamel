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
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:Eio_unix.Net.t ->
  Models.resolved ->
  context:string ->
  last_turn:Charamel_fantasy.Message.t list ->
  (verdict option, [ `Provider of string ]) result
(** [review t ~sw ~clock ~net model ~context ~last_turn] asks [model] to review
    [last_turn] with [context]. An unparsable answer is ignored. Two consecutive identical
    verdict fingerprints quarantine the advisor. *)

val steering_message : verdict -> Charamel_fantasy.Message.t
(** [steering_message verdict] is a user message carrying a blocker steering instruction
    for [verdict]. *)

val reset : t -> unit
(** [reset t] clears quarantine and repeated-verdict state. *)
