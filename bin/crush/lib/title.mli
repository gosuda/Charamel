(** Session title generation.

    The title generator asks the small model for a short title after the first completed
    exchange. *)

val generate :
  sw:Lwt_switch.t ->
  clock:Charamel_os.Time.clock ->
  small:Models.resolved ->
  first_prompt:string ->
  (string, [ `Provider of string ]) result Lwt.t
(** [generate ~sw ~clock ~small ~first_prompt] is a title generated from [first_prompt].
    The prompt is bounded to 2,000 characters, the model output to 40 tokens, and the
    cleaned result to 80 characters. Turning [sw] off ends the stream early and is
    reported as a provider failure. *)
