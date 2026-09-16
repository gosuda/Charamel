(** Session title generation.

    The title generator asks the small model for a short title after the first completed
    exchange. *)

val generate :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  net:Eio_unix.Net.t ->
  small:Models.resolved ->
  first_prompt:string ->
  (string, [ `Provider of string ]) result
(** [generate ~sw ~clock ~net ~small ~first_prompt] is a title generated from
    [first_prompt]. The prompt is bounded to 2,000 characters, the model output to 40
    tokens, and the cleaned result to 80 characters. *)
