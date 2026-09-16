(** Line-oriented accessible form input. *)

type reader = { read_line : unit -> string option; read_password : unit -> string option }

val reader_of_flow :
  stdin:_ Eio.Flow.source -> echo_off:(unit -> unit -> unit) option -> reader
(** [reader_of_flow ~stdin ~echo_off] creates one buffered reader. [echo_off], when
    present, supplies a restoration function around password reads. *)

val prompt_string :
  out:(string -> unit) ->
  reader ->
  prompt:string ->
  default:string ->
  validate:(string -> (unit, string) result) ->
  string
(** [prompt_string] repeats the prompt until validation succeeds, using [default] for
    blank input or end of file. *)

val prompt_int :
  (string -> unit) ->
  reader ->
  prompt:string ->
  low:int ->
  high:int ->
  default:int option ->
  int
(** [prompt_int] reads an integer in the inclusive range, repeating on malformed input. *)

val prompt_bool : (string -> unit) -> reader -> prompt:string -> default:bool -> bool
(** [prompt_bool] reads yes/no, using [default] for a blank line. *)

val prompt_password :
  (string -> unit) ->
  reader ->
  prompt:string ->
  validate:(string -> (unit, string) result) ->
  string
(** [prompt_password] reads a non-defaulted value, repeating on validation errors. *)
