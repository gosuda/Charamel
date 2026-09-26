(** Line-oriented accessible form input.

    Every prompt returns a promise: the reader waits on a channel, and a channel read only
    completes when the user answers. *)

type reader = {
  read_line : unit -> string option Lwt.t;
  read_password : unit -> string option Lwt.t;
}

val reader_of_channel :
  stdin:Lwt_io.input_channel -> echo_off:(unit -> unit) option -> reader
(** [reader_of_channel ~stdin ~echo_off] creates one reader over [stdin]. [echo_off], when
    present, is the closure that puts echo back after a password read. *)

val prompt_string :
  out:(string -> unit Lwt.t) ->
  reader ->
  prompt:string ->
  default:string ->
  validate:(string -> (unit, string) result) ->
  string Lwt.t
(** [prompt_string] repeats the prompt until validation succeeds, using [default] for
    blank input or end of file. *)

val prompt_int :
  (string -> unit Lwt.t) ->
  reader ->
  prompt:string ->
  low:int ->
  high:int ->
  default:int option ->
  int Lwt.t
(** [prompt_int] reads an integer in the inclusive range, repeating on malformed input. *)

val prompt_bool :
  (string -> unit Lwt.t) -> reader -> prompt:string -> default:bool -> bool Lwt.t
(** [prompt_bool] reads yes/no, using [default] for a blank line. *)

val prompt_password :
  out:(string -> unit Lwt.t) ->
  reader ->
  prompt:string ->
  validate:(string -> (unit, string) result) ->
  string Lwt.t
(** [prompt_password] reads a non-defaulted value, repeating on validation errors. *)
