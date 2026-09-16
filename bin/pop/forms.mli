(** Missing email fields for [pop].

    [run] presents the missing-value form through [Charm_huh]. Supplied values remain as
    defaults, so the form can fill only the fields that were absent. *)

type values = {
  to_ : string;
  cc : string;
  bcc : string;
  from : string;
  subject : string;
  body : string;
}
(** Values collected by the form. *)

type error = [ `Aborted | `Timeout ]
(** A form run failure. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf error] renders [error]. *)

val run :
  clock:_ Eio.Time.clock ->
  Eio_unix.Stdenv.base ->
  initial:values ->
  (values, error) result
(** [run ~clock env ~initial] prompts for email fields using [initial] as defaults. *)
