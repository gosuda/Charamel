(** Missing email fields for [pop].

    [run] presents the missing-value form through [Charamel_huh]. Supplied values remain
    as defaults, so the form can fill only the fields that were absent. *)

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
  clock:Charamel_os.Time.clock ->
  fs_root:string ->
  temp_dir:string ->
  initial:values ->
  (values, error) result Lwt.t
(** [run ~clock ~fs_root ~temp_dir ~initial] prompts for email fields using [initial] as
    defaults. *)
