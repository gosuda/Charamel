(** Build version. *)

val current : string
(** [current] is the version recorded for this build by [dune-build-info], or ["dev"] when
    the build carries no version metadata. *)
