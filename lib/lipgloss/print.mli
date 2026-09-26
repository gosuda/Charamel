(** Downsampling print helpers.

    Every helper sends text through {!Charamel_colorprofile.Writer}, so colors are reduced
    to what the selected profile can show and every SGR sequence is removed under
    [Charamel_colorprofile.No_tty] and [Charamel_colorprofile.Ascii]. Text and control
    sequences that are not colors pass through unchanged. *)

val default_profile : Charamel_colorprofile.t
(** [default_profile] is the profile the helpers use when none is given:
    {!Charamel_colorprofile.detect} with [is_tty:false] over {!Stdlib.Sys.getenv_opt}, so
    a caller that has not measured its output stream keeps colors only when the
    environment forces them. *)

val print :
  ?profile:Charamel_colorprofile.t -> ?sink:Lwt_io.output_channel -> string -> unit Lwt.t
(** [print ?profile ?sink text] writes [text] to [sink], standard output by default,
    through a writer for [profile]. *)

val println :
  ?profile:Charamel_colorprofile.t -> ?sink:Lwt_io.output_channel -> string -> unit Lwt.t
(** [println ?profile ?sink text] is {!val:print} followed by a newline. *)

val sprint : ?profile:Charamel_colorprofile.t -> string -> string Lwt.t
(** [sprint ?profile text] is [text] downsampled to [profile] as a string. The result is
    [Lwt] because the downsampler is a byte writer that may flush. *)

val sprintln : ?profile:Charamel_colorprofile.t -> string -> string Lwt.t
(** [sprintln ?profile text] is {!val:sprint} followed by a newline. *)
