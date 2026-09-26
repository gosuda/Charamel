(** Preview output for [pop].

    Preview writes the complete MIME message and performs no network or SMTP operation. *)

val render : Mime.message -> string
(** [render message] is the complete deterministic MIME message. *)

val write : Lwt_io.output_channel -> Mime.message -> unit Lwt.t
(** [write sink message] writes [render message] to [sink]. *)
