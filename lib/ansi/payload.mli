(** Payload validation shared by the sequence constructors. *)

val check : what:string -> string -> unit
(** [check ~what s] raises [Invalid_argument] when [s] is not valid UTF-8, or when [s]
    contains BEL, ESC, or a C1 control scalar. The message names [what]. Otherwise [check]
    returns [unit]. *)
