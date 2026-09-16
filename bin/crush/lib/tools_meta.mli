(** Crush metadata and interaction tools.

    The values expose todo state, interactive questions, runtime information and log
    inspection through the common crush tool boundary. *)

val todos : Tool.t
(** [todos] replaces the current session todo list. *)

val question : Tool.t
(** [question] asks the interactive user one to five typed questions. *)

val crush_info : Tool.t
(** [crush_info] reports configured providers and runtime services. *)

val crush_logs : Tool.t
(** [crush_logs] returns the tail of the crush log. *)

val all : Tool.t list
(** [all] is the metadata tool set in its advertised order. *)
