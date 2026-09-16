(** Delegated-agent tool.

    The tool accepts one prompt or a bounded array of prompts. Array requests use at most
    eight active children and preserve input order in the result. *)

val agent : Tool.t
(** [agent] delegates a prompt or a bounded task array to child agents. *)
