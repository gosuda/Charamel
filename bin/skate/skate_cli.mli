(** Skate command definitions.

    The commands provide local JSON key-value storage through [Charamel_cli.run]. Each
    command receives the standard runtime environment from the application runtime. Keys
    and database names preserve case. A trailing ["@"] suffix selects the default
    database. *)

module Store = Skate_core.Store
(** [Store] is the local JSON store used by the commands. *)

val get : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [get env] is the [get] command evaluated with [env]. *)

val set : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [set env] is the [set] command evaluated with [env]. *)

val delete : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [delete env] is the [delete] command evaluated with [env]. *)

val list : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [list env] is the [list] command evaluated with [env]. *)

val delete_db : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [delete_db env] is the [delete-db] command evaluated with [env]. *)

val dbs : Charamel_cli.Env.t -> unit Lwt.t Cmdliner.Cmd.t
(** [dbs env] is the [dbs] command evaluated with [env]. *)
