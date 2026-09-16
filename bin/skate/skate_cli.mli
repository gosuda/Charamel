(** Skate command definitions.

    The commands provide local JSON key-value storage through [Charm_cli.run]. Each
    command receives the standard Eio environment from the application runtime. Keys and
    database names preserve case. A trailing ["@"] suffix selects the default database. *)

module Store = Skate_core.Store
(** [Store] is the local JSON store used by the commands. *)

val get : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [get env] is the [get] command evaluated with [env]. *)

val set : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [set env] is the [set] command evaluated with [env]. *)

val delete : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [delete env] is the [delete] command evaluated with [env]. *)

val list : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [list env] is the [list] command evaluated with [env]. *)

val delete_db : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [delete_db env] is the [delete-db] command evaluated with [env]. *)

val dbs : Eio_unix.Stdenv.base -> unit Cmdliner.Cmd.t
(** [dbs env] is the [dbs] command evaluated with [env]. *)
