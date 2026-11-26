(** The provider catalog shared by configuration validation and model resolution. *)

val path : unit -> string
(** [path ()] is the catalog file in the process's XDG cache directory. *)

val read : fs_root:string -> (string, string) result Lwt.t
(** [read ~fs_root] reads the regular cache file under [fs_root]. Files larger than
    10 MiB and unreadable files return an error. *)

val load : fs_root:string -> Charamel_fantasy.Provider_info.t list Lwt.t
(** [load ~fs_root] returns the decoded cache, or the embedded catalog when the cache
    cannot be read or decoded. Configuration overlays are applied by the caller. *)
