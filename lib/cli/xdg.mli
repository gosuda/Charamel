(** XDG base directories.

    [Xdg] computes the per-application directories of the XDG Base Directory
    specification. A directory is [var/app] where [var] is the matching [XDG_*_HOME]
    variable, or the [$HOME] fallback when the variable does not qualify. A variable
    qualifies only when it is set to a nonempty absolute path. The functions read the
    process environment and touch no file. The caller creates directories as needed, for
    example through {!Charamel_os.Fs}. *)

val config_dir : app:string -> string
(** [config_dir ~app] is the directory in which [app] stores configuration,
    [$XDG_CONFIG_HOME/app] or [$HOME/.config/app].

    @raise Invalid_argument
      if neither [$XDG_CONFIG_HOME] nor [$HOME] is set to a nonempty absolute path. *)

val data_dir : app:string -> string
(** [data_dir ~app] is the directory in which [app] stores data, [$XDG_DATA_HOME/app] or
    [$HOME/.local/share/app].

    @raise Invalid_argument
      if neither [$XDG_DATA_HOME] nor [$HOME] is set to a nonempty absolute path. *)

val state_dir : app:string -> string
(** [state_dir ~app] is the directory in which [app] stores state, [$XDG_STATE_HOME/app]
    or [$HOME/.local/state/app].

    @raise Invalid_argument
      if neither [$XDG_STATE_HOME] nor [$HOME] is set to a nonempty absolute path. *)

val cache_dir : app:string -> string
(** [cache_dir ~app] is the directory in which [app] stores cached data,
    [$XDG_CACHE_HOME/app] or [$HOME/.cache/app].

    @raise Invalid_argument
      if neither [$XDG_CACHE_HOME] nor [$HOME] is set to a nonempty absolute path. *)
