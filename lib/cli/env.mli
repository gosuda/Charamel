(** The capabilities one command is handed.

    [t] replaces the runtime environment a command used to reach through: where it works,
    what it reads, what it writes, and what time it is. Every field is a value the caller
    already owns, so a test can build a record over pipes and a simulated clock and run
    the very code a binary runs. *)

type t = {
  cwd : string;
      (** [cwd] is the working directory paths without a root are read against. *)
  fs_root : string;
      (** [fs_root] is the root of the process's filesystem view. A path that is relative
          is resolved against [cwd]; one that is absolute is already under [fs_root]. *)
  stdin : Lwt_io.input_channel;  (** The channel a command reads from. *)
  stdout : Lwt_io.output_channel;  (** The channel a command writes results to. *)
  stderr : Lwt_io.output_channel;  (** The channel a command writes diagnostics to. *)
  clock : Charamel_os.Time.clock;  (** The clock a command waits on. *)
}
