(** The registry of every ported upstream bubbletea example.

    Each entry names one directory of [.references/bubbletea/examples/] and holds the
    example's interactive entry point and its scripted smoke. *)

type t = {
  name : string;  (** The upstream directory name, for example ["list-fancy"]. *)
  main : unit -> unit Lwt.t;  (** The interactive entry point. *)
  smoke : unit -> (string * string) list;
      (** The scripted [(needle, frame)] pairs. Each [needle] must occur in its [frame].
      *)
}
(** The type for one registered example. *)

val all : t list
(** [all] is every registered example, sorted by [name]. *)

val find : string -> t option
(** [find name] is the example registered as [name], if any. *)
