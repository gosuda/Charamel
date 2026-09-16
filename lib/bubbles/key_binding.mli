(** Key bindings for Bubble Tea components.

    A binding is enabled only when it has at least one key and its explicit [enabled] flag
    is true. Matching compares only key codes and modifiers. *)

type t = { keys : Charm_tea.Key.t list; help : string * string; enabled : bool }
(** The type for an immutable key binding. [help] is the short key label and description.
*)

val v : ?help:string * string -> ?enabled:bool -> string list -> t
(** [v ?help ?enabled names] parses canonical key names with {!Charm_tea.Key.of_string}.
    [help] defaults to [("", "")] and [enabled] defaults to [true]. Raises
    [Invalid_argument] when a name is not a valid key literal. *)

val of_keys : ?help:string * string -> ?enabled:bool -> Charm_tea.Key.t list -> t
(** [of_keys ?help ?enabled keys] builds a binding from already-decoded keys. [help]
    defaults to [("", "")] and [enabled] defaults to [true]. *)

val matches : Charm_tea.Key.t -> t -> bool
(** [matches key binding] is [true] when [binding] is enabled and one of its keys matches
    [key]. Key text, shifted/base fields and key event kind are ignored by matching. *)

val matches_any : Charm_tea.Key.t -> t list -> bool
(** [matches_any key bindings] is [true] when at least one binding matches [key]. *)

val enabled : t -> bool
(** [enabled binding] is [true] when the explicit flag is set and the binding has keys. *)

val set_enabled : bool -> t -> t
(** [set_enabled value binding] changes the explicit enabled flag. *)

val unbind : t -> t
(** [unbind binding] removes every key, clears the help text, and disables the binding. *)

val set_keys : Charm_tea.Key.t list -> t -> t
(** [set_keys keys binding] replaces the keys while preserving the explicit enabled flag.
*)

val set_help : string * string -> t -> t
(** [set_help help binding] replaces the help label and description. *)
