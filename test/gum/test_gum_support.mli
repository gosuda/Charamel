(** Shared helpers for the gum scripted-UI suites. *)

val key : string -> Charamel_tea.Key.t
(** [key name] is the key that [Charamel_tea.Key.of_string] parses from [name], failing
    the running test when the name is not a valid key. *)
