(** GitHub emoji shortcodes.

    Shortcodes include their enclosing colons and map to UTF-8 text. *)

val table : (string * string) list
(** [table] is the shortcode map in bytewise shortcode order. *)
