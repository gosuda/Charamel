(** Byte-identity battery: select and multi-select frames pin the merged {!Field_impl}
    picker against the pre-merge implementations. Runs unchanged against the baseline tree
    to prove the merge preserved rendering. *)

val cases : (string * [> `Quick ] * (unit -> unit)) list
