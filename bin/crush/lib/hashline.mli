(** Hashline digests and snapshot tags.

    Hashline tags use byte-oriented xxHash32 digests. Normalization removes trailing
    horizontal whitespace from each line before a tag is computed. *)

val xxh32 : string -> Int32.t -> Int32.t
(** [xxh32 data seed] is the xxHash32 digest of the bytes in [data] with [seed] as its
    32-bit seed. *)

val normalize : string -> string
(** [normalize s] is [s] with trailing spaces, tabs and carriage returns removed from
    every line. Each line terminator is retained, and the final line retains whether it
    had a terminator. *)

val tag : string -> string
(** [tag s] is the four uppercase hexadecimal digits of the low 16 bits of
    [xxh32 (normalize s) 0l]. *)
