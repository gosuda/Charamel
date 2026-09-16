(** Human-readable finite durations.

    The input is measured in seconds and rounded to whole nanoseconds before formatting,
    using the compact unit spelling used by Go timers. *)

val to_string : float -> string
(** [to_string seconds] formats [seconds] with leading zero units omitted. Durations of at
    least one second use hours, minutes and seconds (with a trimmed fractional part),
    while shorter durations use the largest unit whose integer part is non-zero. Zero is
    ["0s"]. Raises [Invalid_argument] for a non-finite value or when rounded nanoseconds
    do not fit in a signed 64-bit duration. *)
