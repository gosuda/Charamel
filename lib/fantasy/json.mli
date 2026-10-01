(** JSON member access and constructors for the provider codecs.

    The event surface is large and mostly irrelevant, so decoding reads named members off
    the generic representation instead of declaring a jsont codec for the whole envelope.
*)

val oopt : Jsont.json -> string -> Jsont.json option
(** [oopt j n] is the member of [j] named [n], or [None] when [j] is not an object or
    carries no such member. *)

val is_object : Jsont.json -> bool
(** [is_object j] is [true] if [j] is a JSON object. *)

val has_error : Jsont.json -> bool
(** [has_error j] is [true] if [j] carries an [error] member that is not null. *)

val string_mem : Jsont.json -> string -> string option
(** [string_mem j n] is the string member of [j] named [n], or [None]. *)

val int_mem : Jsont.json -> string -> int
(** [int_mem j n] is the number member of [j] named [n] truncated to an integer, or [0].
*)

val int_option_mem : Jsont.json -> string -> int option
(** [int_option_mem j n] is the number member of [j] named [n] truncated to an integer, or
    [None]. *)

val bool_mem : Jsont.json -> string -> bool
(** [bool_mem j n] is the boolean member of [j] named [n], or [false]. *)

val objects_of_json : Jsont.json -> Jsont.json list option
(** [objects_of_json j] is the elements of [j] when [j] is an array of objects, or [None].
*)

val array_of : Jsont.json -> Jsont.json list option
(** [array_of j] is the elements of [j] when [j] is an array, or [None]. *)

val strings_of_json : Jsont.json -> string list option
(** [strings_of_json j] is the elements of [j] when [j] is an array of strings, or [None].
*)

val valid_json : string -> bool
(** [valid_json s] is [true] when [s] parses as a JSON value. *)

val json_of_string : string -> (Jsont.json, string) result
(** [json_of_string s] is the JSON value encoded in [s]. *)

val string_of_json : Jsont.json -> string
(** [string_of_json j] is the minified encoding of [j]. Raises [Invalid_argument] when [j]
    cannot be encoded. *)

val n : ?meta:Jsont.Meta.t -> string -> Jsont.name
(** [n s] is the member name [s]. *)

val str : ?meta:Jsont.Meta.t -> string -> Jsont.json
(** [str s] is the JSON string [s]. *)

val obj : ?meta:Jsont.Meta.t -> Jsont.object' -> Jsont.json
(** [obj ms] is the JSON object with members [ms]. *)

val arr : ?meta:Jsont.Meta.t -> Jsont.json list -> Jsont.json
(** [arr xs] is the JSON array with elements [xs]. *)

val num : ?meta:Jsont.Meta.t -> float -> Jsont.json
(** [num f] is the JSON number [f]. *)

val bool : ?meta:Jsont.Meta.t -> bool -> Jsont.json
(** [bool b] is the JSON boolean [b]. *)

val int : ?meta:Jsont.Meta.t -> int -> Jsont.json
(** [int i] is [i] as a JSON number, or a JSON string when [i] is outside the safe integer
    range. *)
