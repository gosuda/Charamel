(** A tool the caller offers to the model.

    [t] is the provider-neutral shape: a name, a human-readable description, and a JSON
    schema object describing the call input. Codecs translate it to each provider's wire
    form. *)

type t = { name : string; description : string; schema : Jsont.json }
(** The type for a tool definition. [schema] must be a JSON schema object for the tool's
    input. *)

val v : name:string -> description:string -> schema:Jsont.json -> t
(** [v ~name ~description ~schema] is a tool with those fields. *)

val pp : t Fmt.t
(** [pp] formats the tool's name and description; the schema body is elided. *)
