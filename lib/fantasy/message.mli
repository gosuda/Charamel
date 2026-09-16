(** Conversation messages exchanged with a provider.

    A message is a role plus a list of typed parts. The part set is the intersection every
    provider codec can express: plain text, reasoning with an opaque signature, file
    attachments, tool calls issued by the model, and tool results returned by the caller.
*)

type role =
  | System
  | User
  | Assistant
  | Tool
      (** The type for message roles. [Tool] marks a message carrying tool results back to
          the provider. *)

type part =
  | Text of string
  | Reasoning of { text : string; signature : string option }
  | File of { mime : string; data : string; name : string option }
  | Tool_call of { id : string; name : string; input : Jsont.json }
  | Tool_result of {
      id : string;
      name : string;
      output : [ `Text of string | `Error of string | `Media of string * string ];
    }

(** The type for a message part. [Text] is plain content, [Reasoning] a reasoning trace
    with an optional provider signature, [File] an attachment whose [mime] describes the
    media and whose [data] is the already base64-encoded payload for binary content.
    Providers wrap that payload as their accepted inline-data or data-URL form, so callers
    encode file bytes once and do not pass raw bytes or a pre-wrapped data URL. [name] is
    an optional display name. [Tool_call] a call the assistant issued, and [Tool_result]
    the answer to a previous call, keyed by the call's [id] and [name]; the [output] is
    plain text, an error message, or a media type with content. *)

type t = { role : role; parts : part list }
(** The type for a message: a [role] and its [parts] in order. *)

val text : role -> string -> t
(** [text role s] is a message with a single [Text] part. *)

val tool_results :
  (string * string * [ `Text of string | `Error of string | `Media of string * string ])
  list ->
  t
(** [tool_results results] is a [Tool]-role message holding one [Tool_result] part per
    triple [(id, name, output)]. *)

val pp : t Fmt.t
(** [pp] formats a message compactly for diagnostics; reasoning text is elided to its
    length. *)
