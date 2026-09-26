(** MIME message composition for [pop].

    [Mime] builds a complete RFC 5322 message with RFC 2045 MIME structure: a header
    block, a [multipart/alternative] [text/plain] + [text/html] body when HTML is present,
    and a [multipart/mixed] wrapper when attachments are present. Line endings are CRLF
    throughout (RFC 5322 section 2.1); text bodies travel quoted-printable (RFC 2045
    section 6.7) and attachments base64 (RFC 2045 section 6.8) wrapped at 76 characters.
    Subject lines carry non-ASCII as RFC 2047 encoded-words (UTF-8, base64 "B" form, at
    most 75 characters per word) folded with CRLF continuations. [Date] uses the RFC 5322
    date-time syntax and [Message-ID] is derived deterministically from the message
    content when not supplied, so identical input serialises to identical bytes.

    Every value that reaches a header is validated: an address, a display name, a subject
    or an attachment name containing CR, LF or NUL is rejected with [`Header_injection].
    [Bcc] recipients are recorded on the message for the SMTP envelope and are never
    serialised into a header (RFC 5321 section 7.1 blind-copy semantics). *)

type error = [ `Invalid_address of string | `Header_injection of string | `No_recipients ]
(** The reason a composition failed. [`Invalid_address s] is a malformed address [s]
    (empty, no [@], unbalanced [@]). [`Header_injection s] describes the offending value
    [s], which carried CR, LF or NUL. [`No_recipients] is an envelope with empty [to_],
    [cc] and [bcc]. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf e] renders [e] for CLI and log output. *)

module Address : sig
  type t
  (** A mailbox: an optional display name and a nonempty [local@domain] address. *)

  val v : ?display:string -> string -> (t, error) result
  (** [v ?display addr] is the mailbox for the address [addr], optionally carrying the
      display name [display]. [display] is free text and is emitted quoted when it
      contains a character outside the atext set. Rejects [`Invalid_address] for an empty
      or unbalanced address and [`Header_injection] when [addr] or [display] carries CR,
      LF or NUL. *)

  val addr : t -> string
  (** [addr t] is the bare address, without display name or angle brackets. *)

  val display : t -> string option
  (** [display t] is the display name, when one was given. *)

  val to_header : t list -> string
  (** [to_header l] is the comma-separated mailbox list for a [To], [Cc] or [Reply-To]
      header, folded with CRLF continuations so no line exceeds 78 characters. An empty
      list is the empty string. *)
end

type attachment = {
  name : string;
      (** Bare file name, no path separators; emitted in [Content-Disposition]. *)
  content_type : string;  (** Media type, e.g. [application/pdf]. *)
  data : string;  (** Raw bytes; serialised base64. *)
}
(** An attachment input. The CLI reads files and passes their bytes here; [Mime] never
    touches the filesystem. *)

val attachment :
  ?content_type:string -> name:string -> data:string -> unit -> (attachment, error) result
(** [attachment ?content_type ~name ~data ()] is an attachment with the given media type
    or, when [content_type] is [None], [guess_content_type name]. Rejects
    [`Header_injection] when [name] is empty, contains a path separator, CR, LF or NUL. *)

val guess_content_type : string -> string
(** [guess_content_type name] is the media type for the file name [name] by extension,
    case-insensitive, defaulting to [application/octet-stream]. *)

type message = {
  from : Address.t;
  reply_to : Address.t list;
  to_ : Address.t list;
  cc : Address.t list;
  bcc : Address.t list;  (** Envelope-only recipients: never serialised as a header. *)
  subject : string;
  date : Ptime.t;
  message_id : string option;
      (** [None] derives a deterministic [Message-ID] from the message content. *)
  body_text : string;
  body_html : string option;
  attachments : attachment list;
}
(** A complete message. Build it through {!message}, which validates every field that
    reaches a header; the record itself carries no invariant. *)

val message :
  ?reply_to:Address.t list ->
  ?cc:Address.t list ->
  ?bcc:Address.t list ->
  ?message_id:string ->
  from:Address.t ->
  subject:string ->
  date:Ptime.t ->
  body_text:string ->
  ?body_html:string ->
  ?attachments:attachment list ->
  to_:Address.t list ->
  unit ->
  (message, error) result
(** [message ... ()] validates and assembles a message. [reply_to], [cc], [bcc],
    [message_id], [body_html] and [attachments] default to empty or absent. Rejects
    [`Invalid_address] when the subject is valid but a component failed its own check,
    [`Header_injection] when the subject or any attachment name carries CR, LF or NUL, and
    [`Invalid_address] when a display or address component is malformed. *)

val normalize_crlf : string -> string
(** [normalize_crlf text] is [text] with every line ending ([CR LF], a lone [CR] or a lone
    [LF]) rewritten as [CR LF]. The MIME body encoder and the SMTP [DATA] writer both
    depend on this one rule. *)

val encoded_subject : message -> string
(** [encoded_subject m] is the [Subject] header value: the subject unchanged when it is
    printable ASCII, otherwise RFC 2047 encoded-words (UTF-8, base64 "B" form, at most 75
    characters each) folded with CRLF continuations. *)

val serialise : message -> string
(** [serialise m] is the complete RFC 5322 message: header block, then body. Headers are
    emitted in the order From, To, Cc, Reply-To, Subject, Date, Message-ID, MIME-Version,
    Content-Type; [Bcc] is never emitted. Lines end CRLF and the message ends with CRLF.
    With attachments the body is [multipart/mixed] over the body part and the base64
    attachment parts; with [body_html] it is [multipart/alternative] over quoted-printable
    text and HTML; plain text alone is a single quoted-printable [text/plain] part. The
    boundary is derived deterministically from the content. *)

val envelope : message -> (string * string list, error) result
(** [envelope m] is the SMTP envelope: the reverse-path address and the recipient
    addresses in [to_], [cc] and [bcc] order. [Error `No_recipients] when all three lists
    are empty. *)
