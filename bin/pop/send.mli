(** Resend and SMTP delivery for [pop].

    Resend requests use the HTTPS API through {!Charamel_net} with bounded response
    bodies. SMTP uses the protocol implementation in {!Smtp}. Neither path logs
    credentials. *)

type smtp_config = {
  host : string;
  port : int;
  username : string option;
  password : string option;
  security : Smtp.security;
}
(** SMTP connection settings. *)

type error =
  [ `Configuration of string
  | `Http of int * string
  | `Transport of string
  | `Smtp of Smtp.error ]
(** A delivery failure. The HTTP body is retained only as a bounded diagnostic. *)

val pp_error : Format.formatter -> error -> unit
(** [pp_error ppf error] renders [error] without printing a credential. *)

val resend_payload : Mime.message -> string
(** [resend_payload message] is the JSON request body for the Resend email API.
    Attachments are base64 encoded and blind-copy recipients are represented in the API
    payload without being added to the RFC 5322 message headers. *)

val resend :
  ?endpoint:string -> api_key:string -> Mime.message -> (unit, error) result Lwt.t
(** [resend ?endpoint ~api_key message] sends [message] to the Resend endpoint. [endpoint]
    defaults to [https://api.resend.com/emails] and exists for deterministic local HTTP
    fixtures. Responses are limited to ten mebibytes and every request has a thirty-second
    deadline. *)

val smtp : config:smtp_config -> Mime.message -> (unit, error) result Lwt.t
(** [smtp ~config message] sends [message] through SMTP and includes the To, Cc and Bcc
    envelope recipients. *)

val deliver :
  resend_key:string option ->
  smtp:smtp_config option ->
  Mime.message ->
  (unit, error) result Lwt.t
(** [deliver ~resend_key ~smtp message] chooses Resend when [resend_key] is nonempty and
    otherwise uses [smtp]. *)
