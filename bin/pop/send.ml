type smtp_config = {
  host : string;
  port : int;
  username : string option;
  password : string option;
  security : Smtp.security;
}

type error =
  [ `Configuration of string
  | `Http of int * string
  | `Transport of string
  | `Smtp of Smtp.error ]

let pp_error ppf = function
  | `Configuration message -> Fmt.pf ppf "configuration error: %s" message
  | `Http (status, body) ->
      if String.trim body = "" then Fmt.pf ppf "Resend returned HTTP %d" status
      else Fmt.pf ppf "Resend returned HTTP %d: %s" status body
  | `Transport message -> Fmt.pf ppf "Resend transport error: %s" message
  | `Smtp error -> Fmt.pf ppf "SMTP error: %a" Smtp.pp_error error

let json_string json =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify Jsont.json json with
  | Ok value -> value
  | Error message -> Fmt.failwith "cannot encode Resend request: %s" message

let json_member name value = Jsont.Json.mem (Jsont.Json.name name) value
let json_string_member name value = json_member name (Jsont.Json.string value)
let json_strings values = Jsont.Json.list (List.map Jsont.Json.string values)
let address_values addresses = List.map Mime.Address.addr addresses

let resend_payload message =
  let members =
    [
      json_string_member "from" (Mime.Address.addr message.Mime.from);
      json_member "to" (json_strings (address_values message.Mime.to_));
      json_string_member "subject" message.Mime.subject;
      json_string_member "text" message.Mime.body_text;
    ]
  in
  let members =
    match message.Mime.cc with
    | [] -> json_member "cc" (json_strings []) :: members
    | values -> json_member "cc" (json_strings (address_values values)) :: members
  in
  let members =
    match message.Mime.bcc with
    | [] -> json_member "bcc" (json_strings []) :: members
    | values -> json_member "bcc" (json_strings (address_values values)) :: members
  in
  let members =
    match message.Mime.body_html with
    | None -> members
    | Some html -> json_string_member "html" html :: members
  in
  let attachments =
    List.map
      (fun (attachment : Mime.attachment) ->
        Jsont.Json.object'
          [
            json_string_member "filename" attachment.Mime.name;
            json_string_member "content" (Base64.encode_string attachment.Mime.data);
          ])
      message.Mime.attachments
  in
  let members =
    if attachments = [] then members
    else json_member "attachments" (Jsont.Json.list attachments) :: members
  in
  json_string (Jsont.Json.object' (List.rev members))

let status_code status = Http.Status.to_int status

let pp_net_error ppf = function
  | Eio.Net.Connection_reset _ -> Fmt.string ppf "connection reset"
  | Eio.Net.Connection_failure (Eio.Net.Refused _) -> Fmt.string ppf "connection refused"
  | Eio.Net.Connection_failure Eio.Net.Timeout -> Fmt.string ppf "connection timed out"
  | Eio.Net.Address_lookup_failed error ->
      Fmt.pf ppf "address lookup failed: %s" (Eio.Net.Getaddrinfo_error.to_message error)
  | Eio.Net.Invalid_option -> Fmt.string ppf "invalid network option"

let host_of_uri uri =
  match Uri.host uri with
  | None -> Fmt.failwith "Resend endpoint has no host name"
  | Some host -> (
      match Domain_name.of_string host with
      | Ok domain -> Domain_name.host_exn domain
      | Error (`Msg message) ->
          Fmt.failwith "invalid Resend endpoint host %s: %s" host message)

let https_of_uri uri flow =
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok value -> value
    | Error (`Msg message) ->
        Fmt.failwith "cannot load system CA certificates: %s" message
  in
  match Tls.Config.client ~authenticator () with
  | Ok config -> Tls_eio.client_of_flow config ~host:(host_of_uri uri) flow
  | Error (`Msg message) -> Fmt.failwith "cannot create TLS client: %s" message

let response_body body =
  let reader = Eio.Buf_read.of_flow ~max_size:((10 * 1024 * 1024) + 1) body in
  Eio.Buf_read.take_all reader

let request ~sw ~clock ~net ~endpoint ~api_key message =
  let uri = Uri.of_string endpoint in
  let headers =
    Cohttp.Header.of_list
      [ ("authorization", "Bearer " ^ api_key); ("content-type", "application/json") ]
  in
  let body = Cohttp_eio.Body.of_string (resend_payload message) in
  let perform () =
    let client = Cohttp_eio.Client.make ~https:(Some https_of_uri) net in
    let response, response_body_flow =
      Cohttp_eio.Client.post ~headers ~body client ~sw uri
    in
    let response_text = response_body response_body_flow in
    let status = status_code (Cohttp.Response.status response) in
    if status >= 200 && status < 300 then Ok () else Error (`Http (status, response_text))
  in
  try Eio.Time.with_timeout_exn clock 30. perform with
  | Eio.Time.Timeout -> Error (`Transport "request timed out")
  | Eio.Io (Eio.Net.E error, _) -> Error (`Transport (Fmt.str "%a" pp_net_error error))
  | Eio.Buf_read.Buffer_limit_exceeded ->
      Error (`Transport "Resend response exceeded the 10 MiB limit")
  | Tls_eio.Tls_failure failure ->
      Error
        (`Transport (Fmt.str "TLS handshake failed: %a" Tls.Engine.pp_failure failure))
  | Tls_eio.Tls_alert _ -> Error (`Transport "TLS alert from Resend")
  | End_of_file -> Error (`Transport "Resend closed the connection before its response")
  | Failure message -> Error (`Transport message)
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (`Transport
           (Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument))

let resend ~sw ~clock ~net ?(endpoint = "https://api.resend.com/emails") ~api_key message
    =
  if String.trim api_key = "" then Error (`Configuration "Resend API key is empty")
  else request ~sw ~clock ~net ~endpoint ~api_key message

let smtp ~sw ~clock ~net ~config message =
  match Mime.envelope message with
  | Error error ->
      Error
        (`Configuration (Fmt.str "cannot build SMTP envelope: %a" Mime.pp_error error))
  | Ok (from, recipients) -> (
      let auth =
        match (config.username, config.password) with
        | Some username, Some password -> Some (username, password)
        | _ -> None
      in
      let body = Mime.serialise message in
      match
        Smtp.deliver ~sw ~clock ~net ~host:config.host ~port:config.port
          ~security:config.security ?auth ~from ~recipients ~body ()
      with
      | Ok () -> Ok ()
      | Error error -> Error (`Smtp error))

let deliver ~sw ~clock ~net ~resend_key ~smtp:smtp_config message =
  match resend_key with
  | Some key when String.trim key <> "" -> resend ~sw ~clock ~net ~api_key:key message
  | _ -> (
      match smtp_config with
      | None -> Error (`Configuration "no delivery method configured")
      | Some config -> smtp ~sw ~clock ~net ~config message)
