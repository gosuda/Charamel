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

let net_error = function
  | `Http (http : Charamel_net.http_error) -> `Http (http.status, http.message)
  | `Transport message -> `Transport message
  | `Oauth _ | `Oauth_invalid_grant _ -> `Transport "unexpected OAuth error"

let request ~endpoint ~api_key message =
  let uri = Uri.of_string endpoint in
  let headers =
    [ ("authorization", "Bearer " ^ api_key); ("content-type", "application/json") ]
  in
  let body = Some (resend_payload message) in
  Lwt.bind (Charamel_net.call ~timeout:30. ~headers ~meth:`POST ~body uri) (function
    | Error error -> Lwt.return (Error (net_error error))
    | Ok (_response, stream) ->
        Lwt.bind (Charamel_net.read_body ~timeout:30. stream) (function
          | Ok _drained -> Lwt.return (Ok ())
          | Error error -> Lwt.return (Error (net_error error))))

let resend ?(endpoint = "https://api.resend.com/emails") ~api_key message =
  if String.trim api_key = "" then
    Lwt.return (Error (`Configuration "Resend API key is empty"))
  else request ~endpoint ~api_key message

let smtp ~config message =
  match Mime.envelope message with
  | Error error ->
      Lwt.return
        (Error
           (`Configuration (Fmt.str "cannot build SMTP envelope: %a" Mime.pp_error error)))
  | Ok (from, recipients) ->
      let auth =
        match (config.username, config.password) with
        | Some username, Some password -> Some (username, password)
        | _ -> None
      in
      let body = Mime.serialise message in
      Lwt.bind
        (Smtp.deliver ~host:config.host ~port:config.port ~security:config.security ?auth
           ~from ~recipients ~body ()) (function
        | Ok () -> Lwt.return (Ok ())
        | Error error -> Lwt.return (Error (`Smtp error)))

let deliver ~resend_key ~smtp:smtp_config message =
  match resend_key with
  | Some key when String.trim key <> "" -> resend ~api_key:key message
  | _ -> (
      match smtp_config with
      | None -> Lwt.return (Error (`Configuration "no delivery method configured"))
      | Some config -> smtp ~config message)
