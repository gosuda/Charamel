open Result.Syntax

type options = {
  to_ : string list;
  cc : string list;
  bcc : string list;
  from : string option;
  subject : string option;
  body : string option;
  body_file : string option;
  attachments : string list;
  signature : string option;
  unsafe_html : bool;
}

type prepared = { message : Mime.message; wire : string }

type error =
  [ `Missing of string
  | `Input of string
  | `Address of Mime.error
  | `Message of Mime.error
  | `Markdown of string ]

let pp_error ppf = function
  | `Missing field -> Fmt.pf ppf "missing required field: %s" field
  | `Input message -> Fmt.string ppf message
  | `Address error -> Fmt.pf ppf "invalid address: %a" Mime.pp_error error
  | `Message error -> Fmt.pf ppf "could not compose message: %a" Mime.pp_error error
  | `Markdown message -> Fmt.pf ppf "could not render Markdown: %s" message

let empty_options =
  {
    to_ = [];
    cc = [];
    bcc = [];
    from = None;
    subject = None;
    body = None;
    body_file = None;
    attachments = [];
    signature = None;
    unsafe_html = false;
  }

let split_addresses values =
  let split value =
    let result = Buffer.create (String.length value) in
    let parts = ref [] in
    let quoted = ref false in
    let escaped = ref false in
    let flush () =
      let part = String.trim (Buffer.contents result) in
      if part <> "" then parts := part :: !parts;
      Buffer.clear result
    in
    String.iter
      (fun character ->
        if !escaped then (
          Buffer.add_char result character;
          escaped := false)
        else if character = '\\' && !quoted then (
          Buffer.add_char result character;
          escaped := true)
        else if character = '"' then (
          Buffer.add_char result character;
          quoted := not !quoted)
        else if character = ',' && not !quoted then flush ()
        else Buffer.add_char result character)
      value;
    flush ();
    List.rev !parts
  in
  List.concat_map split values

let stdin_is_tty source =
  match Eio_unix.Resource.fd_opt source with
  | None -> false
  | Some fd -> (
      try Eio_unix.Fd.use_exn "isatty" fd Unix.isatty
      with Unix.Unix_error _ | Invalid_argument _ -> false)

let input_limit = (10 * 1024 * 1024) + 1

let contains text needle =
  let text_length = String.length text and needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > text_length then false
    else if String.sub text index needle_length = needle then true
    else loop (index + 1)
  in
  needle_length = 0 || loop 0

let read_flow ~label flow =
  match Eio.Buf_read.parse ~max_size:input_limit Eio.Buf_read.take_all flow with
  | Ok value -> Ok value
  | Error (`Msg message) ->
      let message =
        if contains (String.lowercase_ascii message) "limit" then
          Fmt.str "%s exceeds the 10 MiB input limit" label
        else Fmt.str "%s: %s" label message
      in
      Error (`Input message)

let read_file ~cwd path =
  if String.trim path = "" then Error (`Input "file path is empty")
  else
    try
      Eio.Path.with_open_in Eio.Path.(cwd / path) (fun flow -> read_flow ~label:path flow)
    with
    | Eio.Io (Eio.Fs.E _, _) as exn ->
        Error (`Input (Fmt.str "could not read %s: %a" path Eio.Exn.pp exn))
    | Unix.Unix_error (error, function_name, argument) ->
        Error
          (`Input
             (Fmt.str "could not read %s: %s (%s %s)" path (Unix.error_message error)
                function_name argument))
    | End_of_file ->
        Error (`Input (Fmt.str "could not read %s: unexpected end of file" path))

let read_body ~cwd ~stdin options =
  match (options.body, options.body_file) with
  | Some _, Some _ -> Error (`Input "--body and --body-file cannot be used together")
  | Some body, None -> Ok body
  | None, Some path -> read_file ~cwd path
  | None, None -> (
      if stdin_is_tty stdin then Error (`Missing "body")
      else
        match read_flow ~label:"stdin" stdin with
        | Error error -> Error error
        | Ok "" -> Error (`Missing "body")
        | Ok body -> Ok body)

let parse_address value =
  match Mime.Address.v (String.trim value) with
  | Ok address -> Ok address
  | Error error -> Error (`Address error)

let parse_addresses values =
  let values = split_addresses values in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest -> (
        match parse_address value with
        | Ok address -> loop (address :: acc) rest
        | Error error -> Error error)
  in
  loop [] values

let parse_from = function
  | None -> Error (`Missing "from")
  | Some value when String.trim value = "" -> Error (`Missing "from")
  | Some value -> parse_address value

let parse_subject = function
  | None | Some "" -> Error (`Missing "subject")
  | Some value when String.trim value = "" -> Error (`Missing "subject")
  | Some value -> Ok value

let render_body ~unsafe_html body =
  if not (String.is_valid_utf_8 body) then Error (`Markdown "body is not valid UTF-8")
  else
    try
      let document = Cmarkit.Doc.of_string ~strict:false body in
      let html = Cmarkit_html.of_doc ~safe:(not unsafe_html) document in
      let plain = Charamel_glamour.render ~theme:Charamel_glamour.Theme.ascii body in
      Ok (plain, html)
    with
    | Invalid_argument message -> Error (`Markdown message)
    | Failure message -> Error (`Markdown message)

let read_attachments ~cwd paths =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | path :: rest -> (
        let* data = read_file ~cwd path in
        let name = Filename.basename path in
        match Mime.attachment ~name ~data () with
        | Ok attachment -> loop (attachment :: acc) rest
        | Error error -> Error (`Message error))
  in
  loop [] paths

let now clock =
  match Ptime.of_float_s (Eio.Time.now clock) with
  | Some value -> value
  | None -> Fmt.failwith "clock returned an invalid POSIX timestamp"

let prepare ~sw:_ ~clock ~cwd ~stdin ?date options =
  if Option.is_some options.body && Option.is_some options.body_file then
    Error (`Input "--body and --body-file cannot be used together")
  else
    let* from = parse_from options.from in
    let* subject = parse_subject options.subject in
    let* to_ = parse_addresses options.to_ in
    let* cc = parse_addresses options.cc in
    let* bcc = parse_addresses options.bcc in
    if to_ = [] && cc = [] && bcc = [] then Error (`Missing "to")
    else
      let* body = read_body ~cwd ~stdin options in
      let body =
        match options.signature with
        | Some signature when String.trim signature <> "" -> body ^ "\n\n" ^ signature
        | _ -> body
      in
      let* body_text, body_html = render_body ~unsafe_html:options.unsafe_html body in
      let* attachments = read_attachments ~cwd options.attachments in
      let date = Option.value date ~default:(now clock) in
      match
        Mime.message ~from ~subject ~date ~body_text ~body_html ~attachments ~to_ ~cc ~bcc
          ()
      with
      | Error error -> Error (`Message error)
      | Ok message -> Ok { message; wire = Mime.serialise message }

let env_raw env name =
  match env name with Some value when value <> "" -> Some value | _ -> None

let env_value env name = Option.map String.trim (env_raw env name)

let int_env env name ~default =
  match env_value env name with
  | None -> Ok default
  | Some value -> (
      match int_of_string_opt value with
      | Some port when port > 0 && port <= 65535 -> Ok port
      | _ -> Error (`Input (Fmt.str "%s must be an integer between 1 and 65535" name)))

let security_env env =
  match env_value env "POP_SMTP_ENCRYPTION" with
  | None -> Ok Smtp.Starttls
  | Some value -> (
      match String.lowercase_ascii value with
      | "starttls" -> Ok Smtp.Starttls
      | "ssl" -> Ok Smtp.Tls
      | "none" -> Ok Smtp.Plain
      | _ -> Error (`Input "POP_SMTP_ENCRYPTION must be one of starttls, ssl, or none"))

let config_of_env ~env =
  match env_value env "POP_SMTP_HOST" with
  | None -> Error (`Missing "POP_SMTP_HOST")
  | Some host -> (
      let* port = int_env env "POP_SMTP_PORT" ~default:587 in
      let* security = security_env env in
      let username = env_raw env "POP_SMTP_USERNAME" in
      let password = env_raw env "POP_SMTP_PASSWORD" in
      match (password, username) with
      | Some _, None -> Error (`Input "POP_SMTP_PASSWORD requires POP_SMTP_USERNAME")
      | _ -> Ok { Send.host; port; username; password; security })
