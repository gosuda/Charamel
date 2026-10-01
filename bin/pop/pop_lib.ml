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

let input_limit = (10 * 1024 * 1024) + 1

let read_flow ~label channel =
  let buffer = Buffer.create 4096 in
  let rec loop () =
    Lwt.bind (Lwt_io.read ~count:65536 channel) (fun chunk ->
        if chunk = "" then Lwt.return (Ok (Buffer.contents buffer))
        else begin
          Buffer.add_string buffer chunk;
          if Buffer.length buffer > input_limit then
            Lwt.return
              (Error (`Input (Fmt.str "%s exceeds the 10 MiB input limit" label)))
          else loop ()
        end)
  in
  loop ()

let resolve ~cwd path =
  if Filename.is_relative path then Filename.concat cwd path else path

let read_file ~cwd path =
  if String.trim path = "" then Lwt.return (Error (`Input "file path is empty"))
  else
    let resolved = resolve ~cwd path in
    Lwt.catch
      (fun () ->
        Lwt_io.with_file ~mode:Lwt_io.Input resolved (fun channel ->
            read_flow ~label:path channel))
      (function
        | Unix.Unix_error (error, function_name, argument) ->
            Lwt.return
              (Error
                 (`Input
                    (Fmt.str "could not read %s: %s (%s %s)" path
                       (Unix.error_message error) function_name argument)))
        | End_of_file ->
            Lwt.return
              (Error (`Input (Fmt.str "could not read %s: unexpected end of file" path)))
        | exn -> Lwt.fail exn)

let read_body ~cwd ~stdin options =
  match (options.body, options.body_file) with
  | Some _, Some _ ->
      Lwt.return (Error (`Input "--body and --body-file cannot be used together"))
  | Some body, None -> Lwt.return (Ok body)
  | None, Some path -> read_file ~cwd path
  | None, None ->
      if Charamel_cli.is_tty Unix.stdin then Lwt.return (Error (`Missing "body"))
      else
        Lwt.bind (read_flow ~label:"stdin" stdin) (function
          | Error error -> Lwt.return (Error error)
          | Ok "" -> Lwt.return (Error (`Missing "body"))
          | Ok body -> Lwt.return (Ok body))

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
    | [] -> Lwt.return (Ok (List.rev acc))
    | path :: rest ->
        Lwt.bind (read_file ~cwd path) (function
          | Error error -> Lwt.return (Error error)
          | Ok data -> (
              let name = Filename.basename path in
              match Mime.attachment ~name ~data () with
              | Ok attachment -> loop (attachment :: acc) rest
              | Error error -> Lwt.return (Error (`Message error))))
  in
  loop [] paths

let now () =
  match Ptime.of_float_s (Unix.gettimeofday ()) with
  | Some value -> value
  | None -> Fmt.failwith "clock returned an invalid POSIX timestamp"

let prepare ~cwd ~stdin ?date options =
  if Option.is_some options.body && Option.is_some options.body_file then
    Lwt.return (Error (`Input "--body and --body-file cannot be used together"))
  else
    match parse_from options.from with
    | Error error -> Lwt.return (Error error)
    | Ok from -> (
        match parse_subject options.subject with
        | Error error -> Lwt.return (Error error)
        | Ok subject -> (
            match parse_addresses options.to_ with
            | Error error -> Lwt.return (Error error)
            | Ok to_ -> (
                match parse_addresses options.cc with
                | Error error -> Lwt.return (Error error)
                | Ok cc -> (
                    match parse_addresses options.bcc with
                    | Error error -> Lwt.return (Error error)
                    | Ok bcc ->
                        if to_ = [] && cc = [] && bcc = [] then
                          Lwt.return (Error (`Missing "to"))
                        else
                          Lwt.bind (read_body ~cwd ~stdin options) (function
                            | Error error -> Lwt.return (Error error)
                            | Ok body -> (
                                let body =
                                  match options.signature with
                                  | Some signature when String.trim signature <> "" ->
                                      body ^ "\n\n" ^ signature
                                  | _ -> body
                                in
                                match
                                  render_body ~unsafe_html:options.unsafe_html body
                                with
                                | Error error -> Lwt.return (Error error)
                                | Ok (body_text, body_html) ->
                                    Lwt.bind (read_attachments ~cwd options.attachments)
                                      (function
                                      | Error error -> Lwt.return (Error error)
                                      | Ok attachments ->
                                          let date =
                                            Option.value date ~default:(now ())
                                          in
                                          (match
                                             Mime.message ~from ~subject ~date ~body_text
                                               ~body_html ~attachments ~to_ ~cc ~bcc ()
                                           with
                                            | Error error -> Error (`Message error)
                                            | Ok message -> Ok message)
                                          |> Lwt.return)))))))

let env_raw env name =
  match env name with Some value when value <> "" -> Some value | _ -> None

let env_value env name = Option.map String.trim (env_raw env name)

let int_env env name ~default =
  match env_value env name with
  | None -> Ok default
  | Some value -> (
      let decimal =
        value <> ""
        && String.for_all (fun character -> character >= '0' && character <= '9') value
      in
      match (decimal, int_of_string_opt value) with
      | true, Some port when port > 0 && port <= 65535 -> Ok port
      | _ ->
          Error (`Input (Fmt.str "%s must be a decimal integer between 1 and 65535" name))
      )

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
