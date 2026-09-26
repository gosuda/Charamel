type reply = { code : int; lines : string list }

type response_error =
  [ `Unexpected of reply
  | `Bad_reply of string
  | `Data_refused of reply
  | `Tls_refused of reply
  | `Auth_refused of reply
  | `Auth_required ]

type error =
  [ response_error
  | `Timeout
  | `Closed
  | `Tls of Tls.Engine.failure
  | `Net of string
  | `No_recipients
  | `Invalid_address of string
  | `Header_injection of string ]

type security = Plain | Starttls | Tls

type t = {
  mutable ic : Lwt_io.input_channel;
  mutable oc : Lwt_io.output_channel;
  fd : Lwt_unix.file_descr;
  mutable via_tls : bool;
  timeout : float;
  mutable capabilities : string list;
  mutable closed : bool;
}

let pp_reply ppf reply =
  match reply.lines with
  | [] -> Format.fprintf ppf "%03d" reply.code
  | first :: rest ->
      Format.fprintf ppf "%03d %s" reply.code first;
      List.iter (fun line -> Format.fprintf ppf " / %s" line) rest

let pp_response_error ppf = function
  | `Unexpected reply -> Format.fprintf ppf "unexpected SMTP reply: %a" pp_reply reply
  | `Bad_reply line -> Format.fprintf ppf "malformed SMTP reply: %s" line
  | `Data_refused reply -> Format.fprintf ppf "DATA refused: %a" pp_reply reply
  | `Tls_refused reply -> Format.fprintf ppf "STARTTLS refused: %a" pp_reply reply
  | `Auth_refused reply -> Format.fprintf ppf "authentication refused: %a" pp_reply reply
  | `Auth_required -> Format.pp_print_string ppf "authentication required"

let pp_error ppf = function
  | (`Unexpected _ | `Bad_reply _ | `Data_refused _ | `Tls_refused _ | `Auth_refused _) as
    error ->
      pp_response_error ppf error
  | `Auth_required -> pp_response_error ppf `Auth_required
  | `Timeout -> Format.pp_print_string ppf "SMTP operation timed out"
  | `Closed -> Format.pp_print_string ppf "SMTP peer closed the connection"
  | `Tls failure ->
      Format.fprintf ppf "TLS handshake failed: %a" Tls.Engine.pp_failure failure
  | `Net message -> Format.fprintf ppf "SMTP network connection failed: %s" message
  | `No_recipients -> Format.pp_print_string ppf "SMTP message has no recipients"
  | `Invalid_address value -> Format.fprintf ppf "invalid SMTP address: %s" value
  | `Header_injection field -> Format.fprintf ppf "SMTP header injection in %s" field

let close t =
  if t.closed then Lwt.return_unit
  else begin
    t.closed <- true;
    Lwt.catch
      (fun () ->
        Lwt.bind (Lwt_io.close t.oc) (fun () ->
            Lwt.bind (Lwt_io.close t.ic) (fun () ->
                if t.via_tls then Lwt.return_unit else Lwt_unix.close t.fd)))
      (fun _ -> Lwt.return_unit)
  end

let map_io_exception = function
  | Lwt_unix.Timeout -> `Timeout
  | End_of_file -> `Closed
  | Unix.Unix_error ((Unix.ECONNRESET | Unix.EPIPE), _, _) -> `Closed
  | Unix.Unix_error (error, _, _) -> `Net (Unix.error_message error)
  | Tls_lwt.Tls_failure failure -> `Tls failure
  | Tls_lwt.Tls_alert alert -> `Tls (`Alert alert)
  | exn -> raise exn

let with_deadline t f =
  Lwt.catch
    (fun () -> Lwt_unix.with_timeout t.timeout f)
    (fun exn -> Lwt.return (Error (map_io_exception exn)))

let parse_reply_line line =
  if String.length line < 4 then Error (`Bad_reply line)
  else
    let is_digit c = c >= '0' && c <= '9' in
    if not (is_digit line.[0] && is_digit line.[1] && is_digit line.[2]) then
      Error (`Bad_reply line)
    else
      let separator = line.[3] in
      if separator <> ' ' && separator <> '-' then Error (`Bad_reply line)
      else
        let code =
          ((Char.code line.[0] - Char.code '0') * 100)
          + ((Char.code line.[1] - Char.code '0') * 10)
          + Char.code line.[2]
          - Char.code '0'
        in
        Ok (code, separator, String.sub line 4 (String.length line - 4))

let max_reply_line = 65536

let read_line_bounded ic =
  let buffer = Buffer.create 128 in
  let rec loop () =
    Lwt.bind (Lwt_io.read_char_opt ic) (fun character ->
        match character with
        | None ->
            Lwt.return
              (if Buffer.length buffer = 0 then None else Some (Buffer.contents buffer))
        | Some '\n' ->
            let text = Buffer.contents buffer in
            let text =
              if String.length text > 0 && text.[String.length text - 1] = '\r' then
                String.sub text 0 (String.length text - 1)
              else text
            in
            Lwt.return (Some text)
        | Some character ->
            Buffer.add_char buffer character;
            if Buffer.length buffer > max_reply_line then
              Lwt.fail (Failure "SMTP reply line exceeds 64 KiB")
            else loop ())
  in
  loop ()

let read_reply t =
  with_deadline t (fun () ->
      let rec loop expected lines =
        Lwt.bind (read_line_bounded t.ic) (fun line_opt ->
            match line_opt with
            | None -> Lwt.return (Error `Closed)
            | Some line -> (
                match parse_reply_line line with
                | Error error -> Lwt.return (Error error)
                | Ok (code, separator, text) ->
                    let expected = Option.value expected ~default:code in
                    if code <> expected then Lwt.return (Error (`Bad_reply line))
                    else if separator = '-' then loop (Some expected) (text :: lines)
                    else Lwt.return (Ok { code; lines = List.rev (text :: lines) })))
      in
      loop None [])

let write_raw t data =
  with_deadline t (fun () ->
      Lwt.bind (Lwt_io.write t.oc data) (fun () -> Lwt.return (Ok ())))

let write_command t command = write_raw t (command ^ "\r\n")

open Lwt_result.Syntax

let exchange t command =
  let* () = write_command t command in
  read_reply t

let require_code expected reply =
  if reply.code = expected then Ok ()
  else if reply.code = 530 || reply.code = 538 then Error `Auth_required
  else Error (`Unexpected reply)

let normalize_crlf text =
  let b = Buffer.create (String.length text + 16) in
  let rec loop index =
    if index = String.length text then ()
    else
      match text.[index] with
      | '\r' ->
          Buffer.add_string b "\r\n";
          if index + 1 < String.length text && text.[index + 1] = '\n' then
            loop (index + 2)
          else loop (index + 1)
      | '\n' ->
          Buffer.add_string b "\r\n";
          loop (index + 1)
      | c ->
          Buffer.add_char b c;
          loop (index + 1)
  in
  loop 0;
  Buffer.contents b

let drop_last_empty list =
  match List.rev list with "" :: tail -> List.rev tail | _ -> list

let dot_stuffed body =
  let normalized = normalize_crlf body in
  let lines =
    String.split_on_char '\n' normalized
    |> List.map (fun line ->
        if String.ends_with ~suffix:"\r" line then
          String.sub line 0 (String.length line - 1)
        else line)
  in
  let lines = drop_last_empty lines in
  let stuffed =
    lines
    |> List.map (fun line ->
        if String.length line > 0 && line.[0] = '.' then "." ^ line else line)
    |> String.concat "\r\n"
  in
  if stuffed = "" then ".\r\n" else stuffed ^ "\r\n.\r\n"

let capability_lines reply =
  let normalize line = String.uppercase_ascii (String.trim line) in
  List.map normalize reply.lines

let has_capability capabilities name =
  List.exists
    (fun line ->
      line = name
      || String.length line > String.length name
         && String.sub line 0 (String.length name) = name
         && line.[String.length name] = ' ')
    capabilities

let ehlo t hostname =
  let* reply = exchange t ("EHLO " ^ hostname) in
  if reply.code = 250 then begin
    t.capabilities <- capability_lines reply;
    Lwt.return (Ok ())
  end
  else
    let* reply = exchange t ("HELO " ^ hostname) in
    if reply.code = 250 then begin
      t.capabilities <- [];
      Lwt.return (Ok ())
    end
    else Lwt.return (Error (`Unexpected reply))

let make_tls_config hostname =
  match Ca_certs.authenticator () with
  | Error (`Msg message) -> Error (`Bad_reply ("TLS authenticator: " ^ message))
  | Ok authenticator -> (
      let host = Domain_name.host_exn (Domain_name.of_string_exn hostname) in
      match Tls.Config.client ~authenticator ~peer_name:host () with
      | Error (`Msg message) -> Error (`Bad_reply ("TLS configuration: " ^ message))
      | Ok config -> Ok config)

let upgrade_tls t ~hostname ?tls_config () =
  with_deadline t (fun () ->
      match
        match tls_config with
        | Some config -> Ok config
        | None -> make_tls_config hostname
      with
      | Error error -> Lwt.return (Error error)
      | Ok config ->
          let host = Domain_name.host_exn (Domain_name.of_string_exn hostname) in
          Lwt.bind (Tls_lwt.Unix.client_of_fd config ~host t.fd) (fun tls_session ->
              let ic, oc = Tls_lwt.of_t tls_session in
              t.ic <- ic;
              t.oc <- oc;
              t.via_tls <- true;
              Lwt.return (Ok ())))

let initialize t ~security ~hostname ?tls_config () =
  let greeting () =
    let* reply = read_reply t in
    if reply.code = 220 then Lwt.return (Ok ())
    else Lwt.return (Error (`Unexpected reply))
  in
  let ehlo_and_starttls () =
    let* () = greeting () in
    let* () = ehlo t hostname in
    match security with
    | Plain -> Lwt.return (Ok ())
    | Tls -> assert false
    | Starttls ->
        if not (has_capability t.capabilities "STARTTLS") then
          Lwt.return
            (Error (`Tls_refused { code = 250; lines = [ "STARTTLS not advertised" ] }))
        else
          let* response = exchange t "STARTTLS" in
          if response.code <> 220 then Lwt.return (Error (`Tls_refused response))
          else
            let* () = upgrade_tls t ~hostname ?tls_config () in
            ehlo t hostname
  in
  match security with
  | Tls ->
      let* () = upgrade_tls t ~hostname ?tls_config () in
      let* () = greeting () in
      ehlo t hostname
  | Plain | Starttls -> ehlo_and_starttls ()

let resolve host port =
  Lwt.catch
    (fun () ->
      Lwt.bind
        (Lwt_unix.getaddrinfo host (string_of_int port)
           [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ])
        (fun results ->
          match results with
          | { Unix.ai_family; ai_addr; _ } :: _ -> Lwt.return (Ok (ai_family, ai_addr))
          | [] -> Lwt.return (Error (`Bad_reply "SMTP address lookup failed"))))
    (fun _ -> Lwt.return (Error (`Bad_reply "SMTP address lookup failed")))

let connect ~host ~port ~security ?hostname ?(timeout = 30.) ?tls_config () =
  let hostname = Option.value hostname ~default:host in
  Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout timeout (fun () ->
          let* family, sockaddr = resolve host port in
          let fd = Lwt_unix.socket family Unix.SOCK_STREAM 0 in
          Lwt.bind (Lwt_unix.connect fd sockaddr) (fun () ->
              let ic =
                Lwt_io.of_fd ~mode:Lwt_io.Input ~close:(fun () -> Lwt.return_unit) fd
              in
              let oc =
                Lwt_io.of_fd ~mode:Lwt_io.Output ~close:(fun () -> Lwt.return_unit) fd
              in
              let session =
                {
                  ic;
                  oc;
                  fd;
                  via_tls = false;
                  timeout;
                  capabilities = [];
                  closed = false;
                }
              in
              (* An unusable handshake must not leak the socket: the session is
                 closed before the error reaches the caller, as before the cutover. *)
              Lwt.bind (initialize session ~security ~hostname ?tls_config ()) (function
                | Ok () -> Lwt.return (Ok session)
                | Error _ as error ->
                    Lwt.bind (close session) (fun () -> Lwt.return error)))))
    (function
      | Lwt_unix.Timeout -> Lwt.return (Error `Timeout)
      | Unix.Unix_error (Unix.ECONNRESET, _, _) -> Lwt.return (Error `Closed)
      | Unix.Unix_error (error, _, _) ->
          Lwt.return (Error (`Net (Unix.error_message error)))
      | exn -> Lwt.fail exn)

let auth_plain t ~user ~pass =
  let payload = Base64.encode_string ("\000" ^ user ^ "\000" ^ pass) in
  let* reply = exchange t ("AUTH PLAIN " ^ payload) in
  if reply.code = 235 then Lwt.return (Ok ())
  else if reply.code = 334 then
    let* final = exchange t payload in
    if final.code = 235 then Lwt.return (Ok ())
    else Lwt.return (Error (`Auth_refused final))
  else Lwt.return (Error (`Auth_refused reply))

let auth_login t ~user ~pass =
  let user64 = Base64.encode_string user in
  let pass64 = Base64.encode_string pass in
  let* challenge = exchange t "AUTH LOGIN" in
  if challenge.code <> 334 then Lwt.return (Error (`Auth_refused challenge))
  else
    let* challenge = exchange t user64 in
    if challenge.code <> 334 then Lwt.return (Error (`Auth_refused challenge))
    else
      let* reply = exchange t pass64 in
      if reply.code = 235 then Lwt.return (Ok ())
      else Lwt.return (Error (`Auth_refused reply))

let authenticate t (user, pass) =
  Lwt.bind (auth_plain t ~user ~pass) (function
    | Ok () -> Lwt.return (Ok ())
    | Error (`Auth_refused _) -> auth_login t ~user ~pass
    | Error error -> Lwt.return (Error error))

let has_envelope_control value =
  String.exists (function '\000' | '\r' | '\n' | '<' | '>' -> true | _ -> false) value

let valid_envelope_address value =
  value <> ""
  && (not (has_envelope_control value))
  && (not (String.exists (function ' ' | '\t' -> true | _ -> false) value))
  &&
  match String.index_opt value '@' with
  | None -> false
  | Some index ->
      index > 0
      && index < String.length value - 1
      && String.index_from_opt value (index + 1) '@' = None

let rec send ?helo ?auth ~from ~recipients ~body t =
  if not (valid_envelope_address from) then Lwt.return (Error (`Invalid_address from))
  else if List.exists (fun recipient -> not (valid_envelope_address recipient)) recipients
  then Lwt.return (Error (`Invalid_address "recipient"))
  else if recipients = [] then Lwt.return (Error `No_recipients)
  else
    let re_ehlo =
      match helo with None -> Lwt.return (Ok ()) | Some name -> ehlo t name
    in
    let deliver () =
      let* () = re_ehlo in
      let* () =
        match auth with
        | None -> Lwt.return (Ok ())
        | Some credentials -> authenticate t credentials
      in
      let* mail = exchange t ("MAIL FROM:<" ^ from ^ ">") in
      let* () = Lwt.return (require_code 250 mail) in
      let rec recipients_loop = function
        | [] -> Lwt.return (Ok ())
        | recipient :: rest ->
            let* response = exchange t ("RCPT TO:<" ^ recipient ^ ">") in
            if response.code = 250 || response.code = 251 then recipients_loop rest
            else Lwt.return (Error (`Unexpected response))
      in
      let* () = recipients_loop recipients in
      let* data_response = exchange t "DATA" in
      if data_response.code <> 354 then Lwt.return (Error (`Data_refused data_response))
      else
        let* () = write_raw t (dot_stuffed body) in
        let* final_response = read_reply t in
        let* () = Lwt.return (require_code 250 final_response) in
        Lwt.return (Ok ())
    in
    Lwt.bind (deliver ()) (function
      | Error error -> Lwt.bind (close t) (fun () -> Lwt.return (Error error))
      | Ok () -> quit t)

and quit t =
  if t.closed then Lwt.return (Ok ())
  else
    Lwt.bind (exchange t "QUIT") (function
      | Error error -> Lwt.bind (close t) (fun () -> Lwt.return (Error error))
      | Ok reply when reply.code = 221 ->
          Lwt.bind (close t) (fun () -> Lwt.return (Ok ()))
      | Ok _reply ->
          Lwt.bind (close t) (fun () -> Lwt.return (Error (`Unexpected _reply))))

let deliver ~host ~port ~security ?hostname ?timeout ?tls_config ?helo ?auth ~from
    ~recipients ~body () =
  let* session = connect ~host ~port ~security ?hostname ?timeout ?tls_config () in
  send ?helo ?auth ~from ~recipients ~body session
