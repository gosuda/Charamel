open Result.Syntax

type flow = [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t
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
  | `Net of Eio.Net.connection_failure
  | `No_recipients
  | `Invalid_address of string
  | `Header_injection of string ]

type security = Plain | Starttls | Tls

type t = {
  mutable flow : flow;
  mutable reader : Eio.Buf_read.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
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
  | `Net _ -> Format.pp_print_string ppf "SMTP network connection failed"
  | `No_recipients -> Format.pp_print_string ppf "SMTP message has no recipients"
  | `Invalid_address value -> Format.fprintf ppf "invalid SMTP address: %s" value
  | `Header_injection field -> Format.fprintf ppf "SMTP header injection in %s" field

let close t =
  if not t.closed then (
    t.closed <- true;
    try Eio.Flow.close t.flow with
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure _), _) -> ())

let map_io_exception = function
  | Eio.Time.Timeout -> `Timeout
  | End_of_file -> `Closed
  | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> `Closed
  | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure failure), _) -> `Net failure
  | Tls_eio.Tls_failure failure -> `Tls failure
  | Tls_eio.Tls_alert alert -> `Tls (`Alert alert)
  | exn -> raise exn

let with_deadline t f =
  try Eio.Time.with_timeout_exn t.clock t.timeout f
  with exn -> Error (map_io_exception exn)

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

let read_reply t =
  with_deadline t (fun () ->
      let rec loop expected lines =
        let line = Eio.Buf_read.line t.reader in
        let* code, separator, text = parse_reply_line line in
        let expected = Option.value expected ~default:code in
        if code <> expected then Error (`Bad_reply line)
        else if separator = '-' then loop (Some expected) (text :: lines)
        else
          let reply = { code; lines = List.rev (text :: lines) } in
          Ok reply
      in
      loop None [])

let write_raw t data =
  with_deadline t (fun () ->
      Eio.Flow.copy_string data t.flow;
      Ok ())

let write_command t command = write_raw t (command ^ "\r\n")

let exchange t command =
  match write_command t command with Error error -> Error error | Ok () -> read_reply t

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

let drop_last_empty = function
  | [] -> []
  | list -> (
      let rec reverse acc = function [] -> acc | x :: xs -> reverse (x :: acc) xs in
      let reversed = reverse [] list in
      match reversed with "" :: tail -> reverse [] tail | _ -> list)

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
  match exchange t ("EHLO " ^ hostname) with
  | Error error -> Error error
  | Ok reply when reply.code = 250 ->
      t.capabilities <- capability_lines reply;
      Ok ()
  | Ok _ -> (
      match exchange t ("HELO " ^ hostname) with
      | Error error -> Error error
      | Ok reply when reply.code = 250 ->
          t.capabilities <- [];
          Ok ()
      | Ok reply -> Error (`Unexpected reply))

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
      let config =
        match tls_config with
        | Some config -> Ok config
        | None -> make_tls_config hostname
      in
      let* config = config in
      let host = Domain_name.host_exn (Domain_name.of_string_exn hostname) in
      let tls_flow = Tls_eio.client_of_flow config ~host t.flow in
      t.flow <- (tls_flow :> flow);
      t.reader <- Eio.Buf_read.of_flow ~max_size:65536 t.flow;
      Ok ())

let initialize t ~security ~hostname ?tls_config () =
  let open Result.Syntax in
  let greeting () =
    let* reply = read_reply t in
    if reply.code = 220 then Ok () else Error (`Unexpected reply)
  in
  let ehlo_and_starttls () =
    let* () = greeting () in
    let* () = ehlo t hostname in
    match security with
    | Plain -> Ok ()
    | Tls -> assert false
    | Starttls ->
        if not (has_capability t.capabilities "STARTTLS") then
          Error (`Tls_refused { code = 250; lines = [ "STARTTLS not advertised" ] })
        else
          let* response = exchange t "STARTTLS" in
          if response.code <> 220 then Error (`Tls_refused response)
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

let connect ~sw ~clock ~net ~host ~port ~security ?hostname ?(timeout = 30.) ?tls_config
    () =
  let hostname = Option.value hostname ~default:host in
  let result =
    try
      let plain_flow =
        Eio.Time.with_timeout_exn clock timeout (fun () ->
            match Eio.Net.getaddrinfo_stream ~service:(string_of_int port) net host with
            | address :: _ -> Eio.Net.connect ~sw net address
            | [] -> failwith "getaddrinfo returned no stream address")
      in
      let flow = (plain_flow :> flow) in
      let session =
        {
          flow;
          reader = Eio.Buf_read.of_flow ~max_size:65536 flow;
          clock :> float Eio.Time.clock_ty Eio.Resource.t;
          timeout;
          capabilities = [];
          closed = false;
        }
      in
      match initialize session ~security ~hostname ?tls_config () with
      | Ok () -> Ok session
      | Error error ->
          close session;
          Error error
    with
    | Eio.Time.Timeout -> Error `Timeout
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure failure), _) -> Error (`Net failure)
    | Eio.Io (Eio.Net.E (Eio.Net.Address_lookup_failed _), _) ->
        Error (`Bad_reply "SMTP address lookup failed")
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> Error `Closed
  in
  result

let auth_plain t ~user ~pass =
  let payload = Base64.encode_string ("\000" ^ user ^ "\000" ^ pass) in
  match exchange t ("AUTH PLAIN " ^ payload) with
  | Ok reply when reply.code = 235 -> Ok ()
  | Ok reply when reply.code = 334 -> (
      match exchange t payload with
      | Ok final when final.code = 235 -> Ok ()
      | Ok final -> Error (`Auth_refused final)
      | Error error -> Error error)
  | Ok reply -> Error (`Auth_refused reply)
  | Error error -> Error error

let auth_login t ~user ~pass =
  let user64 = Base64.encode_string user in
  let pass64 = Base64.encode_string pass in
  match exchange t "AUTH LOGIN" with
  | Error error -> Error error
  | Ok challenge when challenge.code <> 334 -> Error (`Auth_refused challenge)
  | Ok _ -> (
      match exchange t user64 with
      | Error error -> Error error
      | Ok challenge when challenge.code <> 334 -> Error (`Auth_refused challenge)
      | Ok _ -> (
          match exchange t pass64 with
          | Error error -> Error error
          | Ok reply when reply.code = 235 -> Ok ()
          | Ok reply -> Error (`Auth_refused reply)))

let authenticate t (user, pass) =
  match auth_plain t ~user ~pass with
  | Ok () -> Ok ()
  | Error (`Auth_refused _) -> (
      match auth_login t ~user ~pass with
      | Ok () -> Ok ()
      | Error (`Auth_refused _ as second_error) -> Error second_error
      | Error error -> Error error)
  | Error error -> Error error

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
  let open Result.Syntax in
  if not (valid_envelope_address from) then Error (`Invalid_address from)
  else if List.exists (fun recipient -> not (valid_envelope_address recipient)) recipients
  then Error (`Invalid_address "recipient")
  else if recipients = [] then Error `No_recipients
  else
    let re_ehlo = match helo with None -> Ok () | Some name -> ehlo t name in
    let deliver () =
      let* () = re_ehlo in
      let* () =
        match auth with None -> Ok () | Some credentials -> authenticate t credentials
      in
      let* mail = exchange t ("MAIL FROM:<" ^ from ^ ">") in
      let* () = require_code 250 mail in
      let rec recipients_loop = function
        | [] -> Ok ()
        | recipient :: rest ->
            let* response = exchange t ("RCPT TO:<" ^ recipient ^ ">") in
            if response.code = 250 || response.code = 251 then recipients_loop rest
            else Error (`Unexpected response)
      in
      let* () = recipients_loop recipients in
      let* data_response = exchange t "DATA" in
      if data_response.code <> 354 then Error (`Data_refused data_response)
      else
        let* () = write_raw t (dot_stuffed body) in
        let* final_response = read_reply t in
        let* () = require_code 250 final_response in
        Ok ()
    in
    match deliver () with
    | Error error ->
        close t;
        Error error
    | Ok () -> quit t

and quit t =
  if t.closed then Ok ()
  else
    match exchange t "QUIT" with
    | Error error ->
        close t;
        Error error
    | Ok reply when reply.code = 221 ->
        close t;
        Ok ()
    | Ok reply ->
        close t;
        Error (`Unexpected reply)

let deliver ~sw ~clock ~net ~host ~port ~security ?hostname ?timeout ?tls_config ?helo
    ?auth ~from ~recipients ~body () =
  let* session =
    connect ~sw ~clock ~net ~host ~port ~security ?hostname ?timeout ?tls_config ()
  in
  send ?helo ?auth ~from ~recipients ~body session
