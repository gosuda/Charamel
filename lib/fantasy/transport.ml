type t =
  | T : {
      retry : Retry.t;
      base_headers : (string * string) list;
      clock : 'clock Eio.Time.clock;
      net : 'net Eio.Net.t;
    }
      -> t

let make ?(retry = Retry.default) ?(headers = []) ~clock ~net () =
  T { retry; base_headers = headers; clock; net }

type sse_feed = event:string -> data:string -> unit
type consume = Sse of sse_feed | String
type response = { status : int; headers : (string * string) list; body : string }

type failure =
  | Http_failure of Error.http_error * (string * string) list
  | Transport_failure of string

type attempt_result = Attempt_ok of response | Attempt_failed of failure

let max_sse_line = 64 * 1024
let max_sse_event = 1024 * 1024
let max_http_body = 10 * 1024 * 1024
let max_error_body = 4096
let attempt_timeout = 30.
let lower = String.lowercase_ascii

let header_value name headers =
  List.find_map
    (fun (key, value) -> if String.equal (lower key) name then Some value else None)
    headers

let has_header name headers = Option.is_some (header_value name headers)

let body_context body =
  if body = "" then "empty response body"
  else
    let total = String.length body in
    let length = min max_error_body total in
    let text = Bytes.create length in
    for i = 0 to length - 1 do
      let c = body.[i] in
      let code = Char.code c in
      Bytes.set text i (if code < 32 || code = 127 then ' ' else c)
    done;
    let text = Bytes.unsafe_to_string text in
    if total > length then text ^ "..." else text

let http_error ~status ~headers ~body =
  let retryable =
    Retry.retryable_status status
    ||
    match header_value "x-should-retry" headers with
    | Some value -> String.equal (lower (String.trim value)) "true"
    | None -> false
  in
  {
    Error.status;
    title = Cohttp.Code.reason_phrase_of_code status;
    message = body_context body;
    retryable;
  }

type sse_state = { mutable event : string; data : Buffer.t; mutable lines : int }

let sse_dispatch state on_event =
  if state.lines > 0 then (
    let event = if state.event = "" then "message" else state.event in
    let data = Buffer.contents state.data in
    on_event ~event ~data;
    state.event <- "";
    Buffer.clear state.data;
    state.lines <- 0)

let strip_trailing_cr line =
  let length = String.length line in
  if length > 0 && line.[length - 1] = '\r' then String.sub line 0 (length - 1) else line

let feed_sse_line state on_event line =
  let line = strip_trailing_cr line in
  if line = "" then (
    sse_dispatch state on_event;
    Ok ())
  else if line.[0] = ':' then Ok ()
  else
    let name, value =
      match String.index_opt line ':' with
      | Some position ->
          let value =
            String.sub line (position + 1) (String.length line - position - 1)
          in
          let value =
            if String.length value > 0 && value.[0] = ' ' then
              String.sub value 1 (String.length value - 1)
            else value
          in
          (String.sub line 0 position, value)
      | None -> (line, "")
    in
    match name with
    | "event" ->
        if String.length value > 256 then Error "SSE event name exceeds 256 bytes"
        else (
          state.event <- value;
          Ok ())
    | "data" ->
        let separator = if Buffer.length state.data = 0 then 0 else 1 in
        if Buffer.length state.data + separator + String.length value > max_sse_event then
          Error "SSE event data exceeds 1 MiB"
        else (
          if separator > 0 then Buffer.add_char state.data '\n';
          Buffer.add_string state.data value;
          state.lines <- state.lines + 1;
          Ok ())
    | _ -> Ok ()

type sse_line = Line of string | End_of_stream | Timed_out | Too_large

let read_sse_flow ~clock flow on_event =
  let reader = Eio.Buf_read.of_flow ~max_size:(max_sse_line + 1) flow in
  let read_line () =
    try
      match
        Eio.Time.with_timeout clock attempt_timeout (fun () ->
            Ok (Eio.Buf_read.line reader))
      with
      | Ok line -> Line line
      | Error `Timeout -> Timed_out
    with
    | End_of_file -> End_of_stream
    | Eio.Buf_read.Buffer_limit_exceeded -> Too_large
  in
  let state = { event = ""; data = Buffer.create 1024; lines = 0 } in
  let rec loop () =
    match read_line () with
    | Line line -> (
        match feed_sse_line state on_event line with
        | Ok () -> loop ()
        | Error message -> Error message)
    | End_of_stream ->
        sse_dispatch state on_event;
        Ok ()
    | Timed_out -> Error "timed out waiting for an SSE event"
    | Too_large -> Error "SSE line exceeds 64 KiB"
  in
  loop ()

type body_read = Body of string | Body_too_large

let read_body ~clock flow =
  let reader = Eio.Buf_read.of_flow ~max_size:(max_http_body + 1) flow in
  let read () =
    try Ok (Body (Eio.Buf_read.take_all reader))
    with Eio.Buf_read.Buffer_limit_exceeded -> Ok Body_too_large
  in
  match Eio.Time.with_timeout clock attempt_timeout read with
  | Error `Timeout -> Error "timed out reading HTTP response body"
  | Ok Body_too_large -> Error "HTTP response body exceeds 10 MiB"
  | Ok (Body body) when String.length body <= max_http_body -> Ok body
  | Ok (Body _) -> Error "HTTP response body exceeds 10 MiB"

let read_error_body ~clock flow =
  let reader = Eio.Buf_read.of_flow ~max_size:(max_error_body + 1) flow in
  let read () =
    try Ok (Eio.Buf_read.take_all reader)
    with Eio.Buf_read.Buffer_limit_exceeded -> Ok "<response body omitted: too large>"
  in
  match Eio.Time.with_timeout clock attempt_timeout read with
  | Error `Timeout -> Error "timed out reading HTTP error response"
  | Ok body -> Ok body

let status_ok status = status >= 200 && status <= 299

let network_failure_message = function
  | Eio.Net.Connection_reset _ -> "connection reset"
  | Eio.Net.Connection_failure Eio.Net.Timeout -> "connection timed out"
  | Eio.Net.Connection_failure (Eio.Net.Refused _) -> "connection refused"
  | Eio.Net.Address_lookup_failed error ->
      "address lookup failed: " ^ Eio.Net.Getaddrinfo_error.to_message error
  | Eio.Net.Invalid_option -> "invalid network option"

exception Tls_setup_failure of string

let tls_host host =
  match Domain_name.of_string host with
  | Error (`Msg message) -> raise (Tls_setup_failure ("invalid TLS host: " ^ message))
  | Ok raw -> (
      match Domain_name.host raw with
      | Ok host -> host
      | Error (`Msg message) -> raise (Tls_setup_failure ("invalid TLS host: " ^ message))
      )

let https_for uri =
  match Uri.scheme uri with
  | Some scheme when String.equal (lower scheme) "https" ->
      let host =
        match Uri.host uri with
        | Some host -> host
        | None -> raise (Tls_setup_failure "HTTPS URL has no host")
      in
      let host =
        let length = String.length host in
        if length >= 2 && host.[0] = '[' && host.[length - 1] = ']' then
          String.sub host 1 (length - 2)
        else host
      in
      let host = tls_host host in
      let authenticator =
        match Ca_certs.authenticator () with
        | Ok authenticator -> authenticator
        | Error (`Msg message) ->
            raise (Tls_setup_failure ("unable to load CA certificates: " ^ message))
      in
      let config =
        match Tls.Config.client ~authenticator ~peer_name:host () with
        | Ok config -> config
        | Error (`Msg message) ->
            raise (Tls_setup_failure ("unable to configure TLS: " ^ message))
      in
      Some (fun _uri flow -> Tls_eio.client_of_flow config flow)
  | _ -> None

let do_call (T config) ~sw ~url ~meth ~headers ~body ~consume =
  Eio.Switch.check sw;
  let uri = Uri.of_string url in
  let delivered = ref false in
  let consume =
    match consume with
    | String -> String
    | Sse on_event ->
        Sse
          (fun ~event ~data ->
            delivered := true;
            on_event ~event ~data)
  in
  let one_attempt () =
    let https = https_for uri in
    let client = Cohttp_eio.Client.make ~https config.net in
    Eio.Switch.run (fun attempt_sw ->
        let body =
          match meth with `POST -> Some (Cohttp_eio.Body.of_string body) | `GET -> None
        in
        let request_headers =
          let headers = config.base_headers @ headers in
          match meth with
          | `POST when not (has_header "content-type" headers) ->
              ("content-type", "application/json") :: headers
          | _ -> headers
        in
        let request_headers = Cohttp.Header.of_list request_headers in
        let call_result =
          Eio.Time.with_timeout config.clock attempt_timeout (fun () ->
              Ok
                (Cohttp_eio.Client.call client ~sw:attempt_sw ~headers:request_headers
                   ?body
                   (meth :> Http.Method.t)
                   uri))
        in
        match call_result with
        | Error `Timeout -> Attempt_failed (Transport_failure "request timed out")
        | Ok (response, response_body) -> (
            let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
            let response_headers =
              Cohttp.Header.to_list (Cohttp.Response.headers response)
              |> List.map (fun (key, value) -> (lower key, value))
            in
            if status_ok status then
              match consume with
              | Sse on_event -> (
                  match read_sse_flow ~clock:config.clock response_body on_event with
                  | Ok () -> Attempt_ok { status; headers = response_headers; body = "" }
                  | Error message -> Attempt_failed (Transport_failure message))
              | String -> (
                  match read_body ~clock:config.clock response_body with
                  | Ok body -> Attempt_ok { status; headers = response_headers; body }
                  | Error message -> Attempt_failed (Transport_failure message))
            else
              match read_error_body ~clock:config.clock response_body with
              | Ok body ->
                  Attempt_failed
                    (Http_failure
                       ( http_error ~status ~headers:response_headers ~body,
                         response_headers ))
              | Error message -> Attempt_failed (Transport_failure message)))
  in
  let run_attempt () =
    try one_attempt () with
    | Eio.Cancel.Cancelled _ as ex -> raise ex
    | Eio.Io (Eio.Net.E error, _) ->
        Attempt_failed (Transport_failure (network_failure_message error))
    | Tls_eio.Tls_alert _ -> Attempt_failed (Transport_failure "TLS alert")
    | Tls_eio.Tls_failure _ -> Attempt_failed (Transport_failure "TLS handshake failure")
    | Eio.Time.Timeout -> Attempt_failed (Transport_failure "request timed out")
    | End_of_file -> Attempt_failed (Transport_failure "unexpected end of HTTP response")
    | Tls_setup_failure message -> Attempt_failed (Transport_failure message)
  in
  let failure_error = function
    | Http_failure (error, _) -> (`Http error : Error.t)
    | Transport_failure message -> (`Transport message : Error.t)
  in
  let failure_retry_after = function
    | Http_failure (_, headers) -> headers
    | Transport_failure _ -> []
  in
  let failure_retryable = function
    | Http_failure (error, _) -> error.Error.retryable
    | Transport_failure _ -> true
  in
  let may_retry =
    match consume with String -> fun () -> true | Sse _ -> fun () -> not !delivered
  in
  let rec loop attempt =
    match run_attempt () with
    | Attempt_ok response -> Ok response
    | Attempt_failed failure ->
        if failure_retryable failure && attempt < config.retry.Retry.max && may_retry ()
        then (
          let delay =
            Retry.delay config.retry ~attempt:(attempt + 1)
              ~now:(Eio.Time.now config.clock) ~retry_after:(failure_retry_after failure)
          in
          Eio.Time.sleep config.clock delay;
          loop (attempt + 1))
        else Error (failure_error failure)
  in
  loop 0

let call (T _ as transport) ~sw ~url ~meth ~headers ?(body = "") ~consume () =
  do_call transport ~sw ~url ~meth ~headers ~body ~consume

let post t ~sw ~url ~headers ~body =
  Result.map
    (fun response -> (response.status, response.body))
    (call t ~sw ~url ~meth:`POST ~headers ~body ~consume:String ())
