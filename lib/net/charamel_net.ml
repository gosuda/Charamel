open Lwt.Infix
module Retry = Retry
module Ssh_server = Ssh_server
module Endpoint = Cohttp_lwt_unix.Net
module Connection = Cohttp_lwt.Connection.Make (Endpoint)

type http_error = {
  status : int;
  title : string;
  message : string;
  retryable : bool;
  retry_after : float option;
}

type error =
  [ `Oauth of string
  | `Oauth_invalid_grant of string
  | `Http of http_error
  | `Transport of string ]

let max_sse_line = 64 * 1024
let sse_event_name_bound = 256
let max_sse_event = 1024 * 1024
let max_http_body = 10 * 1024 * 1024
let max_error_body = 4096
let attempt_timeout = 30.
let lower = String.lowercase_ascii
let return = Lwt.return
let return_unit = Lwt.return_unit
let transport message = Error (`Transport message)

let pp_error ppf = function
  | `Oauth message -> Fmt.pf ppf "oauth: %s" message
  | `Oauth_invalid_grant message -> Fmt.pf ppf "oauth invalid grant: %s" message
  | `Http { status; title; message; _ } -> Fmt.pf ppf "%d %s: %s" status title message
  | `Transport message -> Fmt.pf ppf "transport: %s" message

let error_message = function
  | `Oauth message | `Oauth_invalid_grant message | `Transport message -> message
  | `Http { message; _ } -> message

let retryable_error = function
  | `Http { retryable; _ } -> retryable
  | `Transport _ -> true
  | `Oauth _ | `Oauth_invalid_grant _ -> false

let header_value name headers =
  List.find_map
    (fun (key, value) -> if String.equal (lower key) name then Some value else None)
    headers

let response_headers response =
  Cohttp.Response.headers response
  |> Cohttp.Header.to_list
  |> List.map (fun (key, value) -> (lower key, value))

let body_context body =
  if String.equal body "" then "empty response body"
  else
    let total = String.length body in
    let length = min max_error_body total in
    let text = Bytes.create length in
    for index = 0 to length - 1 do
      let character = body.[index] in
      let code = Char.code character in
      Bytes.set text index (if code < 32 || code = 127 then ' ' else character)
    done;
    let text = Bytes.unsafe_to_string text in
    if total > length then text ^ "..." else text

let non_negative_number value =
  match float_of_string_opt (String.trim value) with
  | Some number when Float.is_finite number && number >= 0. -> Some number
  | _ -> None

let months =
  [ "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" ]

let month_index name =
  match List.find_index (( = ) name) months with
  | Some index -> Some (index + 1)
  | None -> None

let http_date value =
  let words =
    String.split_on_char ' ' (String.trim value)
    |> List.filter (fun word -> not (String.equal word ""))
  in
  match words with
  | [ _weekday; day; month; year; time; "GMT" ] -> (
      let time = String.split_on_char ':' time in
      match (int_of_string_opt day, month_index month, int_of_string_opt year, time) with
      | Some day, Some month, Some year, [ hour; minute; second ] -> (
          match
            (int_of_string_opt hour, int_of_string_opt minute, int_of_string_opt second)
          with
          | Some hour, Some minute, Some second ->
              Ptime.of_date_time ((year, month, day), ((hour, minute, second), 0))
              |> Option.map Ptime.to_float_s
          | _ -> None)
      | _ -> None)
  | _ -> None

let wall_clock () = Unix.gettimeofday ()

let retry_after_seconds ?(now = wall_clock ()) headers =
  let from_milliseconds =
    Option.bind (header_value "retry-after-ms" headers) non_negative_number
    |> Option.map (fun milliseconds -> milliseconds /. 1000.)
  in
  let from_seconds =
    Option.bind (header_value "retry-after" headers) (fun value ->
        match non_negative_number value with
        | Some seconds -> Some seconds
        | None -> Option.map (fun date -> Float.max 0. (date -. now)) (http_date value))
  in
  match from_milliseconds with Some _ as value -> value | None -> from_seconds

let retry_after ?now response = retry_after_seconds ?now (response_headers response)
let status_ok status = status >= 200 && status <= 299

let unix_message = function
  | Unix.ECONNREFUSED -> "connection refused"
  | Unix.ECONNRESET -> "connection reset"
  | Unix.ECONNABORTED -> "connection aborted"
  | Unix.ETIMEDOUT -> "connection timed out"
  | Unix.EHOSTUNREACH | Unix.ENETUNREACH -> "network unreachable"
  | Unix.EPIPE -> "broken pipe"
  | error -> Fmt.str "network error: %s" (Unix.error_message error)

let transport_message = function
  | End_of_file -> "unexpected end of HTTP response"
  | Lwt_unix.Timeout -> "request timed out"
  | Cohttp_lwt.Connection.Retry -> "connection closed before the response completed"
  | Tls_lwt.Tls_alert _ -> "TLS alert"
  | Tls_lwt.Tls_failure _ -> "TLS handshake failure"
  | Unix.Unix_error (error, _, _) -> unix_message error
  | exn -> Printexc.to_string exn

let guard ?(timeout_message = "request timed out") ~timeout f =
  Lwt.catch
    (fun () -> Lwt_unix.with_timeout timeout f)
    (function
      | Lwt_unix.Timeout -> return (transport timeout_message)
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn -> return (transport (transport_message exn)))

let next_chunk ~timeout ~timeout_message stream : (string option, [> error ]) result Lwt.t
    =
  Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout timeout (fun () -> Lwt_stream.get stream) >|= fun chunk ->
      Ok chunk)
    (function
      | Lwt_unix.Timeout -> return (transport timeout_message)
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn -> return (transport (transport_message exn)))

let check_timeout timeout =
  if Float.is_finite timeout && Float.compare timeout 0. > 0 then ()
  else invalid_arg "Charamel_net: timeout must be positive and finite"

let uri_error uri =
  match Uri.scheme uri with
  | None -> transport "URI has no scheme"
  | Some scheme ->
      if String.equal (lower scheme) "http" || String.equal (lower scheme) "https" then
        match Uri.host uri with
        | None | Some "" -> transport "URI has no host"
        | Some _ -> Ok ()
      else transport (Fmt.str "unsupported URI scheme: %s" scheme)

let context =
  lazy
    (Lwt.catch
       (fun () ->
         match Ca_certs.authenticator () with
         | Error (`Msg message) ->
             return (transport (Fmt.str "unable to load CA certificates: %s" message))
         | Ok authenticator ->
             Conduit_lwt_unix.init ~tls_authenticator:authenticator () >>= fun conduit ->
             return (Ok (Endpoint.init ~ctx:conduit ())))
       (fun exn -> return (transport (transport_message exn))))

let attempt ~timeout ~headers ?body meth uri =
  check_timeout timeout;
  match uri_error uri with
  | Error _ as failure -> return failure
  | Ok () -> (
      Lazy.force context >>= function
      | Error _ as failure -> return failure
      | Ok ctx ->
          let connection = ref None in
          let request () =
            let headers = Cohttp.Header.of_list headers in
            let body = Option.map Cohttp_lwt.Body.of_string body in
            Endpoint.resolve ~ctx uri >>= fun endp ->
            Connection.connect ~ctx ~persistent:false endp >>= fun conn ->
            connection := Some conn;
            Connection.call conn ~headers ?body (meth :> Http.Method.t) uri
            >|= fun (response, body) -> Ok (response, conn, body)
          in
          guard ~timeout ~timeout_message:"request timed out" request >>= fun result ->
          if Result.is_error result then Option.iter Connection.close !connection;
          return result)

let read_error_body ~timeout stream : (string, [> error ]) result Lwt.t =
  let buffer = Buffer.create 1024 in
  let rec loop () =
    if Buffer.length buffer > max_error_body then
      return (Ok "<response body omitted: too large>")
    else
      next_chunk ~timeout ~timeout_message:"timed out reading HTTP error response" stream
      >>= function
      | Error error -> return (Error error)
      | Ok None -> return (Ok (Buffer.contents buffer))
      | Ok (Some chunk) ->
          Buffer.add_string buffer chunk;
          loop ()
  in
  loop ()

let http_error ~status ~headers ~body =
  let retryable =
    Retry.retryable_status status
    ||
    match header_value "x-should-retry" headers with
    | Some value -> String.equal (lower (String.trim value)) "true"
    | None -> false
  in
  {
    status;
    title = Cohttp.Code.reason_phrase_of_code status;
    message = body_context body;
    retryable;
    retry_after = retry_after_seconds headers;
  }

let error_response ~timeout conn response status body =
  let headers = response_headers response in
  read_error_body ~timeout (Cohttp_lwt.Body.to_stream body) >>= fun message ->
  Connection.close conn;
  match message with
  | Error _ as failure -> return failure
  | Ok message -> return (Error (`Http (http_error ~status ~headers ~body:message)))

let expected_length response =
  Option.bind
    (header_value "content-length" (response_headers response))
    int_of_string_opt

(* cohttp ends a stream cleanly when the peer closes early, so a declared body length is
   checked against what actually arrived. *)
let watch_truncation response stream =
  match expected_length response with
  | None -> stream
  | Some expected ->
      let received = ref 0 in
      Lwt_stream.from (fun () ->
          Lwt_stream.get stream >>= function
          | Some chunk ->
              received := !received + String.length chunk;
              return (Some chunk)
          | None -> if !received < expected then Lwt.fail End_of_file else return None)

let call ?(timeout = attempt_timeout) ?(headers = []) ~meth ~body uri :
    (Cohttp.Response.t * string Lwt_stream.t, [> error ]) result Lwt.t =
  attempt ~timeout ~headers ?body meth uri >>= function
  | Error _ as failure -> return failure
  | Ok (response, conn, body) ->
      let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
      if not (status_ok status) then error_response ~timeout conn response status body
      else
        return (Ok (response, watch_truncation response (Cohttp_lwt.Body.to_stream body)))

let input_channel ?close stream =
  let pending = ref "" in
  let exhausted = ref false in
  let fill () =
    if String.length !pending > 0 || !exhausted then return_unit
    else
      Lwt_stream.get stream >>= function
      | Some chunk ->
          pending := chunk;
          return_unit
      | None ->
          exhausted := true;
          return_unit
  in
  let read buffer offset length =
    if length = 0 then return 0
    else
      fill () >>= fun () ->
      let take = min length (String.length !pending) in
      if take = 0 then return 0
      else (
        Lwt_bytes.blit_from_string !pending 0 buffer offset take;
        pending := String.sub !pending take (String.length !pending - take);
        return take)
  in
  Lwt_io.make ?close ~mode:Lwt_io.Input read

let call_raw ?(timeout = attempt_timeout) ?(headers = []) ~meth ~body uri :
    (Cohttp.Response.t * Lwt_io.input_channel, [> error ]) result Lwt.t =
  attempt ~timeout ~headers ?body meth uri >>= function
  | Error _ as failure -> return failure
  | Ok (response, conn, body) ->
      let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
      if not (status_ok status) then error_response ~timeout conn response status body
      else
        let close =
         fun () ->
          Connection.close conn;
          return_unit
        in
        let stream = watch_truncation response (Cohttp_lwt.Body.to_stream body) in
        return (Ok (response, input_channel ~close stream))

let read_body ?(timeout = attempt_timeout) stream : (string, [> error ]) result Lwt.t =
  let buffer = Buffer.create 8192 in
  let rec loop () =
    next_chunk ~timeout ~timeout_message:"timed out reading HTTP response body" stream
    >>= function
    | Error error -> return (Error error)
    | Ok None -> return (Ok (Buffer.contents buffer))
    | Ok (Some chunk) ->
        if Buffer.length buffer + String.length chunk > max_http_body then
          return (transport "HTTP response body exceeds 10 MiB")
        else (
          Buffer.add_string buffer chunk;
          loop ())
  in
  loop ()

type sse_state = { mutable name : string; payload : Buffer.t; mutable lines : int }
type sse_event = { event : string; data : string }

let sse_dispatch state on_event =
  if state.lines > 0 then (
    let event = if String.equal state.name "" then "message" else state.name in
    on_event { event; data = Buffer.contents state.payload };
    state.name <- "";
    Buffer.clear state.payload;
    state.lines <- 0)

let strip_trailing_cr line =
  let length = String.length line in
  if length > 0 && Char.equal line.[length - 1] '\r' then String.sub line 0 (length - 1)
  else line

let sse_overflow message = transport message

let feed_sse_line state on_event line =
  let line = strip_trailing_cr line in
  if String.equal line "" then (
    sse_dispatch state on_event;
    Ok ())
  else if Char.equal line.[0] ':' then Ok ()
  else
    let name, value =
      match String.index_opt line ':' with
      | Some position ->
          let value =
            String.sub line (position + 1) (String.length line - position - 1)
          in
          let value =
            if String.length value > 0 && Char.equal value.[0] ' ' then
              String.sub value 1 (String.length value - 1)
            else value
          in
          (String.sub line 0 position, value)
      | None -> (line, "")
    in
    match name with
    | "event" ->
        if String.length value > sse_event_name_bound then
          sse_overflow "SSE event name exceeds 256 bytes"
        else (
          state.name <- value;
          Ok ())
    | "data" ->
        let separator = if Buffer.length state.payload = 0 then 0 else 1 in
        if Buffer.length state.payload + separator + String.length value > max_sse_event
        then sse_overflow "SSE event data exceeds 1 MiB"
        else (
          if separator > 0 then Buffer.add_char state.payload '\n';
          Buffer.add_string state.payload value;
          state.lines <- state.lines + 1;
          Ok ())
    | _ -> Ok ()

let read_sse ?(timeout = attempt_timeout) stream ~on_event :
    (unit, [> error ]) result Lwt.t =
  let state = { name = ""; payload = Buffer.create 1024; lines = 0 } in
  let partial = Buffer.create 1024 in
  let feed line = feed_sse_line state on_event line in
  let rec feed_chunk chunk start : (unit, [> error ]) result =
    match String.index_from_opt chunk start '\n' with
    | Some position ->
        let length = position - start in
        if Buffer.length partial + length > max_sse_line then
          sse_overflow "SSE line exceeds 64 KiB"
        else (
          Buffer.add_string partial (String.sub chunk start length);
          let line = Buffer.contents partial in
          Buffer.clear partial;
          match feed line with
          | Ok () -> feed_chunk chunk (position + 1)
          | Error error -> Error error)
    | None ->
        let length = String.length chunk - start in
        if Buffer.length partial + length > max_sse_line then
          sse_overflow "SSE line exceeds 64 KiB"
        else (
          Buffer.add_string partial (String.sub chunk start length);
          Ok ())
  in
  let finish () : (unit, [> error ]) result =
    if Buffer.length partial = 0 then Ok ()
    else
      let line = Buffer.contents partial in
      Buffer.clear partial;
      match feed line with Ok () -> Ok () | Error error -> Error error
  in
  let rec pump () : (unit, [> error ]) result Lwt.t =
    next_chunk ~timeout ~timeout_message:"timed out waiting for an SSE event" stream
    >>= function
    | Error error -> return (Error error)
    | Ok None -> (
        match finish () with
        | Error error -> return (Error error)
        | Ok () ->
            sse_dispatch state on_event;
            return (Ok ()))
    | Ok (Some chunk) -> (
        match feed_chunk chunk 0 with
        | Ok () -> pump ()
        | Error error -> return (Error error))
  in
  pump ()

let read_line ?(bound = max_sse_line) ?(timeout = attempt_timeout) ic :
    (string option, [> error ]) result Lwt.t =
  let buffer = Buffer.create 256 in
  let line () = Ok (Some (strip_trailing_cr (Buffer.contents buffer))) in
  let rec loop read_any : (string option, [> error ]) result Lwt.t =
    if Buffer.length buffer >= bound then
      return (transport (Fmt.str "response line exceeds %d bytes" bound))
    else
      Lwt_io.read_char_opt ic >>= function
      | None -> if read_any then return (line ()) else return (Ok None)
      | Some '\n' -> return (line ())
      | Some character ->
          Buffer.add_char buffer character;
          loop true
  in
  guard ~timeout ~timeout_message:"timed out waiting for a response line" (fun () ->
      loop false)

let retry ?(policy = Retry.default) ?(sleep = Lwt_unix.sleep)
    (action : unit -> ('a, [> error ]) result Lwt.t) : ('a, [> error ]) result Lwt.t =
  (* [classify] covers the taxonomy of this library and leaves any kind a consumer added
     alone: an unknown failure is never retried. *)
  let classify error =
    match error with
    | `Http { retryable; retry_after; _ } -> (retryable, retry_after)
    | `Transport _ -> (true, None)
    | `Oauth _ | `Oauth_invalid_grant _ -> (false, None)
    | _ -> (false, None)
  in
  let rec loop attempt =
    action () >>= fun result ->
    match result with
    | Ok _ -> return result
    | Error error ->
        let retryable, retry_after = classify error in
        if retryable && attempt < policy.Retry.max then
          let delay = Retry.delay policy ~attempt:(attempt + 1) ~retry_after in
          sleep delay >>= fun () -> loop (attempt + 1)
        else return (Error error)
  in
  loop 0
