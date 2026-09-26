open Lwt.Infix

type t = { retry : Retry.t; base_headers : (string * string) list }

let make ?(retry = Retry.default) ?(headers = []) () = { retry; base_headers = headers }

type sse_feed = event:string -> data:string -> unit
type consume = Sse of sse_feed | String
type response = { status : int; headers : (string * string) list; body : string }
type failure = Http_failure of Error.http_error | Transport_failure of string
type attempt = Success of response | Failure of failure * float option

let lower = String.lowercase_ascii

let has_header name headers =
  List.exists (fun (key, _) -> String.equal (lower key) name) headers

let net_failure (error : Charamel_net.error) =
  Transport_failure (Charamel_net.error_message error)

let net_http_error (error : Charamel_net.http_error) =
  {
    Error.status = error.status;
    title = error.title;
    message = error.message;
    retryable = error.retryable;
  }

(* [Charamel_net.call_raw] hands back the response body as a channel that owns the
   connection, so [Lwt_io.close] releases it the moment the read ends — including when the
   read is cancelled. [Charamel_net.call] would leave an abandoned stream holding the
   connection for the process lifetime. *)
let body_chunk = 8192

let close_channel channel =
  Lwt.catch
    (fun () -> Lwt_io.close channel)
    (function Unix.Unix_error _ | End_of_file -> Lwt.return_unit | exn -> Lwt.fail exn)

(* A response that ends before its declared length reaches the reader as [End_of_file].
   Fantasy treats that as the end of the body: the events already parsed stay delivered and
   the codec reports the truncation as its own terminal [Finish], which is what
   [stream ended without a terminal event] and the per-codec close messages are for. *)
let body_stream channel =
  Lwt_stream.from (fun () ->
      Lwt.catch
        (fun () ->
          Lwt_io.read ~count:body_chunk channel >|= function
          | "" -> None
          | data -> Some data)
        (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn))

let consume_body consume status headers channel =
  let stream = body_stream channel in
  match consume with
  | Sse on_event -> (
      Charamel_net.read_sse stream ~on_event:(fun (event : Charamel_net.sse_event) ->
          on_event ~event:event.event ~data:event.data)
      >|= function
      | Ok () -> Success { status; headers; body = "" }
      | Error error -> Failure (net_failure error, None))
  | String -> (
      Charamel_net.read_body stream >|= function
      | Ok body -> Success { status; headers; body }
      | Error error -> Failure (net_failure error, None))

let response_headers response =
  Cohttp.Header.to_list (Cohttp.Response.headers response)
  |> List.map (fun (key, value) -> (lower key, value))

let one_attempt t ~url ~meth ~headers ~body ~consume =
  let request_headers =
    let headers = t.base_headers @ headers in
    match meth with
    | `POST when not (has_header "content-type" headers) ->
        ("content-type", "application/json") :: headers
    | _ -> headers
  in
  let request_body = match meth with `POST -> Some body | `GET -> None in
  Charamel_net.call_raw ~headers:request_headers
    ~meth:(meth :> Cohttp.Code.meth)
    ~body:request_body (Uri.of_string url)
  >>= function
  | Error (`Http error) ->
      Lwt.return
        (Failure (Http_failure (net_http_error error), error.Charamel_net.retry_after))
  | Error ((`Transport _ | `Oauth _ | `Oauth_invalid_grant _) as error) ->
      Lwt.return (Failure (net_failure error, None))
  | Ok (response, channel) ->
      let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
      let headers = response_headers response in
      Lwt.finalize
        (fun () -> consume_body consume status headers channel)
        (fun () -> close_channel channel)

(* [Charamel_net] resolved the response's [Retry-After] already, so a delay inside the
   policy ceiling wins over backoff exactly as the header-parsing path used to. *)
let delay retry ~attempt = function
  | Some seconds when seconds <= retry.Retry.max_delay -> seconds
  | _ -> Retry.delay retry ~attempt ~retry_after:[]

let do_call t ~url ~meth ~headers ~body ~consume =
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
  let retryable = function
    | Http_failure error -> error.Error.retryable
    | Transport_failure _ -> true
  in
  let error_of = function
    | Http_failure error -> (`Http error : Error.t)
    | Transport_failure message -> (`Transport message : Error.t)
  in
  (* A stream whose events were already handed to the caller cannot be re-issued. *)
  let may_retry () = match consume with String -> true | Sse _ -> not !delivered in
  let rec loop attempt =
    one_attempt t ~url ~meth ~headers ~body ~consume >>= function
    | Success response -> Lwt.return (Ok response)
    | Failure (failure, retry_after) ->
        if retryable failure && attempt < t.retry.Retry.max && may_retry () then begin
          let seconds = delay t.retry ~attempt:(attempt + 1) retry_after in
          Lwt_unix.sleep seconds >>= fun () -> loop (attempt + 1)
        end
        else Lwt.return (Error (error_of failure))
  in
  loop 0

let call t ~url ~meth ~headers ?(body = "") ~consume () =
  do_call t ~url ~meth ~headers ~body ~consume

let post t ~url ~headers ~body =
  call t ~url ~meth:`POST ~headers ~body ~consume:String ()
  >|= Result.map (fun response -> (response.status, response.body))
