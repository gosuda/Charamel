open Lwt.Infix

type sse_feed = event:string -> data:string -> unit
type failure = Http_failure of Error.http_error | Transport_failure of string

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

let one_attempt ~url ~headers ~body ~on_event =
  let request_headers =
    if has_header "content-type" headers then headers
    else ("content-type", "application/json") :: headers
  in
  Charamel_net.call_raw ~headers:request_headers ~meth:`POST ~body:(Some body)
    (Uri.of_string url)
  >>= function
  | Error (`Http error) ->
      Lwt.return
        (Error (Http_failure (net_http_error error), error.Charamel_net.retry_after))
  | Error ((`Transport _ | `Oauth _ | `Oauth_invalid_grant _) as error) ->
      Lwt.return (Error (net_failure error, None))
  | Ok (_response, channel) ->
      Lwt.finalize
        (fun () ->
          Charamel_net.read_sse (body_stream channel)
            ~on_event:(fun (event : Charamel_net.sse_event) ->
              on_event ~event:event.event ~data:event.data)
          >|= function
          | Ok () -> Ok ()
          | Error error -> Error (net_failure error, None))
        (fun () -> close_channel channel)

(* [Charamel_net] resolved the response's [Retry-After] already, so a delay inside the
   policy ceiling wins over backoff exactly as the header-parsing path used to. *)
let delay ~attempt = function
  | Some seconds when seconds <= Retry.default.Retry.max_delay -> seconds
  | _ -> Retry.delay Retry.default ~attempt ~retry_after:[]

let call ~url ~headers ~body ~on_event =
  let delivered = ref false in
  let on_event ~event ~data =
    delivered := true;
    on_event ~event ~data
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
  let rec loop attempt =
    one_attempt ~url ~headers ~body ~on_event >>= function
    | Ok () -> Lwt.return (Ok ())
    | Error (failure, retry_after) ->
        if retryable failure && attempt < Retry.default.Retry.max && not !delivered then begin
          let seconds = delay ~attempt:(attempt + 1) retry_after in
          Lwt_unix.sleep seconds >>= fun () -> loop (attempt + 1)
        end
        else Lwt.return (Error (error_of failure))
  in
  loop 0
