type error = [ Error.t | `Not_modified ]

let pp_error ppf = function
  | `Not_modified -> Fmt.string ppf "catalog not modified"
  | #Error.t as e -> Error.pp ppf e

let embedded = Catalog_data.providers
let default_base_url = "https://catwalk.charm.sh/v2/providers"

(* A catalog is 1 MiB of JSON today; 10 MiB is the refresh bound, one
   above the largest body accepted so a body of exactly the bound
   still decodes. *)
let body_limit = (10 * 1024 * 1024) + 1

(* Upstream's catwalk client times a request out after 30 seconds. *)
let timeout_s = 30.
let quote etag = "\"" ^ etag ^ "\""

let unquote etag =
  let n = String.length etag in
  if n >= 2 && etag.[0] = '"' && etag.[n - 1] = '"' then String.sub etag 1 (n - 2)
  else etag

let etag_of response =
  match Cohttp.Header.get (Cohttp.Response.headers response) "etag" with
  | Some etag -> unquote etag
  | None -> ""

let status_code = function
  | `OK -> 200
  | `Not_modified -> 304
  | `Code code -> code
  | ( #Http.Status.informational
    | #Http.Status.success
    | #Http.Status.redirection
    | #Http.Status.client_error
    | #Http.Status.server_error ) as status ->
      Http.Status.to_int status

let pp_net_error ppf = function
  | Eio.Net.Connection_reset backend ->
      Fmt.pf ppf "connection reset (%a)" Eio.Exn.Backend.pp backend
  | Eio.Net.Connection_failure (Eio.Net.Refused backend) ->
      Fmt.pf ppf "connection refused (%a)" Eio.Exn.Backend.pp backend
  | Eio.Net.Connection_failure Eio.Net.Timeout -> Fmt.string ppf "connection timed out"
  | Eio.Net.Address_lookup_failed err ->
      Fmt.pf ppf "address lookup failed: %s" (Eio.Net.Getaddrinfo_error.to_message err)
  | Eio.Net.Invalid_option -> Fmt.string ppf "invalid socket option"

let http_error ?(retryable = false) ~status ~title message =
  `Http { Error.status; title; message; retryable }

let host_of_uri uri =
  match Uri.host uri with
  | None -> Fmt.failwith "catalog: endpoint %a has no host name" Uri.pp uri
  | Some host -> (
      match Domain_name.of_string host with
      | Ok host -> Domain_name.host_exn host
      | Error (`Msg msg) ->
          Fmt.failwith "catalog: endpoint host %s is not a domain name: %s" host msg)

(* The HTTPS connector: the endpoint's certificate chain is validated
   against the system trust anchors and its name against the host the
   URI names. TLS draws from the default random generator, which the
   binary installs at startup. *)
let https_of_uri uri flow =
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok authenticator -> authenticator
    | Error (`Msg msg) -> Fmt.failwith "catalog: no system trust anchors: %s" msg
  in
  let config =
    match Tls.Config.client ~authenticator () with
    | Ok config -> config
    | Error (`Msg msg) -> Fmt.failwith "catalog: TLS configuration failed: %s" msg
  in
  Tls_eio.client_of_flow config ~host:(host_of_uri uri) flow

let conditional = function
  | Some etag when etag <> "" -> Cohttp.Header.of_list [ ("if-none-match", quote etag) ]
  | _ -> Cohttp.Header.init ()

let catalog_codec = Jsont.list Provider_info.jsont

let interpret response data =
  let etag = etag_of response in
  match Cohttp.Response.status response with
  | `OK -> (
      match Jsont_bytesrw.decode_string catalog_codec data with
      | Ok providers -> Ok (providers, etag)
      | Error msg ->
          Error
            (http_error ~status:200 ~title:"unreadable catalog"
               (Fmt.str "catalog body did not decode: %s" msg)))
  | `Not_modified -> Error `Not_modified
  | status ->
      let code = status_code status in
      Error
        (http_error ~retryable:(Retry.retryable_status code) ~status:code
           ~title:"catalog refresh failed"
           (Fmt.str "unexpected status %d from the catalog endpoint" code))

let endpoint_url base_url =
  let uri = Uri.of_string base_url in
  match Uri.path uri with
  | "" | "/" -> Uri.with_path uri "/v2/providers" |> Uri.to_string
  | _ -> base_url

let attempt ~sw ~net ?etag base_url =
  let uri = Uri.of_string (endpoint_url base_url) in
  let client = Cohttp_eio.Client.make ~https:(Some https_of_uri) net in
  let response, body = Cohttp_eio.Client.get client ~sw ~headers:(conditional etag) uri in
  let buf = Eio.Buf_read.of_flow ~max_size:body_limit body in
  interpret response (Eio.Buf_read.take_all buf)

(* [guard thunk] runs a request step and turns the failures the
   exchange is known to raise into transport errors. *)
let guard thunk =
  try Ok (thunk ()) with
  | Eio.Io (Eio.Net.E err, _) ->
      Error (`Transport (Fmt.str "catalog request failed: %a" pp_net_error err))
  | Eio.Buf_read.Buffer_limit_exceeded ->
      Error
        (`Transport
           (Fmt.str "catalog body exceeded the %d MiB bound"
              ((body_limit - 1) / (1024 * 1024))))
  | Tls_eio.Tls_failure failure ->
      Error (`Transport (Fmt.str "TLS failure: %a" Tls.Engine.pp_failure failure))
  | Tls_eio.Tls_alert _ -> Error (`Transport "TLS alert from the catalog endpoint")
  | End_of_file -> Error (`Transport "connection closed before the response completed")
  | Failure msg -> Error (`Transport msg)

let fetch ?(base_url = default_base_url) ?etag ~net ~clock () =
  Eio.Switch.run @@ fun sw ->
  let outcome =
    match
      Eio.Time.with_timeout clock timeout_s (fun () ->
          guard (fun () -> attempt ~sw ~net ?etag base_url))
    with
    | Ok result -> result
    | Error (#error as e) -> Error e
    | Error `Timeout ->
        Error (`Transport (Fmt.str "catalog fetch exceeded %g s" timeout_s))
  in
  outcome
