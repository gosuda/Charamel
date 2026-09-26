open Lwt.Infix

type error = [ Error.t | `Not_modified ]

let embedded = Catalog_data.providers
let default_base_url = "https://catwalk.charm.sh/v2/providers"

(* Upstream's catwalk client times a request out after 30 seconds. The body bound is
   {!val:Charamel_net.max_http_body}, which [Charamel_net.read_body] enforces. *)
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

let http_error ?(retryable = false) ~status ~title message =
  `Http { Error.status; title; message; retryable }

let conditional = function
  | Some etag when etag <> "" -> [ ("if-none-match", quote etag) ]
  | _ -> []

let catalog_codec = Jsont.list Provider_info.jsont

let refresh_failed status =
  http_error
    ~retryable:(Retry.retryable_status status)
    ~status ~title:"catalog refresh failed"
    (Fmt.str "unexpected status %d from the catalog endpoint" status)

(* A [200] is the only success the catalog endpoint may answer, because [fetch] returns a
   decoded list and a validator together; [Charamel_net] already reported every non-2xx
   status as an HTTP error, so a 2xx other than [200] is checked here. *)
let interpret response data =
  let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
  if status <> 200 then Error (refresh_failed status)
  else
    match Jsont_bytesrw.decode_string catalog_codec data with
    | Ok providers -> Ok (providers, etag_of response)
    | Error message ->
        Error
          (http_error ~status ~title:"unreadable catalog"
             (Fmt.str "catalog body did not decode: %s" message))

let endpoint_url base_url =
  let uri = Uri.of_string base_url in
  match Uri.path uri with
  | "" | "/" -> Uri.with_path uri "/v2/providers" |> Uri.to_string
  | _ -> base_url

let fetch ?(base_url = default_base_url) ?etag () =
  let uri = Uri.of_string (endpoint_url base_url) in
  Charamel_net.call ~timeout:timeout_s ~headers:(conditional etag) ~meth:`GET ~body:None
    uri
  >>= function
  | Error (`Http { Charamel_net.status = 304; _ }) -> Lwt.return (Error `Not_modified)
  | Error (`Http { Charamel_net.status; _ }) -> Lwt.return (Error (refresh_failed status))
  | Error ((`Transport _ | `Oauth _ | `Oauth_invalid_grant _) as error) ->
      Lwt.return (Error (`Transport (Charamel_net.error_message error)))
  | Ok (response, body) -> (
      Charamel_net.read_body body >>= function
      | Error error -> Lwt.return (Error (`Transport (Charamel_net.error_message error)))
      | Ok data -> Lwt.return (interpret response data))
