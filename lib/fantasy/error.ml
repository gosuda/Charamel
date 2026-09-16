type http_error = { status : int; title : string; message : string; retryable : bool }

type t =
  [ `Oauth of string
  | `Oauth_invalid_grant of string
  | `Http of http_error
  | `Transport of string ]

let pp ppf = function
  | `Oauth msg -> Fmt.pf ppf "oauth: %s" msg
  | `Oauth_invalid_grant msg -> Fmt.pf ppf "oauth invalid grant: %s" msg
  | `Http { status; title; message; _ } -> Fmt.pf ppf "%d %s: %s" status title message
  | `Transport msg -> Fmt.pf ppf "transport: %s" msg

let message = function
  | `Oauth msg -> msg
  | `Oauth_invalid_grant msg -> msg
  | `Http { message; _ } -> message
  | `Transport msg -> msg
