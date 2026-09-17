open Result.Syntax

module Credential = struct
  type t = {
    access : string;
    refresh : string;
    expires_at_ms : int;
    account : string option;
  }

  let pp ppf { expires_at_ms; account; _ } =
    match account with
    | Some account ->
        Fmt.pf ppf "credential (account %s, expires %d)" account expires_at_ms
    | None -> Fmt.pf ppf "credential (expires %d)" expires_at_ms
end

type error = Error.t

let pp_error = Error.pp

module Anthropic = struct
  let client_id = "9d1c250a-e61b-44d9-88ed-5944d1962f5e"
  let authorize_url = "https://claude.ai/oauth/authorize"
  let token_url = "https://platform.claude.com/v1/oauth/token"

  let scopes =
    [
      "org:create_api_key";
      "user:profile";
      "user:inference";
      "user:sessions:claude_code";
      "user:mcp_servers";
      "user:file_upload";
    ]

  type login = { pkce : Pkce.t; state : string; uri : string }

  let default_rng n = Mirage_crypto_rng.generate n

  let begin_login ?(rng = default_rng) ~redirect_uri () =
    if String.trim redirect_uri = "" then invalid_arg "OAuth redirect URI is empty";
    let pkce = Pkce.generate ~rng () in
    let entropy = rng 16 in
    if String.length entropy <> 16 then invalid_arg "OAuth RNG must return 16 bytes";
    let state =
      String.concat "" (List.init 16 (fun i -> Fmt.str "%02x" (Char.code entropy.[i])))
    in
    let uri =
      Uri.add_query_params (Uri.of_string authorize_url)
        [
          ("response_type", [ "code" ]);
          ("client_id", [ client_id ]);
          ("redirect_uri", [ redirect_uri ]);
          ("scope", [ String.concat " " scopes ]);
          ("code_challenge", [ pkce.Pkce.challenge ]);
          ("code_challenge_method", [ "S256" ]);
          ("state", [ state ]);
          ("code", [ "true" ]);
        ]
      |> Uri.to_string
    in
    { pkce; state; uri }

  let single_param uri name =
    match List.filter (fun (key, _) -> key = name) (Uri.query uri) with
    | [ (_, [ value ]) ] -> Some value
    | _ -> None

  let extract_code ~url_or_code ~state =
    let code = String.trim url_or_code in
    let invalid message = Error (`Oauth message) in
    let bare_code () =
      match String.split_on_char '#' code with
      | [ value ] when value <> "" -> Ok value
      | [ value; embedded ] when value <> "" && embedded = state -> Ok value
      | [ _; _ ] -> invalid "empty code or state mismatch"
      | _ -> invalid "invalid authorization code"
    in
    if state = "" then invalid "missing expected state"
    else
      let* uri =
        try Ok (Uri.of_string code)
        with Invalid_argument _ -> Error (`Oauth "invalid authorization code")
      in
      match Option.map String.lowercase_ascii (Uri.scheme uri) with
      | Some ("http" | "https") -> (
          if single_param uri "state" <> Some state then invalid "state mismatch"
          else
            match single_param uri "code" with
            | Some value when value <> "" -> Ok value
            | _ -> invalid "no authorization code in redirect URL")
      | _ -> bare_code ()

  let member name members = Option.map snd (Jsont.Json.find_mem name members)

  let string_member name members =
    match member name members with
    | Some (Jsont.String (value, _)) when String.trim value <> "" -> Some value
    | _ -> None

  type token_response = {
    access_token : string;
    refresh_token : string;
    expires_in : int;
    account_uuid : string option;
  }

  let parse_token_response body =
    match Jsont_bytesrw.decode_string Jsont.json body with
    | Error _ -> Error (`Oauth "malformed token response")
    | Ok (Jsont.Object (members, _)) -> (
        let expires =
          match member "expires_in" members with
          | Some (Jsont.Number (value, _))
            when Float.is_finite value && Float.is_integer value && value > 0.
                 && value < float_of_int (max_int / 1000) ->
              Some (int_of_float value)
          | _ -> None
        in
        match
          ( string_member "access_token" members,
            string_member "refresh_token" members,
            expires )
        with
        | Some access_token, Some refresh_token, Some expires_in ->
            let account_uuid =
              match member "account_uuid" members with
              | Some (Jsont.String (account, _)) when String.trim account <> "" ->
                  Some account
              | _ -> (
                  match member "account" members with
                  | Some (Jsont.Object (account, _)) -> string_member "uuid" account
                  | Some (Jsont.String (account, _)) when String.trim account <> "" ->
                      Some account
                  | _ -> None)
            in
            Ok { access_token; refresh_token; expires_in; account_uuid }
        | _ ->
            Error
              (`Oauth
                 "token response requires nonempty access_token, refresh_token and \
                  positive integral expires_in"))
    | Ok _ -> Error (`Oauth "token response is not an object")

  let refresh_headers =
    [
      ("anthropic-beta", "oauth-2025-04-20");
      ("User-Agent", "anthropic-sdk-typescript/0.112.1 userOAuthProvider");
    ]

  let token_body ~grant ~extra =
    let members =
      List.map
        (fun (name, value) ->
          Jsont.Json.mem (Jsont.Json.name name) (Jsont.Json.string value))
        (("grant_type", grant) :: ("client_id", client_id) :: extra)
    in
    match Jsont_bytesrw.encode_string Jsont.json (Jsont.Json.object' members) with
    | Ok body -> body
    | Error message -> Fmt.failwith "token body encoding failed: %s" message

  let token_error body =
    match Jsont_bytesrw.decode_string Jsont.json body with
    | Ok (Jsont.Object (members, _)) -> string_member "error" members
    | _ -> None

  let token_error_message ~status body =
    match Jsont_bytesrw.decode_string Jsont.json body with
    | Ok (Jsont.Object (members, _)) -> (
        match string_member "error_description" members with
        | Some message -> message
        | None -> (
            match string_member "error" members with
            | Some message -> message
            | None -> Fmt.str "token endpoint returned HTTP %d" status))
    | _ -> Fmt.str "token endpoint returned HTTP %d" status

  let http_error status body =
    {
      Error.status;
      title = "token endpoint";
      message = token_error_message ~status body;
      retryable = Retry.retryable_status status;
    }

  let post_token ~sw ~clock ~net ~headers ~body =
    let request config =
      let https uri flow =
        let host =
          match Uri.host uri with
          | Some host -> Domain_name.host_exn (Domain_name.of_string_exn host)
          | None -> invalid_arg "OAuth endpoint has no host"
        in
        Tls_eio.client_of_flow config ~host flow
      in
      let client = Cohttp_eio.Client.make ~https:(Some https) net in
      let headers =
        Cohttp.Header.of_list
          (("content-type", "application/json")
          :: ("accept", "application/json")
          :: headers)
      in
      let response, response_body =
        Cohttp_eio.Client.call client ~sw ~headers
          ~body:(Cohttp_eio.Body.of_string body)
          `POST (Uri.of_string token_url)
      in
      let response_body =
        Eio.Buf_read.take_all
          (Eio.Buf_read.of_flow ~max_size:((10 * 1024 * 1024) + 1) response_body)
      in
      let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
      if status >= 200 && status < 300 then Ok response_body
      else if status = 400 && token_error response_body = Some "invalid_grant" then
        Error (`Oauth_invalid_grant "invalid_grant")
      else Error (`Http (http_error status response_body))
    in
    try
      Eio.Time.with_timeout_exn clock 60. (fun () ->
          match Eio_unix.run_in_systhread (fun () -> Ca_certs.authenticator ()) with
          | Error (`Msg message) ->
              Error (`Transport (Fmt.str "cannot load TLS trust roots: %s" message))
          | Ok authenticator -> (
              match Tls.Config.client ~authenticator () with
              | Error (`Msg message) ->
                  Error (`Transport (Fmt.str "cannot configure TLS client: %s" message))
              | Ok config -> request config))
    with
    | Eio.Time.Timeout -> Error (`Transport "token request timed out")
    | Eio.Buf_read.Buffer_limit_exceeded ->
        Error (`Transport "token response exceeds 10 MiB")
    | End_of_file -> Error (`Transport "token connection closed prematurely")
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_failure _), _)
    | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _)
    | Eio.Io (Eio.Net.E (Eio.Net.Address_lookup_failed _), _)
    | Eio.Io (Eio.Net.E Eio.Net.Invalid_option, _) ->
        Error (`Transport "token connection failed")
    | Tls_eio.Tls_alert _ | Tls_eio.Tls_failure _ ->
        Error (`Transport "token TLS connection failed")

  let now_ms_of clock =
    let seconds = Eio.Time.now clock in
    if
      (not (Float.is_finite seconds))
      || seconds < 0.
      || seconds > float_of_int max_int /. 1000.
    then invalid_arg "OAuth clock timestamp is out of range";
    int_of_float (seconds *. 1000.)

  let resolve_now now_ms clock =
    match now_ms with Some ms -> ms | None -> now_ms_of clock

  let compute_expires_at_ms now expires_in =
    if now < 0 then invalid_arg "OAuth clock timestamp is negative";
    if expires_in <= 0 then invalid_arg "OAuth expiry is not positive";
    if expires_in > (max_int - now) / 1000 then invalid_arg "OAuth expiry overflows";
    now + (expires_in * 1000) - 300_000

  let credential_of_response now response =
    if now < 0 || response.expires_in <= 0 || response.expires_in > (max_int - now) / 1000
    then Error (`Oauth "token expiry is out of range")
    else
      Ok
        {
          Credential.access = response.access_token;
          refresh = response.refresh_token;
          expires_at_ms = compute_expires_at_ms now response.expires_in;
          account = response.account_uuid;
        }

  let exchange ?now_ms ~sw ~clock ~net ~redirect_uri ~login ~code () =
    let open Result.Syntax in
    if single_param (Uri.of_string login.uri) "redirect_uri" <> Some redirect_uri then
      Error (`Oauth "redirect URI differs from pending login")
    else
      let* code = extract_code ~url_or_code:code ~state:login.state in
      let body =
        token_body ~grant:"authorization_code"
          ~extra:
            [
              ("code", code);
              ("redirect_uri", redirect_uri);
              ("code_verifier", login.pkce.Pkce.verifier);
              ("state", login.state);
            ]
      in
      let* body = post_token ~sw ~clock ~net ~headers:[] ~body in
      let* response = parse_token_response body in
      credential_of_response (resolve_now now_ms clock) response

  let refresh ?now_ms ~sw ~clock ~net credential =
    let open Result.Syntax in
    if String.trim credential.Credential.refresh = "" then
      Error (`Oauth "missing refresh token")
    else
      let body =
        token_body ~grant:"refresh_token"
          ~extra:[ ("refresh_token", credential.Credential.refresh) ]
      in
      let* body = post_token ~sw ~clock ~net ~headers:refresh_headers ~body in
      let* response = parse_token_response body in
      let+ fresh = credential_of_response (resolve_now now_ms clock) response in
      { fresh with Credential.account = credential.Credential.account }

  let ensure_fresh ~sw ~clock ~net credential =
    let now = now_ms_of clock in
    if
      Int64.add (Int64.of_int now) 60_000L
      >= Int64.of_int credential.Credential.expires_at_ms
    then refresh ~now_ms:now ~sw ~clock ~net credential
    else Ok credential
end
