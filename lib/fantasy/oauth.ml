open Lwt.Infix
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

  let invalid_grant body = token_error body = Some "invalid_grant"

  (* [Charamel_net] reads a non-2xx body under {!val:Charamel_net.max_error_body} and
     sanitizes it, so the [error] and [error_description] members a token endpoint reports
     are read back out of that excerpt. *)
  let net_error (error : Charamel_net.error) =
    match error with
    | `Oauth message -> `Oauth message
    | `Oauth_invalid_grant message -> `Oauth_invalid_grant message
    | `Transport message -> `Transport message
    | `Http { Charamel_net.status = 400; message; _ } when invalid_grant message ->
        `Oauth_invalid_grant "invalid_grant"
    | `Http http -> `Http (http_error http.Charamel_net.status http.message)

  let token_timeout = 60.

  let post_token ~headers ~body =
    let request_headers =
      ("content-type", "application/json") :: ("accept", "application/json") :: headers
    in
    Charamel_net.call ~timeout:token_timeout ~headers:request_headers ~meth:`POST
      ~body:(Some body) (Uri.of_string token_url)
    >>= function
    | Error error -> Lwt.return (Error (net_error error))
    | Ok (_response, stream) ->
        Charamel_net.read_body ~timeout:token_timeout stream
        >|= Result.map_error net_error

  let now_ms_of clock =
    let seconds = Charamel_os.Time.now clock in
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

  let credential_of_body ~now_ms ~clock body =
    Result.bind (parse_token_response body) (fun response ->
        credential_of_response (resolve_now now_ms clock) response)

  let exchange ?now_ms ~clock ~redirect_uri ~login ~code () =
    if single_param (Uri.of_string login.uri) "redirect_uri" <> Some redirect_uri then
      Lwt.return (Error (`Oauth "redirect URI differs from pending login"))
    else
      match extract_code ~url_or_code:code ~state:login.state with
      | Error _ as failure -> Lwt.return failure
      | Ok authorization_code ->
          let body =
            token_body ~grant:"authorization_code"
              ~extra:
                [
                  ("code", authorization_code);
                  ("redirect_uri", redirect_uri);
                  ("code_verifier", login.pkce.Pkce.verifier);
                  ("state", login.state);
                ]
          in
          post_token ~headers:[] ~body >>= fun response ->
          Lwt.return (Result.bind response (credential_of_body ~now_ms ~clock))

  let refresh ?now_ms ~clock credential =
    if String.trim credential.Credential.refresh = "" then
      Lwt.return (Error (`Oauth "missing refresh token"))
    else
      let body =
        token_body ~grant:"refresh_token"
          ~extra:[ ("refresh_token", credential.Credential.refresh) ]
      in
      let keep_account fresh =
        { fresh with Credential.account = credential.Credential.account }
      in
      post_token ~headers:refresh_headers ~body >>= fun response ->
      Lwt.return
        (Result.map keep_account
           (Result.bind response (credential_of_body ~now_ms ~clock)))

  let ensure_fresh ~clock credential =
    let now = now_ms_of clock in
    if
      Int64.add (Int64.of_int now) 60_000L
      >= Int64.of_int credential.Credential.expires_at_ms
    then refresh ~now_ms:now ~clock credential
    else Lwt.return (Ok credential)
end
