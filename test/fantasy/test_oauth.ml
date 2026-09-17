(* OAuth and PKCE contract tests cover the S256 vector, begin_login
   URL parameters, extract_code forms, and refresh skew arithmetic.
   The suite is pure logic with no transport. *)

open Charamel_fantasy

let test_pkce_s256 () =
  let seed = String.make 96 '\000' in
  let verifier =
    Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet seed
  in
  let pkce = Pkce.of_verifier verifier in
  let digest = Digestif.SHA256.digest_string verifier in
  let expected =
    Base64.encode_string ~pad:false ~alphabet:Base64.uri_safe_alphabet
      (Digestif.SHA256.to_raw_string digest)
  in
  Alcotest.(check string) "S256 challenge" expected pkce.Pkce.challenge

let test_begin_login_params () =
  let login =
    Oauth.Anthropic.begin_login
      ~rng:(fun n -> String.make n '\001')
      ~redirect_uri:"http://127.0.0.1:54545/callback" ()
  in
  let uri = Uri.of_string login.Oauth.Anthropic.uri in
  Alcotest.(check string)
    "authorize origin+path" "https://claude.ai/oauth/authorize"
    ( Uri.scheme uri |> Option.value ~default:"" |> fun s ->
      s ^ "://" ^ (Uri.host uri |> Option.value ~default:"") ^ Uri.path uri );
  Alcotest.(check string)
    "client_id" Oauth.Anthropic.client_id
    ( Uri.get_query_param' uri "client_id" |> Option.value ~default:[ "" ] |> fun l ->
      List.nth l 0 );
  let param name =
    match Uri.get_query_param' uri name with Some (v :: _) -> v | _ -> ""
  in
  Alcotest.(check string)
    "scope"
    "org:create_api_key user:profile user:inference user:sessions:claude_code \
     user:mcp_servers user:file_upload"
    (param "scope");
  Alcotest.(check string) "code_challenge_method" "S256" (param "code_challenge_method");
  Alcotest.(check string) "response_type" "code" (param "response_type");
  Alcotest.(check string) "code=true" "true" (param "code");
  Alcotest.(check int) "state is 32 hex chars" 32 (String.length (param "state"));
  Alcotest.(check bool) "redirect_uri present" true (param "redirect_uri" <> "")

let test_extract_code_forms () =
  let state = "abc123" in
  let module A = Oauth.Anthropic in
  let error = Alcotest.testable Error.pp ( = ) in
  Alcotest.(check (result string error))
    "bare code" (Ok "code-123")
    (A.extract_code ~url_or_code:"code-123" ~state);
  Alcotest.(check (result string error))
    "code#state" (Ok "code-123")
    (A.extract_code ~url_or_code:"code-123#abc123" ~state);
  Alcotest.(check (result string error))
    "redirect url" (Ok "code-9")
    (A.extract_code
       ~url_or_code:"http://127.0.0.1:54545/callback?code=code-9&state=abc123" ~state);
  let mismatch =
    match A.extract_code ~url_or_code:"code-1#wrong" ~state with
    | Error _ -> true
    | Ok _ -> false
  in
  Alcotest.(check bool) "state mismatch rejected" true mismatch

let test_expiry_skew () =
  let module A = Charamel_fantasy__Oauth.Anthropic in
  (* now + expires_in*1000 - 300_000 *)
  Alcotest.(check int)
    "five minute shave" 1_000_000
    (A.compute_expires_at_ms 500_000 800 |> fun ms -> ms);
  Alcotest.(check int) "expires 3600 from 0" 3_300_000 (A.compute_expires_at_ms 0 3600)

let cases =
  [
    ("pkce s256", `Quick, test_pkce_s256);
    ("begin_login params", `Quick, test_begin_login_params);
    ("extract code forms", `Quick, test_extract_code_forms);
    ("expiry skew", `Quick, test_expiry_skew);
  ]
