open Lwt.Infix
(* Behavior cases for the model catalog.

   The suite checks the embedded snapshot's pricing and provider
   stamping, the codec roundtrip on a fixture body, and the
   conditional response boundary of [Catalog.fetch] against the
   shared HTTP fixture server. Field paths are written in full: the
   library is wrapped, so bare [Model] does not resolve here. *)

let fixture_body path = In_channel.with_open_bin path In_channel.input_all
let fixture = fixture_body "data/catalog.json"
let catalog_codec = Jsont.list Charamel_fantasy.Provider_info.jsont

let decode body =
  match Jsont_bytesrw.decode_string catalog_codec body with
  | Ok providers -> providers
  | Error msg -> Alcotest.failf "decoding a catalog body failed: %s" msg

let encode providers =
  match Jsont_bytesrw.encode_string ~format:Jsont.Minify catalog_codec providers with
  | Ok encoded -> encoded
  | Error msg -> Alcotest.failf "encoding a catalog failed: %s" msg

let get_provider id providers =
  match
    List.find_opt
      (fun (p : Charamel_fantasy.Provider_info.t) ->
        String.equal p.Charamel_fantasy.Provider_info.id id)
      providers
  with
  | Some p -> p
  | None -> Alcotest.failf "provider %s is missing" id

let model_id (m : Charamel_fantasy.Model.t) = m.Charamel_fantasy.Model.id

let get_model id models =
  match List.find_opt (fun m -> String.equal (model_id m) id) models with
  | Some m -> m
  | None -> Alcotest.failf "model %s is missing" id

let count models = List.length (models : Charamel_fantasy.Model.t list)

(* The embedded snapshot *)

let test_embedded_shape () =
  let embedded = Charamel_fantasy.Catalog.embedded in
  Alcotest.(check int) "37 retained providers" 37 (List.length embedded);
  let cut = [ "azure"; "bedrock"; "bedrock-europe"; "vertexai" ] in
  List.iter
    (fun id ->
      Alcotest.(check bool)
        (id ^ " is cut") false
        (List.exists
           (fun (p : Charamel_fantasy.Provider_info.t) ->
             String.equal p.Charamel_fantasy.Provider_info.id id)
           embedded))
    cut;
  List.iter
    (fun p ->
      let name = p.Charamel_fantasy.Provider_info.name in
      Alcotest.(check bool)
        (name ^ " carries models") true
        (count p.Charamel_fantasy.Provider_info.models > 0))
    embedded

let test_embedded_costs () =
  let embedded = Charamel_fantasy.Catalog.embedded in
  let anthropic = get_provider "anthropic" embedded in
  let fable =
    get_model "claude-fable-5-1" anthropic.Charamel_fantasy.Provider_info.models
  in
  Alcotest.(check (float 0.)) "input cost" 10.0 fable.Charamel_fantasy.Model.cost_in;
  Alcotest.(check (float 0.)) "output cost" 50.0 fable.Charamel_fantasy.Model.cost_out;
  Alcotest.(check (float 0.))
    "cached-in prices cache creation (write)" 12.5
    fable.Charamel_fantasy.Model.cost_cache_write;
  Alcotest.(check (float 0.))
    "cached-out prices cache reads (read)" 0.25
    fable.Charamel_fantasy.Model.cost_cache_read;
  Alcotest.(check string)
    "the provider id is stamped on every model" "anthropic"
    fable.Charamel_fantasy.Model.provider

let test_embedded_provider_stamping () =
  let embedded = Charamel_fantasy.Catalog.embedded in
  List.iter
    (fun p ->
      let id = p.Charamel_fantasy.Provider_info.id in
      List.iter
        (fun m ->
          Alcotest.(check string)
            (id ^ " owns " ^ model_id m)
            id m.Charamel_fantasy.Model.provider)
        p.Charamel_fantasy.Provider_info.models)
    embedded

(* The codec *)

let test_roundtrip () =
  let providers = decode fixture in
  Alcotest.(check int) "4 fixture providers" 4 (List.length providers);
  let again = encode providers in
  Alcotest.(check string) "re-encoding is stable" (encode (decode again)) again;
  let groq = get_provider "groq" providers in
  let qwen = get_model "qwen/qwen3-32b" groq.Charamel_fantasy.Provider_info.models in
  Alcotest.(check (float 0.))
    "absent cache costs decode to zero" 0.0 qwen.Charamel_fantasy.Model.cost_cache_write;
  Alcotest.(check bool)
    "absent can_reason decodes to false" false qwen.Charamel_fantasy.Model.can_reason;
  let anthropic = get_provider "anthropic" providers in
  Alcotest.(check string)
    "the environment endpoint is kept verbatim" "$ANTHROPIC_API_ENDPOINT"
    anthropic.Charamel_fantasy.Provider_info.base_url;
  let copilot = get_provider "copilot" providers in
  Alcotest.(check int)
    "a provider without an api key still decodes" 1
    (count copilot.Charamel_fantasy.Provider_info.models);
  let openrouter = get_provider "openrouter" providers in
  Alcotest.(check string)
    "skipped members leave the base url intact" "https://openrouter.ai/api/v1"
    openrouter.Charamel_fantasy.Provider_info.base_url

(* The refresh *)

let base_url server = Uri.to_string (Fixture_http.uri server "")

let test_fetch_modified =
  Alcotest_lwt.test_case "fetch modified" `Quick (fun _switch () ->
      Fixture_http.with_server (fun server ->
          Fixture_http.respond server
            ~headers:[ ("etag", "\"6f1a2b3c4d5e6f70\"") ]
            fixture;
          Charamel_fantasy.Catalog.fetch ~base_url:(base_url server) () >>= fun result ->
          let providers, etag = Result.get_ok result in
          Alcotest.(check int) "the fixture decodes" 4 (List.length providers);
          Alcotest.(check string) "the etag comes back unquoted" "6f1a2b3c4d5e6f70" etag;
          Alcotest.(check (option string))
            "the request went to the catalog path" (Some "/v2/providers")
            (Fixture_http.last_path server);
          Alcotest.(check bool)
            "no conditional request without a cached etag" false
            (List.exists
               (fun (name, _) -> String.equal name "if-none-match")
               (Fixture_http.last_headers server));
          Lwt.return_unit))

let pp_error ppf = function
  | `Not_modified -> Fmt.string ppf "catalog not modified"
  | #Charamel_fantasy.Error.t as e -> Charamel_fantasy.Error.pp ppf e

let test_fetch_not_modified =
  Alcotest_lwt.test_case "fetch not modified" `Quick (fun _switch () ->
      Fixture_http.with_server (fun server ->
          Fixture_http.respond server ~status:304
            ~headers:[ ("etag", "\"6f1a2b3c4d5e6f70\"") ]
            "";
          Charamel_fantasy.Catalog.fetch ~base_url:(base_url server)
            ~etag:"6f1a2b3c4d5e6f70" ()
          >>= fun result ->
          (match result with
          | Error `Not_modified -> ()
          | Error e -> Alcotest.failf "expected Not_modified, got %a" pp_error e
          | Ok (_, etag) ->
              Alcotest.failf "expected Not_modified, got a catalog with etag %s" etag);
          Alcotest.(check bool)
            "the cached etag is offered with If-None-Match" true
            (List.exists
               (fun (name, value) ->
                 String.equal name "if-none-match"
                 && String.equal value "\"6f1a2b3c4d5e6f70\"")
               (Fixture_http.last_headers server));
          Lwt.return_unit))

let test_fetch_http_error =
  Alcotest_lwt.test_case "fetch http error" `Quick (fun _switch () ->
      Fixture_http.with_server (fun server ->
          Fixture_http.respond server ~status:500 ~headers:[] "boom";
          Charamel_fantasy.Catalog.fetch ~base_url:(base_url server) () >>= fun result ->
          (match result with
          | Error
              (`Http
                 ({ status; title; message; retryable } :
                   Charamel_fantasy.Error.http_error)) ->
              Alcotest.(check int) "the status travels" 500 status;
              Alcotest.(check string)
                "the title names the refresh" "catalog refresh failed" title;
              Alcotest.(check string)
                "the message names the endpoint"
                "unexpected status 500 from the catalog endpoint" message;
              Alcotest.(check bool) "a 5xx is retryable" true retryable
          | Error e -> Alcotest.failf "expected an HTTP failure, got %a" pp_error e
          | Ok _ -> Alcotest.fail "expected an HTTP failure");
          Lwt.return_unit))

let test_fetch_unreadable_body =
  Alcotest_lwt.test_case "fetch unreadable body" `Quick (fun _switch () ->
      Fixture_http.with_server (fun server ->
          Fixture_http.respond server
            ~headers:[ ("etag", "\"6f1a2b3c4d5e6f70\"") ]
            "{\"not\": a catalog";
          Charamel_fantasy.Catalog.fetch ~base_url:(base_url server) () >>= fun result ->
          (match result with
          | Error
              (`Http ({ status; message; retryable } : Charamel_fantasy.Error.http_error))
            ->
              Alcotest.(check int) "the status is the response status" 200 status;
              Alcotest.(check bool)
                "an unreadable body is not worth a retry" false retryable;
              Alcotest.(check bool)
                "the message carries the decoder report" true
                (String.starts_with ~prefix:"catalog body did not decode" message)
          | Error e -> Alcotest.failf "expected an HTTP failure, got %a" pp_error e
          | Ok _ -> Alcotest.fail "expected an HTTP failure");
          Lwt.return_unit))

let cases =
  [
    Alcotest_lwt.test_case_sync "embedded shape" `Quick test_embedded_shape;
    Alcotest_lwt.test_case_sync "embedded costs" `Quick test_embedded_costs;
    Alcotest_lwt.test_case_sync "embedded provider stamping" `Quick
      test_embedded_provider_stamping;
    Alcotest_lwt.test_case_sync "codec roundtrip" `Quick test_roundtrip;
    test_fetch_modified;
    test_fetch_not_modified;
    test_fetch_http_error;
    test_fetch_unreadable_body;
  ]
