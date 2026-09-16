module Auth = Crush_core.Auth
module Config = Crush_core.Config

let oauth =
  Auth.Oauth
    {
      Charm_fantasy.Oauth.Credential.access = "access-token";
      refresh = "refresh-token";
      expires_at_ms = 9_000_000;
      account = Some "account";
    }

let with_resource f =
  Eio_main.run @@ fun env ->
  let root = Fmt.str "/tmp/crush-auth-%d-%d" (Unix.getpid ()) (Random.bits ()) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(env#fs / root);
  let root = Eio.Path.(env#fs / root) in
  let path = Eio.Path.(root / "auth.json") in
  Fun.protect
    ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
    (fun () ->
      match Auth.create ~path ~clock:env#clock () with
      | Error error ->
          Alcotest.failf "auth resource creation failed: %a" Auth.pp_error error
      | Ok resource -> f env root path resource)

let config_with provider api_key =
  let configured =
    {
      Config.kind = Config.Anthropic;
      base_url = None;
      api_key;
      headers = [];
      models = [];
    }
  in
  { Config.default with providers = [ (provider, configured) ] }

let check_api_key label expected = function
  | Ok (Some (Auth.Api_key value)) -> Alcotest.(check string) label expected value
  | Ok _ -> Alcotest.failf "%s: unexpected credential" label
  | Error error -> Alcotest.failf "%s: %a" label Auth.pp_error error

let test_codec_roundtrip () =
  let value =
    [
      ("anthropic", oauth);
      ("openai", Auth.Api_key "api-key");
      ("old", Auth.Disabled { reason = "expired"; at_ms = 12 });
    ]
  in
  let encoded = Crush_core.Jsonx.encode Auth.jsont value in
  match Crush_core.Jsonx.decode Auth.jsont encoded with
  | Error message -> Alcotest.failf "auth codec rejected its own output: %s" message
  | Ok decoded -> (
      Alcotest.(check int) "entry count" 3 (List.length decoded);
      (match List.assoc_opt "anthropic" decoded with
      | Some (Auth.Oauth credential) ->
          Alcotest.(check string)
            "access" "access-token" credential.Charm_fantasy__Oauth.Credential.access
      | _ -> Alcotest.fail "oauth entry did not round-trip");
      match List.assoc_opt "old" decoded with
      | Some (Auth.Disabled { reason; _ }) ->
          Alcotest.(check string) "reason" "expired" reason
      | _ -> Alcotest.fail "disabled entry did not round-trip")

let test_precedence () =
  with_resource (fun _env _root _path resource ->
      let config = config_with "anthropic" (Some "config-key") in
      (match Auth.set resource ~provider:"anthropic" (Auth.Api_key "stored-key") with
      | Error error -> Alcotest.failf "set failed: %a" Auth.pp_error error
      | Ok () -> ());
      let env name =
        if name = "ANTHROPIC_API_KEY" then Some "environment-key" else None
      in
      check_api_key "stored wins" "stored-key"
        (Auth.resolve resource ~config ~env ~provider:"anthropic");
      (match Auth.remove resource ~provider:"anthropic" with
      | Error error -> Alcotest.failf "remove failed: %a" Auth.pp_error error
      | Ok () -> ());
      check_api_key "configured wins over environment" "config-key"
        (Auth.resolve resource ~config ~env ~provider:"anthropic");
      let disabled = Auth.Disabled { reason = "revoked"; at_ms = 1 } in
      (match Auth.set resource ~provider:"anthropic" disabled with
      | Error error -> Alcotest.failf "disabled set failed: %a" Auth.pp_error error
      | Ok () -> ());
      match Auth.resolve resource ~config ~env ~provider:"anthropic" with
      | Ok (Some (Auth.Disabled { reason; _ })) ->
          Alcotest.(check string) "disabled wins" "revoked" reason
      | Ok _ -> Alcotest.fail "disabled credential fell through to a key"
      | Error error -> Alcotest.failf "resolve failed: %a" Auth.pp_error error)

let test_set_remove_and_reload () =
  with_resource (fun env root path resource ->
      let set credential =
        match Auth.set resource ~provider:"anthropic" credential with
        | Ok () -> ()
        | Error error -> Alcotest.failf "set failed: %a" Auth.pp_error error
      in
      set (Auth.Api_key "first");
      set (Auth.Api_key "second");
      (match Auth.find resource ~provider:"anthropic" with
      | Some (Auth.Api_key value) -> Alcotest.(check string) "replacement" "second" value
      | _ -> Alcotest.fail "replacement missing");
      let lock = Eio.Path.(root / "auth.json.lock") in
      Alcotest.(check bool)
        "stable lock file exists" true
        (Eio.Path.kind ~follow:false lock = `Regular_file);
      let lock_stat = Eio.Path.stat ~follow:false lock in
      Alcotest.(check int)
        "lock is private" 0o600
        (lock_stat.Eio.File.Stat.perm land 0o777);
      (match Auth.create ~path ~clock:env#clock () with
      | Error error ->
          Alcotest.failf "second resource creation failed: %a" Auth.pp_error error
      | Ok second -> (
          match Auth.find second ~provider:"anthropic" with
          | Some (Auth.Api_key value) ->
              Alcotest.(check string) "disk reload" "second" value
          | _ -> Alcotest.fail "second resource did not reload credential"));
      (match Auth.remove resource ~provider:"anthropic" with
      | Error error -> Alcotest.failf "remove failed: %a" Auth.pp_error error
      | Ok () -> ());
      Alcotest.(check bool) "remove" true (Auth.find resource ~provider:"anthropic" = None);
      Alcotest.(check bool)
        "file removed provider" true
        (match Auth.create ~path ~clock:env#clock () with
        | Error _ -> false
        | Ok second -> Auth.find second ~provider:"anthropic" = None))

let test_unrelated_entries_survive_reload () =
  with_resource (fun env _root path first ->
      let set resource provider credential =
        match Auth.set resource ~provider credential with
        | Ok () -> ()
        | Error error -> Alcotest.failf "set failed: %a" Auth.pp_error error
      in
      set first "anthropic" (Auth.Api_key "a");
      match Auth.create ~path ~clock:env#clock () with
      | Error error ->
          Alcotest.failf "second resource creation failed: %a" Auth.pp_error error
      | Ok second ->
          set second "openai" (Auth.Api_key "b");
          (match
             Auth.resolve first ~config:Config.default
               ~env:(fun _ -> None)
               ~provider:"openai"
           with
          | Ok (Some (Auth.Api_key value)) ->
              Alcotest.(check string) "other provider survives" "b" value
          | Ok _ -> Alcotest.fail "other provider was lost"
          | Error error -> Alcotest.failf "reload failed: %a" Auth.pp_error error);
          Alcotest.(check (list string))
            "both providers" [ "anthropic"; "openai" ] (Auth.providers second))

let test_environment_fallback_is_not_persisted () =
  with_resource (fun _env _root path resource ->
      let config = config_with "anthropic" None in
      let env name =
        if name = "ANTHROPIC_API_KEY" then Some "environment-only" else None
      in
      check_api_key "environment fallback" "environment-only"
        (Auth.resolve resource ~config ~env ~provider:"anthropic");
      Alcotest.(check bool)
        "fallback remains unpersisted" true
        (Eio.Path.kind ~follow:false path = `Not_found);
      Alcotest.(check (list string)) "no stored providers" [] (Auth.providers resource))

let cases =
  [
    Alcotest.test_case "credential codec" `Quick test_codec_roundtrip;
    Alcotest.test_case "credential precedence" `Quick test_precedence;
    Alcotest.test_case "set, remove, and reload" `Quick test_set_remove_and_reload;
    Alcotest.test_case "unrelated entries survive" `Quick
      test_unrelated_entries_survive_reload;
    Alcotest.test_case "environment fallback" `Quick
      test_environment_fallback_is_not_persisted;
  ]
