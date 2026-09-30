open Lwt.Infix
module W = Charamel_ssh_wish
module K = Charamel_ssh_keygen
module Flow = Charamel_net.Ssh_server.Flow

let fail_result context = function
  | Ok value -> value
  | Error message -> Alcotest.failf "%s: %s" context message

let now () = Mtime.of_uint64_ns (Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.))

let write flow data =
  if String.length data = 0 then Lwt.return_unit
  else
    Flow.write flow (Cstruct.of_string data) >>= function
    | Ok () -> Lwt.return_unit
    | Error `Closed -> Lwt.fail End_of_file

let write_all flow data = Lwt_list.iter_s (write flow) data

(* The protocol's replies must go out before the next event is acted on, so a write that
   finds the wire gone cannot be postponed; it is reported instead, and the caller still
   gets the outcome from the events it had already decoded. *)
let write_replies flow data =
  Lwt.catch
    (fun () -> write_all flow data >|= fun () -> false)
    (function End_of_file -> Lwt.return true | exn -> Lwt.fail exn)

let key_from_seed seed =
  Awa.Hostkey.Ed25519_priv (Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets seed))

let awa_of key = key_from_seed (Option.get (K.ed25519_seed key))

type counter_model = { count : int }
type counter_msg = Key of Charamel_tea.Key.t | Stream_tick

let counter_app _session : (counter_model, counter_msg) Charamel_tea.app =
  {
    Charamel_tea.init = (fun () -> ({ count = 0 }, Charamel_tea.Cmd.none));
    update =
      (fun message model ->
        match message with
        | Stream_tick -> ({ count = model.count + 1 }, Charamel_tea.Cmd.quit)
        | Key key -> (
            match key.Charamel_tea.Key.code with
            | Charamel_tea.Key.Char code when Uchar.equal code (Uchar.of_char 'q') ->
                (model, Charamel_tea.Cmd.quit)
            | Charamel_tea.Key.Char code when Uchar.equal code (Uchar.of_char 'k') ->
                ({ count = model.count + 1 }, Charamel_tea.Cmd.none)
            | _ -> (model, Charamel_tea.Cmd.none)));
    view = (fun model -> Charamel_tea.View.v (Fmt.str "count: %d" model.count));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
  }

type exec_model = { status : int option }
type exec_msg = Exec_exit of int

let exec_app _session : (exec_model, exec_msg) Charamel_tea.app =
  {
    Charamel_tea.init =
      (fun () ->
        ( { status = None },
          Charamel_tea.Cmd.exec ~argv:[ "sh"; "-c"; "tty; printf in-session; exit 3" ]
            (fun code -> Exec_exit code) ));
    update =
      (fun message _model ->
        match message with
        | Exec_exit code -> ({ status = Some code }, Charamel_tea.Cmd.quit));
    view =
      (fun model ->
        Charamel_tea.View.v (Fmt.str "exec:%d" (Option.value ~default:(-1) model.status)));
    subscriptions = (fun _ -> Charamel_tea.Sub.none);
  }

let test_env =
  let cwd = Sys.getcwd () in
  {
    Charamel_cli.Env.cwd;
    fs_root = cwd;
    stdin = Lwt_io.stdin;
    stdout = Lwt_io.stdout;
    stderr = Lwt_io.stderr;
    clock = Charamel_os.Time.lwt;
  }

let counter_handler = W.tea ~env:test_env counter_app (fun _session -> Lwt.return_unit)

let stream_handler =
  W.tea_with_stream ~env:test_env
    (fun session -> (counter_app session, Lwt_stream.of_list [ Stream_tick ]))
    (fun _session -> Lwt.return_unit)

let exec_handler = W.tea ~env:test_env exec_app (fun _session -> Lwt.return_unit)

let send_request client flow request =
  let client, wire =
    fail_result "client request"
      (Awa.Client.outgoing_request client ~want_reply:true request)
  in
  write flow wire >>= fun () -> Lwt.return client

let send_q client flow =
  let client, wires = fail_result "client data" (Awa.Client.outgoing_data client "q") in
  write_all flow wires >>= fun () -> Lwt.return client

let pty_request = Awa.Ssh.Pty_req ("xterm", 80l, 24l, 0l, 0l, "")

let request_only request client flow =
  send_request client flow request >>= fun client -> send_q client flow

let request_with_pty request client flow =
  send_request client flow pty_request >>= fun client -> request_only request client flow

let shell_setup = request_with_pty Awa.Ssh.Shell
let bare_shell_setup = request_only Awa.Ssh.Shell
let subsystem_setup name = request_with_pty (Awa.Ssh.Subsystem name)
let exec_setup command = request_with_pty (Awa.Ssh.Exec command)

(* One read is not one packet: the kernel may hand back a partial SSH packet, which the
   client decoder reports as a parse failure rather than as needing more input. Keep
   reading until the buffer decodes. *)
let rec decode client flow buffer attempts =
  match Awa.Client.incoming client (now ()) buffer with
  | Ok result -> Lwt.return result
  | Error message -> (
      if attempts <= 0 then Alcotest.failf "client incoming: %s" message
      else
        Flow.read flow >>= function
        | Ok (`Data data) ->
            decode client flow (buffer ^ Cstruct.to_string data) (attempts - 1)
        | Ok `Eof | Error _ -> Lwt.fail End_of_file)

let rec step ~setup client flow output established status =
  Flow.read flow >>= function
  | Ok (`Data data) ->
      let incoming = Cstruct.to_string data in
      decode client flow incoming 16 >>= fun (client, replies, events) ->
      write_replies flow replies >>= fun gone ->
      Lwt_list.fold_left_s
        (fun (client, established, status, disconnected) event ->
          match event with
          | `Established _ when not established ->
              setup client flow >>= fun client ->
              Lwt.return (client, true, status, disconnected)
          | `Channel_data (_, data) ->
              Buffer.add_string output data;
              Lwt.return (client, established, status, disconnected)
          | `Channel_exit_status (_, code) ->
              Lwt.return (client, established, Some (Int32.to_int code), disconnected)
          | `Channel_stderr (_, data) ->
              Buffer.add_string output data;
              Lwt.return (client, established, status, disconnected)
          | `Disconnected -> Lwt.return (client, established, status, true)
          | `Channel_eof _ | `Established _ ->
              Lwt.return (client, established, status, disconnected))
        (client, established, status, false)
        events
      >>= fun (client, established, status, disconnected) ->
      if gone || disconnected then
        Lwt.return (client, output, Option.value status ~default:1)
      else client_loop ~setup client flow output established status
  | Ok `Eof -> Lwt.return (client, output, Option.value status ~default:1)
  | Error error -> Lwt.fail (Unix.Unix_error (error, "read", ""))

and client_loop ~setup client flow output established status =
  (* The peer may close before our next read or write; once the wire is gone the last
     event-derived status is the outcome. *)
  Lwt.catch
    (fun () -> step ~setup client flow output established status)
    (function
      | End_of_file | Unix.Unix_error _ ->
          Lwt.return (client, output, Option.value status ~default:1)
      | exn -> Lwt.fail exn)

let run_connection_case ~name ?(chain = Fun.id) ?(endpoint = counter_handler)
    ?(client_awa = awa_of (K.generate K.Ed25519)) ?(setup = shell_setup) ?banner_handler
    ~expect_code ~expect_text ?expect_absent () =
  let server_key = K.generate K.Ed25519 in
  let server_public = Awa.Hostkey.pub_of_priv (awa_of server_key) in
  let path = Fmt.str "/tmp/charamel-wish-%d-%s.sock" (Unix.getpid ()) name in
  let unlink_if_present () =
    try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  unlink_if_present ();
  Lwt.finalize
    (fun () ->
      let switch = Lwt_switch.create () in
      let (_ : unit Lwt.t) =
        W.serve ~stop:switch ~host_key:server_key ~addr:(`Unix path) ?banner_handler
          ~public_key_auth:(fun ~user:_ public_key ->
            Awa.Hostkey.pub_eq public_key (Awa.Hostkey.pub_of_priv client_awa))
          (chain endpoint) ()
      in
      let rec connect_when_ready () =
        let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
        Lwt.catch
          (fun () ->
            Lwt_unix.connect fd (Lwt_unix.ADDR_UNIX path) >>= fun () -> Lwt.return fd)
          (fun exn ->
            Lwt_unix.close fd >>= fun () ->
            match exn with
            | Unix.Unix_error ((Unix.ECONNREFUSED | Unix.ENOENT), _, _) ->
                Lwt_unix.sleep 0.01 >>= fun () -> connect_when_ready ()
            | exn -> Lwt.fail exn)
      in
      Lwt_unix.with_timeout 2. (fun () -> connect_when_ready ()) >>= fun fd ->
      let flow = Flow.create fd in
      let client, initial =
        Awa.Client.make ~authenticator:(`Key server_public) ~user:"tester"
          (`Pubkey client_awa)
      in
      write_all flow initial >>= fun () ->
      let output = Buffer.create 256 in
      client_loop ~setup client flow output false None
      >>= fun (_client, output, status) ->
      let output = Buffer.contents output in
      Alcotest.(check int) "exit status" expect_code status;
      List.iter
        (fun needle ->
          Alcotest.(check bool)
            "response" true
            (Test_support.contains ~needle ~haystack:output))
        expect_text;
      (match expect_absent with
      | None -> ()
      | Some needle ->
          Alcotest.(check bool)
            "absent" false
            (Test_support.contains ~needle ~haystack:output));
      (* The peer may already have torn the descriptor down. *)
      Lwt.catch
        (fun () -> Lwt_unix.close fd)
        (function
          | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit | exn -> Lwt.fail exn)
      >>= fun () ->
      Lwt_switch.turn_off switch >>= fun () -> Lwt.return_unit)
    (fun () ->
      unlink_if_present ();
      Lwt.return_unit)

let authenticated_case () =
  run_connection_case ~name:"authenticated" ~expect_code:0 ~expect_text:[ "count: 0" ] ()

let rejected_case () =
  let other = key_from_seed (Option.get (K.ed25519_seed (K.generate K.Ed25519))) in
  run_connection_case ~name:"rejected"
    ~chain:(W.password_or_key_auth ~authorized:[ Awa.Hostkey.pub_of_priv other ])
    ~expect_code:1 ~expect_text:[ "Access denied" ] ()

let blob_of key = List.nth (String.split_on_char ' ' (K.authorized_key key)) 1

let authorized_keys_case () =
  let key = K.generate K.Ed25519 in
  let other = K.generate K.Ed25519 in
  let text =
    String.concat "\n"
      [
        "# a comment line";
        "   ";
        K.authorized_key ~comment:{|laptop|} key;
        Fmt.str {|command="echo hi",no-pty ssh-ed25519 %s phone|} (blob_of other);
        Fmt.str {|ssh-rsa %s|} (blob_of key);
        {|ssh-ed25519 not!base64 someone|};
      ]
  in
  match K.parse_authorized_keys text with
  | [ plain; guarded ] ->
      Alcotest.(check (option string)) "plain options" None plain.options;
      Alcotest.(check string) "plain type" {|ssh-ed25519|} plain.type_name;
      Alcotest.(check string) "plain blob" (K.public_blob key) plain.blob;
      Alcotest.(check string) "plain comment" {|laptop|} plain.comment;
      Alcotest.(check (option string))
        "guarded options" (Some {|command="echo hi",no-pty|}) guarded.options;
      Alcotest.(check string) "guarded blob" (K.public_blob other) guarded.blob;
      Alcotest.(check string) "guarded comment" {|phone|} guarded.comment
  | entries -> Alcotest.failf "expected two entries, got %d" (List.length entries)

let authorized_keys_bare_case () =
  let key = K.generate K.Ed25519 in
  match K.parse_authorized_keys (K.authorized_key key) with
  | [ entry ] ->
      Alcotest.(check string) "bare comment" "" entry.comment;
      Alcotest.(check string) "bare blob" (K.public_blob key) entry.blob
  | entries -> Alcotest.failf "expected one entry, got %d" (List.length entries)

let authorized_keys_wire_case ~name ~line ~client_awa ~expect_code ~expect_text =
  Test_support.with_temp_dir (fun dir ->
      let path = Filename.concat dir "authorized_keys" in
      let document = String.concat "\n" [ "# a server file"; line; {|not a key line|} ] in
      Lwt_io.with_file ~mode:Lwt_io.output path (fun channel ->
          Lwt_io.write channel (document ^ "\n"))
      >>= fun () ->
      run_connection_case ~name
        ~chain:(W.authorized_keys_file ~path)
        ~client_awa ~expect_code ~expect_text ())

let listed_key_case () =
  let client_key = K.generate K.Ed25519 in
  authorized_keys_wire_case ~name:"authkeys-listed"
    ~line:(K.authorized_key ~comment:{|client|} client_key)
    ~client_awa:(awa_of client_key) ~expect_code:0 ~expect_text:[ "count: 0" ]

let absent_key_case () =
  authorized_keys_wire_case ~name:"authkeys-absent"
    ~line:(K.authorized_key ~comment:{|other|} (K.generate K.Ed25519))
    ~client_awa:(awa_of (K.generate K.Ed25519))
    ~expect_code:1 ~expect_text:[ "Access denied" ]

let blob_oracle_case () =
  let rsa =
    match
      Awa.Hostkey.pub_of_priv (Awa.Keys.of_seed `Rsa ~bits:2048 (String.make 32 'r'))
    with
    | Awa.Hostkey.Rsa_pub pub -> pub
    | Ed25519_pub _ -> Alcotest.fail "expected an rsa public key"
  in
  let ed25519 =
    Mirage_crypto_ec.Ed25519.pub_of_priv
      (Result.get_ok (Mirage_crypto_ec.Ed25519.priv_of_octets (String.make 32 'k')))
  in
  Alcotest.(check string)
    "rsa blob"
    (Awa.Wire.blob_of_pubkey (Awa.Hostkey.Rsa_pub rsa))
    (K.wire_rsa_blob ~e:rsa.Mirage_crypto_pk.Rsa.e ~n:rsa.Mirage_crypto_pk.Rsa.n);
  Alcotest.(check string)
    "ed25519 blob"
    (Awa.Wire.blob_of_pubkey (Awa.Hostkey.Ed25519_pub ed25519))
    (K.wire_ed25519_blob (Mirage_crypto_ec.Ed25519.pub_to_octets ed25519))

let write_overwrite_smoke () =
  Test_support.with_temp_dir (fun dir ->
      let path = Filename.concat dir "id" in
      let key_a = K.generate K.Ed25519 in
      let key_b = K.generate K.Ed25519 in
      K.write ~fs_root:dir ~path key_a >>= fun created ->
      Alcotest.(check bool) "create" (Result.is_ok created) true;
      K.write ~fs_root:dir ~path key_b >>= fun refused ->
      Alcotest.(check bool) "exclusive refuses" (Result.is_error refused) true;
      K.write ~fs_root:dir ~path ~overwrite:true key_b >>= fun replaced ->
      Alcotest.(check bool) "overwrite accepts" (Result.is_ok replaced) true;
      K.load_or_generate ~fs_root:dir ~path K.Ed25519 >>= fun loaded ->
      let fingerprint =
        match loaded with Ok (key, `Loaded) -> K.fingerprint_sha256 key | _ -> "none"
      in
      Alcotest.(check string) "replaced body" (K.fingerprint_sha256 key_b) fingerprint;
      Unix.mkdir (Filename.concat dir "sub") 0o755;
      K.write ~fs_root:dir ~path:"sub" key_a >>= fun directory ->
      Alcotest.(check bool) "directory refused" (Result.is_error directory) true;
      Charamel_os.Fs.read_dir dir >|= function
      | Ok names ->
          Alcotest.(check (list string))
            "no residue" [ "id"; "id.pub"; "sub" ]
            (List.sort String.compare names)
      | Error message -> Alcotest.failf "read dir: %s" message)

let session_output_case () =
  let endpoint session =
    W.Session.print session "ab" >>= fun () ->
    W.Session.write_string session "cd" >>= fun written ->
    W.Session.print_line session (string_of_int written) >>= fun () ->
    W.Session.print session (string_of_bool (W.emulated_pty session)) >>= fun () ->
    W.Session.fatal session "boom"
  in
  run_connection_case ~name:"session-output" ~endpoint ~expect_code:1
    ~expect_text:[ "abcd2"; "true"; "boom" ] ()

let subsystem_case () =
  let echo session = W.Session.print_line session "sub ok" in
  run_connection_case ~name:"subsystem"
    ~chain:(W.subsystem [ ("echo", echo) ])
    ~setup:(subsystem_setup "echo") ~expect_code:0 ~expect_text:[ "sub ok" ] ()

let unknown_subsystem_case () =
  run_connection_case ~name:"subsystem-unknown"
    ~chain:
      (W.subsystem [ ("echo", fun session -> W.Session.print_line session "sub ok") ])
    ~setup:(subsystem_setup "nope") ~expect_code:1
    ~expect_text:[ "unknown subsystem: nope" ] ()

let allow_commands_case () =
  run_connection_case ~name:"allow-commands" ~chain:(W.allow_commands [ "echo" ])
    ~setup:(exec_setup "ls") ~expect_code:1
    ~expect_text:[ "Command is not allowed: ls" ]
    ()

let interactive_passes_case () =
  run_connection_case ~name:"allow-commands-interactive"
    ~chain:(W.allow_commands [ "echo" ]) ~expect_code:0 ~expect_text:[ "count: 0" ] ()

let comment_case () =
  run_connection_case ~name:"comment" ~chain:(W.comment "done") ~expect_code:0
    ~expect_text:[ "count: 0"; "done" ] ()

let recover_case () =
  run_connection_case ~name:"recover" ~chain:W.recover
    ~endpoint:(fun _session -> failwith "boom")
    ~expect_code:1 ~expect_text:[] ()

let rate_limit_custom_case () =
  run_connection_case ~name:"rate-custom"
    ~chain:(W.rate_limit_custom (fun _session -> false))
    ~expect_code:1
    ~expect_text:[ "rate limit exceeded, please try again later" ]
    ()

let elapsed_format_case () =
  run_connection_case ~name:"elapsed-format"
    ~chain:(W.elapsed_with_format "took %.1f")
    ~expect_code:0 ~expect_text:[ "took 0." ] ~expect_absent:"elapsed time" ()

let active_term_case () =
  run_connection_case ~name:"active-term" ~chain:W.active_term ~setup:bare_shell_setup
    ~expect_code:1 ~expect_text:[ "Requires an active PTY" ] ()

let banner_handler_case () =
  run_connection_case ~name:"banner-handler"
    ~banner_handler:(fun session ->
      if W.emulated_pty session then Some "welcome" else None)
    ~expect_code:0 ~expect_text:[ "count: 0" ] ()

let tea_with_stream_case () =
  run_connection_case ~name:"stream" ~endpoint:stream_handler ~expect_code:0
    ~expect_text:[ "count: 1" ] ()

let slave_prefix = if Charamel_os__Os_platform.is_macos then "/dev/ttys" else "/dev/pts"

let allocate_pty_case () =
  let endpoint session =
    let report = function
      | Ok code -> W.Session.print_line session (Fmt.str "status:%d" code)
      | Error (`Failed message) ->
          W.Session.print_line session (Fmt.str "failed:%s" message)
    in
    W.command session "sh" [ "-c"; "tty" ] >>= report >>= fun () ->
    W.Session.print_line session (Fmt.str "emulated:%b" (W.emulated_pty session))
    >>= fun () -> W.Session.exit session 0
  in
  run_connection_case ~name:"allocate-pty" ~chain:W.allocate_pty ~endpoint ~expect_code:0
    ~expect_text:[ slave_prefix; "status:0"; "emulated:false" ]
    ()

let tea_exec_case () =
  run_connection_case ~name:"tea-exec" ~chain:W.allocate_pty ~endpoint:exec_handler
    ~expect_code:0
    ~expect_text:[ slave_prefix; "in-session"; "exec:3" ]
    ()

let token_bucket_case () =
  let allow, retained = W.token_bucket ~max_entries:8 ~per_second:1. ~burst:2 () in
  Alcotest.(check bool) "first token" true (allow "a");
  Alcotest.(check bool) "second token" true (allow "a");
  Alcotest.(check bool) "drained" false (allow "a");
  Alcotest.(check int) "one entry" 1 (retained ());
  for index = 0 to 2999 do
    ignore (allow (string_of_int index))
  done;
  Alcotest.(check int) "bounded entries" 8 (retained ());
  Alcotest.(check bool) "evicted key restarts full" true (allow "a");
  Lwt.return_unit

let token_refill_case () =
  let allow, _ = W.token_bucket ~per_second:1000. ~burst:1 () in
  Alcotest.(check bool) "first token" true (allow "k");
  Alcotest.(check bool) "drained" false (allow "k");
  Lwt_unix.sleep 0.02 >>= fun () ->
  Alcotest.(check bool) "refilled" true (allow "k");
  Lwt.return_unit

let logging_case () =
  let saved_level = Logs.level () in
  let saved_reporter = Logs.reporter () in
  let buffer = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buffer in
  Logs.set_level ~all:true (Some Logs.Info);
  Logs.set_reporter (Logs.format_reporter ~dst:ppf ());
  Lwt.finalize
    (fun () ->
      run_connection_case ~name:"logging" ~chain:W.logging ~expect_code:0
        ~expect_text:[ "count: 0" ] ())
    (fun () ->
      Format.pp_print_flush ppf ();
      Logs.set_reporter saved_reporter;
      Logs.set_level ~all:true saved_level;
      Lwt.return_unit)
  >|= fun () ->
  let text = Buffer.contents buffer in
  List.iter
    (fun needle ->
      Alcotest.(check bool) "logged" true (Test_support.contains ~needle ~haystack:text))
    [ "connect user="; "disconnect user=" ]

let posix_only () = if Sys.win32 then Alcotest.skip ()

let lwt_case name test_case =
  Alcotest_lwt.test_case name `Quick (fun _switch () -> test_case ())

let pure_case name test_case =
  lwt_case name (fun () ->
      test_case ();
      Lwt.return_unit)

let suites () =
  [
    ( "protocol",
      [
        lwt_case "authenticated counter" authenticated_case;
        lwt_case "session output family" session_output_case;
        lwt_case "banner handler" banner_handler_case;
        lwt_case "tea with stream" tea_with_stream_case;
      ] );
    ( "password-or-key-auth",
      [
        lwt_case "unknown key rejected" rejected_case;
        lwt_case "key listed in file" listed_key_case;
        lwt_case "key absent from file" absent_key_case;
      ] );
    ( "authorized-keys",
      [
        pure_case "document entries" authorized_keys_case;
        pure_case "bare key line" authorized_keys_bare_case;
        pure_case "blob matches awa encoder" blob_oracle_case;
        lwt_case "keygen write overwrite" write_overwrite_smoke;
      ] );
    ( "middleware",
      [
        lwt_case "subsystem handler" subsystem_case;
        lwt_case "unknown subsystem" unknown_subsystem_case;
        lwt_case "allowed command passes" interactive_passes_case;
        lwt_case "unlisted command rejected" allow_commands_case;
        lwt_case "comment line" comment_case;
        lwt_case "recover exits one" recover_case;
        lwt_case "active term required" active_term_case;
        lwt_case "logging pair" logging_case;
      ] );
    ( "rate-limit",
      [
        lwt_case "token bucket bound" token_bucket_case;
        lwt_case "token bucket refill" token_refill_case;
        lwt_case "custom limiter rejects" rate_limit_custom_case;
      ] );
    ("elapsed", [ lwt_case "custom format" elapsed_format_case ]);
    ( "pty",
      [
        lwt_case "allocated pty runs a command" (fun () ->
            posix_only ();
            allocate_pty_case ());
        lwt_case "tea exec runs in the session pty" (fun () ->
            posix_only ();
            tea_exec_case ());
      ] );
  ]

let () =
  Mirage_crypto_rng_unix.use_default ();
  Test_support.run_lwt "charamel-ssh.wish" (suites ())
