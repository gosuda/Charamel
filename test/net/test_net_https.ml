open Lwt.Infix

(* HTTPS trust-store regression: [Charamel_net] loads the CA trust anchors for
   [https] requests and never for plain [http]. Both cases below serve a
   loopback TLS endpoint with a freshly generated certificate and drive the
   real [Charamel_net.call] client, so no public network is involved. *)

(* [localhost]: conduit's native TLS path needs a DNS hostname
   ([Domain_name.t] rejects IP literals), and the resolver favors IPv4, so this
   reaches the loopback server below. The leaf carries it as a DNS SAN. *)
let cert_host = "localhost"
let cert_ip = "\127\000\000\001"

let distinguished_name cn =
  let open X509.Distinguished_name in
  [ Relative_distinguished_name.singleton (CN (Common_name.v cn)) ]

let fail_msg context = function
  | Ok value -> value
  | Error (`Msg message) -> Alcotest.failf "%s: %s" context message

let fail_signature context = function
  | Ok value -> value
  | Error error ->
      Alcotest.failf "%s: %s" context
        (Fmt.to_to_string X509.Validation.pp_signature_error error)

let validity () =
  let now =
    match Ptime.of_float_s (Unix.gettimeofday ()) with
    | Some now -> now
    | None -> Alcotest.fail "test certificate needs wall-clock time"
  in
  match
    ( Ptime.add_span now (Ptime.Span.of_int_s (-86_400)),
      Ptime.add_span now (Ptime.Span.of_int_s (30 * 86_400)) )
  with
  | Some valid_from, Some valid_until -> (valid_from, valid_until)
  | None, _ | _, None -> Alcotest.fail "test certificate validity is out of range"

(* One validity window for every certificate the fixture mints: [make_ca] and
   [sign_leaf] each sampled the wall clock, so a leaf minted just after its CA
   could end just later and X509 refused to sign it. Sharing one window keeps
   each leaf within its issuer. *)
let shared_validity : (Ptime.t * Ptime.t) Lazy.t = lazy (validity ())

(* [X509.Validation.valid_ca] only accepts an anchor whose extensions mark it a
   CA, so the fixture CA carries them. *)
let ca_extensions =
  X509.Extension.(
    add Basic_constraints
      (true, (true, None))
      (add Key_usage (true, [ `Key_cert_sign ]) empty))

let make_ca ~common_name ~serial () =
  let valid_from, valid_until = Lazy.force shared_validity in
  let subject = distinguished_name common_name in
  let priv = X509.Private_key.generate `P256 in
  let csr = fail_msg "CA request" (X509.Signing_request.create subject priv) in
  let cert =
    fail_signature "CA sign"
      (X509.Signing_request.sign csr ~valid_from ~valid_until ~extensions:ca_extensions
         ~serial priv subject)
  in
  (priv, cert)

let sign_leaf ~ca_priv ~ca_cert () =
  let valid_from, valid_until = Lazy.force shared_validity in
  let priv = X509.Private_key.generate `P256 in
  let csr =
    fail_msg "leaf request"
      (X509.Signing_request.create (distinguished_name cert_host) priv)
  in
  let names = X509.General_name.(add IP [ cert_ip ] (singleton DNS [ cert_host ])) in
  let extensions = X509.Extension.(add Subject_alt_name (false, names) empty) in
  let cert =
    fail_signature "leaf sign"
      (X509.Signing_request.sign_certificate csr ~valid_from ~valid_until ~extensions
         ~serial:"\002" ca_priv ca_cert)
  in
  (priv, cert)

type anchor = { priv : X509.Private_key.t; cert : X509.Certificate.t }

(* One anchor for the process, published through [OCAML_EXTRA_CA_CERTS]: unlike
   [SSL_CERT_FILE] it is honoured on every platform, including the Windows
   system store path. It is installed before the first [https] call, so the
   lazily built TLS context picks it up. *)
let shared_anchor : anchor Lazy.t =
  lazy
    (Mirage_crypto_rng_unix.use_default ();
     let priv, cert = make_ca ~common_name:"charamel-test-ca" ~serial:"\001" () in
     let file = Filename.temp_file "charamel-test-ca" ".pem" in
     let channel = open_out_bin file in
     output_string channel (X509.Certificate.encode_pem cert);
     close_out channel;
     Unix.putenv "OCAML_EXTRA_CA_CERTS" file;
     { priv; cert })

let has_terminator text =
  let needle = "\r\n\r\n" in
  let last = String.length text - String.length needle in
  let rec scan index =
    index <= last
    && (String.sub text index (String.length needle) = needle || scan (index + 1))
  in
  last >= 0 && scan 0

let with_tls_server ?(on_accept = fun () -> ()) ~cert ~priv f =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 0))
  >>= fun () ->
  Lwt_unix.listen socket 8;
  let port =
    match Lwt_unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> Alcotest.fail "TLS fixture is not an INET socket"
  in
  let config =
    match Tls.Config.server ~certificates:(`Single ([ cert ], priv)) () with
    | Ok config -> config
    | Error (`Msg message) -> Alcotest.failf "TLS fixture: %s" message
  in
  let accepted = ref [] in
  let close_fd fd =
    Lwt.catch
      (fun () -> Lwt_unix.close fd)
      (function
        | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit | exn -> Lwt.fail exn)
  in
  let serve fd =
    Lwt.finalize
      (fun () ->
        Lwt.catch
          (fun () ->
            Tls_lwt.Unix.server_of_fd config fd >>= fun session ->
            let reader, writer = Tls_lwt.of_t session in
            let received = Buffer.create 256 in
            let rec read_request () =
              Lwt_io.read ~count:1024 reader >>= fun chunk ->
              if String.length chunk = 0 then Lwt.return_unit
              else (
                Buffer.add_string received chunk;
                if has_terminator (Buffer.contents received) then Lwt.return_unit
                else read_request ())
            in
            read_request () >>= fun () ->
            let body = "hello" in
            let head =
              Printf.sprintf
                "HTTP/1.1 200 OK\r\ncontent-length: %d\r\nconnection: close\r\n\r\n"
                (String.length body)
            in
            Lwt_io.write writer (head ^ body) >>= fun () ->
            Lwt_io.flush writer >>= fun () -> Tls_lwt.Unix.shutdown session `read_write)
          (function
            | Tls_lwt.Tls_alert _ | Tls_lwt.Tls_failure _ | End_of_file
            | Unix.Unix_error _ | Lwt_io.Channel_closed _ | Lwt.Canceled ->
                Lwt.return_unit
            | exn -> Lwt.fail exn))
      (fun () ->
        accepted := List.filter (fun active -> active != fd) !accepted;
        close_fd fd)
  in
  let handlers = ref [] in
  let rec accept_loop () =
    Lwt.catch
      (fun () ->
        Lwt_unix.accept socket >>= fun (fd, _) ->
        accepted := fd :: !accepted;
        on_accept ();
        let handler = serve fd in
        handlers := handler :: !handlers;
        accept_loop ())
      (function
        | Unix.Unix_error (Unix.EBADF, _, _) | Lwt.Canceled -> Lwt.return_unit
        | exn -> Lwt.fail exn)
  in
  let loop = accept_loop () in
  Lwt.finalize
    (fun () -> f port)
    (fun () ->
      Lwt_unix.close socket >>= fun () ->
      loop >>= fun () ->
      Lwt_list.iter_p close_fd !accepted >>= fun () ->
      List.iter Lwt.cancel !handlers;
      Lwt.catch
        (fun () -> Lwt.join !handlers)
        (function Lwt.Canceled -> Lwt.return_unit | exn -> Lwt.fail exn))

let https_get port =
  Charamel_net.call ~meth:`GET ~body:None
    (Uri.of_string (Printf.sprintf "https://localhost:%d/v1/hello" port))

let loads_trust_store =
  Alcotest_lwt.test_case "https validates against the loaded trust store" `Quick
    (fun _switch () ->
      Mirage_crypto_rng_unix.use_default ();
      let anchor = Lazy.force shared_anchor in
      let priv, cert = sign_leaf ~ca_priv:anchor.priv ~ca_cert:anchor.cert () in
      with_tls_server ~cert ~priv (fun port ->
          https_get port >>= fun result ->
          Alcotest.(check int) "status" 200 (Net_test_support.status_code "call" result);
          Net_test_support.collect (Net_test_support.stream_of "call" result)
          >>= fun chunks ->
          Alcotest.(check string) "body" "hello" (String.concat "" chunks);
          Lwt.return_unit))

(* Both messages below come only from the [Tls_lwt] branches of
   [Charamel_net.transport_message]: a plain-HTTP path can never produce them,
   so either one proves the [https] request went through TLS validation. *)
let check_tls_failure name result =
  (match result with
  | Error (`Transport message)
    when String.equal message "TLS handshake failure" || String.equal message "TLS alert"
    ->
      ()
  | Error (`Transport message) ->
      Alcotest.failf "%s: expected a TLS failure, got transport: %s" name message
  | Error _ -> Alcotest.failf "%s: expected a TLS failure" name
  | Ok _ -> Alcotest.failf "%s: expected a TLS failure, got a response" name);
  Lwt.return_unit

let rejects_unknown_chain =
  Alcotest_lwt.test_case "https rejects a chain outside the trust store" `Quick
    (fun _switch () ->
      Mirage_crypto_rng_unix.use_default ();
      let _anchor = Lazy.force shared_anchor in
      let other_priv, other_ca =
        make_ca ~common_name:"charamel-test-other" ~serial:"\003" ()
      in
      let priv, cert = sign_leaf ~ca_priv:other_priv ~ca_cert:other_ca () in
      with_tls_server ~cert ~priv (fun port ->
          https_get port >>= fun result -> check_tls_failure "unknown chain" result))

(* The accepted socket is closed in the [serve] handshake-failure handler, so
   repeated rejected handshakes must not grow the process file-descriptor count.
   Linux-only: the count comes from [/proc/self/fd]. *)
let open_fds () =
  let dir = Unix.opendir "/proc/self/fd" in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
      let count = ref 0 in
      (try
         while true do
           ignore (Unix.readdir dir);
           incr count
         done
       with End_of_file -> ());
      !count - 2)

let rejected_handshakes_leak_no_fds =
  Alcotest_lwt.test_case "rejected handshakes leak no file descriptors" `Quick
    (fun _switch () ->
      if not (Sys.file_exists "/proc/self/fd") then Alcotest.skip ()
      else (
        Mirage_crypto_rng_unix.use_default ();
        let _anchor = Lazy.force shared_anchor in
        let other_priv, other_ca =
          make_ca ~common_name:"charamel-test-leak" ~serial:"\004" ()
        in
        let priv, cert = sign_leaf ~ca_priv:other_priv ~ca_cert:other_ca () in
        let before = open_fds () in
        with_tls_server ~cert ~priv (fun port ->
            let rec loop remaining =
              if remaining = 0 then Lwt.return_unit
              else
                https_get port >>= fun result ->
                check_tls_failure "unknown chain" result >>= fun () -> loop (remaining - 1)
            in
            loop 16)
        >>= fun () ->
        let after = open_fds () in
        Alcotest.(check bool) "no file descriptor growth" true (after <= before);
        Lwt.return_unit))

(* A bare [Lwt.wait] is not cancellable, which would strand the
   [with_tls_server] finalizer if the timeout fires while parked on it.
   Polling a flag with the cancellable [Lwt_unix.sleep] lets teardown run. *)
let wait_for_flag flag =
  let rec loop () = if !flag then Lwt.return_unit else Lwt_unix.sleep 0.01 >>= loop in
  loop ()

let stalled_handshake_does_not_block_teardown =
  Alcotest_lwt.test_case "a stalled handshake does not block fixture teardown" `Quick
    (fun _switch () ->
      Mirage_crypto_rng_unix.use_default ();
      let anchor = Lazy.force shared_anchor in
      let priv, cert = sign_leaf ~ca_priv:anchor.priv ~ca_cert:anchor.cert () in
      let client = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let accepted = ref false in
      Lwt.finalize
        (fun () ->
          Lwt_unix.with_timeout 5. (fun () ->
              with_tls_server
                ~on_accept:(fun () -> accepted := true)
                ~cert ~priv
                (fun port ->
                  Lwt_unix.connect client (Unix.ADDR_INET (Unix.inet_addr_loopback, port))
                  >>= fun () -> wait_for_flag accepted)))
        (fun () -> Lwt_unix.close client))

(* Without a cancellable wait the timeout below strands the [Lwt.finalize]
   cleanup: cancelling a bare [Lwt.wait] waiter is a no-op. *)
let cancelled_wait_still_runs_teardown =
  Alcotest_lwt.test_case "a cancelled accept wait still runs teardown" `Quick
    (fun _switch () ->
      let cleaned = ref false in
      (* [arrived] is never set: models accept not firing before the timeout. *)
      let arrived = ref false in
      let guarded =
        Lwt.finalize
          (fun () -> wait_for_flag arrived)
          (fun () ->
            cleaned := true;
            Lwt.return_unit)
      in
      Lwt.catch
        (fun () -> Lwt_unix.with_timeout 0.05 (fun () -> guarded))
        (function Lwt_unix.Timeout -> Lwt.return_unit | exn -> Lwt.fail exn)
      >>= fun () ->
      (* The finalizer lands on later iterations; yield before judging whether
         teardown ran. A parked waiter still fails after the bound. *)
      let rec wait_clean attempts =
        if !cleaned then Lwt.return_unit
        else if attempts <= 0 then Lwt.return_unit
        else Lwt.pause () >>= fun () -> wait_clean (attempts - 1)
      in
      wait_clean 100 >>= fun () ->
      Alcotest.check Alcotest.bool "teardown ran after cancel" true !cleaned;
      Lwt.return_unit)

let cases =
  [
    loads_trust_store;
    rejects_unknown_chain;
    rejected_handshakes_leak_no_fds;
    stalled_handshake_does_not_block_teardown;
    cancelled_wait_still_runs_teardown;
  ]
