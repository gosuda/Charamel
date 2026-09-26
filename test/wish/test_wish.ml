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

let key_from_seed seed = Awa.Keys.of_seed `Ed25519 seed

type counter_model = { count : int }
type counter_msg = Key of Charamel_tea.Key.t

let counter_app _session : (counter_model, counter_msg) Charamel_tea.app =
  {
    Charamel_tea.init = (fun () -> ({ count = 0 }, Charamel_tea.Cmd.none));
    update =
      (fun (Key key) model ->
        match key.Charamel_tea.Key.code with
        | Charamel_tea.Key.Char code when Uchar.equal code (Uchar.of_char 'q') ->
            (model, Charamel_tea.Cmd.quit)
        | Charamel_tea.Key.Char code when Uchar.equal code (Uchar.of_char 'k') ->
            ({ count = model.count + 1 }, Charamel_tea.Cmd.none)
        | _ -> (model, Charamel_tea.Cmd.none));
    view = (fun model -> Charamel_tea.View.v (Fmt.str "count: %d" model.count));
    subscriptions = (fun _ -> Charamel_tea.Sub.key (fun key -> Key key));
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

let send_request client flow request =
  let client, wire =
    fail_result "client request"
      (Awa.Client.outgoing_request client ~want_reply:true request)
  in
  write flow wire >>= fun () -> Lwt.return client

let send_q client flow =
  let client, wires = fail_result "client data" (Awa.Client.outgoing_data client "q") in
  write_all flow wires >>= fun () -> Lwt.return client

let send_channel_setup client flow =
  send_request client flow (Awa.Ssh.Pty_req ("xterm", 80l, 24l, 0l, 0l, ""))
  >>= fun client ->
  send_request client flow Awa.Ssh.Shell >>= fun client -> send_q client flow

let rec step client flow output established status =
  Flow.read flow >>= function
  | Ok (`Data data) ->
      let incoming = Cstruct.to_string data in
      let client, replies, events =
        fail_result "client incoming" (Awa.Client.incoming client (now ()) incoming)
      in
      write_replies flow replies >>= fun gone ->
      Lwt_list.fold_left_s
        (fun (client, established, status, disconnected) event ->
          match event with
          | `Established _ when not established ->
              send_channel_setup client flow >>= fun client ->
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
      else client_loop client flow output established status
  | Ok `Eof -> Lwt.return (client, output, Option.value status ~default:1)
  | Error error -> Lwt.fail (Unix.Unix_error (error, "read", ""))

and client_loop client flow output established status =
  (* The peer may close before our next read or write; once the wire is gone the last
     event-derived status is the outcome. *)
  Lwt.catch
    (fun () -> step client flow output established status)
    (function
      | End_of_file | Unix.Unix_error _ ->
          Lwt.return (client, output, Option.value status ~default:1)
      | exn -> Lwt.fail exn)

let run_connection_case ~name ~authorized ~client_key ~expect_code ~expect_text =
  let server_key = K.generate K.Ed25519 in
  let server_awa = key_from_seed (Option.get (K.ed25519_seed server_key)) in
  let server_public = Awa.Hostkey.pub_of_priv server_awa in
  let client_awa = key_from_seed (Option.get (K.ed25519_seed client_key)) in
  let path = Fmt.str "/tmp/charamel-wish-%d-%s.sock" (Unix.getpid ()) name in
  let unlink_if_present () =
    try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  unlink_if_present ();
  Lwt.finalize
    (fun () ->
      let switch = Lwt_switch.create () in
      let endpoint =
        (W.tea ~env:test_env (fun session -> counter_app session)) (fun _session ->
            Lwt.return_unit)
      in
      let endpoint = W.access_control ~authorized endpoint in
      let (_ : unit Lwt.t) =
        W.serve ~stop:switch ~host_key:server_key ~addr:(`Unix path)
          ~public_key_auth:(fun ~user:_ public_key ->
            Awa.Hostkey.pub_eq public_key (Awa.Hostkey.pub_of_priv client_awa))
          endpoint ()
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
      client_loop client flow output false None >>= fun (_client, output, status) ->
      let output = Buffer.contents output in
      Alcotest.(check int) "exit status" expect_code status;
      Alcotest.(check bool)
        "response" true
        (Test_support.contains ~needle:expect_text ~haystack:output);
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
  let client_key = K.generate K.Ed25519 in
  let client_awa = key_from_seed (Option.get (K.ed25519_seed client_key)) in
  run_connection_case ~name:"authenticated"
    ~authorized:[ Awa.Hostkey.pub_of_priv client_awa ]
    ~client_key ~expect_code:0 ~expect_text:"count: 0"

let rejected_case () =
  let client_key = K.generate K.Ed25519 in
  let other_key = K.generate K.Ed25519 in
  let other_awa = key_from_seed (Option.get (K.ed25519_seed other_key)) in
  run_connection_case ~name:"rejected"
    ~authorized:[ Awa.Hostkey.pub_of_priv other_awa ]
    ~client_key ~expect_code:1 ~expect_text:"Access denied"

let suites () =
  [
    ( "protocol",
      [
        Alcotest_lwt.test_case "authenticated counter" `Quick (fun _switch () ->
            authenticated_case ());
      ] );
    ( "access-control",
      [
        Alcotest_lwt.test_case "unknown key rejected" `Quick (fun _switch () ->
            rejected_case ());
      ] );
  ]

let () =
  Mirage_crypto_rng_unix.use_default ();
  Test_support.run_lwt "charamel-ssh.wish" (suites ())
