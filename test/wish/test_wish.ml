module W = Charm_ssh_wish
module K = Charm_ssh_keygen

exception Test_done

let fail_result context = function
  | Ok value -> value
  | Error message -> Alcotest.failf "%s: %s" context message

let now () = Mtime.of_uint64_ns (Int64.of_float (Unix.gettimeofday () *. 1_000_000_000.))

let write socket data =
  if String.length data > 0 then Eio.Flow.write socket [ Cstruct.of_string data ]

let write_all socket data = List.iter (write socket) data
let key_from_seed seed = Awa.Keys.of_seed `Ed25519 seed

type counter_model = { count : int }
type counter_msg = Key of Charm_tea.Key.t

let counter_app _session : (counter_model, counter_msg) Charm_tea.app =
  {
    Charm_tea.init = (fun () -> ({ count = 0 }, Charm_tea.Cmd.none));
    update =
      (fun (Key key) model ->
        match key.Charm_tea.Key.code with
        | Charm_tea.Key.Char code when Uchar.equal code (Uchar.of_char 'q') ->
            (model, Charm_tea.Cmd.quit)
        | Charm_tea.Key.Char code when Uchar.equal code (Uchar.of_char 'k') ->
            ({ count = model.count + 1 }, Charm_tea.Cmd.none)
        | _ -> (model, Charm_tea.Cmd.none));
    view = (fun model -> Charm_tea.View.v (Fmt.str "count: %d" model.count));
    subscriptions = (fun _ -> Charm_tea.Sub.key (fun key -> Key key));
  }

let send_request client socket request =
  let client, wire =
    fail_result "client request"
      (Awa.Client.outgoing_request client ~want_reply:true request)
  in
  write socket wire;
  client

let send_q client socket =
  let client, wires = fail_result "client data" (Awa.Client.outgoing_data client "q") in
  write_all socket wires;
  client

let rec client_loop client socket output established status =
  match step client socket output established status with
  | exception Eio.Io _ ->
      (* The peer may close before our next read or write; once the wire is
         gone the last event-derived status is the outcome. *)
      (client, output, Option.value status ~default:1)
  | result -> result

and step client socket output established status =
  let buffer = Cstruct.create 16_384 in
  let count = Eio.Flow.single_read socket buffer in
  let incoming = Cstruct.to_string ~len:count buffer in
  let client, replies, events =
    fail_result "client incoming" (Awa.Client.incoming client (now ()) incoming)
  in
  write_all socket replies;
  let client, established, status, disconnected =
    List.fold_left
      (fun (client, established, status, disconnected) event ->
        match event with
        | `Established _ when not established ->
            let client =
              send_request client socket (Awa.Ssh.Pty_req ("xterm", 80l, 24l, 0l, 0l, ""))
            in
            let client = send_request client socket Awa.Ssh.Shell in
            let client = send_q client socket in
            (client, true, status, disconnected)
        | `Channel_data (_, data) ->
            Buffer.add_string output data;
            (client, established, status, disconnected)
        | `Channel_exit_status (_, code) ->
            (client, established, Some (Int32.to_int code), disconnected)
        | `Channel_stderr (_, data) ->
            Buffer.add_string output data;
            (client, established, status, disconnected)
        | `Disconnected -> (client, established, status, true)
        | `Channel_eof _ | `Established _ -> (client, established, status, disconnected))
      (client, established, status, false)
      events
  in
  if disconnected then (client, output, Option.value status ~default:1)
  else client_loop client socket output established status

let contains ~needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec at offset =
    if offset + needle_length > haystack_length then false
    else if String.sub haystack offset needle_length = needle then true
    else at (offset + 1)
  in
  if needle_length = 0 then true else at 0

let run_connection_case ~env ~name ~authorized ~client_key ~expect_code ~expect_text =
  let server_key = K.generate K.Ed25519 in
  let server_awa = key_from_seed (Option.get (K.ed25519_seed server_key)) in
  let server_public = Awa.Hostkey.pub_of_priv server_awa in
  let client_awa = key_from_seed (Option.get (K.ed25519_seed client_key)) in
  let path = Fmt.str "/tmp/charm-wish-%d-%s.sock" (Unix.getpid ()) name in
  let unlink_if_present () =
    try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in
  unlink_if_present ();
  Fun.protect ~finally:unlink_if_present (fun () ->
      try
        Eio.Switch.run (fun sw ->
            let endpoint =
              (W.tea ~env (fun session -> counter_app session)) (fun _session -> ())
            in
            let endpoint = W.access_control ~authorized endpoint in
            Eio.Fiber.fork ~sw (fun () ->
                W.serve ~sw ~net:env#net ~clock:env#clock ~host_key:server_key
                  ~addr:(`Unix path)
                  ~public_key_auth:(fun ~user:_ public_key ->
                    Awa.Hostkey.pub_eq public_key (Awa.Hostkey.pub_of_priv client_awa))
                  endpoint);
            let rec connect_when_ready () =
              match Eio.Net.connect ~sw env#net (`Unix path) with
              | socket -> socket
              | exception Eio.Io _ ->
                  Eio.Time.sleep env#clock 0.01;
                  connect_when_ready ()
            in
            let socket = Eio.Time.with_timeout_exn env#clock 2. connect_when_ready in
            let client, initial =
              Awa.Client.make ~authenticator:(`Key server_public) ~user:"tester"
                (`Pubkey client_awa)
            in
            write_all socket initial;
            let output = Buffer.create 256 in
            let _client, output, status = client_loop client socket output false None in
            let output = Buffer.contents output in
            Alcotest.(check int) "exit status" expect_code status;
            Alcotest.(check bool) "response" true (contains ~needle:expect_text output);
            Eio.Resource.close socket;
            Eio.Fiber.yield ();
            Eio.Switch.fail sw Test_done)
      with Test_done -> ())

let authenticated_case () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let client_key = K.generate K.Ed25519 in
  let client_awa = key_from_seed (Option.get (K.ed25519_seed client_key)) in
  run_connection_case ~env ~name:"authenticated"
    ~authorized:[ Awa.Hostkey.pub_of_priv client_awa ]
    ~client_key ~expect_code:0 ~expect_text:"count: 0"

let rejected_case () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun env ->
  let client_key = K.generate K.Ed25519 in
  let other_key = K.generate K.Ed25519 in
  let other_awa = key_from_seed (Option.get (K.ed25519_seed other_key)) in
  run_connection_case ~env ~name:"rejected"
    ~authorized:[ Awa.Hostkey.pub_of_priv other_awa ]
    ~client_key ~expect_code:1 ~expect_text:"Access denied"

let suites () =
  [
    ("protocol", [ Alcotest.test_case "authenticated counter" `Quick authenticated_case ]);
    ("access-control", [ Alcotest.test_case "unknown key rejected" `Quick rejected_case ]);
  ]

let () = Alcotest.run "charm-ssh.wish" (suites ())
