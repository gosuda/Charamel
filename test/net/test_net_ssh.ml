open Lwt.Infix

let pair () = Lwt_unix.socketpair Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0

let read_from fd =
  let buffer = Bytes.create 128 in
  Lwt_unix.read fd buffer 0 128 >|= fun received -> Bytes.sub_string buffer 0 received

let flow_write flow text =
  Charamel_net.Ssh_server.Flow.write flow (Cstruct.of_string text) >>= function
  | Ok () -> Lwt.return_unit
  | Error `Closed -> Alcotest.fail "the flow rejected a write to a live socket"

let flow_read flow =
  Charamel_net.Ssh_server.Flow.read flow >>= function
  | Ok (`Data chunk) -> Lwt.return (Cstruct.to_string chunk)
  | Ok `Eof -> Alcotest.fail "the flow saw end of input"
  | Error error ->
      Alcotest.failf "the flow read failed: %a" Charamel_net.Ssh_server.Flow.pp_error
        error

let round_trip =
  Alcotest_lwt.test_case "round-trips bytes through the flow adapter" `Quick
    (fun _switch () ->
      let left, right = pair () in
      let flow = Charamel_net.Ssh_server.Flow.create left in
      flow_write flow "hello flow" >>= fun () ->
      read_from right >>= fun received ->
      Alcotest.(check string) "peer read" "hello flow" received;
      Lwt_unix.write_string right "pong" 0 4 >>= fun _written ->
      flow_read flow >|= fun echoed -> Alcotest.(check string) "flow read" "pong" echoed)

let partial_writes =
  Alcotest_lwt.test_case "loops over partial writes" `Quick (fun _switch () ->
      let size = 131_072 in
      let left, right = pair () in
      let flow = Charamel_net.Ssh_server.Flow.create left in
      let payload = Bytes.create size in
      Bytes.fill payload 0 size 'k';
      let received = Bytes.create size in
      let rec drain count =
        if count >= size then Lwt.return count
        else
          Lwt_unix.read right received count (size - count) >>= fun got ->
          drain (count + got)
      in
      Lwt.both
        (Charamel_net.Ssh_server.Flow.write flow (Cstruct.of_bytes payload))
        (drain 0)
      >>= fun (written, count) ->
      (match written with
      | Ok () -> ()
      | Error `Closed -> Alcotest.fail "the flow closed during a bulk write");
      Alcotest.(check int) "received" size count;
      Alcotest.(check string)
        "payload" (Bytes.to_string payload)
        (Bytes.sub_string received 0 count);
      Lwt.return_unit)

let writev_order =
  Alcotest_lwt.test_case "keeps buffer order on writev" `Quick (fun _switch () ->
      let left, right = pair () in
      let flow = Charamel_net.Ssh_server.Flow.create left in
      Charamel_net.Ssh_server.Flow.writev flow
        [ Cstruct.of_string "ab"; Cstruct.of_string "cd"; Cstruct.of_string "ef" ]
      >>= function
      | Error `Closed -> Alcotest.fail "the flow rejected a vector write"
      | Ok () ->
          read_from right >|= fun received ->
          Alcotest.(check string) "order" "abcdef" received)

let closed_peer =
  Alcotest_lwt.test_case "reads end of input once the peer is gone" `Quick
    (fun _switch () ->
      let left, right = pair () in
      let flow = Charamel_net.Ssh_server.Flow.create left in
      Lwt_unix.close right >>= fun () ->
      Charamel_net.Ssh_server.Flow.read flow >>= function
      | Ok `Eof -> Lwt.return_unit
      | Ok (`Data chunk) ->
          Alcotest.failf "expected end of input, read %d bytes" (Cstruct.length chunk)
      | Error error ->
          Alcotest.failf "the flow read failed: %a" Charamel_net.Ssh_server.Flow.pp_error
            error)

let half_close =
  Alcotest_lwt.test_case "half-closes the write direction" `Quick (fun _switch () ->
      let left, right = pair () in
      let flow = Charamel_net.Ssh_server.Flow.create left in
      flow_write flow "first" >>= fun () ->
      Charamel_net.Ssh_server.Flow.shutdown flow `write >>= fun () ->
      read_from right >>= fun received ->
      Alcotest.(check string) "peer drained" "first" received;
      read_from right >|= fun rest -> Alcotest.(check string) "peer sees end" "" rest)

let idempotent_close =
  Alcotest_lwt.test_case "closes a flow twice without an error" `Quick (fun _switch () ->
      let left, _right = pair () in
      let flow = Charamel_net.Ssh_server.Flow.create left in
      Charamel_net.Ssh_server.Flow.close flow >>= fun () ->
      Charamel_net.Ssh_server.Flow.close flow)

let banner =
  Alcotest_lwt.test_case "greets a plain socket with the SSH version banner" `Quick
    (fun _switch () ->
      let host_key = Awa.Keys.of_seed `Ed25519 "charamel-net-test" in
      Charamel_net.Ssh_server.listen ~port:0 ~host_key ~users:[]
        ~exec:(fun _request -> Lwt.return_unit)
        ()
      >>= fun (server : Charamel_net.Ssh_server.server) ->
      if server.port <= 0 then Alcotest.fail "the listener reported no port"
      else
        let client = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        Lwt_unix.connect client
          (Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, server.port))
        >>= fun () ->
        read_from client >>= fun greeting ->
        Alcotest.(check bool)
          "banner prefix" true
          (String.length greeting >= 7 && String.starts_with ~prefix:"SSH-2.0" greeting);
        Lwt.catch (fun () -> Lwt_unix.close client) (fun _exn -> Lwt.return_unit)
        >>= fun () -> server.stop ())

let users =
  Alcotest_lwt.test_case "validates and finds SSH users" `Quick (fun _switch () ->
      let host_key = Awa.Keys.of_seed `Ed25519 "charamel-net-user" in
      let key = Awa.Hostkey.pub_of_priv host_key in
      let user =
        Charamel_net.Ssh_server.make_user "operator" ~password:"s3cret" [ key ]
      in
      let database = [ user ] in
      Alcotest.check_raises "user without a credential"
        (Invalid_argument "password must be Some, and/or keys must not be empty")
        (fun () -> ignore (Charamel_net.Ssh_server.make_user "nobody" []));
      Alcotest.(check bool)
        "find operator" true
        (Option.is_some (Charamel_net.Ssh_server.lookup_user "operator" database));
      Alcotest.(check bool)
        "missing user" false
        (Option.is_some (Charamel_net.Ssh_server.lookup_user "absent" database));
      Lwt.return_unit)

let cases =
  [
    round_trip;
    partial_writes;
    writev_order;
    closed_peer;
    half_close;
    idempotent_close;
    banner;
    users;
  ]
