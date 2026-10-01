let () =
  Lwt_main.run
  @@ Alcotest_lwt.run "charamel.net"
       [
         ("http", Test_net_http.cases);
         ("smtp fixture", Test_net_smtp.cases);
         ("ssh server", Test_net_ssh.cases);
       ]
