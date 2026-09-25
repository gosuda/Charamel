let runtime_probe mode =
  let action () =
    match mode with
    | "error" -> Charamel_cli.error "probe failure"
    | "silent" -> Charamel_cli.exit 1
    | "timeout" -> raise Eio.Time.Timeout
    | "interrupt" -> raise Sys.Break
    | _ -> ()
  in
  Charamel_cli.run ~name:"cli-probe" ~version:"dev" ~doc:"CLI runtime probe"
    ~default:(fun _env -> Cmdliner.Term.(const action $ const ()))
    []

let child_status mode =
  let args = if mode = "usage" then [ "--unknown-option" ] else [] in
  let status, _stdout, _stderr =
    Test_support.run_cli ~exe:Sys.executable_name
      ~env:[| "CHARAMEL_CLI_RUNTIME_PROBE=" ^ mode |]
      args
  in
  status

let status_case mode expected =
  Alcotest.test_case mode `Quick (fun () ->
      Alcotest.(check int) mode expected (child_status mode))

let cases =
  [
    status_case "error" 1;
    status_case "silent" 1;
    status_case "usage" 2;
    status_case "timeout" 124;
    status_case "interrupt" 130;
  ]

let () =
  match Sys.getenv_opt "CHARAMEL_CLI_RUNTIME_PROBE" with
  | Some mode -> runtime_probe mode
  | None ->
      Alcotest.run "cli-behavior"
        [ ("nearest", Test_cli_nearest.cases); ("runtime", cases) ]
