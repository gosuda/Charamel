module Xdg = Charamel_cli.Xdg

(* Each vector runs the probe in a child process whose environment holds
   exactly the vector's bindings, so the suite never mutates its own
   environment. The child re-executes this binary. The guard serves that
   request and exits before the test driver starts. *)

let app = "probeapp"

let probe () =
  List.iter
    (fun (name, get) ->
      let value = try get ~app with Invalid_argument _ -> "!error" in
      print_string (name ^ "=" ^ value ^ "\n"))
    [
      ("config", Xdg.config_dir);
      ("data", Xdg.data_dir);
      ("state", Xdg.state_dir);
      ("cache", Xdg.cache_dir);
    ]

let () =
  match Sys.argv with
  | [| _; "--xdg-probe"; _ |] ->
      probe ();
      exit 0
  | _ -> ()

let capture bindings =
  Eio_main.run (fun env ->
      Eio.Time.with_timeout_exn env#clock 5. (fun () ->
          Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all
            ~env:(Array.of_list bindings)
            [ Sys.executable_name; "--xdg-probe"; "run" ]))

type vector = { name : string; env : string list; expect : string list }

let home_only =
  [
    "config=/probe/home/.config/probeapp";
    "data=/probe/home/.local/share/probeapp";
    "state=/probe/home/.local/state/probeapp";
    "cache=/probe/home/.cache/probeapp";
  ]

let every_dir_errors = [ "config=!error"; "data=!error"; "state=!error"; "cache=!error" ]

let vectors =
  [
    {
      name = "qualifying variables win over home";
      env =
        [
          "HOME=/probe/home";
          "XDG_CONFIG_HOME=/probe/cfg";
          "XDG_DATA_HOME=/probe/data";
          "XDG_STATE_HOME=/probe/state";
          "XDG_CACHE_HOME=/probe/cache";
        ];
      expect =
        [
          "config=/probe/cfg/probeapp";
          "data=/probe/data/probeapp";
          "state=/probe/state/probeapp";
          "cache=/probe/cache/probeapp";
        ];
    };
    { name = "home fallback leaves"; env = [ "HOME=/probe/home" ]; expect = home_only };
    {
      name = "relative variable is ignored";
      env = [ "HOME=/probe/home"; "XDG_CONFIG_HOME=probe/relative" ];
      expect = home_only;
    };
    {
      name = "empty variable is ignored";
      env = [ "HOME=/probe/home"; "XDG_DATA_HOME="; "XDG_CACHE_HOME=/probe/cache" ];
      expect =
        [
          "config=/probe/home/.config/probeapp";
          "data=/probe/home/.local/share/probeapp";
          "state=/probe/home/.local/state/probeapp";
          "cache=/probe/cache/probeapp";
        ];
    };
    {
      name = "variable qualifies without home";
      env = [ "XDG_CONFIG_HOME=/probe/cfg" ];
      expect =
        [ "config=/probe/cfg/probeapp"; "data=!error"; "state=!error"; "cache=!error" ];
    };
    { name = "no home and no variables"; env = []; expect = every_dir_errors };
    { name = "empty home"; env = [ "HOME=" ]; expect = every_dir_errors };
  ]

let vector_case { name; env; expect } =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check Alcotest.string name (String.concat "\n" expect ^ "\n") (capture env))

let cases : unit Alcotest.test_case list = List.map vector_case vectors
