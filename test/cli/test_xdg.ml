open Lwt.Syntax
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
  let* status, output, _stderr =
    Test_support.run_cli ~exe:Sys.executable_name ~env:(Array.of_list bindings)
      ~timeout:5. [ "--xdg-probe"; "run" ]
  in
  Alcotest.(check int) "probe exit status" 0 status;
  Lwt.return output

(* The fake root must qualify as absolute on the platform under test: "/probe"
   is only absolute on POSIX, so Windows vectors hang off a drive letter. *)
let probe_root = if Sys.win32 then "C:\\probe" else "/probe"
let root sub = Filename.concat probe_root sub
let probe_home = root "home"

(* [Xdg] builds each answer the same way the vectors expect it, so the
   expectations are computed with the library's own joins rather than
   literal strings — the same literals read differently across platforms. *)
let dir name base = name ^ "=" ^ Filename.concat base app

type vector = { name : string; env : string list; expect : string list }

let home_only =
  [
    dir "config" (Filename.concat probe_home ".config");
    dir "data" (Filename.concat probe_home ".local/share");
    dir "state" (Filename.concat probe_home ".local/state");
    dir "cache" (Filename.concat probe_home ".cache");
  ]

let every_dir_errors = [ "config=!error"; "data=!error"; "state=!error"; "cache=!error" ]

let vectors =
  [
    {
      name = "qualifying variables win over home";
      env =
        [
          "HOME=" ^ probe_home;
          "XDG_CONFIG_HOME=" ^ root "cfg";
          "XDG_DATA_HOME=" ^ root "data";
          "XDG_STATE_HOME=" ^ root "state";
          "XDG_CACHE_HOME=" ^ root "cache";
        ];
      expect =
        [
          dir "config" (root "cfg");
          dir "data" (root "data");
          dir "state" (root "state");
          dir "cache" (root "cache");
        ];
    };
    { name = "home fallback leaves"; env = [ "HOME=" ^ probe_home ]; expect = home_only };
    {
      name = "relative variable is ignored";
      env = [ "HOME=" ^ probe_home; "XDG_CONFIG_HOME=probe/relative" ];
      expect = home_only;
    };
    {
      name = "empty variable is ignored";
      env = [ "HOME=" ^ probe_home; "XDG_DATA_HOME="; "XDG_CACHE_HOME=" ^ root "cache" ];
      expect =
        [
          dir "config" (Filename.concat probe_home ".config");
          dir "data" (Filename.concat probe_home ".local/share");
          dir "state" (Filename.concat probe_home ".local/state");
          dir "cache" (root "cache");
        ];
    };
    {
      name = "variable qualifies without home";
      env = [ "XDG_CONFIG_HOME=" ^ root "cfg" ];
      expect =
        [ dir "config" (root "cfg"); "data=!error"; "state=!error"; "cache=!error" ];
    };
    { name = "no home and no variables"; env = []; expect = every_dir_errors };
    { name = "empty home"; env = [ "HOME=" ]; expect = every_dir_errors };
  ]

let vector_case { name; env; expect } =
  Alcotest_lwt.test_case name `Quick (fun _switch () ->
      let* output = capture env in
      Alcotest.check Alcotest.string name (String.concat "\n" expect ^ "\n") output;
      Lwt.return_unit)

let cases : unit Alcotest_lwt.test_case list = List.map vector_case vectors
