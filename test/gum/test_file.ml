let contains_sub text needle =
  let rec loop index =
    index + String.length needle <= String.length text
    && (String.sub text index (String.length needle) = needle || loop (index + 1))
  in
  needle = "" || loop 0

let executable () =
  let test_path = Unix.realpath Sys.executable_name in
  let test_dir = Filename.dirname test_path in
  let build_dir = Filename.dirname (Filename.dirname test_dir) in
  Filename.concat build_dir "bin/gum/main.exe"

let with_root f =
  let path = Filename.temp_file "gum-file-cli" ".dir" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote path)))
    (fun () -> f path)

let env_for root =
  let path name = Filename.concat root name in
  [|
    "PATH=/usr/bin:/bin";
    "LANG=C";
    "TERM=xterm-256color";
    "HOME=" ^ path "home";
    "XDG_CONFIG_HOME=" ^ path "xdg-config";
    "XDG_DATA_HOME=" ^ path "xdg-data";
    "XDG_STATE_HOME=" ^ path "xdg-state";
    "XDG_CACHE_HOME=" ^ path "xdg-cache";
  |]

let run_cli root args =
  Eio_main.run @@ fun env ->
  let status = ref None in
  let errors = Buffer.create 128 in
  let output =
    Eio.Time.with_timeout_exn env#clock 10. (fun () ->
        Eio.Process.parse_out env#process_mgr Eio.Buf_read.take_all ~env:(env_for root)
          ~stdin:(Eio.Flow.string_source "") ~stderr:(Eio.Flow.buffer_sink errors)
          ~is_success:(fun code ->
            status := Some code;
            true)
          (executable () :: args))
  in
  (Option.value !status ~default:127, output, Buffer.contents errors)

let test_cli_rejects_without_selection_kind () =
  with_root (fun root ->
      let status, _, diagnostics =
        run_cli root [ "file"; "--no-file"; "--no-directory" ]
      in
      Alcotest.(check int) "status" 1 status;
      Alcotest.(check bool)
        "selection diagnostic" true
        (contains_sub diagnostics "at least one"))

let cases =
  [
    Alcotest.test_case "CLI selection policy" `Quick
      test_cli_rejects_without_selection_kind;
  ]
