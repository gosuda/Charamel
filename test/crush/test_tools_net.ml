open Lwt_direct
module Artifact = Crush_core.Artifact
module Config = Crush_core.Config
module Hooks = Crush_core.Hooks
module Jobs = Crush_core.Jobs
module Mcp = Crush_core.Mcp
module Permission = Crush_core.Permission
module Skills = Crush_core.Skills
module Todos = Crush_core.Todos
module Tool = Crush_core.Tool
module Tools_net = Crush_core.Tools_net

let json_field name value = Jsont.Json.mem (Jsont.Json.name name) value

let fetch_value ?format ?timeout_s url =
  let optional name value fields =
    match value with None -> fields | Some value -> json_field name value :: fields
  in
  [ json_field "url" (Jsont.Json.string url) ]
  |> optional "format" (Option.map Jsont.Json.string format)
  |> optional "timeout_s" (Option.map Jsont.Json.int timeout_s)
  |> Jsont.Json.object'

let make_ctx sw =
  let open Lwt.Syntax in
  let clock = Charamel_os.Time.lwt in
  let cwd = "/tmp" in
  let config = Config.default in
  let permission =
    Permission.create ~config:config.Config.permissions ~yolo:true ~cwd
      ~plans_dir:"/tmp/.crush/plans" ()
  in
  let hooks = Hooks.create ~config:[] ~cwd in
  let* mcp = Mcp.create ~cwd ~config in
  let artifacts = Artifact.create ~fs_root:"/" ~dir:"tmp/crush-net-test-artifacts" in
  let jobs = Jobs.create ~sw ~artifacts in
  let+ skills = Skills.load ~fs_root:"/" ~config ~home:"/tmp" in
  {
    Tool.clock;
    fs_root = "/";
    random = (fun length -> String.make length '\000');
    env = Sys.getenv_opt;
    cwd;
    session = "net-test";
    call_id = "net-call";
    config;
    permission;
    hooks;
    lsp = None;
    mcp;
    artifacts;
    jobs;
    todos = Todos.create ();
    skills;
    log_path = "/tmp/crush-net-test.log";
    interactive = false;
    is_subagent = false;
    ask = None;
    run_subagent = None;
    read_tracker = Hashtbl.create 8;
  }

let with_ctx f =
  let open Lwt.Syntax in
  Lwt_switch.with_switch (fun sw ->
      let* ctx = make_ctx sw in
      Lwt_direct.spawn (fun () -> f ctx))

let close_fd_quietly fd =
  Lwt.catch (fun () -> Lwt_unix.close fd) (fun _exn -> Lwt.return_unit)

let with_server ~response ~requests f =
  let open Lwt.Syntax in
  let listener = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt listener Lwt_unix.SO_REUSEADDR true;
  await (Lwt_unix.bind listener (Unix.ADDR_INET (Unix.inet_addr_loopback, 0)));
  Lwt_unix.listen listener 4;
  let port =
    match Lwt_unix.getsockname listener with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> Alcotest.fail "loopback listener did not have a TCP address"
  in
  let remaining = ref requests in
  let consume_headers flow =
    let rec loop () =
      let* line = Lwt_io.read_line flow in
      if line = "" then Lwt.return_unit else loop ()
    in
    Lwt.catch loop (fun exn ->
        match exn with End_of_file -> Lwt.return_unit | exn -> Lwt.fail exn)
  in
  let handle_client client_fd =
    let input = Lwt_io.of_fd ~mode:Lwt_io.input client_fd in
    let output =
      Lwt_io.of_fd ~close:(fun () -> Lwt.return_unit) ~mode:Lwt_io.output client_fd
    in
    Lwt.catch
      (fun () ->
        let* () = consume_headers input in
        let* () = Lwt_io.write output response in
        let* () = Lwt_io.flush output in
        decr remaining;
        Lwt_io.close input)
      (fun _exn -> close_fd_quietly client_fd)
  in
  let rec accept_loop () =
    if !remaining = 0 then Lwt.return_unit
    else
      Lwt.try_bind
        (fun () -> Lwt_unix.accept listener)
        (fun (client_fd, _addr) ->
          Lwt.async (fun () -> handle_client client_fd);
          accept_loop ())
        (fun exn ->
          match exn with
          | Lwt.Canceled | End_of_file | Unix.Unix_error _ -> Lwt.return_unit
          | exn -> Lwt.fail exn)
  in
  Lwt.async accept_loop;
  Fun.protect
    (fun () -> f (Fmt.str "http://127.0.0.1:%d/document" port))
    ~finally:(fun () ->
      remaining := 0;
      let wake_fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
      await
      @@
      let* () =
        Lwt.catch
          (fun () ->
            Lwt_unix.connect wake_fd (Unix.ADDR_INET (Unix.inet_addr_loopback, port)))
          (fun _exn -> Lwt.return_unit)
      in
      let* () = close_fd_quietly wake_fd in
      close_fd_quietly listener)

let output_or_fail = function
  | Ok output -> output
  | Error error -> Alcotest.failf "fetch failed: %a" Tool.pp_error error

let test_markdown_and_text () =
  await @@ with_ctx
  @@ fun ctx ->
  let body =
    "<html><head><style>bad-style</style><script>bad-script</script></head>"
    ^ "<body><h1>Hello</h1><p>A &amp; B</p><ul><li>One</li></ul>"
    ^ "<a href=\"/next\">next</a></body></html>"
  in
  let response =
    Fmt.str
      "HTTP/1.1 200 OK\r\n\
       Content-Type: text/html\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n\
       %s"
      (String.length body) body
  in
  with_server ~response ~requests:2 (fun url ->
      let markdown =
        output_or_fail (Tools_net.fetch.Tool.run ctx (fetch_value ~format:"markdown" url))
      in
      Alcotest.(check bool)
        "heading is converted" true
        (Test_support.contains ~needle:"# Hello" ~haystack:markdown.Tool.content);
      Alcotest.(check bool)
        "entities are decoded" true
        (Test_support.contains ~needle:"A & B" ~haystack:markdown.Tool.content);
      Alcotest.(check bool)
        "list item is converted" true
        (Test_support.contains ~needle:"- One" ~haystack:markdown.Tool.content);
      Alcotest.(check bool)
        "link is converted" true
        (Test_support.contains ~needle:"[next](/next)" ~haystack:markdown.Tool.content);
      Alcotest.(check bool)
        "script is omitted" false
        (Test_support.contains ~needle:"bad-script" ~haystack:markdown.Tool.content);
      let text =
        output_or_fail (Tools_net.fetch.Tool.run ctx (fetch_value ~format:"text" url))
      in
      Alcotest.(check bool)
        "text keeps body" true
        (Test_support.contains ~needle:"Hello" ~haystack:text.Tool.content);
      Alcotest.(check bool)
        "text omits style" false
        (Test_support.contains ~needle:"bad-style" ~haystack:text.Tool.content))

let test_status_error () =
  await @@ with_ctx
  @@ fun ctx ->
  let body = "missing" in
  let response =
    Fmt.str "HTTP/1.1 404 Not Found\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      (String.length body) body
  in
  with_server ~response ~requests:1 (fun url ->
      match Tools_net.fetch.Tool.run ctx (fetch_value ~timeout_s:5 url) with
      | Error (`Unavailable message) ->
          Alcotest.(check bool)
            "status is reported" true
            (Test_support.contains ~needle:"HTTP 404" ~haystack:message)
      | Error error -> Alcotest.failf "unexpected error: %a" Tool.pp_error error
      | Ok _ -> Alcotest.fail "404 response was accepted")

let cases =
  [
    Test_tools_test_support.case "loopback HTML conversion" `Quick test_markdown_and_text;
    Test_tools_test_support.case "HTTP status error" `Quick test_status_error;
  ]
