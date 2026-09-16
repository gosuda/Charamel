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

let contains text part =
  let text_length = String.length text and part_length = String.length part in
  let rec at index =
    if index + part_length > text_length then false
    else if String.sub text index part_length = part then true
    else at (index + 1)
  in
  part_length = 0 || at 0

let make_ctx env sw =
  let cwd = "/tmp" in
  let config = Config.default in
  let permission =
    Permission.create ~config:config.Config.permissions ~yolo:true ~cwd
      ~plans_dir:"/tmp/.crush/plans" ()
  in
  let hooks = Hooks.create ~config:[] ~proc_mgr:env#process_mgr ~clock:env#clock ~cwd in
  let mcp =
    Mcp.create ~sw ~proc_mgr:env#process_mgr ~net:env#net ~clock:env#clock ~cwd ~config
  in
  let artifacts = Artifact.create ~fs:env#fs ~dir:"/tmp/crush-net-test-artifacts" in
  let jobs = Jobs.create ~sw ~proc_mgr:env#process_mgr ~clock:env#clock ~artifacts in
  let skills = Skills.load ~fs:env#fs ~config ~home:"/tmp" in
  {
    Tool.sw;
    clock = env#clock;
    fs = env#fs;
    net = env#net;
    proc_mgr = env#process_mgr;
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
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw -> f env (make_ctx env sw)

let with_server (ctx : Tool.ctx) ~response ~requests f =
  let listener =
    Eio.Net.listen ~reuse_addr:true ~backlog:4 ~sw:ctx.Tool.sw ctx.Tool.net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr listener with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "loopback listener did not have a TCP address"
  in
  Eio.Fiber.fork ~sw:ctx.Tool.sw (fun () ->
      let rec serve remaining =
        if remaining = 0 then ()
        else
          let flow, _ = Eio.Net.accept ~sw:ctx.Tool.sw listener in
          let reader = Eio.Buf_read.of_flow flow ~max_size:65_536 in
          let rec consume_headers () =
            match Eio.Buf_read.line reader with
            | "" -> ()
            | _ -> consume_headers ()
            | exception End_of_file -> ()
          in
          consume_headers ();
          Eio.Flow.copy_string response flow;
          serve (remaining - 1)
      in
      serve requests);
  f (Fmt.str "http://127.0.0.1:%d/document" port)

let output_or_fail = function
  | Ok output -> output
  | Error error -> Alcotest.failf "fetch failed: %a" Tool.pp_error error

let test_markdown_and_text () =
  with_ctx @@ fun _env ctx ->
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
  with_server ctx ~response ~requests:2 (fun url ->
      let markdown =
        output_or_fail (Tools_net.fetch.Tool.run ctx (fetch_value ~format:"markdown" url))
      in
      Alcotest.(check bool)
        "heading is converted" true
        (contains markdown.Tool.content "# Hello");
      Alcotest.(check bool)
        "entities are decoded" true
        (contains markdown.Tool.content "A & B");
      Alcotest.(check bool)
        "list item is converted" true
        (contains markdown.Tool.content "- One");
      Alcotest.(check bool)
        "link is converted" true
        (contains markdown.Tool.content "[next](/next)");
      Alcotest.(check bool)
        "script is omitted" false
        (contains markdown.Tool.content "bad-script");
      let text =
        output_or_fail (Tools_net.fetch.Tool.run ctx (fetch_value ~format:"text" url))
      in
      Alcotest.(check bool) "text keeps body" true (contains text.Tool.content "Hello");
      Alcotest.(check bool)
        "text omits style" false
        (contains text.Tool.content "bad-style"))

let test_status_error () =
  with_ctx @@ fun _env ctx ->
  let body = "missing" in
  let response =
    Fmt.str "HTTP/1.1 404 Not Found\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      (String.length body) body
  in
  with_server ctx ~response ~requests:1 (fun url ->
      match Tools_net.fetch.Tool.run ctx (fetch_value ~timeout_s:5 url) with
      | Error (`Unavailable message) ->
          Alcotest.(check bool) "status is reported" true (contains message "HTTP 404")
      | Error error -> Alcotest.failf "unexpected error: %a" Tool.pp_error error
      | Ok _ -> Alcotest.fail "404 response was accepted")

let cases =
  [
    Alcotest.test_case "loopback HTML conversion" `Quick test_markdown_and_text;
    Alcotest.test_case "HTTP status error" `Quick test_status_error;
  ]
