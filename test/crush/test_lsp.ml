module Lsp = Crush_core.Lsp
module Config = Crush_core.Config

let source_text = "a\240\159\152\128b\n"

let fixture_script =
  {|import json
import sys

def send(value):
    payload = json.dumps(value, separators=(',', ':')).encode()
    sys.stdout.buffer.write(b'Content-Length: ' + str(len(payload)).encode() + b'\r\n\r\n' + payload)
    sys.stdout.buffer.flush()

def read_message():
    content_length = None
    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            return None
        if line in (b'\r\n', b'\n'):
            break
        name, value = line.decode().split(':', 1)
        if name.lower() == 'content-length':
            content_length = int(value.strip())
    if content_length is None:
        return None
    body = sys.stdin.buffer.read(content_length)
    return json.loads(body)

while True:
    message = read_message()
    if message is None:
        break
    method = message.get('method')
    request_id = message.get('id')
    if method == 'initialize':
        send({'jsonrpc': '2.0', 'id': request_id, 'result': {'capabilities': {}}})
    elif method == 'shutdown':
        send({'jsonrpc': '2.0', 'id': request_id, 'result': None})
    elif method == 'textDocument/didOpen':
        document = message['params']['textDocument']
        send({'jsonrpc': '2.0', 'method': 'textDocument/publishDiagnostics', 'params': {'uri': document['uri'], 'diagnostics': [{'range': {'start': {'line': 0, 'character': 1}, 'end': {'line': 0, 'character': 3}}, 'severity': 2, 'message': 'fixture warning', 'source': 'fixture'}]}})
    elif method == 'textDocument/definition':
        uri = message['params']['textDocument']['uri']
        send({'jsonrpc': '2.0', 'id': request_id, 'result': [{'uri': uri, 'range': {'start': {'line': 0, 'character': 0}, 'end': {'line': 0, 'character': 1}}}]})
    elif method == 'textDocument/references':
        uri = message['params']['textDocument']['uri']
        send({'jsonrpc': '2.0', 'id': request_id, 'result': [{'uri': uri, 'range': {'start': {'line': 0, 'character': 0}, 'end': {'line': 0, 'character': 1}}}]})
    elif method == 'textDocument/documentSymbol':
        send({'jsonrpc': '2.0', 'id': request_id, 'result': [{'name': 'fixture', 'kind': 12, 'range': {'start': {'line': 0, 'character': 0}, 'end': {'line': 0, 'character': 1}}, 'children': []}]})
    elif method == 'textDocument/rename':
        uri = message['params']['textDocument']['uri']
        send({'jsonrpc': '2.0', 'id': request_id, 'result': {'changes': {uri: [{'range': {'start': {'line': 0, 'character': 0}, 'end': {'line': 0, 'character': 1}}, 'newText': 'z'}]}}})
    elif method == 'exit':
        break
|}

let malformed_script =
  {|import sys
sys.stdout.write('Content-Length: nope\r\n\r\n')
sys.stdout.flush()
|}

let config_with_server script =
  let base = Config.default in
  let server =
    {
      Config.command = "python3";
      args = [ "-u"; "-c"; script ];
      filetypes = [ "ml" ];
      root_markers = [];
      init_options = None;
    }
  in
  {
    base with
    lsp = [ ("fixture", server) ];
    options = { base.options with auto_lsp = false };
  }

let with_lsp script f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let path = Filename.temp_file "crush-lsp" ".ml" in
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / path) source_text;
  let lsp =
    Lsp.create ~sw ~proc_mgr:env#process_mgr ~clock:env#clock ~fs:env#fs ~cwd:"/tmp"
      ~config:(config_with_server script)
  in
  Fun.protect
    (fun () -> f env lsp path)
    ~finally:(fun () ->
      Lsp.stop_all lsp;
      Eio.Path.unlink ~missing_ok:true Eio.Path.(env#fs / path))

let check_location expected (actual : Lsp.location) =
  Alcotest.(check string) "location path" expected actual.path;
  Alcotest.(check int) "location line" 1 actual.line;
  Alcotest.(check int) "location column" 1 actual.col

let protocol_round_trip () =
  with_lsp fixture_script (fun _env lsp path ->
      Alcotest.(check (option string))
        "fixture handle" (Some "fixture") (Lsp.handles lsp ~path);
      Lsp.touch lsp ~path;
      let values = Lsp.diagnostics lsp ~path ~wait:0.5 in
      Alcotest.(check int) "diagnostic count" 1 (List.length values);
      Alcotest.(check string)
        "diagnostic message" "fixture warning" (List.hd values).message;
      (match Lsp.definition lsp ~path ~line:1 ~col:1 with
      | Error error -> Alcotest.failf "definition failed: %a" Lsp.pp_error error
      | Ok locations ->
          Alcotest.(check int) "definition count" 1 (List.length locations);
          check_location path (List.hd locations));
      (match Lsp.references lsp ~path ~line:1 ~col:1 with
      | Error error -> Alcotest.failf "references failed: %a" Lsp.pp_error error
      | Ok locations -> Alcotest.(check int) "references count" 1 (List.length locations));
      (match Lsp.document_symbols lsp ~path with
      | Error error -> Alcotest.failf "symbols failed: %a" Lsp.pp_error error
      | Ok [ symbol ] ->
          Alcotest.(check string) "symbol path fallback" path symbol.range.path;
          Alcotest.(check string) "symbol kind" "Function" symbol.kind
      | Ok _ -> Alcotest.fail "unexpected symbols");
      (match Lsp.find_symbol lsp ~path ~name:"fixture" with
      | Error error -> Alcotest.failf "find symbol failed: %a" Lsp.pp_error error
      | Ok None -> Alcotest.fail "symbol was not found"
      | Ok (Some symbol) -> Alcotest.(check string) "found symbol" "fixture" symbol.name);
      match Lsp.rename lsp ~path ~line:1 ~col:1 ~new_name:"renamed" with
      | Error error -> Alcotest.failf "rename failed: %a" Lsp.pp_error error
      | Ok [ (changed_path, [ edit ]) ] ->
          Alcotest.(check string) "rename path" path changed_path;
          Alcotest.(check string) "rename text" "z" edit.new_text
      | Ok _ -> Alcotest.fail "unexpected rename edits")

let unicode_edit () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun _sw ->
  let path = Filename.temp_file "crush-lsp-edit" ".ml" in
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / path) source_text;
  let edit =
    { Lsp.range = { path; line = 1; col = 2; end_line = 1; end_col = 4 }; new_text = "X" }
  in
  (match Lsp.apply_edits ~fs:env#fs [ (path, [ edit ]) ] with
  | Error error -> Alcotest.failf "unicode edit failed: %a" Lsp.pp_error error
  | Ok [ changed ] ->
      Alcotest.(check string) "changed path" path changed;
      Alcotest.(check string)
        "unicode edit" "aXb\n"
        (Eio.Path.load Eio.Path.(env#fs / path))
  | Ok _ -> Alcotest.fail "unexpected changed path list");
  Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(env#fs / path) source_text;
  let split =
    { Lsp.range = { path; line = 1; col = 3; end_line = 1; end_col = 4 }; new_text = "Y" }
  in
  (match Lsp.apply_edits ~fs:env#fs [ (path, [ split ]) ] with
  | Error (`Io (_, message)) ->
      Alcotest.(check bool) "surrogate boundary rejected" true (String.length message > 0)
  | Error error -> Alcotest.failf "wrong split error: %a" Lsp.pp_error error
  | Ok _ -> Alcotest.fail "surrogate boundary was accepted");
  Eio.Path.unlink ~missing_ok:true Eio.Path.(env#fs / path)

let malformed_frame () =
  with_lsp malformed_script (fun _env lsp path ->
      Lsp.touch lsp ~path;
      match Lsp.servers lsp with
      | [ ("fixture", Lsp.Failed message) ] ->
          Alcotest.(check bool)
            "malformed frame reports failure" true
            (String.length message > 0)
      | values ->
          Alcotest.failf "unexpected malformed server state count %d" (List.length values))

let cases =
  [
    Alcotest.test_case "real child JSON-RPC" `Quick protocol_round_trip;
    Alcotest.test_case "UTF-16 Unicode workspace edit" `Quick unicode_edit;
    Alcotest.test_case "malformed frame" `Quick malformed_frame;
  ]
