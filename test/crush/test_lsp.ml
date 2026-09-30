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
    options = { base.Config.options with auto_lsp = false };
  }

let lsp_temp_path prefix =
  Fmt.str "%s%s%s-%d-%d.ml"
    (Filename.get_temp_dir_name ())
    Filename.dir_sep prefix (Unix.getpid ()) (Random.bits ())

let clock = Charamel_os.Time.lwt

let with_lsp ?(cwd = Filename.get_temp_dir_name ()) script f =
  let path = lsp_temp_path "crush-lsp" in
  Test_tools_test_support.write_file path source_text;
  let sw = Lwt_switch.create () in
  let lsp = Lsp.create ~sw ~clock ~fs_root:"/" ~cwd ~config:(config_with_server script) in
  Fun.protect
    (fun () -> f lsp path)
    ~finally:(fun () ->
      Lwt_direct.await (Lsp.stop_all lsp);
      try Sys.remove path with Sys_error _ -> ())

let check_location expected (actual : Lsp.location) =
  Alcotest.(check string) "location path" expected actual.Lsp.path;
  Alcotest.(check int) "location line" 1 actual.Lsp.line;
  Alcotest.(check int) "location column" 1 actual.Lsp.col

let protocol_round_trip () =
  with_lsp fixture_script (fun lsp path ->
      Alcotest.(check (option string))
        "fixture handle" (Some "fixture") (Lsp.handles lsp ~path);
      Lwt_direct.await (Lsp.touch lsp ~path);
      let values = Lwt_direct.await (Lsp.diagnostics lsp ~path ~wait:0.5) in
      Alcotest.(check int) "diagnostic count" 1 (List.length values);
      Alcotest.(check string)
        "diagnostic message" "fixture warning" (List.hd values).Lsp.message;
      (match Lwt_direct.await (Lsp.definition lsp ~path ~line:1 ~col:1) with
      | Error error -> Alcotest.failf "definition failed: %a" Lsp.pp_error error
      | Ok locations ->
          Alcotest.(check int) "definition count" 1 (List.length locations);
          check_location path (List.hd locations));
      (match Lwt_direct.await (Lsp.references lsp ~path ~line:1 ~col:1) with
      | Error error -> Alcotest.failf "references failed: %a" Lsp.pp_error error
      | Ok locations -> Alcotest.(check int) "references count" 1 (List.length locations));
      (match Lwt_direct.await (Lsp.document_symbols lsp ~path) with
      | Error error -> Alcotest.failf "symbols failed: %a" Lsp.pp_error error
      | Ok [ symbol ] ->
          Alcotest.(check string) "symbol path fallback" path symbol.Lsp.range.Lsp.path;
          Alcotest.(check string) "symbol kind" "Function" symbol.Lsp.kind
      | Ok _ -> Alcotest.fail "unexpected symbols");
      (match Lwt_direct.await (Lsp.find_symbol lsp ~path ~name:"fixture") with
      | Error error -> Alcotest.failf "find symbol failed: %a" Lsp.pp_error error
      | Ok None -> Alcotest.fail "symbol was not found"
      | Ok (Some symbol) ->
          Alcotest.(check string) "found symbol" "fixture" symbol.Lsp.name);
      match
        Lwt_direct.await (Lsp.rename lsp ~path ~line:1 ~col:1 ~new_name:"renamed")
      with
      | Error error -> Alcotest.failf "rename failed: %a" Lsp.pp_error error
      | Ok [ (changed_path, [ edit ]) ] ->
          Alcotest.(check string) "rename path" path changed_path;
          Alcotest.(check string) "rename text" "z" edit.Lsp.new_text
      | Ok _ -> Alcotest.fail "unexpected rename edits")

let unicode_edit () =
  let path = lsp_temp_path "crush-lsp-edit" in
  Test_tools_test_support.write_file path source_text;
  let edit =
    { Lsp.range = { path; line = 1; col = 2; end_line = 1; end_col = 4 }; new_text = "X" }
  in
  (match Lwt_direct.await (Lsp.apply_edits ~cwd:(Sys.getcwd ()) [ (path, [ edit ]) ]) with
  | Error error -> Alcotest.failf "unicode edit failed: %a" Lsp.pp_error error
  | Ok [ changed ] ->
      Alcotest.(check string) "changed path" path changed;
      Alcotest.(check string)
        "unicode edit" "aXb\n"
        (Test_tools_test_support.load_file path)
  | Ok _ -> Alcotest.fail "unexpected changed path list");
  Test_tools_test_support.write_file path source_text;
  let split =
    { Lsp.range = { path; line = 1; col = 3; end_line = 1; end_col = 4 }; new_text = "Y" }
  in
  (match
     Lwt_direct.await (Lsp.apply_edits ~cwd:(Sys.getcwd ()) [ (path, [ split ]) ])
   with
  | Error (`Io _) -> ()
  | Error error -> Alcotest.failf "wrong split error: %a" Lsp.pp_error error
  | Ok _ -> Alcotest.fail "surrogate boundary was accepted");
  Sys.remove path

let malformed_frame () =
  with_lsp malformed_script (fun lsp path ->
      Lwt_direct.await (Lsp.touch lsp ~path);
      match Lwt_direct.await (Lsp.servers lsp) with
      | [ ("fixture", Lsp.Failed _) ] -> ()
      | values ->
          Alcotest.failf "unexpected malformed server state count %d" (List.length values))

let root_slash_matches () =
  with_lsp ~cwd:"/" fixture_script (fun lsp path ->
      Alcotest.(check (option string))
        "fixture handle under root" (Some "fixture") (Lsp.handles lsp ~path))

let handles_respects_cwd () =
  with_lsp fixture_script (fun lsp _path ->
      Alcotest.(check (option string))
        "inside cwd" (Some "fixture")
        (Lsp.handles lsp ~path:(lsp_temp_path "crush-lsp-inside"));
      Alcotest.(check (option string))
        "outside cwd" None
        (Lsp.handles lsp ~path:"/home/foo.ml"))

let cases =
  [
    Test_tools_test_support.case "real child JSON-RPC" `Quick protocol_round_trip;
    Test_tools_test_support.case "UTF-16 Unicode workspace edit" `Quick unicode_edit;
    Test_tools_test_support.case "malformed frame" `Quick malformed_frame;
    Test_tools_test_support.case "root slash matches" `Quick root_slash_matches;
    Test_tools_test_support.case "handles respects cwd" `Quick handles_respects_cwd;
  ]
