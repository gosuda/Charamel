module Tool = Crush_core.Tool
module Tools_lsp = Crush_core.Tools_lsp
module Tools_mcp = Crush_core.Tools_mcp
module Mcp = Crush_core.Mcp

let member name value =
  match value with
  | Jsont.Object (members, _) -> Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

let object_schema tool = match tool.Tool.schema with Jsont.Object _ -> true | _ -> false

let required name schema =
  match member "required" schema with
  | Some (Jsont.Array (values, _)) ->
      List.exists (function Jsont.String (value, _) -> value = name | _ -> false) values
  | _ -> false

let names_and_order () =
  let names = List.map (fun (tool : Tool.t) -> tool.name) Tools_lsp.all in
  Alcotest.(check (list string))
    "LSP order"
    [
      "lsp_diagnostics";
      "lsp_definition";
      "lsp_references";
      "lsp_symbols";
      "lsp_rename";
      "lsp_restart";
    ]
    names;
  Alcotest.(check bool)
    "diagnostics are read-only" true Tools_lsp.lsp_diagnostics.read_only;
  Alcotest.(check bool) "rename is mutable" false Tools_lsp.lsp_rename.read_only

let schemas_are_typed () =
  Alcotest.(check bool)
    "diagnostics schema object" true
    (object_schema Tools_lsp.lsp_diagnostics);
  Alcotest.(check bool)
    "navigation symbol required" true
    (required "symbol" Tools_lsp.lsp_definition.schema);
  Alcotest.(check bool)
    "rename replacement required" true
    (required "new_name" Tools_lsp.lsp_rename.schema)

let dynamic_mcp_name_and_schema () =
  let schema =
    Jsont.Json.object'
      [
        Jsont.Json.mem (Jsont.Json.name "type") (Jsont.Json.string "object");
        Jsont.Json.mem (Jsont.Json.name "required")
          (Jsont.Json.list [ Jsont.Json.string "value" ]);
        Jsont.Json.mem
          (Jsont.Json.name "properties")
          (Jsont.Json.object'
             [
               Jsont.Json.mem (Jsont.Json.name "value")
                 (Jsont.Json.object'
                    [
                      Jsont.Json.mem (Jsont.Json.name "type") (Jsont.Json.string "string");
                    ]);
             ]);
      ]
  in
  let definition : Mcp.tool =
    { server = "my server"; name = "echo"; description = "Echo"; schema }
  in
  let tool = Tools_mcp.mcp_tool definition in
  let expected = Mcp.tool_name ~server:definition.server definition.name in
  Alcotest.(check string) "sanitized MCP name" expected tool.name;
  Alcotest.(check bool) "dynamic tool is mutable" false tool.read_only;
  Alcotest.(check bool) "schema retained" true (Jsont.Json.equal schema tool.schema);
  Alcotest.(check bool)
    "resource list is read-only" true Tools_mcp.list_mcp_resources.read_only;
  Alcotest.(check bool)
    "resource read is read-only" true Tools_mcp.read_mcp_resource.read_only

let cases =
  [
    Alcotest.test_case "LSP names and order" `Quick names_and_order;
    Alcotest.test_case "LSP schemas" `Quick schemas_are_typed;
    Alcotest.test_case "MCP dynamic registration" `Quick dynamic_mcp_name_and_schema;
  ]
