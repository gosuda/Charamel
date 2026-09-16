let ( let* ) = Result.bind

type resource_args = { server : string }
type read_resource_args = { server : string; uri : string }

let resource_codec =
  let open Jsont in
  Object.map (fun (server : string) -> ({ server } : resource_args))
  |> Object.mem "server" string ~enc:(fun (value : resource_args) -> value.server)
  |> Object.finish

let read_resource_codec =
  let open Jsont in
  Object.map (fun (server : string) (uri : string) ->
      ({ server; uri } : read_resource_args))
  |> Object.mem "server" string ~enc:(fun (value : read_resource_args) -> value.server)
  |> Object.mem "uri" string ~enc:(fun (value : read_resource_args) -> value.uri)
  |> Object.finish

let resource_schema =
  Tool.schema_object ~required:[ "server" ]
    [ ("server", Tool.s_string ~desc:"MCP server name" ()) ]

let read_resource_schema =
  Tool.schema_object ~required:[ "server"; "uri" ]
    [
      ("server", Tool.s_string ~desc:"MCP server name" ());
      ("uri", Tool.s_string ~desc:"Resource URI" ());
    ]

let map_mcp_error = function
  | `Unknown_server server -> `Unavailable (Fmt.str "unknown MCP server %s" server)
  | `Not_connected server ->
      `Unavailable (Fmt.str "MCP server %s is not connected" server)
  | `Rpc (server, code, message) ->
      `Unavailable (Fmt.str "MCP %s returned JSON-RPC %d: %s" server code message)
  | `Timeout _ -> `Timeout 10.
  | `Transport (server, message) -> `Io (server, message)

let truncate_output (ctx : Tool.ctx) text =
  let content, artifact =
    Artifact.truncate ctx.Tool.artifacts ~random:ctx.Tool.random text
  in
  Tool.ok ?artifact content

let run_resources (ctx : Tool.ctx) input =
  let* ({ server } : resource_args) = Tool.decode resource_codec input in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"list_mcp_resources" ~action:server ~path:""
      ~description:(Fmt.str "List resources from MCP server %s" server)
  in
  match Mcp.resources ctx.Tool.mcp ~server with
  | Error error -> Error (map_mcp_error error)
  | Ok resources ->
      let line (resource : Mcp.resource) =
        Fmt.str "%s\t%s\t%s" resource.Mcp.uri resource.Mcp.name
          (Option.value resource.Mcp.mime ~default:"")
      in
      let text =
        match resources with
        | [] -> "no resources"
        | values -> String.concat "\n" (List.map line values)
      in
      Ok (truncate_output ctx text)

let run_read_resource (ctx : Tool.ctx) input =
  let* ({ server; uri } : read_resource_args) = Tool.decode read_resource_codec input in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"read_mcp_resource" ~action:server ~path:""
      ~description:(Fmt.str "Read MCP resource %s" uri)
  in
  match Mcp.read_resource ctx.Tool.mcp ~server ~uri with
  | Error error -> Error (map_mcp_error error)
  | Ok content -> Ok (truncate_output ctx (Mcp.content_text content))

let member name value =
  match value with
  | Jsont.Object (members, _) -> Option.map snd (Jsont.Json.find_mem name members)
  | _ -> None

let string_member name value =
  match member name value with Some (Jsont.String (text, _)) -> Some text | _ -> None

let array_member name value =
  match member name value with Some (Jsont.Array (values, _)) -> Some values | _ -> None

let object_member name value =
  match member name value with
  | Some (Jsont.Object (values, _)) -> Some values
  | _ -> None

let has_member name value = Option.is_some (member name value)

let json_kind = function
  | Jsont.Null _ -> "null"
  | Jsont.Bool _ -> "boolean"
  | Jsont.Number _ -> "number"
  | Jsont.String _ -> "string"
  | Jsont.Array _ -> "array"
  | Jsont.Object _ -> "object"

let type_matches expected value =
  match expected with
  | "object" -> ( match value with Jsont.Object _ -> true | _ -> false)
  | "array" -> ( match value with Jsont.Array _ -> true | _ -> false)
  | "string" -> ( match value with Jsont.String _ -> true | _ -> false)
  | "number" -> ( match value with Jsont.Number _ -> true | _ -> false)
  | "integer" -> (
      match value with Jsont.Number (number, _) -> Float.is_integer number | _ -> false)
  | "boolean" -> ( match value with Jsont.Bool _ -> true | _ -> false)
  | "null" -> ( match value with Jsont.Null _ -> true | _ -> false)
  | _ -> true

let rec validate_schema schema value path =
  let fail message = Error (Fmt.str "%s: %s" path message) in
  match string_member "type" schema with
  | Some expected when not (type_matches expected value) ->
      fail (Fmt.str "expected %s, got %s" expected (json_kind value))
  | _ -> (
      match array_member "enum" schema with
      | Some values when not (List.exists (Jsont.Json.equal value) values) ->
          fail "value is not in enum"
      | _ -> (
          match value with
          | Jsont.Object (_, _) -> (
              let required = Option.value (array_member "required" schema) ~default:[] in
              let missing =
                List.find_map
                  (function
                    | Jsont.String (name, _) when not (has_member name value) -> Some name
                    | _ -> None)
                  required
              in
              match missing with
              | Some name -> fail (Fmt.str "missing required property %s" name)
              | None -> (
                  match object_member "properties" schema with
                  | None -> Ok ()
                  | Some properties ->
                      let rec check = function
                        | [] -> Ok ()
                        | ((name, _), property) :: rest -> (
                            match member name value with
                            | None -> check rest
                            | Some property_value ->
                                let* () =
                                  validate_schema property property_value
                                    (path ^ "." ^ name)
                                in
                                check rest)
                      in
                      check properties))
          | Jsont.Array (values, _) -> (
              match member "items" schema with
              | None -> Ok ()
              | Some item_schema ->
                  let rec check index = function
                    | [] -> Ok ()
                    | item :: rest ->
                        let* () =
                          validate_schema item_schema item (Fmt.str "%s[%d]" path index)
                        in
                        check (index + 1) rest
                  in
                  check 0 values)
          | _ -> Ok ()))

let schema_valid schema input = validate_schema schema input "$"

let run_dynamic (definition : Mcp.tool) (ctx : Tool.ctx) input =
  let* input = Tool.decode Jsont.json input in
  let* () =
    match schema_valid definition.Mcp.schema input with
    | Ok () -> Ok ()
    | Error message -> Error (`Invalid_input message)
  in
  let name = Mcp.tool_name ~server:definition.Mcp.server definition.Mcp.name in
  let* () =
    Tool.request ctx ~read_only:false ~tool:name ~action:name ~path:""
      ~description:(Fmt.str "Call MCP tool %s" name)
  in
  match
    Mcp.call ctx.Tool.mcp ~server:definition.Mcp.server ~tool:definition.Mcp.name ~input
  with
  | Error error -> Error (map_mcp_error error)
  | Ok (content, is_error) ->
      let output = truncate_output ctx (Mcp.content_text content) in
      Ok (if is_error then { output with is_error = true } else output)

let mcp_tool (definition : Mcp.tool) =
  {
    Tool.name = Mcp.tool_name ~server:definition.Mcp.server definition.Mcp.name;
    description = definition.Mcp.description;
    schema = definition.Mcp.schema;
    read_only = false;
    run = run_dynamic definition;
  }

let list_mcp_resources =
  {
    Tool.name = "list_mcp_resources";
    description = "List resources exposed by an MCP server";
    schema = resource_schema;
    read_only = true;
    run = run_resources;
  }

let read_mcp_resource =
  {
    Tool.name = "read_mcp_resource";
    description = "Read a resource exposed by an MCP server";
    schema = read_resource_schema;
    read_only = true;
    run = run_read_resource;
  }

let all = [ list_mcp_resources; read_mcp_resource ]
