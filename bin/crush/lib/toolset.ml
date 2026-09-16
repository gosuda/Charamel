let duplicate_names tools =
  let seen = Hashtbl.create (List.length tools) in
  List.iter
    (fun (tool : Tool.t) ->
      if Hashtbl.mem seen tool.Tool.name then
        invalid_arg (Fmt.str "duplicate tool name: %s" tool.Tool.name)
      else Hashtbl.add seen tool.Tool.name ())
    tools;
  tools

let build ~(ctx_template : Tool.ctx) ~mcp_tools ~subagent =
  let lsp_tools =
    match ctx_template.Tool.lsp with None -> [] | Some _ -> Tools_lsp.all
  in
  let interactive_tools =
    if
      subagent
      || (not ctx_template.Tool.interactive)
      || Option.is_none ctx_template.Tool.ask
    then []
    else [ Tools_meta.question ]
  in
  let state_tools = if subagent then [] else [ Tools_meta.todos ] in
  let has_mcp = Mcp.states ctx_template.Tool.mcp <> [] in
  let protocol_tools =
    if has_mcp then [ Tools_mcp.list_mcp_resources; Tools_mcp.read_mcp_resource ] else []
  in
  let agent_tools =
    if subagent || Option.is_none ctx_template.Tool.run_subagent then []
    else [ Tools_agent.agent ]
  in
  let mcp_tools = List.map Tools_mcp.mcp_tool mcp_tools in
  let tools =
    [
      Tools_fs.read;
      Tools_fs.edit;
      Tools_fs.write;
      Tools_shell.bash;
      Tools_search.ls;
      Tools_search.glob;
      Tools_search.grep;
      Tools_net.fetch;
    ]
    @ lsp_tools @ state_tools @ interactive_tools
    @ [
        Tools_meta.crush_info;
        Tools_meta.crush_logs;
        Tools_shell.job_output;
        Tools_shell.job_kill;
      ]
    @ protocol_tools @ agent_tools @ mcp_tools
  in
  duplicate_names tools

let find tools name = List.find_opt (fun (tool : Tool.t) -> tool.Tool.name = name) tools
let fantasy tools = List.map Tool.to_fantasy tools
