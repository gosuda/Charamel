let suites =
  [
    ("hashline", Test_hashline.cases);
    ("config", Test_config.cases);
    ("models", Test_models.cases);
    ("auth", Test_auth.cases);
    ("session", Test_session.cases);
    ("state-file", Test_state_file.cases);
    ("todos", Test_todos.cases);
    ("tool", Test_tool_runtime.cases);
    ("path", Test_path.cases);
    ("permission", Test_permission.cases);
    ("rules", Test_rules.cases);
    ("skills", Test_skills.cases);
    ("hooks", Test_hooks.cases);
    ("jobs", Test_jobs.cases);
    ("artifact", Test_artifact.cases);
    ("compaction", Test_compaction.cases);
    ("advisor", Test_advisor.cases);
    ("agent", Test_agent.cases);
    ("lsp", Test_lsp.cases);
    ("mcp", Test_mcp.cases);
    ("tools-fs", Test_tools_fs.cases);
    ("tools-meta", Test_tools_meta.cases);
    ("tools-net", Test_tools_net.cases);
    ("tools-search", Test_tools_search.cases);
    ("tools-shell", Test_tools_shell.cases);
    ("tools-agent", Test_tools_agent.cases);
    ("tools-protocol", Test_tools_protocol.cases);
    ("cli", Test_cli.cases);
    ("ui", Test_ui.cases);
  ]

let () = Test_support.run_lwt "crush" suites
