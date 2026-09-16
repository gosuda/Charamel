(** MCP-backed agent tools.

    Resource operations and dynamically registered MCP functions share the common crush
    tool boundary. Dynamic tool schemas remain the server's JSON schemas and are checked
    before a request is sent. *)

val list_mcp_resources : Tool.t
(** [list_mcp_resources] lists resources published by an MCP server. *)

val read_mcp_resource : Tool.t
(** [read_mcp_resource] reads one resource published by an MCP server. *)

val mcp_tool : Mcp.tool -> Tool.t
(** [mcp_tool tool] is the agent tool for the MCP definition [tool]. *)

val all : Tool.t list
(** [all] is the pair of static MCP resource tools. *)
