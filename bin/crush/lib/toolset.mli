(** Crush's model-visible tool registry.

    [build] assembles the fixed local tools, optional protocol tools and MCP definitions
    in their advertised order. The registry rejects duplicate names before any model
    request can use them. *)

val build :
  ctx_template:Tool.ctx -> mcp_tools:Mcp.tool list -> subagent:bool -> Tool.t list
(** [build ~ctx_template ~mcp_tools ~subagent] is the tool registry for the context.
    Sub-agent registries omit interactive and delegation tools. *)

val find : Tool.t list -> string -> Tool.t option
(** [find tools name] is the tool named [name], or [None] when absent. *)

val fantasy : Tool.t list -> Charamel_fantasy.Tool.t list
(** [fantasy tools] converts [tools] to the provider tool definitions. *)
