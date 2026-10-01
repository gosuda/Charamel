(** Model Context Protocol client.

    [Mcp] discovers and calls tools, resources and prompts over stdio JSONL or streamable
    HTTP. Every request has a bounded response body and a per-server deadline. *)

type tool = { server : string; name : string; description : string; schema : Jsont.json }
(** [tool] is one tool advertised by an MCP server. *)

type resource = {
  server : string;
  uri : string;
  name : string;
  mime : string option;
  description : string option;
}
(** [resource] is one resource advertised by an MCP server. *)

type prompt = {
  server : string;
  name : string;
  description : string;
  arguments : (string * bool) list;
}
(** [prompt] is one prompt advertised by an MCP server. The boolean marks a required
    argument. *)

type content =
  | Text of string
  | Image of { mime : string; data : string }
  | Resource of { uri : string; text : string option }
      (** [content] is one content item returned by an MCP operation. *)

type state =
  | Connecting
  | Connected of { tools : int; resources : int; prompts : int }
  | Failed of string
  | Disabled  (** [state] is the lifecycle state of one configured server. *)

type error =
  [ `Unknown_server of string
  | `Not_connected of string
  | `Rpc of string * int * string
  | `Timeout of string
  | `Transport of string * string ]
(** [error] is an MCP transport or protocol failure. *)

type t
(** [t] is an MCP client owning one connection fiber per configured server. *)

val create : cwd:string -> config:Config.t -> t Lwt.t
(** [create ~cwd ~config] starts one connection fiber per configured MCP server and waits
    concurrently for the ten-second readiness gate. Ready servers are retained; failed or
    timed-out servers remain visible in {!val:states}. *)

val states : t -> (string * state) list
(** [states t] is the current state of each configured server in config order. This is a
    synchronous view of the connection records and performs no I/O. *)

val tools : t -> tool list
(** [tools t] is the tools from every connected server. *)

val tool_name : server:string -> string -> string
(** [tool_name ~server name] is the collision-resistant model name [mcp_<server>_<name>].
*)

val call :
  t ->
  server:string ->
  tool:string ->
  input:Jsont.json ->
  (content list * bool, error) result Lwt.t
(** [call t ~server ~tool ~input] invokes [tools/call] and returns its content and the
    server's [isError] flag. *)

val resources : t -> server:string -> (resource list, error) result Lwt.t
(** [resources t ~server] returns all resources, following [nextCursor]. *)

val read_resource : t -> server:string -> uri:string -> (content list, error) result Lwt.t
(** [read_resource t ~server ~uri] invokes [resources/read] for [uri]. *)

val prompts : t -> server:string -> (prompt list, error) result Lwt.t
(** [prompts t ~server] returns all prompts, following [nextCursor]. *)

val get_prompt :
  t ->
  server:string ->
  name:string ->
  args:(string * string) list ->
  (string, error) result Lwt.t
(** [get_prompt t ~server ~name ~args] invokes [prompts/get] and joins returned message
    text with a blank line. *)

val close : t -> unit Lwt.t
(** [close t] rejects pending calls, stops the connection fibers, closes transports and
    terminates stdio children. It is idempotent. *)

val pp_error : error Fmt.t
(** [pp_error] formats an MCP error. *)

val content_text : content list -> string
(** [content_text contents] joins text content with newlines and represents non-text
    content with a bounded diagnostic marker. *)
