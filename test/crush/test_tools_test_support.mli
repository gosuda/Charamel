(** Shared filesystem and tool execution helpers for Crush tests. *)

val temp_root : string -> string
(** [temp_root prefix] is a unique temporary path using [prefix]. *)

val make_context :
  ?allowed_tools:string list -> Lwt_switch.t -> string -> Crush_core.Tool.ctx Lwt.t
(** [make_context ?allowed_tools sw root] is a tool context rooted at [root].
    [allowed_tools] defaults to ["read"; "write"; "edit"]. *)

val with_context :
  ?allowed_tools:string list -> (string -> Crush_core.Tool.ctx -> 'a) -> 'a Lwt.t
(** [with_context ?allowed_tools f] runs direct-style [f] with a fresh tool context rooted
    at a temporary directory, passing the directory path, and returns the promise for its
    result. [allowed_tools] defaults to ["read"; "write"; "edit"]. *)

val mkdir_p : string -> unit
(** [mkdir_p dir] creates [dir] and any missing parents with mode [0o755]. *)

val with_scratch : (string -> unit) -> unit
(** [with_scratch f] runs direct-style [f] with a fresh temporary directory path, removed
    afterwards. Call it from inside a {!val:case} body. *)

type http_fixture = { port : int; shutdown : unit -> unit Lwt.t }
(** [http_fixture] is a started loopback HTTP responder. *)

val start_http_fixture : content_type:string -> string -> http_fixture
(** [start_http_fixture ~content_type body] answers every request on a fresh loopback port
    with [body] (draining the request first). Call it from inside a {!val:case} body; shut
    it down via [shutdown]. *)

val with_http_server : content_type:string -> body:(unit -> string) -> (int -> 'a) -> 'a
(** [with_http_server ~content_type ~body f] runs direct-style [f] with the port of a
    fixture that calls [body] once per request, and shuts it down afterwards. *)

val with_http_fixture : content_type:string -> string -> (int -> 'a) -> 'a
(** [with_http_fixture ~content_type body f] runs direct-style [f] with the port of a
    fixture that answers every request with [body], and shuts it down afterwards. *)

val write_file : string -> string -> unit
(** [write_file path content] writes [content] to [path], creating parent directories. *)

val load_file : string -> string
(** [load_file path] is the content of [path]. *)

val json_object : (string * Jsont.json) list -> Jsont.json
(** [json_object fields] is a JSON object containing [fields]. *)

val run_tool :
  Crush_core.Tool.t -> Crush_core.Tool.ctx -> Jsont.json -> Crush_core.Tool.output
(** [run_tool tool context value] runs [tool] and returns its output, failing the test on
    a tool error. *)

val case : string -> Alcotest.speed_level -> (unit -> unit) -> unit Alcotest_lwt.test_case
(** [case name speed f] is an Alcotest-Lwt case running direct-style [f] on the suite's
    single Lwt reactor via [Lwt_direct.spawn]. Every Crush test case uses it so
    direct-style code under test can [await] promises. *)
