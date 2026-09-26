(** Glow input sources and bounded HTTP retrieval. *)

type location =
  | Stdin
  | File of string
  | Directory of string
  | Url of string  (** The kinds of input accepted by the command. *)

type document = {
  content : string;
  path : string option;
  base_url : string option;
  markdown : bool;
}
(** A loaded document. [path] is present only for local files, and [base_url] is used to
    resolve relative links in rendered Markdown. *)

type error = [ `Invalid of string | `Io of string * string | `Http of string ]
(** An input or transport failure. *)

val classify :
  argument:string option -> cwd:string -> stdin_is_tty:bool -> (location, error) result
(** [classify ~argument ~cwd ~stdin_is_tty] classifies one command argument. Missing input
    reads a pipe when standard input is not a terminal and opens the current directory
    otherwise. *)

val is_markdown_path : string -> bool
(** [is_markdown_path path] is [true] for the supported Markdown extensions. *)

val remove_frontmatter : string -> string
(** [remove_frontmatter text] removes one leading YAML frontmatter block. *)

val discover_markdown : root:string -> show_hidden:bool -> string list
(** [discover_markdown ~root ~show_hidden] recursively lists Markdown files in stable
    order, skipping [.git] and [node_modules]. *)

val readme_candidates : host:string -> owner:string -> repo:string -> string list
(** [readme_candidates ~host ~owner ~repo] is the ordered set of raw README URLs used when
    a repository API does not expose a download URL. *)

val read :
  cwd:string -> stdin:Lwt_io.input_channel -> location -> (document, error) result Lwt.t
(** [read ~cwd ~stdin location] reads a bounded local or network source, resolving a
    relative [File] path against [cwd]. Network operations have a thirty-second overall
    deadline, a five-redirect limit, and a ten-megabyte response limit. *)
