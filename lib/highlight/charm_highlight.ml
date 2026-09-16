type spec = Spec.t = {
  names : string list;
  keywords : string list;
  types : string list;
  builtins : string list;
  constants : string list;
  line_comment : string list;
  block_comment : (string * string) list;
  strings : (string * string * bool) list;
  raw_strings : (string * string) list;
  number : Re.t;
  ident : Re.t;
  operators : string list;
  attribute : Re.t option;
  case_sensitive : bool;
}

type kind = Spec.kind =
  | Keyword
  | Type
  | Builtin
  | Constant
  | String
  | Number
  | Comment
  | Operator
  | Punct
  | Ident
  | Attribute
  | Text

let languages =
  [
    Ocaml.spec;
    Go.spec;
    Rust.spec;
    Python.spec;
    Javascript.spec;
    Typescript.spec;
    Json.spec;
    Yaml.spec;
    Toml.spec;
    Bash.spec;
    C.spec;
    Cpp.spec;
    Java.spec;
    Kotlin.spec;
    Swift.spec;
    Ruby.spec;
    Php.spec;
    Html.spec;
    Css.spec;
    Sql.spec;
    Markdown.spec;
    Diff.spec;
    Dockerfile.spec;
    Makefile.spec;
    Lua.spec;
    Zig.spec;
  ]

let tokenize = Scanner.tokenize

module Theme = Theme

let normalize_query query =
  let query = String.lowercase_ascii query in
  let query =
    if String.length query > 0 && query.[0] = '.' then
      String.sub query 1 (String.length query - 1)
    else query
  in
  match String.rindex_opt query '.' with
  | Some index when index + 1 < String.length query ->
      String.sub query (index + 1) (String.length query - index - 1)
  | _ -> query

let find query =
  let query = normalize_query query in
  let matches spec =
    List.exists (fun name -> String.lowercase_ascii name = query) spec.names
  in
  List.find_opt matches languages

let render ?(theme = Theme.charm ~is_dark:true) spec source =
  let tokens = tokenize spec source in
  let buffer = Buffer.create (String.length source + 32) in
  List.iter
    (fun (kind, text) ->
      Buffer.add_string buffer (Charm_lipgloss.Style.render (theme kind) text))
    tokens;
  Buffer.contents buffer
