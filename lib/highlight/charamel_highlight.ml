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

let tab_columns = 4

let to_cells text =
  let out = Buffer.create (String.length text) in
  let i = ref 0 in
  let finish character =
    Buffer.add_char out character;
    incr i
  in
  while !i < String.length text do
    let next = !i + 1 < String.length text && text.[!i] = '\r' && text.[!i + 1] = '\n' in
    if next then begin
      Buffer.add_char out '\n';
      i := !i + 2
    end
    else if text.[!i] = '\t' then begin
      Buffer.add_string out (String.make tab_columns ' ');
      incr i
    end
    else finish text.[!i]
  done;
  Buffer.contents out

let paint_line style line =
  if Charamel_ansi.Style.equal style Charamel_ansi.Style.default then line
  else Charamel_ansi.Style.to_sgr style ^ line ^ "\x1b[m"

let paint style text =
  String.concat "\n"
    (List.map (paint_line style) (String.split_on_char '\n' (to_cells text)))

let render ?(theme = Theme.charm ~is_dark:true) spec source =
  let buffer = Buffer.create (String.length source + 32) in
  List.iter
    (fun (kind, text) -> Buffer.add_string buffer (paint (theme kind) text))
    (tokenize spec source);
  Buffer.contents buffer
