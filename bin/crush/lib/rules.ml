type rule = { path : string; globs : string list; always : bool; body : string }
type indexed_rule = { value : rule; patterns : Re.re list }
type t = { cwd : string; rules : indexed_rule list }

let max_file_bytes = 65_536
let path_of fs path = Eio.Path.(fs / path)

let read_file_opt path =
  try
    Eio.Path.with_open_in path (fun flow ->
        match
          Eio.Buf_read.parse ~max_size:(max_file_bytes + 1) Eio.Buf_read.take_all flow
        with
        | Ok content -> Some content
        | Error (`Msg _) -> None)
  with Eio.Io _ | Unix.Unix_error _ -> None

let kind path =
  try Some (Eio.Path.kind ~follow:true path) with Eio.Io _ | Unix.Unix_error _ -> None

let trim_cr line =
  let length = String.length line in
  if length > 0 && Char.equal line.[length - 1] '\r' then String.sub line 0 (length - 1)
  else line

let lines text = List.map trim_cr (String.split_on_char '\n' text)

let invalid path detail =
  invalid_arg (Fmt.str "invalid rule frontmatter in %s: %s" path detail)

let strip_quotes value =
  let value = String.trim value in
  let length = String.length value in
  if length >= 2 then
    let first = value.[0] and last = value.[length - 1] in
    if
      (Char.equal first '"' && Char.equal last '"')
      || (Char.equal first '\'' && Char.equal last '\'')
    then String.sub value 1 (length - 2)
    else value
  else value

let parse_globs value =
  let value = String.trim value in
  let value =
    if
      String.length value >= 2 && value.[0] = '[' && value.[String.length value - 1] = ']'
    then String.sub value 1 (String.length value - 2)
    else value
  in
  if String.equal (String.trim value) "" then []
  else
    String.split_on_char ',' value
    |> List.map (fun item -> strip_quotes (String.trim item))
    |> List.map (fun item ->
        if String.equal item "" then invalid_arg "empty rule glob" else item)

let parse_frontmatter path header_lines =
  let rec loop globs always = function
    | [] -> (globs, always)
    | line :: rest -> (
        let line = String.trim line in
        if String.equal line "" || (String.length line > 0 && line.[0] = '#') then
          loop globs always rest
        else
          match String.index_opt line ':' with
          | None -> invalid path ("expected key: value, got " ^ line)
          | Some colon ->
              let key = String.trim (String.sub line 0 colon) in
              let value =
                String.trim (String.sub line (colon + 1) (String.length line - colon - 1))
              in
              if String.equal key "globs" then (
                if not (String.equal value "") then loop (parse_globs value) always rest
                else
                  let rec list_values acc = function
                    | item :: tail ->
                        let item = String.trim item in
                        if String.length item > 1 && item.[0] = '-' then
                          let value =
                            String.trim (String.sub item 1 (String.length item - 1))
                          in
                          list_values (strip_quotes value :: acc) tail
                        else (List.rev acc, item :: tail)
                    | [] -> (List.rev acc, [])
                  in
                  let values, remaining = list_values [] rest in
                  if List.exists (String.equal "") values then
                    invalid path "empty rule glob";
                  loop values always remaining)
              else if String.equal key "always" then
                let value = String.lowercase_ascii value in
                if String.equal value "true" then loop globs (Some true) rest
                else if String.equal value "false" then loop globs (Some false) rest
                else invalid path "always must be true or false"
              else invalid path ("unsupported field " ^ key))
  in
  loop [] None header_lines

let parse_document path content =
  match lines content with
  | first :: rest when String.equal first "---" ->
      let rec close_header count = function
        | [] -> invalid path "unterminated frontmatter"
        | line :: tail when String.equal line "---" -> (count, tail)
        | _ :: tail -> close_header (count + 1) tail
      in
      let header_count, body_lines = close_header 0 rest in
      let header = List.filteri (fun index _ -> index < header_count) rest in
      let globs, always = parse_frontmatter path header in
      let always = Option.value always ~default:(globs = []) in
      Some (globs, always, String.concat "\n" body_lines)
  | _ -> Some ([], true, content)

let compile_globs path globs =
  List.map
    (fun glob ->
      match Re.Glob.glob_result ~anchored:true ~pathname:true glob with
      | Ok pattern -> Re.compile pattern
      | Error `Parse_error -> invalid path ("invalid glob " ^ glob))
    globs

let make_rule path content =
  match parse_document path content with
  | None -> None
  | Some (globs, always, body) ->
      Some { value = { path; globs; always; body }; patterns = compile_globs path globs }

let regular_file path = match kind path with Some `Regular_file -> true | _ -> false
let directory path = match kind path with Some `Directory -> true | _ -> false

let discover_file fs path =
  if not (regular_file (path_of fs path)) then None
  else Option.bind (read_file_opt (path_of fs path)) (make_rule path)

let discover_directory fs path =
  if not (directory (path_of fs path)) then []
  else
    let entries =
      try Eio.Path.read_dir (path_of fs path) with Eio.Io _ | Unix.Unix_error _ -> []
    in
    List.filter_map
      (fun entry ->
        if Filename.check_suffix entry ".md" then
          discover_file fs (Filename.concat path entry)
        else None)
      entries

let discover_context_file fs path =
  if not (regular_file (path_of fs path)) then None
  else
    Option.map
      (fun body -> { value = { path; globs = []; always = true; body }; patterns = [] })
      (read_file_opt (path_of fs path))

let canonical_path path =
  let absolute = String.length path > 0 && Char.equal path.[0] '/' in
  let rec push components = function
    | [] -> components
    | component :: rest ->
        if String.equal component "" || String.equal component "." then
          push components rest
        else if String.equal component ".." then
          match components with
          | _ :: tail -> push tail rest
          | [] -> if absolute then push [] rest else push [ ".." ] rest
        else push (component :: components) rest
  in
  let components = push [] (String.split_on_char '/' path) |> List.rev in
  let body = String.concat "/" components in
  if absolute then if String.equal body "" then "/" else "/" ^ body else body

let absolute_path ~cwd path =
  canonical_path (if Filename.is_relative path then Filename.concat cwd path else path)

let load ~fs ~cwd ~(config : Config.t) =
  let cwd = canonical_path cwd in
  let rules =
    List.concat_map
      (fun relative ->
        let path = absolute_path ~cwd relative in
        match kind (path_of fs path) with
        | Some `Directory -> discover_directory fs path
        | Some `Regular_file -> Option.to_list (discover_context_file fs path)
        | _ -> [])
      config.Config.context_paths
  in
  { cwd; rules }

let path_is_under ~root path =
  String.equal root "/" || String.equal path root
  || String.length path > String.length root
     && String.sub path 0 (String.length root) = root
     && Char.equal path.[String.length root] '/'

let relative_path t path =
  let path =
    canonical_path
      (if Filename.is_relative path then Filename.concat t.cwd path else path)
  in
  if not (path_is_under ~root:t.cwd path) then None
  else
    let offset = String.length t.cwd + if String.equal path t.cwd then 0 else 1 in
    Some
      (if offset >= String.length path then ""
       else String.sub path offset (String.length path - offset))

let matches indexed path =
  List.exists (fun pattern -> Re.execp pattern path) indexed.patterns

let for_path t path =
  match relative_path t path with
  | None -> []
  | Some relative ->
      t.rules
      |> List.filter (fun indexed ->
          indexed.value.globs <> [] && matches indexed relative)
      |> List.map (fun indexed -> indexed.value)

let render_body body =
  let length = ref (String.length body) in
  while !length > 0 && Char.equal body.[!length - 1] '\n' do
    decr length
  done;
  String.sub body 0 !length

let render_rule rule = "## " ^ rule.path ^ "\n" ^ render_body rule.body ^ "\n\n"

let context_text t =
  t.rules
  |> List.filter (fun indexed -> indexed.value.always)
  |> List.map (fun indexed -> render_rule indexed.value)
  |> String.concat ""

let attach_text t ~touched =
  let selected = ref [] in
  List.iter
    (fun path ->
      List.iter
        (fun rule ->
          if
            not
              (List.exists
                 (fun existing -> String.equal existing.path rule.path)
                 !selected)
          then selected := !selected @ [ rule ])
        (for_path t path))
    touched;
  List.map render_rule !selected |> String.concat ""
