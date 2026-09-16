type skill = { name : string; description : string; dir : string; body : string }
type t = { skills : skill list }

let max_file_bytes = 65_536
let max_name_bytes = 256
let max_description_bytes = 8_192
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

let is_directory path =
  try match Eio.Path.kind ~follow:true path with `Directory -> true | _ -> false
  with Eio.Io _ | Unix.Unix_error _ -> false

let is_regular_file path =
  try match Eio.Path.kind ~follow:true path with `Regular_file -> true | _ -> false
  with Eio.Io _ | Unix.Unix_error _ -> false

let trim_cr line =
  let length = String.length line in
  if length > 0 && Char.equal line.[length - 1] '\r' then String.sub line 0 (length - 1)
  else line

let split_lines text = List.map trim_cr (String.split_on_char '\n' text)

let invalid path detail =
  invalid_arg (Fmt.str "invalid skill frontmatter in %s: %s" path detail)

let parse_header path lines =
  let name = ref None and description = ref None in
  List.iter
    (fun line ->
      let line = String.trim line in
      if String.equal line "" || (String.length line > 0 && Char.equal line.[0] '#') then
        ()
      else
        match String.index_opt line ':' with
        | None -> invalid path ("expected key: value, got " ^ line)
        | Some colon ->
            let key = String.trim (String.sub line 0 colon) in
            let value =
              String.trim (String.sub line (colon + 1) (String.length line - colon - 1))
            in
            if String.equal key "name" then name := Some value
            else if String.equal key "description" then description := Some value
            else invalid path ("unsupported field " ^ key))
    lines;
  (!name, Option.value !description ~default:"")

let parse_document path content ~default_name =
  if String.length content > max_file_bytes then None
  else
    match split_lines content with
    | first :: rest when String.equal first "---" ->
        let rec close_header index = function
          | [] -> invalid path "unterminated frontmatter"
          | line :: tail when String.equal line "---" -> (index, tail)
          | _ :: tail -> close_header (index + 1) tail
        in
        let header_count, body_lines = close_header 0 rest in
        let header_lines = List.filteri (fun index _ -> index < header_count) rest in
        let front_name, description = parse_header path header_lines in
        let name = Option.value front_name ~default:default_name in
        if String.equal name "" then invalid path "name must not be empty";
        if String.length name > max_name_bytes then invalid path "name is too long";
        if
          String.equal name "." || String.equal name ".." || String.contains name '/'
          || String.contains name '\\'
        then invalid path "name contains a path separator";
        if String.length description > max_description_bytes then
          invalid path "description is too long";
        let body = String.concat "\n" body_lines in
        Some (name, description, body)
    | _ ->
        if String.length default_name > max_name_bytes then
          invalid path "directory name is too long";
        Some (default_name, "", content)

let discover_directory fs directory =
  if not (is_directory (path_of fs directory)) then []
  else
    let entries =
      try Eio.Path.read_dir (path_of fs directory)
      with Eio.Io _ | Unix.Unix_error _ -> []
    in
    List.filter_map
      (fun name ->
        let skill_dir = Filename.concat directory name in
        let document = Filename.concat skill_dir "SKILL.md" in
        if
          (not (is_directory (path_of fs skill_dir)))
          || not (is_regular_file (path_of fs document))
        then None
        else
          match read_file_opt (path_of fs document) with
          | None -> None
          | Some content ->
              Option.map
                (fun (name, description, body) ->
                  { name; description; dir = skill_dir; body })
                (parse_document document content ~default_name:name))
      entries

let load ~fs ~(config : Config.t) ~home =
  let directories =
    config.Config.skills_paths @ [ Filename.concat home ".crush/skills" ]
  in
  let skills =
    List.fold_left
      (fun accumulated directory ->
        let discovered = discover_directory fs directory in
        List.fold_left
          (fun current skill ->
            if List.exists (fun existing -> String.equal existing.name skill.name) current
            then current
            else current @ [ skill ])
          accumulated discovered)
      [] directories
  in
  { skills }

let find t name = List.find_opt (fun skill -> String.equal skill.name name) t.skills
let all t = t.skills

let index_text t =
  t.skills
  |> List.map (fun skill -> "- " ^ skill.name ^ ": " ^ skill.description)
  |> String.concat "\n"

let safe_relative_path path =
  path <> ""
  && (not (String.equal path "."))
  && (not (String.equal path ".."))
  && (not (String.starts_with ~prefix:"/" path))
  && List.for_all
       (fun component -> component <> "" && component <> "." && component <> "..")
       (String.split_on_char '/' path)

let read_skill_relative fs skill relative =
  try
    Eio.Path.with_subtree (path_of fs skill.dir) (fun subtree ->
        read_file_opt Eio.Path.(subtree / relative))
  with Eio.Io _ | Unix.Unix_error _ -> None

let resolve_uri t ~fs uri =
  let missing () = Error (`Not_found uri) in
  if not (String.starts_with ~prefix:"skill://" uri) then missing ()
  else
    let target = String.sub uri 8 (String.length uri - 8) in
    match String.index_opt target '/' with
    | None -> (
        match find t target with Some skill -> Ok skill.body | None -> missing ())
    | Some slash -> (
        let name = String.sub target 0 slash in
        let relative = String.sub target (slash + 1) (String.length target - slash - 1) in
        if not (safe_relative_path relative) then missing ()
        else
          match find t name with
          | None -> missing ()
          | Some skill -> (
              match read_skill_relative fs skill relative with
              | Some body when String.length body <= max_file_bytes -> Ok body
              | _ -> missing ()))
