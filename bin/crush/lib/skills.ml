open Lwt.Infix

type skill = { name : string; description : string; dir : string; body : string }
type t = { skills : skill list }

let max_file_bytes = 65_536
let max_name_bytes = 256
let max_description_bytes = 8_192
let path_of fs_root path = Filename.concat fs_root path

let kind path =
  Lwt.catch
    (fun () -> Lwt_unix.stat path >|= fun stats -> Some stats.Unix.st_kind)
    (function Unix.Unix_error _ | Sys_error _ -> Lwt.return_none | exn -> Lwt.fail exn)

let is_directory path = kind path >|= function Some Unix.S_DIR -> true | _ -> false
let is_regular_file path = kind path >|= function Some Unix.S_REG -> true | _ -> false

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
  match Io.lines content with
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
      (name, description, body)
  | _ ->
      if String.length default_name > max_name_bytes then
        invalid path "directory name is too long";
      (default_name, "", content)

let discover_entry fs_root directory name =
  let skill_dir = Filename.concat directory name in
  let document = Filename.concat skill_dir "SKILL.md" in
  let reject = Lwt.return_none in
  is_directory (path_of fs_root skill_dir) >>= fun is_dir ->
  if not is_dir then reject
  else
    is_regular_file (path_of fs_root document) >>= fun is_file ->
    if not is_file then reject
    else
      Io.read_bounded (path_of fs_root document) ~max:max_file_bytes >>= function
      | None -> reject
      | Some content ->
          let skill_name, description, body =
            parse_document document content ~default_name:name
          in
          Lwt.return_some { name = skill_name; description; dir = skill_dir; body }

let discover_directory fs_root directory =
  is_directory (path_of fs_root directory) >>= fun is_dir ->
  if not is_dir then Lwt.return_nil
  else
    Charamel_os.Fs.read_dir (path_of fs_root directory) >>= function
    | Error _ -> Lwt.return_nil
    | Ok entries ->
        Lwt_list.map_s (discover_entry fs_root directory) entries >|= fun found ->
        List.filter_map Fun.id found

let load ~fs_root ~(config : Config.t) ~home =
  let directories =
    config.Config.skills_paths @ [ Filename.concat home ".crush/skills" ]
  in
  let rec visit accumulated = function
    | [] -> Lwt.return { skills = accumulated }
    | directory :: rest ->
        discover_directory fs_root directory >>= fun discovered ->
        let skills =
          List.fold_left
            (fun current skill ->
              if
                List.exists
                  (fun existing -> String.equal existing.name skill.name)
                  current
              then current
              else current @ [ skill ])
            accumulated discovered
        in
        visit skills rest
  in
  visit [] directories

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

let resolve fs_root relative =
  Lwt.catch
    (fun () ->
      Lwt_preemptive.detach Unix.realpath (path_of fs_root relative) >|= fun target ->
      Some target)
    (function Unix.Unix_error _ | Sys_error _ -> Lwt.return_none | exn -> Lwt.fail exn)

let read_skill_relative fs_root skill relative =
  resolve fs_root skill.dir >>= fun root ->
  match root with
  | None -> Lwt.return_none
  | Some root -> (
      resolve fs_root (Filename.concat skill.dir relative) >>= fun target ->
      match target with
      | Some target when Path.within ~root target ->
          Io.read_bounded target ~max:max_file_bytes
      | _ -> Lwt.return_none)

let resolve_uri t ~fs_root uri =
  let missing () = Lwt.return_error (`Not_found uri) in
  if not (String.starts_with ~prefix:"skill://" uri) then missing ()
  else
    let target = String.sub uri 8 (String.length uri - 8) in
    match String.index_opt target '/' with
    | None -> (
        match find t target with
        | Some skill -> Lwt.return_ok skill.body
        | None -> missing ())
    | Some slash -> (
        let name = String.sub target 0 slash in
        let relative = String.sub target (slash + 1) (String.length target - slash - 1) in
        if not (safe_relative_path relative) then missing ()
        else
          match find t name with
          | None -> missing ()
          | Some skill -> (
              read_skill_relative fs_root skill relative >>= function
              | Some body -> Lwt.return_ok body
              | None -> missing ()))
