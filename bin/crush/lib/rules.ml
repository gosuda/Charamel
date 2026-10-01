open Lwt.Infix

type rule = { path : string; globs : string list; always : bool; body : string }
type indexed_rule = { value : rule; patterns : Re.re list }
type t = { cwd : string; rules : indexed_rule list }

let max_file_bytes = 65_536
let path_of fs_root path = Path.under ~root:fs_root path

let kind path =
  Lwt.catch
    (fun () -> Lwt_unix.stat path >|= fun stats -> Some stats.Unix.st_kind)
    (function Unix.Unix_error _ | Sys_error _ -> Lwt.return_none | exn -> Lwt.fail exn)

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
  match Io.lines content with
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
  Option.map
    (fun (globs, always, body) ->
      { value = { path; globs; always; body }; patterns = compile_globs path globs })
    (parse_document path content)

let regular_file path = kind path >|= function Some Unix.S_REG -> true | _ -> false
let is_directory path = kind path >|= function Some Unix.S_DIR -> true | _ -> false

let discover_file fs_root path =
  let file = path_of fs_root path in
  regular_file file >>= fun is_file ->
  if not is_file then Lwt.return_none
  else
    Io.read_bounded file ~max:max_file_bytes >>= function
    | None -> Lwt.return_none
    | Some content -> Lwt.return (make_rule path content)

let rec discover_entries fs_root directory = function
  | [] -> Lwt.return_nil
  | entry :: rest -> (
      discover_file fs_root (Filename.concat directory entry) >>= fun found ->
      discover_entries fs_root directory rest >|= fun others ->
      match found with None -> others | Some rule -> rule :: others)

let discover_directory fs_root directory =
  let root = path_of fs_root directory in
  is_directory root >>= fun is_dir ->
  if not is_dir then Lwt.return_nil
  else
    Charamel_os.Fs.read_dir root >>= function
    | Error _ -> Lwt.return_nil
    | Ok entries ->
        discover_entries fs_root directory
          (List.filter (fun entry -> Filename.check_suffix entry ".md") entries)

let discover_context_file fs_root path =
  let file = path_of fs_root path in
  regular_file file >>= fun is_file ->
  if not is_file then Lwt.return_none
  else
    Io.read_bounded file ~max:max_file_bytes >|= function
    | None -> None
    | Some body ->
        Some { value = { path; globs = []; always = true; body }; patterns = [] }

let rec discover_context_paths fs_root cwd = function
  | [] -> Lwt.return_nil
  | relative :: rest -> (
      let path = Path.normalize ~cwd relative in
      kind (path_of fs_root path) >>= function
      | Some Unix.S_DIR ->
          discover_directory fs_root path >>= fun found ->
          discover_context_paths fs_root cwd rest >|= fun others -> found @ others
      | Some Unix.S_REG -> (
          discover_context_file fs_root path >>= fun found ->
          discover_context_paths fs_root cwd rest >|= fun others ->
          match found with None -> others | Some rule -> rule :: others)
      | Some _ | None -> discover_context_paths fs_root cwd rest)

let load ~fs_root ~cwd ~(config : Config.t) =
  let cwd = Path.normalize cwd in
  discover_context_paths fs_root cwd config.Config.context_paths >|= fun rules ->
  { cwd; rules }

let relative_path t path = Path.relative ~root:t.cwd (Path.normalize ~cwd:t.cwd path)

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
