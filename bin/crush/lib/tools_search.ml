open Result.Syntax

let max_ls_entries = 1000
let max_glob_results = 100
let max_grep_results = 500
let max_file_bytes = 10 * 1024 * 1024

let protect_io path f =
  try Ok (f ()) with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Error (`Not_found path)
  | Eio.Io (Eio.Fs.E _, _) as exn -> Error (`Io (path, Fmt.str "%a" Eio.Exn.pp exn))
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (`Io (path, Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument))

let path ctx absolute = Eio.Path.(ctx.Tool.fs / absolute)

let canonical_or_abs ctx absolute =
  match Tool.canonical ctx absolute with
  | Ok target -> Ok target
  | Error (`Not_found _) -> Ok absolute
  | Error error -> Error error

let output ctx text =
  let content, artifact =
    Artifact.truncate ctx.Tool.artifacts ~random:ctx.Tool.random text
  in
  Tool.ok ?artifact content

let trim_slashes path =
  let length = String.length path in
  if length = 0 || path = "/" then path
  else
    let rec trim index =
      if index > 0 && path.[index - 1] = '/' then trim (index - 1) else index
    in
    String.sub path 0 (trim length)

let basename path =
  match String.rindex_opt path '/' with
  | None -> path
  | Some index when index + 1 >= String.length path -> "/"
  | Some index -> String.sub path (index + 1) (String.length path - index - 1)

let relative_to root absolute =
  let root = trim_slashes root in
  if absolute = root then "."
  else
    let prefix = if root = "/" then "/" else root ^ "/" in
    if String.starts_with ~prefix absolute then
      String.sub absolute (String.length prefix)
        (String.length absolute - String.length prefix)
    else absolute

let relative_to_cwd ctx absolute = relative_to (trim_slashes ctx.Tool.cwd) absolute
let hidden_name name = String.length name > 0 && name.[0] = '.'
let generated_name name = name = ".git" || name = "_build" || name = "node_modules"

let compile_glob ?(pathname = true) pattern =
  match Re.Glob.glob_result ~anchored:true ~pathname ~double_asterisk:true pattern with
  | Ok expression -> Ok (Re.compile expression)
  | Error `Parse_error -> Error (Fmt.str "invalid glob pattern: %s" pattern)

let compile_regex pattern =
  match Re.Perl.re_result pattern with
  | Ok expression -> Ok (Re.compile expression)
  | Error `Parse_error -> Error (Fmt.str "invalid regular expression: %s" pattern)
  | Error `Not_supported -> Error (Fmt.str "unsupported regular expression: %s" pattern)

let matches_any expressions value =
  List.exists (fun expression -> Re.execp expression value) expressions

let ls_params_jsont =
  Jsont.Object.map (fun path ignore depth -> (path, ignore, depth))
  |> Jsont.Object.opt_mem "path" Jsont.string
  |> Jsont.Object.mem "ignore" (Jsont.list Jsont.string) ~dec_absent:[]
  |> Jsont.Object.mem "depth" Jsont.int ~dec_absent:3
  |> Jsont.Object.finish

let glob_params_jsont =
  Jsont.Object.map (fun pattern path -> (pattern, path))
  |> Jsont.Object.mem "pattern" Jsont.string
  |> Jsont.Object.opt_mem "path" Jsont.string
  |> Jsont.Object.finish

let grep_params_jsont =
  Jsont.Object.map (fun pattern path include_ literal max_results ->
      (pattern, path, include_, literal, max_results))
  |> Jsont.Object.mem "pattern" Jsont.string
  |> Jsont.Object.opt_mem "path" Jsont.string
  |> Jsont.Object.opt_mem "include" Jsont.string
  |> Jsont.Object.mem "literal" Jsont.bool ~dec_absent:false
  |> Jsont.Object.mem "max_results" Jsont.int ~dec_absent:100
  |> Jsont.Object.finish

let ls_schema =
  Tool.schema_object
    [
      ("path", Tool.s_string ~desc:"Directory or file path." ());
      ("ignore", Tool.s_array (Tool.s_string ()));
      ("depth", Tool.s_int ~default:3 ());
    ]

let glob_schema =
  Tool.schema_object ~required:[ "pattern" ]
    [
      ("pattern", Tool.s_string ~desc:"Anchored shell-style path pattern." ());
      ("path", Tool.s_string ~desc:"Directory in which to search." ());
    ]

let grep_schema =
  Tool.schema_object ~required:[ "pattern" ]
    [
      ("pattern", Tool.s_string ~desc:"Perl regular expression or literal text." ());
      ("path", Tool.s_string ~desc:"Directory or file path." ());
      ("include", Tool.s_string ~desc:"Basename glob filter." ());
      ("literal", Tool.s_bool ~default:false ());
      ("max_results", Tool.s_int ~default:100 ());
    ]

type tree_item = { depth : int; name : string; directory : bool }

let compile_ignores ignores =
  List.fold_left
    (fun result pattern ->
      match result with
      | Error _ as error -> error
      | Ok expressions ->
          begin match compile_glob pattern with
          | Ok expression -> Ok (expression :: expressions)
          | Error message -> Error (`Invalid_input message)
          end)
    (Ok []) ignores

let ls_entries ctx root ~ignore ~depth =
  let* ignore_patterns = compile_ignores ignore in
  let count = ref 0 in
  let truncated = ref false in
  let result = ref [] in
  let ignored relative name =
    matches_any ignore_patterns relative || matches_any ignore_patterns name
  in
  let add depth name directory =
    if !count < max_ls_entries then begin
      incr count;
      result := { depth; name; directory } :: !result
    end
    else truncated := true
  in
  let include_hidden = hidden_name (basename root) in
  let include_generated = generated_name (basename root) in
  let rec walk current relative current_depth =
    if !count >= max_ls_entries then begin
      truncated := true;
      Ok ()
    end
    else
      match
        protect_io current (fun () -> Eio.Path.read_dir_entries (path ctx current))
      with
      | Error _ as error -> error
      | Ok entries ->
          let entries =
            List.sort (fun (_, left) (_, right) -> String.compare left right) entries
          in
          let rec visit = function
            | [] -> Ok ()
            | _ when !count >= max_ls_entries ->
                truncated := true;
                Ok ()
            | (kind, name) :: rest ->
                let child = if current = "/" then "/" ^ name else current ^ "/" ^ name in
                let child_relative =
                  if relative = "." then name else relative ^ "/" ^ name
                in
                let hidden = hidden_name name && not include_hidden in
                let generated = generated_name name && not include_generated in
                if hidden || generated || ignored child_relative name then visit rest
                else
                  let directory = kind = `Directory in
                  add (current_depth + 1) name directory;
                  if directory && current_depth < depth then begin
                    let* () = walk child child_relative (current_depth + 1) in
                    visit rest
                  end
                  else visit rest
          in
          visit entries
  in
  let root_path = path ctx root in
  match protect_io root (fun () -> Eio.Path.kind ~follow:true root_path) with
  | Error error -> Error error
  | Ok `Directory ->
      let root_name = basename root in
      begin
        let* () = if depth < 1 then Ok () else walk root "." 0 in
        let root_item = { depth = 0; name = root_name; directory = true } in
        let items = root_item :: List.rev !result in
        let lines =
          List.map
            (fun item ->
              String.make (2 * item.depth) ' '
              ^ item.name
              ^ if item.directory then "/" else "")
            items
        in
        let lines = if !truncated then lines @ [ "(truncated)" ] else lines in
        Ok (String.concat "\n" lines)
      end
  | Ok _ -> Ok (basename root)

let run_with_timeout ctx seconds f =
  let* value = Tool.with_timeout ctx seconds f in
  value

let run_ls ctx json =
  let* path_opt, ignore, depth = Tool.decode ls_params_jsont json in
  if depth < 0 then Error (`Invalid_input "depth must not be negative")
  else
    let depth = min 10 depth in
    let absolute =
      Tool.absolute ctx (Option.value path_opt ~default:ctx.Tool.cwd) |> trim_slashes
    in
    let target_result = canonical_or_abs ctx absolute in
    let request_path =
      match target_result with Ok target -> target | Error _ -> absolute
    in
    begin
      let* () =
        Tool.request ctx ~read_only:true ~tool:"ls" ~action:"ls" ~path:request_path
          ~description:absolute
      in
      begin
        let* target = target_result in
        run_with_timeout ctx 30. (fun () ->
            match ls_entries ctx target ~ignore ~depth with
            | Ok text -> Ok (output ctx text)
            | Error error -> Error error)
      end
    end

let collect_files ctx ?(include_hidden = false) ?(include_generated = false) root callback
    =
  let include_hidden = include_hidden || hidden_name (basename root) in
  let include_generated = include_generated || generated_name (basename root) in
  let rec walk current relative =
    match
      protect_io current (fun () -> Eio.Path.kind ~follow:false (path ctx current))
    with
    | Error error -> Error error
    | Ok `Symbolic_link -> Ok ()
    | Ok `Regular_file ->
        callback current (if relative = "." then basename current else relative)
    | Ok `Directory -> begin
        let* entries =
          protect_io current (fun () -> Eio.Path.read_dir_entries (path ctx current))
        in
        let entries =
          List.sort (fun (_, left) (_, right) -> String.compare left right) entries
        in
        let rec visit = function
          | [] -> Ok ()
          | (kind, name) :: rest ->
              let hidden = hidden_name name && not include_hidden in
              let generated = generated_name name && not include_generated in
              if hidden || generated then visit rest
              else
                let child = if current = "/" then "/" ^ name else current ^ "/" ^ name in
                let child_relative =
                  if relative = "." then name else relative ^ "/" ^ name
                in
                begin match kind with
                | `Symbolic_link -> visit rest
                | _ ->
                    begin match walk child child_relative with
                    | Ok () -> visit rest
                    | Error _ as error -> error
                    end
                end
        in
        visit entries
      end
    | Ok _ -> Ok ()
  in
  walk root "."

type glob_match = { relative : string; mtime : float }

let take count values =
  let rec loop remaining values acc =
    if remaining = 0 then List.rev acc
    else
      match values with
      | [] -> List.rev acc
      | value :: rest -> loop (remaining - 1) rest (value :: acc)
  in
  loop count values []

let pattern_segments pattern = String.split_on_char '/' pattern

let pattern_mentions_hidden pattern =
  List.exists
    (fun segment -> String.length segment > 0 && segment.[0] = '.')
    (pattern_segments pattern)

let pattern_mentions pattern name =
  List.exists (fun segment -> segment = name) (pattern_segments pattern)

let pattern_mentions_generated pattern =
  pattern_mentions pattern ".git"
  || pattern_mentions pattern "_build"
  || pattern_mentions pattern "node_modules"

let run_glob ctx pattern path_opt =
  let pattern =
    if String.starts_with ~prefix:"./" pattern then
      String.sub pattern 2 (String.length pattern - 2)
    else pattern
  in
  match compile_glob pattern with
  | Error message -> Error (`Invalid_input message)
  | Ok expression ->
      let absolute =
        Tool.absolute ctx (Option.value path_opt ~default:ctx.Tool.cwd) |> trim_slashes
      in
      let target_result = canonical_or_abs ctx absolute in
      let request_path =
        match target_result with Ok target -> target | Error _ -> absolute
      in
      begin
        let* () =
          Tool.request ctx ~read_only:true ~tool:"glob" ~action:pattern ~path:request_path
            ~description:pattern
        in
        begin
          let* target = target_result in
          let matches = ref [] in
          let callback file relative =
            let candidate =
              if String.starts_with ~prefix:"/" pattern then file else relative
            in
            if not (Re.execp expression candidate) then Ok ()
            else
              let* stat =
                protect_io file (fun () -> Eio.Path.stat ~follow:true (path ctx file))
              in
              matches :=
                { relative = relative_to_cwd ctx file; mtime = stat.Eio.File.Stat.mtime }
                :: !matches;
              Ok ()
          in
          run_with_timeout ctx 30. (fun () ->
              let* () =
                collect_files ctx
                  ~include_hidden:(pattern_mentions_hidden pattern)
                  ~include_generated:(pattern_mentions_generated pattern)
                  target callback
              in
              let sorted =
                List.sort
                  (fun left right ->
                    let by_mtime = Float.compare right.mtime left.mtime in
                    if by_mtime <> 0 then by_mtime
                    else String.compare left.relative right.relative)
                  !matches
              in
              let sorted = take max_glob_results sorted in
              let text =
                match sorted with
                | [] -> "No files found"
                | _ -> String.concat "\n" (List.map (fun item -> item.relative) sorted)
              in
              Ok (output ctx text))
        end
      end

let read_lines content =
  if content = "" then []
  else
    let lines = String.split_on_char '\n' content in
    if String.ends_with ~suffix:"\n" content then List.rev (List.tl (List.rev lines))
    else lines

let binary content =
  let length = min 8192 (String.length content) in
  let rec loop index =
    if index = length then false
    else if content.[index] = '\000' then true
    else loop (index + 1)
  in
  loop 0

let truncate_line line =
  if String.length line <= 500 then line
  else
    let rec boundary position last =
      if position >= 500 then last
      else
        let decoded = String.get_utf_8_uchar line position in
        if not (Uchar.utf_decode_is_valid decoded) then last
        else
          let next = position + Uchar.utf_decode_length decoded in
          if next > 500 then last else boundary next next
    in
    String.sub line 0 (boundary 0 0) ^ "..."

let run_grep ctx pattern path_opt include_opt literal max_results =
  if max_results < 0 then Error (`Invalid_input "max_results must not be negative")
  else
    let matcher =
      if literal then Ok (Re.compile (Re.str pattern))
      else
        match compile_regex pattern with
        | Ok expression -> Ok expression
        | Error message -> Error (`Invalid_input message)
    in
    let include_matcher =
      match include_opt with
      | None -> Ok None
      | Some pattern ->
          begin match compile_glob ~pathname:false pattern with
          | Ok expression -> Ok (Some expression)
          | Error message -> Error (`Invalid_input message)
          end
    in
    match (matcher, include_matcher) with
    | Error error, _ | _, Error error -> Error error
    | Ok matcher, Ok include_matcher ->
        let absolute =
          Tool.absolute ctx (Option.value path_opt ~default:ctx.Tool.cwd) |> trim_slashes
        in
        let target_result = canonical_or_abs ctx absolute in
        let request_path =
          match target_result with Ok target -> target | Error _ -> absolute
        in
        begin
          let* () =
            Tool.request ctx ~read_only:true ~tool:"grep" ~action:pattern
              ~path:request_path ~description:pattern
          in
          begin
            let* target = target_result in
            let count = ref 0 in
            let truncated = ref false in
            let output_lines = ref [] in
            let callback file _relative =
              let name = basename file in
              let excluded =
                Option.fold ~none:false
                  ~some:(fun expression -> not (Re.execp expression name))
                  include_matcher
              in
              if excluded then Ok ()
              else
                match
                  protect_io file (fun () -> Eio.Path.stat ~follow:true (path ctx file))
                with
                | Error error -> Error error
                | Ok stat
                  when Optint.Int63.to_int stat.Eio.File.Stat.size > max_file_bytes ->
                    Ok ()
                | Ok _ -> begin
                    let* content =
                      protect_io file (fun () -> Eio.Path.load (path ctx file))
                    in
                    if binary content || not (String.is_valid_utf_8 content) then Ok ()
                    else
                      let lines = read_lines content in
                      let rec visit line_number = function
                        | [] -> Ok ()
                        | line :: rest when !count >= max_results ->
                            if Re.execp matcher line then begin
                              truncated := true;
                              Ok ()
                            end
                            else visit (line_number + 1) rest
                        | line :: rest ->
                            if Re.execp matcher line then begin
                              incr count;
                              output_lines :=
                                Fmt.str "%s:%d:%s" (relative_to_cwd ctx file) line_number
                                  (truncate_line line)
                                :: !output_lines
                            end;
                            visit (line_number + 1) rest
                      in
                      visit 1 lines
                  end
            in
            run_with_timeout ctx 5. (fun () ->
                let* () = collect_files ctx target callback in
                let lines = List.rev !output_lines in
                let lines =
                  if !truncated then lines @ [ Fmt.str "(truncated at %d)" max_results ]
                  else lines
                in
                Ok (output ctx (String.concat "\n" lines)))
          end
        end

let ls =
  {
    Tool.name = "ls";
    description = "List files and directories.";
    schema = ls_schema;
    read_only = true;
    run = run_ls;
  }

let glob =
  {
    Tool.name = "glob";
    description = "Find files by an anchored path pattern.";
    schema = glob_schema;
    read_only = true;
    run =
      (fun ctx json ->
        let* pattern, path_opt = Tool.decode glob_params_jsont json in
        run_glob ctx pattern path_opt);
  }

let grep =
  {
    Tool.name = "grep";
    description = "Search UTF-8 text files.";
    schema = grep_schema;
    read_only = true;
    run =
      (fun ctx json ->
        let* pattern, path_opt, include_opt, literal, max_results =
          Tool.decode grep_params_jsont json
        in
        run_grep ctx pattern path_opt include_opt literal
          (min max_results max_grep_results));
  }
