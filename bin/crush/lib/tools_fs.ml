open Result.Syntax

let max_file_bytes = 10 * 1024 * 1024
let max_line_bytes = 2000

type line_file = { lines : string array; trailing_newline : bool }

let split_lines s =
  if s = "" then { lines = [||]; trailing_newline = false }
  else
    let trailing_newline = String.ends_with ~suffix:"\n" s in
    let parts = String.split_on_char '\n' s in
    let parts = if trailing_newline then List.rev (List.tl (List.rev parts)) else parts in
    { lines = Array.of_list parts; trailing_newline }

let join_lines file =
  let body = String.concat "\n" (Array.to_list file.lines) in
  if file.trailing_newline && Array.length file.lines > 0 then body ^ "\n" else body

let split_patch_lines source =
  if source = "" then [||]
  else
    let lines = String.split_on_char '\n' source in
    let lines =
      if String.ends_with ~suffix:"\n" source then List.rev (List.tl (List.rev lines))
      else lines
    in
    Array.of_list lines

let parse_positive_int text =
  let rec digits index =
    if index = String.length text then true
    else
      let c = text.[index] in
      c >= '0' && c <= '9' && digits (index + 1)
  in
  if text = "" || not (digits 0) then None else int_of_string_opt text

let parse_range_token token =
  match String.index_opt token '.' with
  | Some dot when dot + 1 < String.length token && token.[dot + 1] = '=' ->
      let first = String.sub token 0 dot in
      let last = String.sub token (dot + 2) (String.length token - dot - 2) in
      begin match (parse_positive_int first, parse_positive_int last) with
      | Some first, Some last -> Some (first, last)
      | _ -> None
      end
  | _ -> None

let valid_tag tag =
  let is_hex c = (c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') in
  String.length tag = 4 && String.fold_left (fun result c -> result && is_hex c) true tag

let parse_header line =
  let length = String.length line in
  if length < 5 || line.[0] <> '[' || line.[length - 1] <> ']' then None
  else
    let inner = String.sub line 1 (length - 2) in
    Option.bind (String.rindex_opt inner '#') (fun hash ->
        let path = String.sub inner 0 hash in
        let tag = String.sub inner (hash + 1) (String.length inner - hash - 1) in
        if path = "" || not (valid_tag tag) then None else Some (path, tag))

type command =
  | Put_range_command of int * int
  | Put_before_command of int
  | Put_after_command of int
  | Cut_command of int * int

let parse_command line =
  let invalid () = Error "invalid patch operation" in
  let parse_number text make =
    match parse_positive_int text with
    | Some value -> Ok (make value)
    | None -> invalid ()
  in
  if String.length line >= 4 && String.sub line 0 4 = "PUT " then
    let rest = String.sub line 4 (String.length line - 4) in
    if not (String.ends_with ~suffix:":" rest) then invalid ()
    else
      let body = String.sub rest 0 (String.length rest - 1) in
      if body <> "" && body.[0] = '<' then
        parse_number
          (String.sub body 1 (String.length body - 1))
          (fun n -> Put_before_command n)
      else if body <> "" && body.[0] = '>' then
        parse_number
          (String.sub body 1 (String.length body - 1))
          (fun n -> Put_after_command n)
      else
        match parse_range_token body with
        | Some (first, last) -> Ok (Put_range_command (first, last))
        | None -> invalid ()
  else if String.length line >= 4 && String.sub line 0 4 = "CUT " then
    let rest = String.sub line 4 (String.length line - 4) in
    begin match parse_range_token rest with
    | Some (first, last) -> Ok (Cut_command (first, last))
    | None -> invalid ()
    end
  else invalid ()

module Patch = struct
  type op =
    | Put of { first : int; last : int; body : string list }
    | Insert_before of { line : int; body : string list }
    | Insert_after of { line : int; body : string list }
    | Cut of { first : int; last : int }

  type patch = { path : string; tag : string; ops : op list }

  let parse source =
    let lines = split_patch_lines source in
    if Array.length lines = 0 then Error "line 1: missing patch header"
    else
      match parse_header lines.(0) with
      | None -> Error "line 1: invalid patch header"
      | Some (path, tag) ->
          let rec body_rows index acc =
            if
              index < Array.length lines
              && String.length lines.(index) > 0
              && lines.(index).[0] = '+'
            then
              body_rows (index + 1)
                (String.sub lines.(index) 1 (String.length lines.(index) - 1) :: acc)
            else (index, List.rev acc)
          in
          let rec loop index ops =
            if index >= Array.length lines then Ok { path; tag; ops = List.rev ops }
            else if String.length lines.(index) > 0 && lines.(index).[0] = '+' then
              Error (Fmt.str "line %d: body row has no operation" (index + 1))
            else
              match parse_command lines.(index) with
              | Error message -> Error (Fmt.str "line %d: %s" (index + 1) message)
              | Ok command ->
                  let next, body = body_rows (index + 1) [] in
                  let op =
                    match command with
                    | Put_range_command (first, last) -> Put { first; last; body }
                    | Put_before_command line -> Insert_before { line; body }
                    | Put_after_command line -> Insert_after { line; body }
                    | Cut_command (first, last) -> Cut { first; last }
                  in
                  begin match (command, body) with
                  | Cut_command _, _ :: _ ->
                      Error (Fmt.str "line %d: CUT does not take a body" (index + 2))
                  | _ -> loop next (op :: ops)
                  end
          in
          loop 1 []

  type interval = { first : int; last : int }

  let apply content patch =
    let source = split_lines content in
    let total = Array.length source.lines in
    let intervals =
      List.filter_map
        (function
          | Put { first; last; _ } | Cut { first; last } -> Some { first; last }
          | Insert_before _ | Insert_after _ -> None)
        patch.ops
    in
    let rec validate_intervals = function
      | [] -> Ok ()
      | first :: rest ->
          if first.first < 1 || first.last < first.first || first.last > total then
            Error
              (Fmt.str "line range %d.= %d is outside 1..%d" first.first first.last total)
          else if
            List.exists
              (fun other -> other.first <= first.last && first.first <= other.last)
              rest
          then Error (Fmt.str "overlapping line range %d.= %d" first.first first.last)
          else validate_intervals rest
    in
    let validate_insertions =
      List.fold_left
        (fun result op ->
          match (result, op) with
          | (Error _ as error), _ -> error
          | Ok (), Insert_before { line; _ } ->
              if line < 1 || line > total + 1 then
                Error (Fmt.str "insert-before line %d is outside 1..%d" line (total + 1))
              else if
                List.exists
                  (fun interval -> interval.first <= line && line <= interval.last)
                  intervals
              then Error (Fmt.str "insert-before line %d overlaps a line range" line)
              else Ok ()
          | Ok (), Insert_after { line; _ } ->
              if line < 0 || line > total then
                Error (Fmt.str "insert-after line %d is outside 0..%d" line total)
              else if
                line <> 0
                && List.exists
                     (fun interval -> interval.first <= line && line <= interval.last)
                     intervals
              then Error (Fmt.str "insert-after line %d overlaps a line range" line)
              else Ok ()
          | Ok (), (Put _ | Cut _) -> Ok ())
        (Ok ()) patch.ops
    in
    match (validate_intervals intervals, validate_insertions) with
    | Error message, _ | _, Error message -> Error message
    | Ok (), Ok () ->
        let insertion_table = Hashtbl.create 8 in
        let replacement_table = Hashtbl.create 8 in
        let added, removed =
          List.fold_left
            (fun (added, removed) op ->
              match op with
              | Put { first; last; body } ->
                  Hashtbl.add replacement_table (first - 1) (last, body);
                  (added + List.length body, removed + last - first + 1)
              | Cut { first; last } ->
                  Hashtbl.add replacement_table (first - 1) (last, []);
                  (added, removed + last - first + 1)
              | Insert_before { line; body } ->
                  let position = line - 1 in
                  let previous =
                    Option.value (Hashtbl.find_opt insertion_table position) ~default:[]
                  in
                  Hashtbl.replace insertion_table position (previous @ body);
                  (added + List.length body, removed)
              | Insert_after { line; body } ->
                  let position = line in
                  let previous =
                    Option.value (Hashtbl.find_opt insertion_table position) ~default:[]
                  in
                  Hashtbl.replace insertion_table position (previous @ body);
                  (added + List.length body, removed))
            (0, 0) patch.ops
        in
        let output = ref [] in
        let add_rows rows = List.iter (fun row -> output := row :: !output) rows in
        let rec emit position =
          begin match Hashtbl.find_opt insertion_table position with
          | Some rows -> add_rows rows
          | None -> ()
          end;
          if position < total then
            match Hashtbl.find_opt replacement_table position with
            | Some (last, rows) ->
                add_rows rows;
                emit last
            | None ->
                output := source.lines.(position) :: !output;
                emit (position + 1)
        in
        emit 0;
        let result_lines = Array.of_list (List.rev !output) in
        Ok
          ( join_lines { lines = result_lines; trailing_newline = source.trailing_newline },
            added,
            removed )
end

let protect_io path f =
  try Ok (f ()) with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Error (`Not_found path)
  | Eio.Io (Eio.Fs.E _, _) as exn -> Error (`Io (path, Fmt.str "%a" Eio.Exn.pp exn))
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (`Io (path, Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument))

let path ctx path = Eio.Path.(ctx.Tool.fs / path)

type write_target = Existing of string | New of string

let append_components path components = List.fold_left Filename.concat path components

let rec canonical_new_target ctx current components =
  match Tool.canonical ctx current with
  | Error (`Not_found _) ->
      let parent = Filename.dirname current in
      if parent = current then Error (`Not_found current)
      else canonical_new_target ctx parent (Filename.basename current :: components)
  | resolved ->
      let* parent = resolved in
      Ok (New (append_components parent components))

let resolve_write_target ctx absolute =
  let target_path = path ctx absolute in
  let* kind = protect_io absolute (fun () -> Eio.Path.kind ~follow:false target_path) in
  match kind with
  | `Not_found -> canonical_new_target ctx absolute []
  | _ ->
      let* target = Tool.canonical ctx absolute in
      Ok (Existing target)

let canonical_for_read ctx absolute =
  match Tool.canonical ctx absolute with
  | Error (`Not_found _) -> Ok absolute
  | resolved -> resolved

let output ctx ?(diagnostics = []) text =
  let content, artifact =
    Artifact.truncate ctx.Tool.artifacts ~random:ctx.Tool.random text
  in
  Tool.ok ?artifact ~diagnostics content

let diagnostics ctx target =
  match ctx.Tool.lsp with
  | None -> []
  | Some lsp ->
      begin match
        protect_io target (fun () ->
            Lsp.touch lsp ~path:target;
            Lsp.diagnostics lsp ~path:target ~wait:0.5)
      with
      | Ok values -> values
      | Error _ -> []
      end

let touch_lsp ctx target =
  match ctx.Tool.lsp with
  | None -> ()
  | Some lsp ->
      begin match Lsp.handles lsp ~path:target with
      | None -> ()
      | Some _ -> ignore (protect_io target (fun () -> Lsp.touch lsp ~path:target))
      end

let truncate_line line =
  if String.length line <= max_line_bytes then line
  else
    let rec boundary position last =
      if position >= max_line_bytes then last
      else
        let decoded = String.get_utf_8_uchar line position in
        if not (Uchar.utf_decode_is_valid decoded) then last
        else
          let next = position + Uchar.utf_decode_length decoded in
          if next > max_line_bytes then last else boundary next next
    in
    String.sub line 0 (boundary 0 0) ^ "..."

let binary_content content =
  let length = min 8192 (String.length content) in
  let rec loop index =
    if index = length then false
    else if content.[index] = '\000' then true
    else loop (index + 1)
  in
  loop 0

let parse_selector path =
  let invalid = (path, None) in
  match String.rindex_opt path ':' with
  | None -> invalid
  | Some colon -> (
      let suffix = String.sub path (colon + 1) (String.length path - colon - 1) in
      let base = String.sub path 0 colon in
      if base = "" then invalid
      else if String.length suffix > 1 && suffix.[0] = '-' then
        begin match
          parse_positive_int (String.sub suffix 1 (String.length suffix - 1))
        with
        | Some count -> (base, Some (`Last count))
        | None -> invalid
        end
      else
        match String.index_opt suffix '-' with
        | Some dash when dash > 0 ->
            let first = String.sub suffix 0 dash in
            let last = String.sub suffix (dash + 1) (String.length suffix - dash - 1) in
            begin match (parse_positive_int first, parse_positive_int last) with
            | Some first, Some last -> (base, Some (`Range (first, last)))
            | _ -> invalid
            end
        | _ ->
            begin match String.index_opt suffix '+' with
            | Some plus when plus > 0 ->
                let first = String.sub suffix 0 plus in
                let count =
                  String.sub suffix (plus + 1) (String.length suffix - plus - 1)
                in
                begin match (parse_positive_int first, parse_positive_int count) with
                | Some first, Some count -> (base, Some (`Count (first, count)))
                | _ -> invalid
                end
            | _ -> (
                match parse_positive_int suffix with
                | Some line -> (base, Some (`One line))
                | None -> invalid)
            end)

let selected_bounds total = function
  | None -> if total > 200 then (1, 200, true) else (1, total, false)
  | Some (`One line) -> if total = 0 then (1, 0, false) else (line, total, false)
  | Some (`Range (first, last)) ->
      let first = max 1 first and last = min total last in
      (first, last, false)
  | Some (`Count (first, count)) ->
      if count <= 0 then (first, first - 1, false)
      else (first, min total (first + count - 1), false)
  | Some (`Last count) ->
      if count <= 0 then (total + 1, total, false)
      else (max 1 (total - count + 1), total, false)

let format_read_content absolute tag content selector =
  let file = split_lines content in
  let total = Array.length file.lines in
  let first, last, default_truncated = selected_bounds total selector in
  let buffer = Buffer.create (String.length content + 64) in
  Buffer.add_string buffer (Fmt.str "[%s#%s]\n" absolute tag);
  if first <= last then
    for number = first to last do
      Buffer.add_string buffer
        (Fmt.str "%d:%s" number (truncate_line file.lines.(number - 1)));
      if number < last then Buffer.add_char buffer '\n'
    done;
  if default_truncated then begin
    if first <= last then Buffer.add_char buffer '\n';
    Buffer.add_string buffer (Fmt.str "(more lines: %d; read with :201-400)" total)
  end;
  Buffer.contents buffer

let selector_error = function
  | Some (`One line) when line <= 0 -> Some "line selector must be greater than zero"
  | Some (`Range (first, last)) when first <= 0 || last <= 0 || first > last ->
      Some "line range must use positive ordered bounds"
  | Some (`Count (first, count)) when first <= 0 || count <= 0 ->
      Some "line count selector must use positive bounds"
  | Some (`Last count) when count <= 0 ->
      Some "last-lines selector must be greater than zero"
  | _ -> None

let read_params_jsont =
  Jsont.Object.map (fun path -> path)
  |> Jsont.Object.mem "path" Jsont.string
  |> Jsont.Object.finish

let write_params_jsont =
  Jsont.Object.map (fun path content -> (path, content))
  |> Jsont.Object.mem "path" Jsont.string
  |> Jsont.Object.mem "content" Jsont.string
  |> Jsont.Object.finish

let edit_params_jsont =
  Jsont.Object.map (fun patch -> patch)
  |> Jsont.Object.mem "patch" Jsont.string
  |> Jsont.Object.finish

let read_schema =
  Tool.schema_object ~required:[ "path" ]
    [ ("path", Tool.s_string ~desc:"File path or artifact URI." ()) ]

let write_schema =
  Tool.schema_object ~required:[ "path"; "content" ]
    [ ("path", Tool.s_string ()); ("content", Tool.s_string ()) ]

let edit_schema =
  Tool.schema_object ~required:[ "patch" ]
    [ ("patch", Tool.s_string ~desc:"Hashline patch text." ()) ]

let run_read ctx json =
  let* raw_path = Tool.decode read_params_jsont json in
  let base, selector = parse_selector raw_path in
  begin match selector_error selector with
  | Some message -> Error (`Invalid_input message)
  | None ->
      if String.length base >= 10 && String.sub base 0 10 = "artifact://" then begin
        let id = String.sub base 10 (String.length base - 10) in
        let* () =
          Tool.request ctx ~read_only:true ~tool:"read" ~action:"read" ~path:base
            ~description:base
        in
        begin match Artifact.load ctx.Tool.artifacts ~id with
        | Error (`Not_found missing) -> Error (`Not_found missing)
        | Error (`Io (path, message)) -> Error (`Io (path, message))
        | Ok content ->
            if (not (String.is_valid_utf_8 content)) || binary_content content then
              Ok (Tool.fail "not valid UTF-8")
            else
              Ok
                (output ctx
                   (format_read_content base (Hashline.tag content) content selector))
        end
      end
      else if String.length base >= 8 && String.sub base 0 8 = "skill://" then begin
        let* () =
          Tool.request ctx ~read_only:true ~tool:"read" ~action:"read" ~path:base
            ~description:base
        in
        begin match Skills.resolve_uri ctx.Tool.skills ~fs:ctx.Tool.fs base with
        | Error (`Not_found missing) -> Error (`Not_found missing)
        | Ok content ->
            if (not (String.is_valid_utf_8 content)) || binary_content content then
              Ok (Tool.fail "not valid UTF-8")
            else
              Ok
                (output ctx
                   (format_read_content base (Hashline.tag content) content selector))
        end
      end
      else
        let absolute = Tool.absolute ctx base in
        let target_result = canonical_for_read ctx absolute in
        let request_path =
          match target_result with Ok target -> target | Error _ -> absolute
        in
        begin
          let* () =
            Tool.request ctx ~read_only:true ~tool:"read" ~action:"read"
              ~path:request_path ~description:base
          in
          begin
            let* target = target_result in
            let target_path = path ctx target in
            let* kind =
              protect_io target (fun () -> Eio.Path.kind ~follow:true target_path)
            in
            match kind with
            | `Directory -> Ok (Tool.fail (Fmt.str "%s is a directory; use ls" target))
            | `Regular_file ->
                let* stat =
                  protect_io target (fun () -> Eio.Path.stat ~follow:true target_path)
                in
                if Optint.Int63.to_int stat.Eio.File.Stat.size > max_file_bytes then
                  Ok (Tool.fail (Fmt.str "%s is too large (maximum is 10 MiB)" target))
                else
                  let* content =
                    protect_io target (fun () -> Eio.Path.load target_path)
                  in
                  if (not (String.is_valid_utf_8 content)) || binary_content content then
                    Ok (Tool.fail "not valid UTF-8")
                  else begin
                    touch_lsp ctx target;
                    let now = int_of_float (Eio.Time.now ctx.Tool.clock *. 1000.) in
                    Hashtbl.replace ctx.Tool.read_tracker target now;
                    Ok
                      (output ctx
                         (format_read_content target (Hashline.tag content) content
                            selector))
                  end
            | _ -> Ok (Tool.fail (Fmt.str "%s is not a regular file" target))
          end
        end
  end

let save_file ctx target content =
  let target_path = path ctx target in
  let parent = Option.map fst (Eio.Path.split target_path) in
  try
    Eio.Cancel.protect (fun () ->
        begin match parent with
        | Some parent -> Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 parent
        | None -> ()
        end;
        Eio.Path.save ~create:(`Or_truncate 0o644) target_path content);
    Ok ()
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Error (`Not_found target)
  | Eio.Io (Eio.Fs.E _, _) as exn -> Error (`Io (target, Fmt.str "%a" Eio.Exn.pp exn))
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (`Io
           (target, Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument))

let run_write ctx json =
  let* target_name, content = Tool.decode write_params_jsont json in
  let absolute = Tool.absolute ctx target_name in
  let target_result = resolve_write_target ctx absolute in
  let request_path =
    match target_result with
    | Ok (Existing target) | Ok (New target) -> target
    | Error _ -> absolute
  in
  begin
    let* () =
      Tool.request ctx ~read_only:false ~tool:"write" ~action:"write" ~path:request_path
        ~description:target_name
    in
    let* resolved = target_result in
    match resolved with
    | New target -> begin
        let* () = save_file ctx target content in
        Hashtbl.replace ctx.Tool.read_tracker target
          (int_of_float (Eio.Time.now ctx.Tool.clock *. 1000.));
        let tag = Hashline.tag content in
        Ok
          (output ctx ~diagnostics:(diagnostics ctx target)
             (Fmt.str "wrote %s (%d bytes) [%s#%s]" target (String.length content) target
                tag))
      end
    | Existing target ->
        let target_path = path ctx target in
        let* kind =
          protect_io target (fun () -> Eio.Path.kind ~follow:true target_path)
        in
        begin match kind with
        | `Regular_file ->
            if
              not
                (Hashtbl.mem ctx.Tool.read_tracker target
                || Hashtbl.mem ctx.Tool.read_tracker absolute)
            then
              Ok
                (Tool.fail
                   (Fmt.str
                      "%s exists and was not read this session; read it first or use edit"
                      target))
            else begin
              let* () = save_file ctx target content in
              Hashtbl.replace ctx.Tool.read_tracker target
                (int_of_float (Eio.Time.now ctx.Tool.clock *. 1000.));
              let tag = Hashline.tag content in
              Ok
                (output ctx ~diagnostics:(diagnostics ctx target)
                   (Fmt.str "wrote %s (%d bytes) [%s#%s]" target (String.length content)
                      target tag))
            end
        | `Directory -> Ok (Tool.fail (Fmt.str "%s is a directory" target))
        | _ -> Ok (Tool.fail (Fmt.str "%s is not a regular file" target))
        end
  end

let first_patch_line = function
  | Patch.Put { first; _ } | Patch.Cut { first; _ } -> first
  | Patch.Insert_before { line; _ } | Patch.Insert_after { line; _ } -> max 1 line

let format_context absolute content anchor =
  let file = split_lines content in
  let total = Array.length file.lines in
  let first = max 1 (anchor - 10) in
  let last = min total (anchor + 9) in
  let buffer = Buffer.create 512 in
  Buffer.add_string buffer (Fmt.str "[%s#%s]\n" absolute (Hashline.tag content));
  if first <= last then
    for number = first to last do
      Buffer.add_string buffer
        (Fmt.str "%d:%s" number (truncate_line file.lines.(number - 1)));
      if number < last then Buffer.add_char buffer '\n'
    done;
  Buffer.contents buffer

let changed_regions (patch : Patch.patch) =
  let rows = ref [] in
  let add_body first body =
    List.iteri
      (fun index row ->
        if List.length !rows < 60 then
          rows := Fmt.str "%d:%s" (first + index) (truncate_line row) :: !rows)
      body
  in
  List.iter
    (function
      | Patch.Put { first; body; _ } -> add_body first body
      | Patch.Insert_before { line; body } -> add_body line body
      | Patch.Insert_after { line; body } -> add_body (max 1 (line + 1)) body
      | Patch.Cut _ -> ())
    patch.Patch.ops;
  let rows = List.rev !rows in
  String.concat "\n" rows

let edit_output patch target added removed tag =
  let summary = Fmt.str "edited %s: +%d -%d [%s#%s]" target added removed target tag in
  let regions = changed_regions patch in
  if regions = "" then summary else summary ^ "\n" ^ regions

let run_edit ctx json =
  let* patch_text = Tool.decode edit_params_jsont json in
  begin match Patch.parse patch_text with
  | Error message -> Error (`Invalid_input message)
  | Ok patch ->
      let absolute = Tool.absolute ctx patch.Patch.path in
      let target_result = Tool.canonical ctx absolute in
      let request_path =
        match target_result with Ok target -> target | Error _ -> absolute
      in
      begin
        let* () =
          Tool.request ctx ~read_only:false ~tool:"edit" ~action:"edit" ~path:request_path
            ~description:
              (if String.length patch_text <= 80 then patch_text
               else String.sub patch_text 0 80)
        in
        begin
          let* target = target_result in
          let target_path = path ctx target in
          begin match
            protect_io target (fun () -> Eio.Path.kind ~follow:true target_path)
          with
          | Error error -> Error error
          | Ok `Regular_file ->
              begin match protect_io target (fun () -> Eio.Path.load target_path) with
              | Error error -> Error error
              | Ok content when Hashline.tag content <> patch.Patch.tag ->
                  let anchor =
                    match patch.Patch.ops with [] -> 1 | op :: _ -> first_patch_line op
                  in
                  let message =
                    Fmt.str "stale tag: %s is now #%s; re-read before editing\n%s" target
                      (Hashline.tag content)
                      (format_context target content anchor)
                  in
                  Ok (Tool.fail message)
              | Ok content ->
                  begin match Patch.apply content patch with
                  | Error message -> Error (`Invalid_input message)
                  | Ok (updated, added, removed) -> begin
                      let* () = save_file ctx target updated in
                      Hashtbl.replace ctx.Tool.read_tracker target
                        (int_of_float (Eio.Time.now ctx.Tool.clock *. 1000.));
                      let tag = Hashline.tag updated in
                      Ok
                        (output ctx ~diagnostics:(diagnostics ctx target)
                           (edit_output patch target added removed tag))
                    end
                  end
              end
          | Ok `Directory -> Ok (Tool.fail (Fmt.str "%s is a directory" target))
          | Ok _ -> Ok (Tool.fail (Fmt.str "%s is not a regular file" target))
          end
        end
      end
  end

let read =
  {
    Tool.name = "read";
    description = "Read a UTF-8 file with line ranges.";
    schema = read_schema;
    read_only = true;
    run = run_read;
  }

let write =
  {
    Tool.name = "write";
    description = "Write file contents.";
    schema = write_schema;
    read_only = false;
    run = run_write;
  }

let edit =
  {
    Tool.name = "edit";
    description = "Apply a hashline patch.";
    schema = edit_schema;
    read_only = false;
    run = run_edit;
  }
