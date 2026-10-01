type t = {
  style : string;
  width : int;
  pager : bool;
  tui : bool;
  all : bool;
  line_numbers : bool;
  preserve_new_lines : bool;
  mouse : bool;
}

type overrides = {
  style : string option;
  width : int option;
  pager : bool option;
  tui : bool option;
  all : bool option;
  line_numbers : bool option;
  preserve_new_lines : bool option;
  mouse : bool option;
}

type error =
  [ `Io of string * string | `Json of string * string | `Env of string * string ]

let default : t =
  {
    style = "auto";
    width = 0;
    pager = false;
    tui = false;
    all = false;
    line_numbers = false;
    preserve_new_lines = false;
    mouse = false;
  }

let empty_overrides : overrides =
  {
    style = None;
    width = None;
    pager = None;
    tui = None;
    all = None;
    line_numbers = None;
    preserve_new_lines = None;
    mouse = None;
  }

let apply (config : t) (overrides : overrides) : t =
  {
    style = Option.value overrides.style ~default:config.style;
    width = Option.value overrides.width ~default:config.width;
    pager = Option.value overrides.pager ~default:config.pager;
    tui = Option.value overrides.tui ~default:config.tui;
    all = Option.value overrides.all ~default:config.all;
    line_numbers = Option.value overrides.line_numbers ~default:config.line_numbers;
    preserve_new_lines =
      Option.value overrides.preserve_new_lines ~default:config.preserve_new_lines;
    mouse = Option.value overrides.mouse ~default:config.mouse;
  }

let overrides_jsont =
  let open Jsont in
  Object.map (fun style width pager all line_numbers preserve_new_lines mouse ->
      ({ style; width; pager; tui = None; all; line_numbers; preserve_new_lines; mouse }
        : overrides))
  |> Object.opt_mem "style" string |> Object.opt_mem "width" int
  |> Object.opt_mem "pager" bool |> Object.opt_mem "all" bool
  |> Object.opt_mem "line_numbers" bool
  |> Object.opt_mem "preserve_new_lines" bool
  |> Object.opt_mem "mouse" bool |> Object.finish

let json_member name value = Jsont.Json.mem (Jsont.Json.name name) value

let json_of_t (config : t) : Jsont.json =
  Jsont.Json.object'
    [
      json_member "style" (Jsont.Json.string config.style);
      json_member "width" (Jsont.Json.int config.width);
      json_member "pager" (Jsont.Json.bool config.pager);
      json_member "all" (Jsont.Json.bool config.all);
      json_member "line_numbers" (Jsont.Json.bool config.line_numbers);
      json_member "preserve_new_lines" (Jsont.Json.bool config.preserve_new_lines);
      json_member "mouse" (Jsont.Json.bool config.mouse);
    ]

let default_json =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json (json_of_t default)
  with
  | Ok text -> text ^ "\n"
  | Error message ->
      Fmt.failwith "glow: failed to encode default configuration: %s" message

let parse_bool value =
  match String.lowercase_ascii (String.trim value) with
  | "1" | "true" | "yes" | "on" -> Ok true
  | "0" | "false" | "no" | "off" -> Ok false
  | _ -> Error (Fmt.str "expected true or false, got %S" value)

let parse_int name value =
  match int_of_string_opt (String.trim value) with
  | Some n when n >= 0 -> Ok n
  | _ -> Error (Fmt.str "%s must be a non-negative integer, got %S" name value)

let qualifies path = path <> "" && not (Filename.is_relative path)

let xdg_config_path ~env =
  let base =
    match env "XDG_CONFIG_HOME" with
    | Some path when qualifies path -> Some path
    | _ -> (
        match env "HOME" with
        | Some home when qualifies home -> Some (Filename.concat home ".config")
        | _ -> None)
  in
  Option.map
    (fun path -> Filename.concat (Filename.concat path "glow") "config.json")
    base

let parent path =
  let value = Filename.dirname path in
  if value = path then None else Some value

let upward_paths ~cwd =
  let rec loop directory =
    let names =
      [
        Filename.concat directory ".glow.json";
        Filename.concat directory "glow.json";
        Filename.concat (Filename.concat directory ".glow") "config.json";
      ]
    in
    match parent directory with None -> names | Some next -> names @ loop next
  in
  loop cwd

let config_path ~explicit ~cwd ~env =
  match explicit with
  | Some path when path <> "" -> Some path
  | _ -> (
      let candidates = upward_paths ~cwd in
      match List.find_opt Sys.file_exists candidates with
      | Some path -> Some path
      | None -> xdg_config_path ~env)

let parse_overrides path text : (overrides, error) result =
  match Jsont_bytesrw.decode_string overrides_jsont text with
  | Ok value -> Ok value
  | Error message -> Error (`Json (path, message))

let env_string env name =
  match env name with Some value when String.trim value <> "" -> Some value | _ -> None

let env_bool source env name =
  match env_string env name with
  | None -> Ok None
  | Some value -> (
      match parse_bool value with
      | Ok parsed -> Ok (Some parsed)
      | Error message -> Error (`Env (source, Fmt.str "%s: %s" name message)))

let env_int source env name =
  match env_string env name with
  | None -> Ok None
  | Some value -> (
      match parse_int name value with
      | Ok parsed -> Ok (Some parsed)
      | Error message -> Error (`Env (source, message)))

let merge_env source env (config : t) : (t, error) result =
  let open Result.Syntax in
  let style = env_string env "GLOW_STYLE" in
  let* width = env_int source env "GLOW_WIDTH" in
  let* pager = env_bool source env "GLOW_PAGER" in
  let* tui = env_bool source env "GLOW_TUI" in
  let* all = env_bool source env "GLOW_ALL" in
  let* line_numbers = env_bool source env "GLOW_LINE_NUMBERS" in
  let* preserve_new_lines = env_bool source env "GLOW_PRESERVE_NEW_LINES" in
  let* mouse = env_bool source env "GLOW_MOUSE" in
  Ok
    (apply config
       { style; width; pager; tui; all; line_numbers; preserve_new_lines; mouse })

let load ~explicit ~cwd ~env ~read =
  let open Result.Syntax in
  let explicit =
    match explicit with
    | Some path when String.trim path <> "" -> Some path
    | _ -> env_string env "GLOW_CONFIG"
  in
  let candidates =
    match explicit with
    | Some path -> [ path ]
    | None -> upward_paths ~cwd @ Option.to_list (xdg_config_path ~env)
  in
  let rec find = function
    | [] ->
        let* config = merge_env "environment" env default in
        Ok (config, None)
    | path :: rest -> (
        match read path with
        | None -> find rest
        | Some (Error message) -> Error (`Io (path, message))
        | Some (Ok text) ->
            let* overrides = parse_overrides path text in
            let merged = apply default overrides in
            let* merged = merge_env path env merged in
            Ok (merged, Some path))
  in
  find candidates
