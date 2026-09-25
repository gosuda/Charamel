let separator = if Sys.win32 then ';' else ':'
let default_extensions = [ ".COM"; ".EXE"; ".BAT"; ".CMD" ]

(* [PATHEXT] is an ordered, semicolon-separated list of extensions, held in the system's own
   case; an unusable value falls back to the defaults the shell uses. *)
let split_path variable =
  match Sys.getenv_opt variable with
  | None -> []
  | Some value ->
      List.filter (fun entry -> entry <> "") (String.split_on_char separator value)

(* [PATHEXT] is an ordered, semicolon-separated list of extensions in the system's own case;
   an unusable value falls back to the defaults a shell uses. *)
let path_extensions () =
  match split_path "PATHEXT" with [] -> default_extensions | xs -> xs

let extensions name =
  if not Sys.win32 then [ "" ]
  else
    let upper = String.uppercase_ascii name in
    let extensions = path_extensions () in
    let known =
      List.exists (fun extension -> String.ends_with ~suffix:extension upper) extensions
    in
    if known then [ "" ] else "" :: extensions

let candidates root name =
  if Sys.win32 then List.map (fun extension -> root ^ name ^ extension) (extensions name)
  else [ Filename.concat root name ]

let executable candidate =
  try
    (Unix.stat candidate).Unix.st_kind = Unix.S_REG
    && (Sys.win32
       ||
         try
           Unix.access candidate [ Unix.X_OK ];
           true
         with Unix.Unix_error _ -> false)
  with Unix.Unix_error _ -> false

let has_separator name =
  String.contains name '/' || (Sys.win32 && String.contains name '\\')

let find name =
  if name = "" then None
  else if has_separator name then List.find_opt executable (candidates "" name)
  else
    let directories = split_path "PATH" in
    let rec search = function
      | [] -> None
      | directory :: rest -> (
          let root =
            if directory = "" then Filename.current_dir_name ^ Filename.dir_sep
            else directory ^ Filename.dir_sep
          in
          match List.find_opt executable (candidates root name) with
          | Some found -> Some found
          | None -> search rest)
    in
    search directories
