let separator_for win32 = if win32 then ';' else ':'
let default_extensions = [ ".COM"; ".EXE"; ".BAT"; ".CMD" ]

(* [PATHEXT] is an ordered, semicolon-separated list of extensions, held in the system's own
   case; an unusable value falls back to the defaults the shell uses. *)
let split_path ~win32 variable =
  match Sys.getenv_opt variable with
  | None -> []
  | Some value ->
      List.filter
        (fun entry -> entry <> "")
        (String.split_on_char (separator_for win32) value)

(* [PATHEXT] is an ordered, semicolon-separated list of extensions in the system's own case;
   an unusable value falls back to the defaults a shell uses. *)
let path_extensions ~win32 () =
  match split_path ~win32 "PATHEXT" with [] -> default_extensions | xs -> xs

let extensions ~win32 name =
  if not win32 then [ "" ]
  else
    let upper = String.uppercase_ascii name in
    let extensions = path_extensions ~win32 () in
    let known =
      List.exists (fun extension -> String.ends_with ~suffix:extension upper) extensions
    in
    if known then [ "" ] else "" :: extensions

let candidates ~win32 root name =
  if win32 then
    List.map (fun extension -> root ^ name ^ extension) (extensions ~win32 name)
  else [ Filename.concat root name ]

let executable ~win32 candidate =
  try
    (Unix.stat candidate).Unix.st_kind = Unix.S_REG
    && (win32
       ||
         try
           Unix.access candidate [ Unix.X_OK ];
           true
         with Unix.Unix_error _ -> false)
  with Unix.Unix_error _ -> false

let has_separator ~win32 name =
  String.contains name '/' || (win32 && String.contains name '\\')

let find ?(win32 = Sys.win32) name =
  if name = "" then None
  else if has_separator ~win32 name then
    List.find_opt (executable ~win32) (candidates ~win32 "" name)
  else
    let directories = split_path ~win32 "PATH" in
    let rec search = function
      | [] -> None
      | directory :: rest -> (
          let root =
            if directory = "" then Filename.current_dir_name ^ Filename.dir_sep
            else directory ^ Filename.dir_sep
          in
          match List.find_opt (executable ~win32) (candidates ~win32 root name) with
          | Some found -> Some found
          | None -> search rest)
    in
    search directories

(* The spawn funnel hands the program to [CreateProcess] as its application name, and a
   bare name misses there on the runners where the shells are found by search: the
   absolute path always resolves, so a bare name is resolved first. A path and the
   empty program go through unchanged, and a miss also goes through unchanged so the
   system reports it exactly as before. *)
let for_spawn ?(win32 = Sys.win32) prog =
  if prog = "" || has_separator ~win32 prog then prog
  else Option.value (find ~win32 prog) ~default:prog
