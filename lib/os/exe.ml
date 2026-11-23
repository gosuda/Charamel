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

(* [PATHEXT] is the order the shell tries: a name with no known extension is tried
   with each entry in turn, and only then as written, because the extensionless file
   beside a package shim is the POSIX shell script, which [CreateProcess] cannot
   start, while the command shim it sits beside runs. *)
let extensions ~win32 name =
  if not win32 then [ "" ]
  else
    let upper = String.uppercase_ascii name in
    let extensions = path_extensions ~win32 () in
    let known =
      List.exists (fun extension -> String.ends_with ~suffix:extension upper) extensions
    in
    if known then [ "" ] else extensions @ [ "" ]

let candidates ~win32 root name =
  if win32 then
    List.map (fun extension -> root ^ name ^ extension) (extensions ~win32 name)
  else [ Filename.concat root name ]

(* A name whose final component carries a dot already names its extension, even
   when [PATHEXT] does not list it: the local probe keeps the exact file first
   when it is startable, so a [PATH] copy cannot shadow a runnable match. *)
let has_extension name =
  match String.rindex_opt name '.' with None -> false | Some dot -> dot > 0

(* [CreateProcess] appends [.exe] when the name carries no extension, so an
   extensionless file in the current directory is the POSIX shell script beside a
   package shim, which the launcher cannot start: the local probe tries the
   [PATHEXT] suffixed candidates only, never the bare name, so a [PATH] hit is not
   shadowed by a script the system would never select. *)
let local_candidates ~win32 prog =
  match extensions ~win32 prog with
  | [ "" ] -> [ prog ]
  | exts ->
      let suffixed =
        List.filter_map
          (fun extension -> if extension = "" then None else Some (prog ^ extension))
          exts
      in
      if has_extension prog then prog :: suffixed else suffixed

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

(* The image mark is the first two bytes of a PE file; batch files are recognised
   separately, so this probe only answers whether the exact local name is a PE image. *)
let starts_with_mz candidate =
  try
    let fd = Unix.openfile candidate [ Unix.O_RDONLY ] 0 in
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        let magic = Bytes.create 2 in
        Unix.read fd magic 0 2 = 2 && Bytes.get magic 0 = 'M' && Bytes.get magic 1 = 'Z')
  with Unix.Unix_error _ -> false

(* A [PATH] entry spelled relative to the current directory would resolve to a
   relative hit, which the spawn funnel cannot hand to [CreateProcess] as an
   application name; it is read from the current directory instead, so every hit is
   absolute. A drive-letter entry such as [C:\...] is already placed. *)
let is_absolute_entry ~win32 directory =
  String.length directory > 0
  && (directory.[0] = '/'
     || directory.[0] = '\\'
     || (win32 && String.length directory >= 2 && directory.[1] = ':'))

let absolute_directory ~win32 directory =
  if directory = "" then Sys.getcwd ()
  else if is_absolute_entry ~win32 directory then directory
  else Filename.concat (Sys.getcwd ()) directory

let has_separator ~win32 name =
  String.contains name '/' || (win32 && String.contains name '\\')

(* A drive-letter prefix such as [C:] names a path on that drive even with no slash,
   so it is explicit like a name with a separator: joining a directory in front of
   it would spell a path no API accepts. *)
let is_drive_relative ~win32 name =
  win32
  && String.length name >= 2
  && name.[1] = ':'
  &&
  let initial = name.[0] in
  (initial >= 'A' && initial <= 'Z') || (initial >= 'a' && initial <= 'z')

let is_explicit ~win32 name = has_separator ~win32 name || is_drive_relative ~win32 name

let find ?(win32 = Sys.win32) name =
  if name = "" then None
  else if is_explicit ~win32 name then
    match List.find_opt (executable ~win32) (candidates ~win32 "" name) with
    | Some found when is_absolute_entry ~win32 found -> Some found
    | Some found -> Some (Filename.concat (Sys.getcwd ()) found)
    | None -> None
  else
    let directories = split_path ~win32 "PATH" in
    let rec search = function
      | [] -> None
      | directory :: rest -> (
          let root = absolute_directory ~win32 directory ^ Filename.dir_sep in
          match List.find_opt (executable ~win32) (candidates ~win32 root name) with
          | Some found -> Some found
          | None -> search rest)
    in
    search directories

(* An explicitly named batch file runs through the command interpreter, so it keeps
   the exact local name whatever [PATHEXT] lists: [PATHEXT] only orders the suffixes
   the probe appends to an extensionless name. [CreateProcess] starts an explicitly
   named file with any other unlisted extension only as a PE image, so a same-named
   data file is never a runnable match: the exact local name keeps precedence only
   when it carries the image mark. *)
let is_batch_file name =
  let upper = String.uppercase_ascii name in
  List.exists
    (fun extension -> String.ends_with ~suffix:extension upper)
    [ ".BAT"; ".CMD" ]

(* The spawn funnel hands the program to [CreateProcess] as its application name, and a
   bare name misses there on the runners where the shells are found by search: the
   absolute path always resolves, so a bare name is resolved first. A [CreateProcess]
   bare name still reads the parent's current directory before [PATH], so a runnable
   file here keeps that resolution instead of being shadowed by a [PATH] copy. A path
   and the empty program go through unchanged, and a miss also goes through unchanged
   so the system reports it exactly as before. *)
let spawnable ~win32 prog candidate =
  executable ~win32 candidate
  && (candidate <> prog
     || extensions ~win32 prog = [ "" ]
     || is_batch_file prog || starts_with_mz candidate)

let for_spawn ?(win32 = Sys.win32) prog =
  if prog = "" || is_explicit ~win32 prog then prog
  else
    let local =
      if win32 then List.find_opt (spawnable ~win32 prog) (local_candidates ~win32 prog)
      else None
    in
    match local with
    | Some candidate -> Filename.concat (Sys.getcwd ()) candidate
    | None -> Option.value (find ~win32 prog) ~default:prog

(* [CreateProcess] receives one command line, and each program splits it itself. A
   program built on the C runtime splits it by the [CommandLineToArgvW] rules: a word
   is quoted only when it is empty or holds whitespace or a quote, a quote inside it is
   escaped with a backslash, and the backslashes before an escaped or closing quote are
   doubled. *)
let needs_quotes argument =
  argument = ""
  || String.exists
       (function ' ' | '\t' | '\n' | '\011' | '"' -> true | _ -> false)
       argument

let quote_argument argument =
  if not (needs_quotes argument) then argument
  else
    let buffer = Buffer.create (String.length argument + 2) in
    Buffer.add_char buffer '"';
    let backslashes =
      String.fold_left
        (fun backslashes character ->
          match character with
          | '\\' ->
              Buffer.add_char buffer '\\';
              backslashes + 1
          | '"' ->
              Buffer.add_string buffer (String.make (backslashes + 1) '\\');
              Buffer.add_char buffer '"';
              0
          | character ->
              Buffer.add_char buffer character;
              0)
        0 argument
    in
    Buffer.add_string buffer (String.make backslashes '\\');
    Buffer.add_char buffer '"';
    Buffer.contents buffer

let program_name program =
  let start =
    List.fold_left
      (fun start separator ->
        match String.rindex_opt program separator with
        | Some index -> max start (index + 1)
        | None -> start)
      0 [ '/'; '\\'; ':' ]
  in
  String.lowercase_ascii (String.sub program start (String.length program - start))

let is_command_interpreter program =
  match program_name program with "cmd" | "cmd.exe" -> true | _ -> false

let is_command_switch word =
  match String.lowercase_ascii word with "/c" | "/k" -> true | _ -> false

(* [cmd.exe] does not split its command line by those rules: it finds its [/c] or [/k]
   switch in the raw text and runs everything after it as command text, first removing
   one surrounding pair of quotes. A quoted ["/c"] followed by a backslash-escaped
   command reaches it as one unterminated quoted command name, so the switches go
   through as written and the words after [/c] or [/k] go through verbatim, joined by
   spaces, inside the one pair of quotes [cmd.exe] removes. *)
let rec interpreter_words = function
  | [] -> []
  | [ word ] -> [ word ]
  | switch :: text when is_command_switch switch ->
      [ switch; "\"" ^ String.concat " " text ^ "\"" ]
  | word :: rest -> word :: interpreter_words rest

let command_line = function
  | [] -> ""
  | program :: arguments ->
      let words =
        if is_command_interpreter program then interpreter_words arguments
        else List.map quote_argument arguments
      in
      String.concat " " (quote_argument program :: words)
