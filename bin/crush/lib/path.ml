(* Every operation takes the platform explicitly and defaults it to the host, so a test on
   one system can pin the rules of the other. *)

let is_drive_letter character =
  (Char.code character >= Char.code 'A' && Char.code character <= Char.code 'Z')
  || (Char.code character >= Char.code 'a' && Char.code character <= Char.code 'z')

let drive_prefix windows path =
  if windows && String.length path >= 2 && is_drive_letter path.[0] && path.[1] = ':' then
    Some (String.sub path 0 2)
  else None

let is_unc windows path =
  windows && String.length path >= 2 && path.[0] = '/' && path.[1] = '/'

let slash_after path from =
  match String.index_from_opt path from '/' with
  | Some index -> index
  | None -> String.length path

(* A file URI on Windows carries the root slash of the drive it names, so [/C:/x] means
   the same file as [C:/x]. Recognizing that here stops a URI path from being read as a
   path on some other drive. *)
let drop_uri_drive windows path =
  if
    windows
    && String.length path > 3
    && path.[0] = '/'
    && is_drive_letter path.[1]
    && path.[2] = ':'
  then String.sub path 1 (String.length path - 1)
  else path

let unify windows path =
  let path = drop_uri_drive windows path in
  if not windows then path
  else String.map (fun character -> if character = '\\' then '/' else character) path

(* The root of a UNC path is [//server/share/]: the two leading separators, the server,
   one share, and the separator that closes the share. *)
let unc_root_length path =
  let length = String.length path in
  let server_end = slash_after path 2 in
  if server_end >= length then length
  else
    let share_end = slash_after path (server_end + 1) in
    if share_end >= length then length else share_end + 1

let root_length windows path =
  if is_unc windows path then unc_root_length path
  else if drive_prefix windows path <> None then 2
  else if String.length path > 0 && path.[0] = '/' then 1
  else 0

(* A drive letter is folded to upper case so [c:/x] and [C:/x] name the same root; the
   case of the rest of the path is the host's business, so it is left alone. *)
let root_of windows path =
  let length = root_length windows path in
  if length = 0 then ""
  else
    let root = String.sub path 0 length in
    let root =
      match drive_prefix windows root with
      | Some drive ->
          String.make 1 (Char.uppercase_ascii drive.[0])
          ^ String.sub root 1 (String.length root - 1)
      | None -> root
    in
    if root.[String.length root - 1] = '/' then root else root ^ "/"

let is_absolute ?(windows = Sys.win32) path =
  let path = unify windows path in
  (String.length path > 0 && path.[0] = '/') || drive_prefix windows path <> None

let strip_root windows path =
  let start = root_length windows path in
  String.sub path start (String.length path - start)

let components path = List.filter (fun part -> part <> "") (String.split_on_char '/' path)

let join root body =
  if body = [] then root
  else
    let separated = root.[String.length root - 1] = '/' in
    root ^ String.concat "/" (if separated then body else "" :: body)

(* A path that starts at the root of the current drive — [/b] — is absolute only relative
   to that drive, so it takes [cwd]'s drive letter; without a [cwd] to read a drive from
   it stays as written. *)
let drive_relative windows cwd path =
  if not (windows && path.[0] = '/' && not (is_unc windows path)) then path
  else
    match cwd with
    | None -> path
    | Some cwd -> (
        match drive_prefix windows (unify windows cwd) with
        | Some drive -> drive ^ path
        | None -> path)

let resolve windows cwd path =
  let path = unify windows path in
  if path <> "" && is_absolute ~windows path then drive_relative windows cwd path
  else
    match cwd with
    | None -> path
    | Some cwd ->
        let cwd = unify windows cwd in
        if cwd = "" || cwd = "." then path else String.concat "/" [ cwd; path ]

let clamp components =
  let rec walk stack = function
    | [] -> List.rev stack
    | "." :: rest -> walk stack rest
    | ".." :: rest -> (
        match stack with [] -> walk [] rest | _ :: parent -> walk parent rest)
    | value :: rest -> walk (value :: stack) rest
  in
  walk [] components

let drop_last values =
  let rec walk leading = function
    | [] | [ _ ] -> List.rev leading
    | value :: rest -> walk (value :: leading) rest
  in
  walk [] values

let drop count values =
  let rec walk count = function
    | [] -> []
    | value :: rest -> if count <= 0 then value :: rest else walk (count - 1) rest
  in
  walk count values

let normalize ?(windows = Sys.win32) ?cwd path =
  let joined = resolve windows cwd path in
  if not (is_absolute ~windows joined) then
    match clamp (components joined) with [] -> "." | body -> String.concat "/" body
  else
    let root = root_of windows joined in
    join root (clamp (components (strip_root windows joined)))

let parts ?(windows = Sys.win32) path =
  if not (is_absolute ~windows path) then None
  else
    let path = normalize ~windows path in
    Some (root_of windows path, components (strip_root windows path))

let parent ?(windows = Sys.win32) path =
  match parts ~windows path with
  | Some (root, body) -> (
      match drop_last body with [] -> None | parent -> Some (join root parent))
  | None -> (
      match clamp (components path) with
      | [] | [ _ ] -> None
      | body -> Some (String.concat "/" (drop_last body)))

let rec is_prefix prefix value =
  match (prefix, value) with
  | [], _ -> true
  | _ :: _, [] -> false
  | first :: rest, other :: others -> String.equal first other && is_prefix rest others

let within ?(windows = Sys.win32) ~root path =
  match (parts ~windows root, parts ~windows path) with
  | Some (root, root_parts), Some (path, path_parts) ->
      String.equal root path && is_prefix root_parts path_parts
  | _ -> false

let relative ?(windows = Sys.win32) ~root path =
  match (parts ~windows root, parts ~windows path) with
  | Some (root, root_parts), Some (path, path_parts)
    when String.equal root path && is_prefix root_parts path_parts ->
      Some (String.concat "/" (drop (List.length root_parts) path_parts))
  | _ -> None

(* [root] is a sandbox prefix: relative paths join it as-is, and an absolute
   [path] keeps only its components — the mirror of [C:\a\b] under [root] is
   [root\a\b], the same shape POSIX gets from plain concatenation
   ("/root" + "/a/b" is "/root/a/b"). A bare [root] — "/" or a drive root —
   names the real filesystem, so absolute paths pass through, which is what
   "fs_root is /" has always meant on POSIX. *)
let under ?(windows = Sys.win32) ~root path =
  let is_sep character = Char.equal character '/' || Char.equal character '\\' in
  let past_separators value =
    let length = String.length value in
    let rec walk index =
      if index < length && is_sep value.[index] then walk (index + 1) else index
    in
    walk 0
  in
  if not (is_absolute ~windows path) then Filename.concat root path
  else
    let root_rest = strip_root windows (unify windows root) in
    if past_separators root_rest = String.length root_rest then path
    else
      let rest = strip_root windows path in
      let index = past_separators rest in
      Filename.concat root (String.sub rest index (String.length rest - index))
