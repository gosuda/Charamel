type error = [ `No_home ]

let usable value = value <> "" && not (Filename.is_relative value)

let get name =
  match Sys.getenv_opt name with Some value when usable value -> Some value | _ -> None

let home_names = if Sys.win32 then [ "USERPROFILE"; "HOME" ] else [ "HOME" ]

let home () =
  match List.filter_map get home_names with
  | directory :: _ -> Ok directory
  | [] -> Error `No_home

let no_base var fallback =
  invalid_arg
    ("Charamel_os.Dirs: $" ^ var ^ " is not set to an absolute directory and " ^ fallback)

let under_home var relative app =
  match List.filter_map get home_names with
  | base :: _ -> Filename.concat (Filename.concat base relative) app
  | [] -> no_base var "no home directory is usable"

let xdg var relative app =
  match get var with
  | Some base -> Filename.concat base app
  | None -> under_home var relative app

(* Windows puts configuration under a dot-directory in the profile, the same shape POSIX uses,
   while data, state and cache go straight into [LocalAppData] because that store has no
   [~/.local] counterpart. A caller may still opt into the POSIX layout by setting [XDG_*]. *)
let windows_config app =
  match get "XDG_CONFIG_HOME" with
  | Some base -> Filename.concat base app
  | None -> under_home "XDG_CONFIG_HOME" ".config" app

let windows_local var app =
  match get var with
  | Some base -> Filename.concat base app
  | None -> (
      match get "LOCALAPPDATA" with
      | Some base -> Filename.concat base app
      | None -> no_base var "no LocalAppData is usable")

let config_dir ~app =
  if Sys.win32 then windows_config app else xdg "XDG_CONFIG_HOME" ".config" app

let data_dir ~app =
  if Sys.win32 then windows_local "XDG_DATA_HOME" app
  else xdg "XDG_DATA_HOME" ".local/share" app

let state_dir ~app =
  if Sys.win32 then windows_local "XDG_STATE_HOME" app
  else xdg "XDG_STATE_HOME" ".local/state" app

let cache_dir ~app =
  if Sys.win32 then windows_local "XDG_CACHE_HOME" app
  else xdg "XDG_CACHE_HOME" ".cache" app

let rest path count = String.sub path count (String.length path - count)

let expand_tilde path =
  let expanded remainder =
    match home () with
    | Ok directory -> Ok (Filename.concat directory remainder)
    | Error `No_home -> Error `No_home
  in
  if path = "~" then home ()
  else if String.starts_with ~prefix:"~/" path then expanded (rest path 2)
  else if Sys.win32 && String.starts_with ~prefix:"~\\" path then expanded (rest path 2)
  else Ok path

let temp_dir () =
  match List.filter_map get [ "TMPDIR"; "TEMP"; "TMP" ] with
  | directory :: _ -> directory
  | [] -> (
      if not Sys.win32 then "/tmp"
      else
        match get "SystemRoot" with
        | Some root -> Filename.concat root "Temp"
        | None -> "C:\\Temp")
