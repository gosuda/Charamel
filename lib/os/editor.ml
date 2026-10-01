let words variable fallback =
  match Sys.getenv_opt variable with
  | Some value when String.trim value <> "" -> Shell.split_words value
  | _ -> fallback

let editor () = words "EDITOR" (if Sys.win32 then [ "notepad" ] else [ "vi" ])
let pager () = words "PAGER" (if Sys.win32 then [ "more" ] else [ "less"; "-r" ])
let posix_browser = if Os_platform.is_macos then [ "open" ] else [ "xdg-open" ]

let browser () =
  match Sys.getenv_opt "BROWSER" with
  | Some value when String.trim value <> "" -> Shell.split_words value
  | _ -> if Sys.win32 then [ "cmd"; "/c"; "start" ] else posix_browser
