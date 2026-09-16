type source = Stdin | File of string | Execute of string
type loaded = { text : string; path : string option }

let read_flow flow =
  let buffer = Buffer.create 4096 in
  let sink = Eio.Flow.buffer_sink buffer in
  try
    Eio.Flow.copy flow sink;
    Ok (Buffer.contents buffer)
  with
  | End_of_file -> Ok (Buffer.contents buffer)
  | Eio.Io _ -> Error "could not read input"

let read ~fs ~stdin = function
  | Stdin -> Result.map (fun text -> { text; path = None }) (read_flow stdin)
  | File path -> (
      try Ok { text = Eio.Path.(load (fs / path)); path = Some path } with
      | Eio.Io _ -> Error (Fmt.str "file not found: %s" path)
      | Unix.Unix_error (error, function_name, argument) ->
          Error
            (Fmt.str "could not read %s: %s (%s %s)" path (Unix.error_message error)
               function_name argument))
  | Execute _ -> Error "execute input must be captured through a PTY"

let rec cut_lines ~lines text =
  match lines with
  | [] -> text
  | start :: finish :: _ ->
      let all = String.split_on_char '\n' text in
      let first = max 0 start in
      let last =
        if finish < 0 then List.length all - 1 else min (List.length all - 1) finish
      in
      if first > last || first >= List.length all then ""
      else
        let selected =
          all
          |> List.mapi (fun index line -> (index, line))
          |> List.filter_map (fun (index, line) ->
              if index >= first && index <= last then Some line else None)
        in
        String.concat "\n" selected
  | [ start ] -> cut_lines ~lines:[ start; -1 ] text

let language ~override ~path =
  let explicit = String.trim override in
  if explicit <> "" && String.lowercase_ascii explicit <> "ansi" then
    Charm_highlight.find explicit
  else
    match path with
    | Some path when explicit = "" ->
        let extension = Filename.extension path in
        if extension <> "" then Charm_highlight.find extension
        else Charm_highlight.find path
    | _ -> None

let is_ansi ~language text =
  String.lowercase_ascii language = "ansi" || Charm_ansi.Text.strip text <> text

let wrap ~width text = if width > 0 then Charm_ansi.Text.wrap ~width text else text
