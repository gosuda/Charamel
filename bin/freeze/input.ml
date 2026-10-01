type source = Stdin | File of string | Execute of string
type loaded = { text : string; path : string option }

let read_channel channel =
  let buffer = Buffer.create 4096 in
  let rec loop () =
    Lwt.bind (Lwt_io.read ~count:65536 channel) (fun chunk ->
        if chunk = "" then Lwt.return (Ok (Buffer.contents buffer))
        else begin
          Buffer.add_string buffer chunk;
          loop ()
        end)
  in
  loop ()

let resolve ~fs_root path =
  if Filename.is_relative path then Filename.concat fs_root path else path

let read ~fs_root ~stdin = function
  | Stdin ->
      Lwt.bind (read_channel stdin) (function
        | Ok text -> Lwt.return (Ok { text; path = None })
        | Error error -> Lwt.return (Error error))
  | File path ->
      let resolved = resolve ~fs_root path in
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.Input resolved (fun channel ->
              Lwt.bind (read_channel channel) (function
                | Ok text -> Lwt.return (Ok { text; path = Some path })
                | Error error -> Lwt.return (Error error))))
        (function
          | Unix.Unix_error (error, function_name, argument) ->
              Lwt.return
                (Error
                   (Fmt.str "could not read %s: %s (%s %s)" path
                      (Unix.error_message error) function_name argument))
          | End_of_file | Sys_error _ ->
              Lwt.return (Error (Fmt.str "file not found: %s" path))
          | exn -> Lwt.fail exn)
  | Execute _ -> Lwt.return (Error "execute input must be captured through a PTY")

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
    Charamel_highlight.find explicit
  else
    match path with
    | Some path when explicit = "" ->
        let extension = Filename.extension path in
        if extension <> "" then Charamel_highlight.find extension
        else Charamel_highlight.find path
    | _ -> None

let is_ansi ~language text =
  String.lowercase_ascii language = "ansi" || Charamel_ansi.Text.strip text <> text

let wrap ~width text = if width > 0 then Charamel_ansi.Text.wrap ~width text else text
