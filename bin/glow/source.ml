type location = Stdin | File of string | Directory of string | Url of string

type document = {
  content : string;
  path : string option;
  base_url : string option;
  markdown : bool;
}

type error = [ `Invalid of string | `Io of string * string | `Http of string ]

let markdown_extensions = [ ".md"; ".markdown"; ".mdown"; ".mkdn"; ".mkd" ]

let is_markdown_path path =
  let lower = String.lowercase_ascii path in
  Filename.basename lower = "readme"
  || List.exists
       (fun extension -> Filename.check_suffix lower extension)
       markdown_extensions

let has_uri_scheme value =
  match String.index_opt value ':' with
  | Some index ->
      index + 2 < String.length value
      && Char.equal value.[index + 1] '/'
      && Char.equal value.[index + 2] '/'
  | None -> false

let classify ~argument ~cwd ~stdin_is_tty =
  match argument with
  | None when not stdin_is_tty -> Ok Stdin
  | None -> Ok (Directory cwd)
  | Some "-" -> Ok Stdin
  | Some value when value = "" -> if stdin_is_tty then Ok (Directory cwd) else Ok Stdin
  | Some value
    when String.starts_with ~prefix:"http://" value
         || String.starts_with ~prefix:"https://" value ->
      Ok (Url value)
  | Some value
    when String.starts_with ~prefix:"github.com/" value
         || String.starts_with ~prefix:"gitlab.com/" value ->
      Ok (Url ("https://" ^ value))
  | Some value when has_uri_scheme value ->
      Error (`Invalid (Fmt.str "unsupported URL scheme in %s" value))
  | Some value -> (
      let path =
        if Filename.is_relative value then Filename.concat cwd value else value
      in
      match Unix.stat path with
      | { Unix.st_kind = Unix.S_DIR; _ } -> Ok (Directory path)
      | { Unix.st_kind = Unix.S_REG; _ } -> Ok (File path)
      | _ -> Error (`Invalid (Fmt.str "unsupported source: %s" value))
      | exception Unix.Unix_error (error, operation, argument) ->
          Error
            (`Io
               (value, Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument))
      )

let remove_frontmatter text =
  if not (String.starts_with ~prefix:"---\n" text) then text
  else
    match
      Re.exec_opt
        (Re.compile (Re.seq [ Re.str "---\n"; Re.rep Re.any; Re.str "\n---\n" ]))
        text
    with
    | None -> text
    | Some group ->
        let stop = Re.Group.stop group 0 in
        String.sub text stop (String.length text - stop)

let hidden_name name = String.length name > 0 && Char.equal name.[0] '.'

let discover_markdown ~root ~show_hidden =
  let rec walk directory acc =
    let names =
      try Array.to_list (Sys.readdir directory) |> List.sort String.compare
      with Sys_error _ -> []
    in
    List.fold_left
      (fun acc name ->
        let hidden = hidden_name name in
        if ((not show_hidden) && hidden) || name = ".git" || name = "node_modules" then
          acc
        else
          let path = Filename.concat directory name in
          match Unix.stat path with
          | { Unix.st_kind = Unix.S_DIR; _ } -> walk path acc
          | { Unix.st_kind = Unix.S_REG; _ } when is_markdown_path name -> path :: acc
          | _ -> acc
          | exception Unix.Unix_error _ -> acc)
      acc names
  in
  walk root [] |> List.sort String.compare

let readme_candidates ~host ~owner ~repo =
  let base = "https://" ^ host in
  let encoded_owner = Uri.pct_encode owner in
  let encoded_repo = Uri.pct_encode repo in
  if host = "github.com" then
    [
      "https://raw.githubusercontent.com/" ^ encoded_owner ^ "/" ^ encoded_repo
      ^ "/HEAD/README.md";
      base ^ "/" ^ encoded_owner ^ "/" ^ encoded_repo ^ "/raw/HEAD/README.md";
    ]
  else [ base ^ "/" ^ encoded_owner ^ "/" ^ encoded_repo ^ "/-/raw/HEAD/README.md" ]

let path_for ~cwd path =
  if Filename.is_relative path then Filename.concat cwd path else path

let read_local ~cwd path =
  let resolved = path_for ~cwd path in
  Lwt.catch
    (fun () ->
      Lwt.bind (Lwt_io.with_file ~mode:Lwt_io.Input resolved Lwt_io.read) (fun text ->
          Lwt.return (Ok text)))
    (function
      | Unix.Unix_error (error, operation, argument) ->
          Lwt.return
            (Error
               (`Io
                  ( path,
                    Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument )))
      | End_of_file | Sys_error _ ->
          Lwt.return (Error (`Io (path, "could not read file")))
      | exn -> Lwt.fail exn)

let max_input = 10 * 1024 * 1024

let read_channel_bounded ~label channel =
  let buffer = Buffer.create 4096 in
  let rec loop () =
    Lwt.bind (Lwt_io.read ~count:65536 channel) (fun chunk ->
        if chunk = "" then Lwt.return (Ok (Buffer.contents buffer))
        else begin
          Buffer.add_string buffer chunk;
          if Buffer.length buffer > max_input then
            Lwt.return (Error (`Io (label, "input exceeds 10 MiB")))
          else loop ()
        end)
  in
  loop ()

exception Body_too_large

let body_text body =
  let buffer = Buffer.create 4096 in
  Lwt.bind
    (Lwt_stream.iter_s
       (fun chunk ->
         Buffer.add_string buffer chunk;
         if Buffer.length buffer > max_input then Lwt.fail Body_too_large
         else Lwt.return_unit)
       (Cohttp_lwt.Body.to_stream body))
    (fun () -> Lwt.return (Buffer.contents buffer))

let status_code response = Cohttp.Code.code_of_status (Cohttp.Response.status response)
let status_ok status = status >= 200 && status < 300
let redirect status = List.mem status [ 301; 302; 303; 307; 308 ]

let fetch_url uri =
  let rec get redirects uri =
    if redirects > 5 then Lwt.return (Error (`Http "too many HTTP redirects"))
    else
      Lwt.bind (Cohttp_lwt_unix.Client.get uri) (fun (response, body) ->
          let status = status_code response in
          if redirect status then
            match Cohttp.Header.get (Cohttp.Response.headers response) "location" with
            | None ->
                Lwt.return
                  (Error
                     (`Http (Fmt.str "HTTP %d redirect has no Location header" status)))
            | Some target ->
                get (redirects + 1) (Uri.resolve "" uri (Uri.of_string target))
          else
            Lwt.catch
              (fun () ->
                Lwt.bind (body_text body) (fun text ->
                    if status_ok status then Lwt.return (Ok (text, uri))
                    else
                      Lwt.return
                        (Error (`Http (Fmt.str "HTTP %d: %s" status (String.trim text))))))
              (function
                | Body_too_large ->
                    Lwt.return (Error (`Http "HTTP response exceeds 10 MiB"))
                | exn -> Lwt.fail exn))
  in
  get 0 uri

let json_string_member name = function
  | Jsont.Object (members, _) -> (
      match Jsont.Json.find_mem name members with
      | Some (_, Jsont.String (value, _)) -> Some value
      | _ -> None)
  | _ -> None

let parse_download field text =
  match Jsont_bytesrw.decode_string Jsont.json text with
  | Ok value -> json_string_member field value
  | Error _ -> None

let repo_readme ~host ~owner ~repo =
  let api, field =
    if host = "github.com" then
      (Fmt.str "https://api.github.com/repos/%s/%s/readme" owner repo, "download_url")
    else
      ( Fmt.str "https://%s/api/v4/projects/%s" host (Uri.pct_encode (owner ^ "/" ^ repo)),
        "readme_url" )
  in
  Lwt.bind
    (fetch_url (Uri.of_string api))
    (function
      | Error _ as error -> Lwt.return error
      | Ok (body, _) -> (
          match parse_download field body with
          | Some url ->
              let url =
                if host <> "github.com" then
                  Re.replace_string (Re.compile (Re.str "/blob/")) ~by:"/raw/" url
                else url
              in
              fetch_url (Uri.of_string url)
          | None ->
              let rec fallback = function
                | [] -> Lwt.return (Error (`Http "can't find README in repository"))
                | candidate :: rest ->
                    Lwt.bind
                      (fetch_url (Uri.of_string candidate))
                      (function
                        | Ok _ as result -> Lwt.return result | Error _ -> fallback rest)
              in
              fallback (readme_candidates ~host ~owner ~repo)))

let uri_parts uri =
  let host = Option.value (Uri.host uri) ~default:"" in
  let parts =
    Uri.path uri |> String.split_on_char '/' |> List.filter (fun part -> part <> "")
  in
  match parts with owner :: repo :: _ -> Some (host, owner, repo) | _ -> None

let fetch_document raw =
  Lwt.catch
    (fun () ->
      let uri = Uri.of_string raw in
      match (Uri.scheme uri, Uri.host uri) with
      | Some ("http" | "https"), Some host ->
          let operation () =
            match
              if host = "github.com" || host = "gitlab.com" then uri_parts uri else None
            with
            | Some (host, owner, repo) -> repo_readme ~host ~owner ~repo
            | None -> fetch_url uri
          in
          Lwt.bind
            (Lwt.catch
               (fun () ->
                 Lwt.map (fun value -> Ok value) (Lwt_unix.with_timeout 30. operation))
               (function
                 | Lwt_unix.Timeout -> Lwt.return (Error `Timeout) | exn -> Lwt.fail exn))
            (function
              | Error `Timeout -> Lwt.return (Error (`Http "HTTP request timed out"))
              | Ok (Error _ as error) -> Lwt.return error
              | Ok (Ok (text, final_uri)) ->
                  let base =
                    let path = Uri.path final_uri in
                    let directory = Filename.dirname path in
                    Some
                      (Fmt.str "%s://%s%s/"
                         (Option.value (Uri.scheme final_uri) ~default:"https")
                         (Option.value (Uri.host final_uri) ~default:"")
                         directory)
                  in
                  Lwt.return
                    (Ok { content = text; path = None; base_url = base; markdown = true }))
      | Some scheme, _ ->
          Lwt.return (Error (`Invalid (Fmt.str "unsupported URL scheme: %s" scheme)))
      | _ -> Lwt.return (Error (`Invalid "URL must include http or https scheme")))
    (function
      | Tls_lwt.Tls_alert _ -> Lwt.return (Error (`Http "TLS alert while fetching URL"))
      | Tls_lwt.Tls_failure failure ->
          Lwt.return
            (Error (`Http (Fmt.str "TLS failure: %a" Tls.Engine.pp_failure failure)))
      | Unix.Unix_error (error, operation, argument) ->
          Lwt.return
            (Error
               (`Http
                  (Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument)))
      | Invalid_argument message -> Lwt.return (Error (`Invalid message))
      | exn -> Lwt.fail exn)

let read ~cwd ~stdin = function
  | Stdin ->
      Lwt.bind (read_channel_bounded ~label:"stdin" stdin) (function
        | Ok content ->
            Lwt.return (Ok { content; path = None; base_url = None; markdown = true })
        | Error error -> Lwt.return (Error error))
  | File path ->
      Lwt.bind (read_local ~cwd path) (function
        | Ok content ->
            Lwt.return
              (Ok
                 {
                   content;
                   path = Some path;
                   base_url = None;
                   markdown = is_markdown_path path;
                 })
        | Error error -> Lwt.return (Error error))
  | Directory path ->
      Lwt.return
        (Error (`Invalid (Fmt.str "directory source requires a terminal: %s" path)))
  | Url url -> fetch_document url
