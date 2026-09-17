open Result.Syntax

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

let path_for env path =
  if Filename.is_relative path then Eio.Path.(env#cwd / path)
  else Eio.Path.(env#fs / path)

let read_local env path =
  try Ok (Eio.Path.load (path_for env path)) with
  | Eio.Io _ as exn -> Error (`Io (path, Fmt.str "%a" Eio.Exn.pp exn))
  | Unix.Unix_error (error, operation, argument) ->
      Error
        (`Io (path, Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument))

let read_stdin env =
  try
    Ok
      (Eio.Buf_read.take_all
         (Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) env#stdin))
  with
  | Eio.Buf_read.Buffer_limit_exceeded -> Error (`Io ("stdin", "input exceeds 10 MiB"))
  | End_of_file -> Ok ""
  | Eio.Io _ as exn -> Error (`Io ("stdin", Fmt.str "%a" Eio.Exn.pp exn))

let uri_parts uri =
  let host = Option.value (Uri.host uri) ~default:"" in
  let parts =
    Uri.path uri |> String.split_on_char '/' |> List.filter (fun part -> part <> "")
  in
  match parts with owner :: repo :: _ -> Some (host, owner, repo) | _ -> None

let status_code response = Cohttp.Code.code_of_status (Cohttp.Response.status response)

let body_text body =
  Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:(10 * 1024 * 1024) body)

let https_of_uri uri flow =
  let authenticator =
    match Ca_certs.authenticator () with
    | Ok value -> value
    | Error (`Msg message) -> Fmt.failwith "glow: TLS trust store unavailable: %s" message
  in
  let config =
    match Tls.Config.client ~authenticator () with
    | Ok value -> value
    | Error (`Msg message) -> Fmt.failwith "glow: TLS configuration failed: %s" message
  in
  let host =
    match Uri.host uri with
    | Some value -> Domain_name.host_exn (Domain_name.of_string_exn value)
    | None -> Fmt.failwith "glow: URL has no host"
  in
  Tls_eio.client_of_flow config ~host flow

let status_ok status = status >= 200 && status < 300
let redirect status = List.mem status [ 301; 302; 303; 307; 308 ]

let fetch_url ~net ~sw uri =
  let client = Cohttp_eio.Client.make ~https:(Some https_of_uri) net in
  let rec get redirects uri =
    if redirects > 5 then Error (`Http "too many HTTP redirects")
    else
      let response, body = Cohttp_eio.Client.get client ~sw uri in
      let status = status_code response in
      if redirect status then
        let location = Cohttp.Header.get (Cohttp.Response.headers response) "location" in
        match location with
        | None -> Error (`Http (Fmt.str "HTTP %d redirect has no Location header" status))
        | Some target -> get (redirects + 1) (Uri.resolve "" uri (Uri.of_string target))
      else
        try
          let text = body_text body in
          if status_ok status then Ok (text, uri)
          else Error (`Http (Fmt.str "HTTP %d: %s" status (String.trim text)))
        with Eio.Buf_read.Buffer_limit_exceeded ->
          Error (`Http "HTTP response exceeds 10 MiB")
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

let repo_readme ~net ~sw ~host ~owner ~repo =
  let api, field =
    if host = "github.com" then
      (Fmt.str "https://api.github.com/repos/%s/%s/readme" owner repo, "download_url")
    else
      ( Fmt.str "https://%s/api/v4/projects/%s" host (Uri.pct_encode (owner ^ "/" ^ repo)),
        "readme_url" )
  in
  let* body, _ = fetch_url ~net ~sw (Uri.of_string api) in
  match parse_download field body with
  | Some url ->
      let url =
        if host <> "github.com" then
          Re.replace_string (Re.compile (Re.str "/blob/")) ~by:"/raw/" url
        else url
      in
      fetch_url ~net ~sw (Uri.of_string url)
  | None ->
      let rec fallback = function
        | [] -> Error (`Http "can't find README in repository")
        | candidate :: rest -> (
            match fetch_url ~net ~sw (Uri.of_string candidate) with
            | Ok result -> Ok result
            | Error _ -> fallback rest)
      in
      fallback (readme_candidates ~host ~owner ~repo)

let fetch_document ~clock ~net raw =
  try
    let uri = Uri.of_string raw in
    match (Uri.scheme uri, Uri.host uri) with
    | Some ("http" | "https"), Some host -> (
        let operation () =
          Eio.Switch.run (fun sw ->
              match
                if host = "github.com" || host = "gitlab.com" then uri_parts uri else None
              with
              | Some (host, owner, repo) -> repo_readme ~net ~sw ~host ~owner ~repo
              | None -> fetch_url ~net ~sw uri)
        in
        match Eio.Time.with_timeout clock 30. (fun () -> operation ()) with
        | Ok (text, final_uri) ->
            let base =
              let path = Uri.path final_uri in
              let directory = Filename.dirname path in
              Some
                (Fmt.str "%s://%s%s/"
                   (Option.value (Uri.scheme final_uri) ~default:"https")
                   (Option.value (Uri.host final_uri) ~default:"")
                   directory)
            in
            Ok { content = text; path = None; base_url = base; markdown = true }
        | Error `Timeout -> Error (`Http "HTTP request timed out")
        | Error (`Http message) -> Error (`Http message))
    | Some scheme, _ -> Error (`Invalid (Fmt.str "unsupported URL scheme: %s" scheme))
    | _ -> Error (`Invalid "URL must include http or https scheme")
  with
  | Eio.Io _ as exception_ -> Error (`Http (Fmt.str "%a" Eio.Exn.pp exception_))
  | Tls_eio.Tls_alert _ -> Error (`Http "TLS alert while fetching URL")
  | Tls_eio.Tls_failure failure ->
      Error (`Http (Fmt.str "TLS failure: %a" Tls.Engine.pp_failure failure))
  | Unix.Unix_error (error, operation, argument) ->
      Error (`Http (Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument))
  | Invalid_argument message -> Error (`Invalid message)

let read ~env ~clock ~net = function
  | Stdin -> (
      match read_stdin env with
      | Ok content -> Ok { content; path = None; base_url = None; markdown = true }
      | Error error -> Error error)
  | File path -> (
      match read_local env path with
      | Ok content ->
          Ok
            {
              content;
              path = Some path;
              base_url = None;
              markdown = is_markdown_path path;
            }
      | Error error -> Error error)
  | Directory path ->
      Error (`Invalid (Fmt.str "directory source requires a terminal: %s" path))
  | Url url -> fetch_document ~clock ~net url
