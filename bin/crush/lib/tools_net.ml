open Result.Syntax

type output_format = Text | Markdown | Html
type render_mode = Plain | Markdown
type params = { url : string; format : output_format option; timeout_s : int option }

let format_jsont : output_format Jsont.t =
  Jsont.enum [ ("text", Text); ("markdown", (Markdown : output_format)); ("html", Html) ]

let params_jsont =
  Jsont.Object.map (fun url format timeout_s -> { url; format; timeout_s })
  |> Jsont.Object.mem "url" Jsont.string
  |> Jsont.Object.opt_mem "format" format_jsont
  |> Jsont.Object.opt_mem "timeout_s" Jsont.int
  |> Jsont.Object.finish

let contains_at source offset needle =
  let source_length = String.length source and needle_length = String.length needle in
  offset >= 0
  && offset + needle_length <= source_length
  &&
  let rec loop index =
    if index = needle_length then true
    else if source.[offset + index] = needle.[index] then loop (index + 1)
    else false
  in
  loop 0

let find_from source offset needle =
  let rec loop index =
    if index + String.length needle > String.length source then None
    else if contains_at source index needle then Some index
    else loop (index + 1)
  in
  loop offset

let find_char source offset wanted =
  let rec loop index =
    if index = String.length source then None
    else if source.[index] = wanted then Some index
    else loop (index + 1)
  in
  loop offset

let add_uchar buffer code = Buffer.add_utf_8_uchar buffer (Uchar.of_int code)

let entity_value entity =
  match entity with
  | "amp" -> Some (`Text "&")
  | "lt" -> Some (`Text "<")
  | "gt" -> Some (`Text ">")
  | "quot" -> Some (`Text "\"")
  | "#39" -> Some (`Text "'")
  | "nbsp" -> Some (`Uchar 0xA0)
  | value when String.length value > 1 && value.[0] = '#' ->
      let number =
        if value.[1] = 'x' || value.[1] = 'X' then
          int_of_string_opt ("0x" ^ String.sub value 2 (String.length value - 2))
        else int_of_string_opt (String.sub value 1 (String.length value - 1))
      in
      Option.bind number (fun code ->
          if Uchar.is_valid code then Some (`Uchar code) else None)
  | _ -> None

let decode_entities text =
  let result = Buffer.create (String.length text) in
  let rec loop index =
    if index = String.length text then ()
    else if text.[index] <> '&' then (
      Buffer.add_char result text.[index];
      loop (index + 1))
    else
      match find_char text (index + 1) ';' with
      | None ->
          Buffer.add_char result '&';
          loop (index + 1)
      | Some end_ ->
          let entity = String.sub text (index + 1) (end_ - index - 1) in
          (match entity_value entity with
          | Some (`Text value) -> Buffer.add_string result value
          | Some (`Uchar code) -> add_uchar result code
          | None ->
              Buffer.add_char result '&';
              Buffer.add_string result entity;
              Buffer.add_char result ';');
          loop (end_ + 1)
  in
  loop 0;
  Buffer.contents result

type tag = { closing : bool; name : string; attrs : string }

let is_tag_name_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '-'

let parse_tag source first last =
  let raw = String.sub source (first + 1) (last - first - 1) |> String.trim in
  let closing = String.length raw > 0 && raw.[0] = '/' in
  let raw =
    if closing then String.sub raw 1 (String.length raw - 1) |> String.trim else raw
  in
  let name_end =
    let rec loop index =
      if index = String.length raw || not (is_tag_name_char raw.[index]) then index
      else loop (index + 1)
    in
    loop 0
  in
  if name_end = 0 then None
  else
    Some
      {
        closing;
        name = String.lowercase_ascii (String.sub raw 0 name_end);
        attrs = String.sub raw name_end (String.length raw - name_end);
      }

let find_tag_end source first =
  let rec loop index quote =
    if index = String.length source then None
    else
      match (quote, source.[index]) with
      | Some q, c when c = q -> loop (index + 1) None
      | Some _, _ -> loop (index + 1) quote
      | None, (('\'' | '"') as q) -> loop (index + 1) (Some q)
      | None, '>' -> Some index
      | None, _ -> loop (index + 1) None
  in
  loop first None

let attr_value attrs wanted =
  let lower = String.lowercase_ascii attrs in
  let wanted = String.lowercase_ascii wanted in
  let rec skip_spaces index =
    if index < String.length attrs && (attrs.[index] = ' ' || attrs.[index] = '\t') then
      skip_spaces (index + 1)
    else index
  in
  let rec scan index =
    if index >= String.length attrs then None
    else if
      index + String.length wanted <= String.length attrs
      && String.sub lower index (String.length wanted) = wanted
      && (index = 0 || not (is_tag_name_char lower.[index - 1]))
      && (index + String.length wanted = String.length attrs
         || not (is_tag_name_char lower.[index + String.length wanted]))
    then
      let equals = skip_spaces (index + String.length wanted) in
      if equals < String.length attrs && attrs.[equals] = '=' then
        let value_start = skip_spaces (equals + 1) in
        if value_start = String.length attrs then None
        else
          let quote =
            match attrs.[value_start] with ('\'' | '"') as q -> Some q | _ -> None
          in
          let value_start =
            match quote with None -> value_start | Some _ -> value_start + 1
          in
          let value_end =
            match quote with
            | Some q ->
                Option.value
                  (find_char attrs value_start q)
                  ~default:(String.length attrs)
            | None ->
                let rec stop i =
                  if i = String.length attrs || attrs.[i] = ' ' || attrs.[i] = '\t' then i
                  else stop (i + 1)
                in
                stop value_start
          in
          Some (String.sub attrs value_start (value_end - value_start) |> decode_entities)
      else scan (index + 1)
    else scan (index + 1)
  in
  scan 0

let add_string buffer last_newline text =
  if text <> "" then (
    Buffer.add_string buffer text;
    last_newline := String.length text > 0 && text.[String.length text - 1] = '\n')

let add_newline buffer last_newline =
  if not !last_newline then (
    Buffer.add_char buffer '\n';
    last_newline := true)

let block_tag = function
  | "address" | "article" | "aside" | "blockquote" | "dd" | "div" | "dl" | "dt"
  | "fieldset" | "figcaption" | "figure" | "footer" | "form" | "h1" | "h2" | "h3" | "h4"
  | "h5" | "h6" | "header" | "hr" | "li" | "main" | "nav" | "ol" | "p" | "pre" | "section"
  | "table" | "tbody" | "td" | "tfoot" | "th" | "thead" | "tr" | "ul" ->
      true
  | _ -> false

let heading_level name =
  match name with
  | "h1" -> Some 1
  | "h2" -> Some 2
  | "h3" -> Some 3
  | "h4" -> Some 4
  | "h5" -> Some 5
  | "h6" -> Some 6
  | _ -> None

let render_html (format : render_mode) html =
  let lower = String.lowercase_ascii html in
  let output = Buffer.create (String.length html) in
  let last_newline = ref false in
  let in_pre = ref false in
  let in_code = ref false in
  let anchors = ref [] in
  let add_text text = add_string output last_newline (decode_entities text) in
  let rec scan index =
    if index >= String.length html then ()
    else if contains_at html index "<!--" then
      let after =
        Option.value (find_from html (index + 4) "-->") ~default:(String.length html - 3)
      in
      scan (min (String.length html) (after + 3))
    else if html.[index] <> '<' then (
      let next = Option.value (find_char html index '<') ~default:(String.length html) in
      add_text (String.sub html index (next - index));
      scan next)
    else
      match find_tag_end html (index + 1) with
      | None ->
          add_text "<";
          scan (index + 1)
      | Some tag_end -> (
          match parse_tag html index tag_end with
          | None ->
              add_text (String.sub html index (tag_end - index + 1));
              scan (tag_end + 1)
          | Some tag ->
              if (not tag.closing) && (tag.name = "script" || tag.name = "style") then (
                add_newline output last_newline;
                let marker = "</" ^ tag.name in
                let after =
                  match find_from lower (tag_end + 1) marker with
                  | None -> String.length html
                  | Some close_start ->
                      Option.value
                        (find_tag_end html (close_start + 1))
                        ~default:(String.length html - 1)
                      + 1
                in
                scan (min (String.length html) after))
              else (
                (match (format, tag.closing, tag.name) with
                | Plain, true, name ->
                    if block_tag name then add_newline output last_newline else ()
                | Plain, false, name ->
                    if name = "br" then add_newline output last_newline
                    else if name = "li" then add_newline output last_newline
                    else ()
                | Markdown, false, "br" -> add_newline output last_newline
                | Markdown, false, "pre" ->
                    add_newline output last_newline;
                    add_string output last_newline "```\n";
                    in_pre := true
                | Markdown, true, "pre" ->
                    add_newline output last_newline;
                    add_string output last_newline "```\n";
                    in_pre := false
                | Markdown, false, "code" when not !in_pre ->
                    Buffer.add_char output '`';
                    last_newline := false;
                    in_code := true
                | Markdown, true, "code" when not !in_code -> ()
                | Markdown, true, "code" ->
                    Buffer.add_char output '`';
                    last_newline := false;
                    in_code := false
                | Markdown, false, "li" ->
                    add_newline output last_newline;
                    add_string output last_newline "- "
                | Markdown, true, "li" -> add_newline output last_newline
                | Markdown, false, name -> (
                    match heading_level name with
                    | Some level ->
                        add_newline output last_newline;
                        add_string output last_newline (String.make level '#' ^ " ")
                    | None when name = "blockquote" ->
                        add_newline output last_newline;
                        add_string output last_newline "> "
                    | None when name = "hr" ->
                        add_newline output last_newline;
                        add_string output last_newline "---\n"
                    | None when name = "a" ->
                        Buffer.add_char output '[';
                        last_newline := false;
                        anchors := attr_value tag.attrs "href" :: !anchors
                    | None when name = "img" ->
                        let alt =
                          Option.value (attr_value tag.attrs "alt") ~default:"image"
                        in
                        let src = attr_value tag.attrs "src" in
                        add_string output last_newline ("Image: " ^ alt);
                        Option.iter
                          (fun value -> add_string output last_newline (" -> " ^ value))
                          src
                    | None when block_tag name -> add_newline output last_newline
                    | None -> ())
                | Markdown, true, name -> (
                    match heading_level name with
                    | Some _ -> add_newline output last_newline
                    | None when name = "a" -> (
                        match !anchors with
                        | href :: rest ->
                            anchors := rest;
                            Buffer.add_char output ']';
                            Option.iter
                              (fun value ->
                                add_string output last_newline ("(" ^ value ^ ")"))
                              href
                        | [] -> Buffer.add_char output ']')
                    | None when block_tag name -> add_newline output last_newline
                    | None -> ()));
                scan (tag_end + 1)))
  in
  scan 0;
  Buffer.contents output |> String.trim

let read_body body =
  let limit = 5 * 1024 * 1024 in
  let output = Buffer.create 4096 in
  let count = ref 0 in
  let chunk = Cstruct.create 65_536 in
  let rec loop () =
    match Eio.Flow.single_read body chunk with
    | bytes when bytes > 0 ->
        let available = limit - !count in
        let keep = min available bytes in
        if keep < bytes then Error `Too_large
        else (
          Buffer.add_substring output
            (Cstruct.to_string (Cstruct.sub chunk 0 keep))
            0 keep;
          count := !count + keep;
          loop ())
    | _ -> loop ()
    | exception End_of_file -> Ok (Buffer.contents output)
  in
  loop ()

let status_code response = Http.Status.to_int (Http.Response.status response)
let is_redirect = function 301 | 302 | 303 | 307 | 308 -> true | _ -> false

let parse_http_uri raw =
  let uri = Uri.of_string raw in
  match (Uri.scheme uri, Uri.host uri) with
  | Some ("http" | "https"), Some host when host <> "" -> (
      match Domain_name.of_string host with
      | Ok _ -> Ok uri
      | Error (`Msg message) ->
          Error (`Invalid_input (Fmt.str "invalid URL host: %s" message)))
  | Some ("http" | "https"), _ -> Error (`Invalid_input "URL must include a host")
  | Some scheme, _ -> Error (`Invalid_input (Fmt.str "unsupported URL scheme: %s" scheme))
  | None, _ -> Error (`Invalid_input "URL must include http or https scheme")

let make_client (ctx : Tool.ctx) =
  match Ca_certs.authenticator () with
  | Error (`Msg message) ->
      Error (`Unavailable (Fmt.str "TLS trust store unavailable: %s" message))
  | Ok authenticator -> (
      match Tls.Config.client ~authenticator () with
      | Error (`Msg message) ->
          Error (`Unavailable (Fmt.str "TLS configuration failed: %s" message))
      | Ok tls_config ->
          let https uri flow =
            let host =
              match Uri.host uri with
              | Some host -> host
              | None -> invalid_arg "HTTP URL has no host"
            in
            match Ipaddr.of_string host with
            | Ok ip -> Tls_eio.client_of_flow tls_config ~ip flow
            | Error _ ->
                let domain = Domain_name.of_string_exn host |> Domain_name.host_exn in
                Tls_eio.client_of_flow tls_config ~host:domain flow
          in
          Ok (Cohttp_eio.Client.make ~https:(Some https) ctx.Tool.net))

let response_body body = read_body body

let discard_body body =
  let limit = 5 * 1024 * 1024 in
  let count = ref 0 in
  let chunk = Cstruct.create 65_536 in
  let rec loop () =
    match Eio.Flow.single_read body chunk with
    | bytes when bytes > 0 ->
        if !count + bytes > limit then Error `Too_large
        else (
          count := !count + bytes;
          loop ())
    | _ -> loop ()
    | exception End_of_file -> Ok ()
  in
  loop ()

let rec fetch_uri (ctx : Tool.ctx) client ~headers ~sw ~(format : output_format)
    ~redirects uri =
  let response, body = Cohttp_eio.Client.get client ~headers ~sw uri in
  let status = status_code response in
  if is_redirect status then
    match discard_body body with
    | Error `Too_large ->
        Error (`Unavailable (Fmt.str "HTTP %d redirect body exceeds 5 MiB" status))
    | Ok () -> (
        if redirects >= 5 then Error (`Unavailable "too many HTTP redirects")
        else
          match Http.Header.get (Http.Response.headers response) "location" with
          | None ->
              Error
                (`Unavailable (Fmt.str "HTTP %d redirect has no Location header" status))
          | Some location ->
              let target = Uri.resolve "" uri (Uri.of_string location) in
              let* target = parse_http_uri (Uri.to_string target) in
              fetch_uri ctx client ~headers ~sw ~format ~redirects:(redirects + 1) target)
  else
    match response_body body with
    | Error `Too_large ->
        Error (`Unavailable (Fmt.str "HTTP %d response body exceeds 5 MiB" status))
    | Ok body_text when status < 200 || status >= 300 ->
        let detail =
          if body_text = "" then ""
          else
            let length = min 4096 (String.length body_text) in
            ": " ^ String.trim (String.sub body_text 0 length)
        in
        Error (`Unavailable (Fmt.str "HTTP %d%s" status detail))
    | Ok body_text ->
        let rendered =
          match format with
          | Html -> body_text
          | Text -> render_html Plain body_text
          | Markdown -> render_html Markdown body_text
        in
        Ok rendered

let output_with_artifact (ctx : Tool.ctx) content =
  let content, artifact =
    Artifact.truncate ctx.Tool.artifacts ~random:ctx.Tool.random content
  in
  Tool.ok ?artifact content

let fetch_schema =
  Tool.schema_object ~required:[ "url" ]
    [
      ("url", Tool.s_string ~desc:"HTTP or HTTPS URL." ());
      ( "format",
        Tool.s_string
          ~enum:[ "text"; "markdown"; "html" ]
          ~desc:"Output representation." () );
      ("timeout_s", Tool.s_int ~default:30 ());
    ]

let fetch =
  {
    Tool.name = "fetch";
    description =
      "Fetch an HTTP or HTTPS document with bounded redirects, body and deadline.";
    schema = fetch_schema;
    read_only = true;
    run =
      (fun (ctx : Tool.ctx) value ->
        let* params = Tool.decode params_jsont value in
        let timeout_s = Option.value params.timeout_s ~default:30 in
        let format = Option.value params.format ~default:(Markdown : output_format) in
        if timeout_s < 1 || timeout_s > 120 then
          Error (`Invalid_input "timeout_s must be between 1 and 120")
        else
          let* uri = parse_http_uri params.url in
          let* () =
            Tool.request ctx ~read_only:true ~tool:"fetch" ~action:params.url ~path:""
              ~description:(Fmt.str "Fetch %s" params.url)
          in
          let operation () =
            Eio.Switch.run @@ fun fetch_sw ->
            let headers =
              Http.Header.of_list
                [
                  ("user-agent", "crush/" ^ Charm_cli.Version.current);
                  ("accept", "text/html, text/plain, text/markdown, */*");
                ]
            in
            let* client = make_client ctx in
            fetch_uri ctx client ~headers ~sw:fetch_sw ~format ~redirects:0 uri
          in
          try
            match Tool.with_timeout ctx (float_of_int timeout_s) operation with
            | Ok (Ok content) -> Ok (output_with_artifact ctx content)
            | Ok (Error error) -> Error error
            | Error error -> Error error
          with
          | Eio.Io _ as exception_ ->
              Error (`Io (params.url, Fmt.str "%a" Eio.Exn.pp exception_))
          | Tls_eio.Tls_alert alert ->
              Error
                (`Unavailable
                   (Fmt.str "TLS alert while fetching %s: %s" params.url
                      (Tls.Packet.alert_type_to_string alert)))
          | Tls_eio.Tls_failure failure ->
              Error
                (`Unavailable
                   (Fmt.str "TLS failure while fetching %s: %a" params.url
                      Tls.Engine.pp_failure failure))
          | Unix.Unix_error (error, operation, argument) ->
              Error
                (`Io
                   ( params.url,
                     Fmt.str "%s: %s (%s)" operation (Unix.error_message error) argument
                   ))
          | Invalid_argument message -> Error (`Invalid_input message)
          | Failure message -> Error (`Unavailable message));
  }
