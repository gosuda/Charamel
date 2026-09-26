type error = [ `Invalid_address of string | `Header_injection of string | `No_recipients ]

let pp_error ppf = function
  | `Invalid_address value -> Format.fprintf ppf "invalid address: %s" value
  | `Header_injection field -> Format.fprintf ppf "header injection in %s" field
  | `No_recipients -> Format.pp_print_string ppf "no recipients"

let has_header_control value =
  let bad = function '\000' | '\r' | '\n' -> true | _ -> false in
  String.exists bad value

let contains_whitespace value =
  String.exists (function ' ' | '\t' -> true | _ -> false) value

module Address = struct
  type t = { display : string option; addr : string }

  let v ?display addr =
    if has_header_control addr then Error (`Header_injection addr)
    else
      match display with
      | Some name when has_header_control name -> Error (`Header_injection name)
      | _ ->
          let at = String.index_opt addr '@' in
          let valid =
            match at with
            | None -> false
            | Some i ->
                i > 0
                && i < String.length addr - 1
                && String.index_from_opt addr (i + 1) '@' = None
                && not (contains_whitespace addr)
          in
          if not valid then Error (`Invalid_address addr) else Ok { display; addr }

  let addr t = t.addr
  let display t = t.display

  let quote_display name =
    let atom_char = function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?' | '^' | '_'
      | '`' | '{' | '|' | '}' | '~' ->
          true
      | _ -> false
    in
    if String.for_all atom_char name then name
    else
      let b = Buffer.create (String.length name + 2) in
      Buffer.add_char b '"';
      String.iter
        (fun c ->
          if c = '"' || c = '\\' then Buffer.add_char b '\\';
          Buffer.add_char b c)
        name;
      Buffer.add_char b '"';
      Buffer.contents b

  let one t =
    match t.display with
    | None -> t.addr
    | Some display -> Format.asprintf "%s <%s>" (quote_display display) t.addr

  let to_header list =
    let rec loop current out = function
      | [] -> List.rev (if current = "" then out else current :: out)
      | item :: rest ->
          let item = one item in
          let next = if current = "" then item else current ^ ", " ^ item in
          if current <> "" && String.length next > 78 then
            loop (" " ^ item) (current :: out) rest
          else loop next out rest
    in
    String.concat "\r\n" (loop "" [] list)
end

type attachment = { name : string; content_type : string; data : string }

let lowercase_ascii s = String.lowercase_ascii s

let extension name =
  match String.rindex_opt name '.' with
  | None -> ""
  | Some i ->
      if i = String.length name - 1 then ""
      else lowercase_ascii (String.sub name (i + 1) (String.length name - i - 1))

let guess_content_type name =
  match extension name with
  | "txt" -> "text/plain"
  | "html" | "htm" -> "text/html"
  | "css" -> "text/css"
  | "js" -> "text/javascript"
  | "json" -> "application/json"
  | "xml" -> "application/xml"
  | "csv" -> "text/csv"
  | "tsv" -> "text/tab-separated-values"
  | "md" | "markdown" -> "text/markdown"
  | "yaml" | "yml" -> "application/yaml"
  | "pdf" -> "application/pdf"
  | "png" -> "image/png"
  | "jpg" | "jpeg" -> "image/jpeg"
  | "gif" -> "image/gif"
  | "svg" -> "image/svg+xml"
  | "webp" -> "image/webp"
  | "zip" -> "application/zip"
  | "gz" -> "application/gzip"
  | "tar" -> "application/x-tar"
  | "doc" -> "application/msword"
  | "docx" -> "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  | "xls" -> "application/vnd.ms-excel"
  | "xlsx" -> "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  | "ppt" -> "application/vnd.ms-powerpoint"
  | "pptx" -> "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  | "odt" -> "application/vnd.oasis.opendocument.text"
  | "mp3" -> "audio/mpeg"
  | "wav" -> "audio/wav"
  | "mp4" -> "video/mp4"
  | _ -> "application/octet-stream"

let attachment ?content_type ~name ~data () =
  let invalid_name =
    name = ""
    || String.exists
         (function '\000' | '\r' | '\n' | '/' | '\\' -> true | _ -> false)
         name
  in
  if invalid_name then Error (`Header_injection name)
  else
    let content_type = Option.value content_type ~default:(guess_content_type name) in
    if content_type = "" || has_header_control content_type then
      Error (`Header_injection content_type)
    else Ok { name; content_type; data }

type message = {
  from : Address.t;
  reply_to : Address.t list;
  to_ : Address.t list;
  cc : Address.t list;
  bcc : Address.t list;
  subject : string;
  date : Ptime.t;
  message_id : string option;
  body_text : string;
  body_html : string option;
  attachments : attachment list;
}

let validate_address_list list =
  List.iter
    (fun a ->
      if has_header_control (Address.addr a) then
        invalid_arg "address contains a header control";
      match Address.display a with
      | Some name when has_header_control name ->
          invalid_arg "display name contains a header control"
      | _ -> ())
    list

let message ?(reply_to = []) ?(cc = []) ?(bcc = []) ?message_id ~from ~subject ~date
    ~body_text ?body_html ?(attachments = []) ~to_ () =
  if has_header_control subject then Error (`Header_injection "Subject")
  else
    let check_id =
      match message_id with
      | Some id when has_header_control id -> Error (`Header_injection "Message-ID")
      | _ -> Ok ()
    in
    match check_id with
    | Error _ as e -> e
    | Ok () -> (
        let all_addresses = from :: (reply_to @ to_ @ cc @ bcc) in
        let bad_address =
          List.find_map
            (fun address ->
              if has_header_control (Address.addr address) then
                Some (`Header_injection "address")
              else
                match Address.display address with
                | Some display when has_header_control display ->
                    Some (`Header_injection "display")
                | _ -> None)
            all_addresses
        in
        let bad_attachment =
          List.find_map
            (fun attachment ->
              if
                attachment.name = ""
                || String.exists
                     (function '\000' | '\r' | '\n' | '/' | '\\' -> true | _ -> false)
                     attachment.name
              then Some (`Header_injection "attachment name")
              else if
                attachment.content_type = "" || has_header_control attachment.content_type
              then Some (`Header_injection "attachment Content-Type")
              else None)
            attachments
        in
        match (bad_address, bad_attachment) with
        | Some error, _ | None, Some error -> Error error
        | None, None ->
            Ok
              {
                from;
                reply_to;
                to_;
                cc;
                bcc;
                subject;
                date;
                message_id;
                body_text;
                body_html;
                attachments;
              })

let format_date ?(tz_offset_s = 0) date =
  let (year, month, day), ((hour, minute, second), offset) =
    Ptime.to_date_time ~tz_offset_s date
  in
  let weekday =
    match Ptime.weekday ~tz_offset_s date with
    | `Sun -> "Sun"
    | `Mon -> "Mon"
    | `Tue -> "Tue"
    | `Wed -> "Wed"
    | `Thu -> "Thu"
    | `Fri -> "Fri"
    | `Sat -> "Sat"
  in
  let month_name =
    [|
      "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec";
    |].(month - 1)
  in
  let sign = if offset < 0 then '-' else '+' in
  let absolute_offset = abs offset in
  let offset_h = absolute_offset / 3600 in
  let offset_m = absolute_offset mod 3600 / 60 in
  Format.asprintf "%s, %02d %s %04d %02d:%02d:%02d %c%02d%02d" weekday day month_name year
    hour minute second sign offset_h offset_m

let normalize_crlf text =
  let b = Buffer.create (String.length text + 16) in
  let rec loop i =
    if i = String.length text then ()
    else
      match text.[i] with
      | '\r' ->
          Buffer.add_string b "\r\n";
          if i + 1 < String.length text && text.[i + 1] = '\n' then loop (i + 2)
          else loop (i + 1)
      | '\n' ->
          Buffer.add_string b "\r\n";
          loop (i + 1)
      | c ->
          Buffer.add_char b c;
          loop (i + 1)
  in
  loop 0;
  Buffer.contents b

let hex n = String.make 1 (String.get "0123456789ABCDEF" n)

let qp_token c =
  let n = Char.code c in
  if n = 9 || (n >= 32 && n <= 126 && n <> 61) then String.make 1 c
  else "=" ^ hex (n lsr 4) ^ hex (n land 15)

let qp_line line =
  let b = Buffer.create (String.length line + 16) in
  let column = ref 0 in
  let add token =
    if !column + String.length token > 75 then (
      Buffer.add_string b "=\r\n";
      column := 0);
    Buffer.add_string b token;
    column := !column + String.length token
  in
  String.iteri
    (fun index c ->
      let last = index = String.length line - 1 in
      let token =
        if last && (c = ' ' || c = '\t') then
          "=" ^ hex (Char.code c lsr 4) ^ hex (Char.code c land 15)
        else qp_token c
      in
      add token)
    line;
  Buffer.contents b

let quoted_printable text =
  let text = normalize_crlf text in
  let b = Buffer.create (String.length text + 16) in
  let rec next_line start =
    match String.index_from_opt text start '\n' with
    | None ->
        let line =
          if start < String.length text && text.[String.length text - 1] = '\r' then
            String.sub text start (String.length text - start - 1)
          else String.sub text start (String.length text - start)
        in
        Buffer.add_string b (qp_line line);
        Buffer.add_string b "\r\n"
    | Some finish ->
        let line_end =
          if finish > start && text.[finish - 1] = '\r' then finish - 1 else finish
        in
        Buffer.add_string b (qp_line (String.sub text start (line_end - start)));
        Buffer.add_string b "\r\n";
        if finish + 1 < String.length text then next_line (finish + 1)
  in
  if text = "" then Buffer.add_string b "\r\n" else next_line 0;
  Buffer.contents b

let base64_lines data =
  let encoded = Base64.encode_string data in
  let b = Buffer.create (String.length encoded + (String.length encoded / 76 * 2) + 2) in
  let rec loop offset =
    if offset < String.length encoded then (
      let length = min 76 (String.length encoded - offset) in
      Buffer.add_substring b encoded offset length;
      Buffer.add_string b "\r\n";
      loop (offset + length))
  in
  loop 0;
  Buffer.contents b

let subject_needs_encoding value =
  String.exists (fun c -> Char.code c < 32 || Char.code c > 126) value

let utf8_chunk_end value start =
  let target = min (String.length value) (start + 30) in
  let rec back n =
    if n <= start then target
    else if n < String.length value && Char.code value.[n] land 0xC0 = 0x80 then
      back (n - 1)
    else n
  in
  let result = back target in
  if result = start then target else result

let encoded_subject_value value =
  if not (subject_needs_encoding value) then value
  else
    let words = ref [] in
    let start = ref 0 in
    while !start < String.length value do
      let finish = utf8_chunk_end value !start in
      let chunk = String.sub value !start (finish - !start) in
      words := ("=?UTF-8?B?" ^ Base64.encode_string chunk ^ "?=") :: !words;
      start := finish
    done;
    String.concat "\r\n " (List.rev !words)

let encoded_subject message = encoded_subject_value message.subject
let digest_hex input = Digestif.SHA256.to_hex (Digestif.SHA256.digest_string input)

let message_id message =
  match message.message_id with
  | Some id -> if String.length id > 1 && id.[0] = '<' then id else "<" ^ id ^ ">"
  | None ->
      let seed =
        String.concat "\000"
          [
            Address.addr message.from;
            message.subject;
            format_date message.date;
            message.body_text;
          ]
      in
      let digest = digest_hex seed in
      let domain =
        match String.rindex_opt (Address.addr message.from) '@' with
        | None -> "localhost"
        | Some i ->
            String.sub (Address.addr message.from) (i + 1)
              (String.length (Address.addr message.from) - i - 1)
      in
      "<" ^ String.sub digest 0 24 ^ "@" ^ domain ^ ">"

type part = {
  content_type : string;
  transfer_encoding : string;
  disposition : string option;
  data : string;
}

let part_headers part =
  let disposition =
    match part.disposition with
    | None -> ""
    | Some value -> "Content-Disposition: " ^ value ^ "\r\n"
  in
  "Content-Type: " ^ part.content_type ^ "\r\n" ^ "Content-Transfer-Encoding: "
  ^ part.transfer_encoding ^ "\r\n" ^ disposition ^ "\r\n"

let render_part part = part_headers part ^ part.data

let multipart boundary parts =
  let b = Buffer.create 256 in
  List.iter
    (fun part ->
      Buffer.add_string b "--";
      Buffer.add_string b boundary;
      Buffer.add_string b "\r\n";
      Buffer.add_string b (render_part part))
    parts;
  Buffer.add_string b "--";
  Buffer.add_string b boundary;
  Buffer.add_string b "--\r\n";
  Buffer.contents b

let filename_parameter name =
  let safe =
    String.for_all
      (function
        | 'a' .. 'z'
        | 'A' .. 'Z'
        | '0' .. '9'
        | '!' | '#' | '$' | '%' | '&' | '+' | '-' | '.' | '^' | '_' | '`' | '|' | '~' ->
            true
        | _ -> false)
      name
  in
  if safe then "filename=\"" ^ name ^ "\""
  else
    let b = Buffer.create (String.length name * 3) in
    Buffer.add_string b "filename*=UTF-8''";
    String.iter
      (fun c ->
        let n = Char.code c in
        if n >= 33 && n <= 126 && n <> 37 && n <> 39 && n <> 59 && n <> 92 then
          Buffer.add_char b c
        else Buffer.add_string b ("%" ^ hex (n lsr 4) ^ hex (n land 15)))
      name;
    Buffer.contents b

let validate_for_serialise message =
  if has_header_control message.subject then
    invalid_arg "Mime.serialise: Subject contains CR/LF/NUL";
  (match message.message_id with
  | Some id when has_header_control id ->
      invalid_arg "Mime.serialise: Message-ID contains CR/LF/NUL"
  | Some _ | None -> ());
  validate_address_list
    (message.from :: (message.reply_to @ message.to_ @ message.cc @ message.bcc));
  List.iter
    (fun (attachment : attachment) ->
      if
        attachment.name = ""
        || String.exists
             (function '\000' | '\r' | '\n' | '/' | '\\' -> true | _ -> false)
             attachment.name
      then invalid_arg "Mime.serialise: invalid attachment name";
      if attachment.content_type = "" || has_header_control attachment.content_type then
        invalid_arg "Mime.serialise: invalid attachment content type")
    message.attachments

let serialise message =
  validate_for_serialise message;
  let body_seed =
    String.concat "\000"
      (message.body_text
      :: Option.value message.body_html ~default:""
      :: List.map
           (fun (a : attachment) -> a.name ^ a.content_type ^ a.data)
           message.attachments)
  in
  let digest = digest_hex body_seed in
  let outer_boundary = "pop-mixed-" ^ String.sub digest 0 24 in
  let alternative_boundary = "pop-alt-" ^ String.sub digest 24 24 in
  let text_part =
    {
      content_type = "text/plain; charset=UTF-8";
      transfer_encoding = "quoted-printable";
      disposition = None;
      data = quoted_printable message.body_text;
    }
  in
  let body_part =
    match message.body_html with
    | None -> text_part
    | Some html ->
        let html_part =
          {
            content_type = "text/html; charset=UTF-8";
            transfer_encoding = "quoted-printable";
            disposition = None;
            data = quoted_printable html;
          }
        in
        {
          content_type =
            "multipart/alternative; boundary=\"" ^ alternative_boundary ^ "\"";
          transfer_encoding = "7bit";
          disposition = None;
          data = multipart alternative_boundary [ text_part; html_part ];
        }
  in
  let parts =
    body_part
    :: List.map
         (fun (attachment : attachment) ->
           {
             content_type = attachment.content_type;
             transfer_encoding = "base64";
             disposition = Some ("attachment; " ^ filename_parameter attachment.name);
             data = base64_lines attachment.data;
           })
         message.attachments
  in
  let content_type, body, transfer_encoding =
    match message.attachments with
    | [] ->
        let transfer =
          match message.body_html with None -> Some "quoted-printable" | Some _ -> None
        in
        (body_part.content_type, body_part.data, transfer)
    | _ ->
        ( "multipart/mixed; boundary=\"" ^ outer_boundary ^ "\"",
          multipart outer_boundary parts,
          None )
  in
  let headers = Buffer.create 512 in
  Buffer.add_string headers ("From: " ^ Address.to_header [ message.from ] ^ "\r\n");
  if message.to_ <> [] then
    Buffer.add_string headers ("To: " ^ Address.to_header message.to_ ^ "\r\n");
  if message.cc <> [] then
    Buffer.add_string headers ("Cc: " ^ Address.to_header message.cc ^ "\r\n");
  if message.reply_to <> [] then
    Buffer.add_string headers ("Reply-To: " ^ Address.to_header message.reply_to ^ "\r\n");
  Buffer.add_string headers ("Subject: " ^ encoded_subject message ^ "\r\n");
  Buffer.add_string headers ("Date: " ^ format_date message.date ^ "\r\n");
  Buffer.add_string headers ("Message-ID: " ^ message_id message ^ "\r\n");
  Buffer.add_string headers "MIME-Version: 1.0\r\n";
  Buffer.add_string headers ("Content-Type: " ^ content_type ^ "\r\n");
  (match transfer_encoding with
  | Some encoding ->
      Buffer.add_string headers ("Content-Transfer-Encoding: " ^ encoding ^ "\r\n\r\n")
  | None -> Buffer.add_string headers "\r\n");
  Buffer.add_string headers body;
  let result = Buffer.contents headers in
  if String.length result >= 2 && String.sub result (String.length result - 2) 2 = "\r\n"
  then result
  else result ^ "\r\n"

let envelope message =
  let all = message.to_ @ message.cc @ message.bcc in
  match all with
  | [] -> Error `No_recipients
  | _ ->
      let rec unique seen acc = function
        | [] -> List.rev acc
        | address :: rest ->
            let value = Address.addr address in
            if List.exists (String.equal value) seen then unique seen acc rest
            else unique (value :: seen) (value :: acc) rest
      in
      Ok (Address.addr message.from, unique [] [] all)
