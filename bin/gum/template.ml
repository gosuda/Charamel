type error = [ `Msg of string ]
type token = Word of string | Quoted of string | Lparen | Rparen
type value = Literal of string | Call of string * value list

let errorf fmt = Fmt.kstr (fun message -> `Msg message) fmt
let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let decode_escape = function
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 't' -> '\t'
  | '0' -> '\000'
  | '\\' -> '\\'
  | '"' -> '"'
  | '\'' -> '\''
  | c -> c

let tokenize text =
  let length = String.length text in
  let rec quoted index buffer =
    if index >= length then Error (errorf "unterminated quoted string")
    else
      match text.[index] with
      | '"' -> Ok (Quoted (Buffer.contents buffer), index + 1)
      | '\\' when index + 1 < length ->
          Buffer.add_char buffer (decode_escape text.[index + 1]);
          quoted (index + 2) buffer
      | '\\' -> Error (errorf "unterminated escape in quoted string")
      | c ->
          Buffer.add_char buffer c;
          quoted (index + 1) buffer
  in
  let rec word index buffer =
    if index >= length then Ok (Word (Buffer.contents buffer), index)
    else
      match text.[index] with
      | c when is_space c || c = '(' || c = ')' ->
          Ok (Word (Buffer.contents buffer), index)
      | c ->
          Buffer.add_char buffer c;
          word (index + 1) buffer
  in
  let rec loop index acc =
    if index >= length then Ok (List.rev acc)
    else if is_space text.[index] then loop (index + 1) acc
    else
      match text.[index] with
      | '(' -> loop (index + 1) (Lparen :: acc)
      | ')' -> loop (index + 1) (Rparen :: acc)
      | '"' -> (
          match quoted (index + 1) (Buffer.create 16) with
          | Error _ as error -> error
          | Ok (token, next) -> loop next (token :: acc))
      | _ -> (
          match word index (Buffer.create 16) with
          | Error _ as error -> error
          | Ok (token, next) -> loop next (token :: acc))
  in
  loop 0 []

let ( let* ) result f =
  match result with Ok value -> f value | Error _ as error -> error

let parse_expression tokens =
  let tokens = Array.of_list tokens in
  let length = Array.length tokens in
  let position = ref 0 in
  let rec value () =
    if !position >= length then Error (errorf "missing template argument")
    else
      match tokens.(!position) with
      | Quoted text ->
          incr position;
          Ok (Literal text)
      | Word _ ->
          Error (errorf "template arguments must be quoted strings or nested calls")
      | Rparen -> Error (errorf "unexpected )")
      | Lparen -> (
          incr position;
          if !position >= length then Error (errorf "missing function name")
          else
            match tokens.(!position) with
            | Word name ->
                incr position;
                call name true
            | Quoted _ -> Error (errorf "function name must be an identifier")
            | Lparen | Rparen -> Error (errorf "missing function name"))
  and call name parenthesized =
    let rec arguments acc =
      if !position >= length then
        if parenthesized then Error (errorf "missing ) after %s" name)
        else Ok (List.rev acc)
      else
        match tokens.(!position) with
        | Rparen when parenthesized ->
            incr position;
            Ok (List.rev acc)
        | Rparen -> Error (errorf "unexpected )")
        | _ ->
            let* item = value () in
            arguments (item :: acc)
    in
    let* args = arguments [] in
    Ok (Call (name, args))
  in
  let result =
    if length = 0 then Error (errorf "empty action")
    else
      match tokens.(0) with
      | Word name ->
          position := 1;
          call name false
      | Quoted _ -> Error (errorf "template action must call a function")
      | Lparen -> value ()
      | Rparen -> Error (errorf "unexpected )")
  in
  match result with
  | Error _ as error -> error
  | Ok _ when !position <> length -> Error (errorf "unexpected template arguments")
  | Ok value -> Ok value

let parse_decimal text =
  if text = "" || not (String.for_all (fun c -> c >= '0' && c <= '9') text) then None
  else try Some (int_of_string text) with Failure _ -> None

let color text =
  match parse_decimal text with
  | Some index -> (
      match Charm_ansi.Color.indexed index with
      | Some color -> Ok color
      | None -> Error (errorf "invalid color %S" text))
  | None -> (
      match Charm_ansi.Color.of_hex text with
      | Some color -> Ok color
      | None -> Error (errorf "invalid color %S" text))

let style_call name args =
  let arg_count expected =
    if List.length args = expected then Ok ()
    else
      Error
        (errorf "%s expects %d argument%s" name expected
           (if expected = 1 then "" else "s"))
  in
  let unary name f =
    match (arg_count 1, args) with
    | Ok (), [ Literal text ] ->
        Ok
          ( f Charm_lipgloss.Style.empty |> fun style ->
            Charm_lipgloss.Style.render style text )
    | (Error _ as error), _ -> error
    | Ok (), _ -> Error (errorf "%s expects one argument" name)
  in
  match name with
  | "Overline" -> unary name (fun style -> style)
  | "Bold" -> unary name (Charm_lipgloss.Style.bold true)
  | "Faint" -> unary name (Charm_lipgloss.Style.faint true)
  | "Italic" -> unary name (Charm_lipgloss.Style.italic true)
  | "Underline" -> unary name (Charm_lipgloss.Style.underline true)
  | "Blink" -> unary name (Charm_lipgloss.Style.blink true)
  | "Reverse" -> unary name (Charm_lipgloss.Style.reverse true)
  | "CrossOut" -> unary name (Charm_lipgloss.Style.strikethrough true)
  | "Foreground" | "Background" -> (
      match args with
      | [ Literal colour; Literal text ] -> (
          match color colour with
          | Error _ as error -> error
          | Ok colour ->
              let style =
                if name = "Foreground" then
                  Charm_lipgloss.Style.foreground colour Charm_lipgloss.Style.empty
                else Charm_lipgloss.Style.background colour Charm_lipgloss.Style.empty
              in
              Ok (Charm_lipgloss.Style.render style text))
      | _ -> Error (errorf "%s expects a color and text" name))
  | "Color" -> (
      match args with
      | [ Literal foreground; Literal text ] -> (
          match color foreground with
          | Error _ as error -> error
          | Ok foreground ->
              let style =
                Charm_lipgloss.Style.foreground foreground Charm_lipgloss.Style.empty
              in
              Ok (Charm_lipgloss.Style.render style text))
      | [ Literal foreground; Literal background; Literal text ] -> (
          match (color foreground, color background) with
          | (Error _ as error), _ -> error
          | _, (Error _ as error) -> error
          | Ok foreground, Ok background ->
              let style =
                Charm_lipgloss.Style.empty
                |> Charm_lipgloss.Style.foreground foreground
                |> Charm_lipgloss.Style.background background
              in
              Ok (Charm_lipgloss.Style.render style text))
      | _ ->
          Error
            (errorf
               "Color expects a foreground color, an optional background color, and text")
      )
  | _ -> Error (errorf "unknown template function %S" name)

let rec evaluate = function
  | Literal text -> Ok text
  | Call (name, args) ->
      let rec values acc = function
        | [] -> style_call name (List.rev acc)
        | value :: rest -> (
            match evaluate value with
            | Error _ as error -> error
            | Ok text -> values (Literal text :: acc) rest)
      in
      values [] args

let trim_right_buffer buffer =
  let text = Buffer.contents buffer in
  let index = ref (String.length text) in
  while !index > 0 && is_space text.[!index - 1] do
    decr index
  done;
  if !index < String.length text then Buffer.truncate buffer !index

let find_close source start =
  let length = String.length source in
  let rec loop index depth quoted escaped =
    if index >= length then None
    else if quoted then
      if escaped then loop (index + 1) depth true false
      else if source.[index] = '\\' then loop (index + 1) depth true true
      else if source.[index] = '"' then loop (index + 1) depth false false
      else loop (index + 1) depth true false
    else
      match source.[index] with
      | '"' -> loop (index + 1) depth true false
      | '(' -> loop (index + 1) (depth + 1) false false
      | ')' when depth > 0 -> loop (index + 1) (depth - 1) false false
      | '}' when depth = 0 && index + 1 < length && source.[index + 1] = '}' -> Some index
      | _ -> loop (index + 1) depth false false
  in
  loop start 0 false false

let render source =
  let output = Buffer.create (String.length source + 16) in
  let length = String.length source in
  let rec loop index =
    match String.index_from_opt source index '{' with
    | None ->
        Buffer.add_substring output source index (length - index);
        Ok (Buffer.contents output)
    | Some open_index when open_index + 1 >= length || source.[open_index + 1] <> '{' ->
        Buffer.add_substring output source index (open_index - index + 1);
        loop (open_index + 1)
    | Some open_index -> (
        Buffer.add_substring output source index (open_index - index);
        let trim_left = open_index + 2 < length && source.[open_index + 2] = '-' in
        if trim_left then trim_right_buffer output;
        let body_start = if trim_left then open_index + 3 else open_index + 2 in
        match find_close source body_start with
        | None -> Error (errorf "unterminated template action")
        | Some close_index -> (
            let trim_right = close_index > body_start && source.[close_index - 1] = '-' in
            let body_end = if trim_right then close_index - 1 else close_index in
            let body = String.sub source body_start (body_end - body_start) in
            match tokenize body with
            | Error _ as error -> error
            | Ok tokens -> (
                match parse_expression tokens with
                | Error _ as error -> error
                | Ok expression -> (
                    match evaluate expression with
                    | Error _ as error -> error
                    | Ok text ->
                        Buffer.add_string output text;
                        let next = close_index + 2 in
                        if trim_right then
                          let rec skip index =
                            if index < length && is_space source.[index] then
                              skip (index + 1)
                            else index
                          in
                          loop (skip next)
                        else loop next))))
  in
  loop 0
