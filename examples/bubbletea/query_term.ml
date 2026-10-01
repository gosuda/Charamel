module Textinput = Charamel_bubbles.Textinput

type model = { input : Textinput.t; err : string option }
type msg = Key of Charamel_tea.Key.t | Input of Textinput.msg | Other

let hex_digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - 48)
  | 'a' .. 'f' -> Some (Char.code c - 87)
  | 'A' .. 'F' -> Some (Char.code c - 55)
  | _ -> None

let take_hex s i n =
  let rec go acc k =
    if k = n then Some (acc, i + n)
    else if i + k >= String.length s then None
    else
      match hex_digit (String.get s (i + k)) with
      | Some d -> go ((acc * 16) + d) (k + 1)
      | None -> None
  in
  go 0 0

let take_octal s i =
  let rec go acc k pos =
    if k = 0 || pos >= String.length s then (acc, pos)
    else
      match String.get s pos with
      | '0' .. '7' as c -> go ((acc * 8) + (Char.code c - 48)) (k - 1) (pos + 1)
      | _ -> (acc, pos)
  in
  go 0 3 i

let add_utf8 buf cp =
  let byte n = Buffer.add_char buf (Char.unsafe_chr n) in
  if cp < 0x80 then byte cp
  else if cp < 0x800 then begin
    byte (0xC0 + (cp lsr 6));
    byte (0x80 + (cp land 0x3F))
  end
  else if cp < 0x10000 then begin
    byte (0xE0 + (cp lsr 12));
    byte (0x80 + ((cp lsr 6) land 0x3F));
    byte (0x80 + (cp land 0x3F))
  end
  else begin
    byte (0xF0 + (cp lsr 18));
    byte (0x80 + ((cp lsr 12) land 0x3F));
    byte (0x80 + ((cp lsr 6) land 0x3F));
    byte (0x80 + (cp land 0x3F))
  end

let simple_escape = function
  | 'a' -> Some '\007'
  | 'b' -> Some '\008'
  | 'f' -> Some '\012'
  | 'n' -> Some '\n'
  | 'r' -> Some '\r'
  | 't' -> Some '\t'
  | 'v' -> Some '\011'
  | '\\' -> Some '\\'
  | '\'' -> Some '\''
  | '"' -> Some '"'
  | 'e' -> Some '\027'
  | _ -> None

let unquote s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec go i =
    if i >= len then Ok (Buffer.contents buf)
    else
      let c = String.get s i in
      if c <> '\\' then begin
        Buffer.add_char buf c;
        go (i + 1)
      end
      else if i + 1 >= len then Error "invalid syntax"
      else
        let e = String.get s (i + 1) in
        match simple_escape e with
        | Some c ->
            Buffer.add_char buf c;
            go (i + 2)
        | None -> (
            match e with
            | 'x' -> (
                match take_hex s (i + 2) 2 with
                | Some (value, next) ->
                    Buffer.add_char buf (Char.unsafe_chr value);
                    go next
                | None -> Error "invalid syntax")
            | 'u' | 'U' -> (
                let n = if e = 'u' then 4 else 8 in
                match take_hex s (i + 2) n with
                | Some (value, next)
                  when value <= 0x10FFFF && not (value >= 0xD800 && value <= 0xDFFF) ->
                    add_utf8 buf value;
                    go next
                | _ -> Error "invalid syntax")
            | '0' .. '7' ->
                let value, next = take_octal s (i + 1) in
                Buffer.add_char buf (Char.unsafe_chr (value land 0xFF));
                go next
            | _ -> Error "invalid syntax")
  in
  go 0

let starts_with_escape seq = String.length seq > 0 && String.get seq 0 = '\027'

let submit model =
  match unquote (Textinput.value model.input) with
  | Error message -> ({ model with err = Some message }, Charamel_tea.Cmd.none)
  | Ok seq ->
      if not (starts_with_escape seq) then
        ( { model with err = Some "sequence is not an ANSI escape sequence" },
          Charamel_tea.Cmd.none )
      else ({ input = Textinput.reset model.input; err = None }, Charamel_tea.Cmd.raw seq)

let route_key model key =
  match Textinput.key model.input key with
  | Some input_msg ->
      let input, cmd = Textinput.update input_msg model.input in
      ({ model with input }, Charamel_tea.Cmd.map (fun m -> Input m) cmd)
  | None -> (model, Charamel_tea.Cmd.none)

let app : (model, msg) Charamel_tea.app =
  {
    init =
      (fun () ->
        let input, focus_cmd =
          Textinput.focus (Textinput.v ~char_limit:156 ~width:20 ~virtual_cursor:false ())
        in
        ({ input; err = None }, Charamel_tea.Cmd.map (fun m -> Input m) focus_cmd));
    update =
      (fun msg model ->
        match msg with
        | Key key -> (
            match Charamel_tea.Key.to_string key with
            | "ctrl+c" -> (model, Charamel_tea.Cmd.quit)
            | "enter" -> submit model
            | _ -> route_key { model with err = None } key)
        | Input input_msg ->
            let input, cmd = Textinput.update input_msg model.input in
            ({ model with input }, Charamel_tea.Cmd.map (fun m -> Input m) cmd)
        | Other -> (model, Charamel_tea.Cmd.none));
    view =
      (fun model ->
        let error_line =
          match model.err with Some message -> "\n\nError: " ^ message | None -> ""
        in
        let content =
          Textinput.view model.input ^ error_line
          ^ "\n\nPress ctrl+c to quit, enter to write the sequence to terminal"
        in
        Charamel_tea.View.v ?cursor:(Textinput.cursor model.input) content);
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map (fun m -> Input m) (Textinput.subscriptions model.input);
            Charamel_tea.Sub.terminal (fun _ -> Other);
          ]);
  }

let main () = Smoke.run_ app

let smoke () =
  Smoke.expect app [] [ "Press ctrl+c to quit, enter to write the sequence to terminal" ]
  @ Smoke.expect app
      [ `Text "nope"; Smoke.key "enter" ]
      [ "Error: sequence is not an ANSI escape sequence" ]
  @ Smoke.expect app [ `Text "\\q"; Smoke.key "enter" ] [ "Error: invalid syntax" ]
  @ Smoke.expect app [ `Text "\\e[10t"; Smoke.key "enter"; `Text "X" ] [ "> X" ]
