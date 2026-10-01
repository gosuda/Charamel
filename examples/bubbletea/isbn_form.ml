module Textinput = Charamel_bubbles.Textinput
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color

type msg = Key of Charamel_tea.Key.t | Isbn of Textinput.msg | Title of Textinput.msg
type model = { isbn : Textinput.t; title : Textinput.t; focused : int }

let input_style = Style.foreground (Color.of_hex_or "#FF985A") Style.empty
let continue_style = Style.foreground (Color.of_hex_or "#719AFC") Style.empty
let valid_style = Style.foreground (Color.of_hex_or "#12C78F") Style.empty
let err_style = Style.foreground (Color.of_hex_or "#FF388B") Style.empty

let contains ~needle ~frame =
  let nl = String.length needle and fl = String.length frame in
  let rec go at =
    if at + nl > fl then false
    else if String.equal (String.sub frame at nl) needle then true
    else go (at + 1)
  in
  go 0

let checksum digits =
  let rec go i acc =
    if i >= String.length digits then acc
    else
      let digit = Char.code digits.[i] - Char.code '0' in
      let digit = if i mod 2 <> 0 then digit * 3 else digit in
      go (i + 1) (acc + digit)
  in
  go 0 0

let isbn13_validator value =
  let digits = String.concat "" (String.split_on_char '-' value) in
  if String.length digits <> 13 then Error "ISBN is of wrong length"
  else if not (String.for_all (fun c -> c >= '0' && c <= '9') digits) then
    Error "ISBN contains invalid characters"
  else
    let prefix = String.sub digits 0 3 in
    if prefix <> "978" && prefix <> "979" then Error "ISBN has invalid GS1 prefix"
    else if checksum digits mod 10 <> 0 then Error "ISBN has invalid check digit"
    else Ok ()

let banned_title_words =
  [ "very"; "bad"; "words"; "that"; "should"; "not"; "appear"; "in"; "book"; "titles" ]

let find_banned title =
  List.find_opt (fun word -> contains ~needle:word ~frame:title) banned_title_words

let book_title_validator value =
  let title = String.trim value in
  if String.length title = 0 then Error "Book title is empty"
  else
    match find_banned title with
    | Some word -> Error (Fmt.str "Book title contains banned word \"%s\"" word)
    | None -> Ok ()

let isbn_input =
  Textinput.v ~prompt:"" ~placeholder:"978-X-XXX-XXXXX-X" ~char_limit:17 ~width:30
    ~validate:isbn13_validator ()

let title_input =
  Textinput.v ~prompt:"" ~placeholder:"Title" ~char_limit:100 ~width:100
    ~validate:book_title_validator ()

let start = { isbn = fst (Textinput.focus isbn_input); title = title_input; focused = 0 }
let filled input = String.length (Textinput.value input) <> 0
let clean input = Textinput.error input = None

let can_find_book model =
  filled model.isbn && clean model.isbn && filled model.title && clean model.title

let focus_on index (model : model) : model =
  let isbn =
    if index = 0 then fst (Textinput.focus model.isbn) else Textinput.blur model.isbn
  in
  let title =
    if index = 1 then fst (Textinput.focus model.title) else Textinput.blur model.title
  in
  { isbn; title; focused = index }

let route_key key input =
  match Textinput.key input key with
  | Some msg -> Textinput.update msg input
  | None -> (input, Charamel_tea.Cmd.none)

let update_inputs key model =
  let isbn, isbn_cmd = route_key key model.isbn in
  let title, title_cmd = route_key key model.title in
  let cmds =
    [
      Charamel_tea.Cmd.map (fun msg -> Isbn msg) isbn_cmd;
      Charamel_tea.Cmd.map (fun msg -> Title msg) title_cmd;
    ]
  in
  ({ model with isbn; title }, Charamel_tea.Cmd.batch cmds)

let on_key key model =
  match Charamel_tea.Key.to_string key with
  | "up" | "down" -> update_inputs key (focus_on (1 - model.focused) model)
  | "enter" when can_find_book model -> (model, Charamel_tea.Cmd.quit)
  | "ctrl+c" | "escape" -> (model, Charamel_tea.Cmd.quit)
  | _ -> update_inputs key model

let error_text label input =
  if not (filled input) then ""
  else
    match Textinput.error input with
    | Some err -> Style.render err_style err
    | None -> Style.render valid_style label

let label text = Style.render (Style.width 30 input_style) text

let view model =
  let continue_text =
    if can_find_book model then Style.render continue_style "Find ->" else ""
  in
  Charamel_tea.View.v
    (Fmt.str " Search book:\n %s\n %s\n %s\n\n %s\n %s\n %s\n\n %s\n" (label "ISBN")
       (Textinput.view model.isbn)
       (error_text "Valid ISBN" model.isbn)
       (label "Title") (Textinput.view model.title)
       (error_text "Valid title" model.title)
       continue_text
    ^ "\n")

let app : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (start, Charamel_tea.Cmd.none));
    update =
      (fun msg model ->
        match msg with
        | Key key -> on_key key model
        | Isbn isbn_msg ->
            let isbn, cmd = Textinput.update isbn_msg model.isbn in
            ({ model with isbn }, Charamel_tea.Cmd.map (fun msg -> Isbn msg) cmd)
        | Title title_msg ->
            let title, cmd = Textinput.update title_msg model.title in
            ({ model with title }, Charamel_tea.Cmd.map (fun msg -> Title msg) cmd));
    view;
    subscriptions =
      (fun model ->
        Charamel_tea.Sub.batch
          [
            Charamel_tea.Sub.key (fun key -> Key key);
            Charamel_tea.Sub.map
              (fun msg -> Isbn msg)
              (Textinput.subscriptions model.isbn);
            Charamel_tea.Sub.map
              (fun msg -> Title msg)
              (Textinput.subscriptions model.title);
          ]);
  }

let main () = Smoke.run_ app
let valid_isbn = `Text "978-3-548-37257-0"

let smoke () =
  Smoke.expect app [ Smoke.key "escape" ] [ "Search book:"; "978-X-XXX-XXXXX-X"; "Title" ]
  @ Smoke.expect app [ valid_isbn; Smoke.key "escape" ] [ "Valid ISBN" ]
  @ Smoke.expect app [ `Text "123"; Smoke.key "escape" ] [ "ISBN is of wrong length" ]
  @ Smoke.expect app
      [ valid_isbn; Smoke.key "down"; `Text "Dune"; Smoke.key "escape" ]
      [ "Valid title"; "Find ->" ]
