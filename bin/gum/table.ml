open Result.Syntax

type error =
  [ `Csv of Csv.error
  | `No_data
  | `Invalid_width of int
  | `Invalid_columns
  | `Invalid_padding of string
  | `Invalid_return_column of int ]

type options = {
  separator : string;
  columns : string list;
  widths : int list;
  height : int;
  print : bool;
  file : string;
  border : string;
  show_help : bool;
  hide_count : bool;
  lazy_quotes : bool;
  fields_per_record : int;
  return_column : int;
  timeout : float option;
  padding : string;
  border_style : Gum_style.t;
  cell_style : Gum_style.t;
  header_style : Gum_style.t;
  selected_style : Gum_style.t;
}

let style ?foreground ?bold () = Gum_style.defaults ?foreground ?bold ()

let default_options =
  {
    separator = ",";
    columns = [];
    widths = [];
    height = 0;
    print = false;
    file = "";
    border = "rounded";
    show_help = true;
    hide_count = false;
    lazy_quotes = false;
    fields_per_record = 0;
    return_column = 0;
    timeout = None;
    padding = "0 0";
    border_style = style ();
    cell_style = style ();
    header_style = style ~bold:true ();
    selected_style = style ~foreground:"212" ~bold:true ();
  }

let error_message = function
  | `Csv error -> Csv.error_message error
  | `No_data -> "no data provided"
  | `Invalid_width width -> Fmt.str "invalid width: %d" width
  | `Invalid_columns -> "invalid number of columns"
  | `Invalid_padding message -> message
  | `Invalid_return_column column -> Fmt.str "invalid return column: %d" column

let separator_char separator =
  if String.length separator = 1 then Ok (String.get separator 0)
  else Error (`Csv `Invalid_separator)

let pad_rows headers rows =
  let width = List.length headers in
  List.map
    (fun row ->
      let length = List.length row in
      if length > width then Error `Invalid_columns
      else Ok (row @ List.init (width - length) (fun _ -> "")))
    rows
  |> List.fold_left
       (fun acc item ->
         match (acc, item) with
         | (Error _ as error), _ -> error
         | _, (Error _ as error) -> error
         | Ok rows, Ok row -> Ok (row :: rows))
       (Ok [])
  |> Result.map List.rev

let parse_input options input =
  match List.find_opt (fun width -> width <= 0) options.widths with
  | Some width -> Error (`Invalid_width width)
  | None -> (
      if String.trim input = "" then Error `No_data
      else
        let* separator = separator_char options.separator in
        match
          Csv.parse ~separator ~lazy_quotes:options.lazy_quotes
            ~fields_per_record:
              (if options.fields_per_record = 0 then -1 else options.fields_per_record)
            input
        with
        | Error error -> Error (`Csv error)
        | Ok [] -> Error `No_data
        | Ok rows ->
            let headers, data =
              match options.columns with
              | [] -> (List.hd rows, List.tl rows)
              | columns -> (columns, rows)
            in
            if headers = [] then Error `No_data
            else Result.map (fun data -> (headers, data)) (pad_rows headers data))

let resolve_widths options headers rows =
  let header_widths =
    List.mapi (fun index value -> (index, Charm_ansi.Text.width value)) headers
  in
  let data_widths =
    List.fold_left
      (fun widths row ->
        List.mapi
          (fun index value ->
            let current = List.nth widths index in
            max current (Charm_ansi.Text.width value))
          row
        |> List.map2 max widths)
      (List.map snd header_widths) rows
  in
  let explicit = options.widths in
  let widths =
    List.mapi
      (fun index auto ->
        match List.nth_opt explicit index with
        | None -> max 1 auto
        | Some width when width > 0 -> width
        | Some width -> raise (Invalid_argument (Fmt.str "invalid width: %d" width)))
      data_widths
  in
  widths

let constrain_row widths row =
  List.mapi
    (fun index value ->
      let width = List.nth widths index in
      Charm_ansi.Text.pad_right ~width (Charm_ansi.Text.truncate ~tail:"…" ~width value))
    row

let render_static_with_padding options ~headers ~rows ~padding =
  let widths = resolve_widths options headers rows in
  let headers = if options.widths = [] then headers else constrain_row widths headers in
  let rows = if options.widths = [] then rows else List.map (constrain_row widths) rows in
  let border =
    Option.value (Gum_flag.border options.border) ~default:Charm_lipgloss.Border.none
  in
  let header_style = Gum_style.to_style options.header_style in
  let cell_style = Gum_style.to_style options.cell_style in
  let style ~row ~col:_ = if row = -1 then header_style else cell_style in
  let rendered =
    Charm_lipgloss.Table.render
      (Charm_lipgloss.Table.v ~headers ~rows ~border ~style
         ?height:(if options.height > 0 then Some options.height else None)
         ())
  in
  let frame =
    Charm_lipgloss.Style.render (Gum_style.to_style options.border_style) rendered
  in
  Charm_lipgloss.Style.render
    (Charm_lipgloss.Style.padding padding Charm_lipgloss.Style.empty)
    frame

let render_static options ~headers ~rows =
  match Gum_flag.parse_padding options.padding with
  | Ok padding -> Ok (render_static_with_padding options ~headers ~rows ~padding)
  | Error (`Msg message) -> Error (`Invalid_padding message)

type status = Running | Selected of string list | Quit | Aborted

type model = {
  table : Charm_bubbles.Table.t;
  status : status;
  show_help : bool;
  hide_count : bool;
  padding : Charm_lipgloss.Sides.t;
  border_style : Charm_lipgloss.Style.t;
}

type msg = Table of Charm_bubbles.Table.msg | Key of Charm_tea.Key.t

let key_name key = Charm_tea.Key.to_string key
let is_abort key = String.equal (key_name key) "ctrl+c"
let is_submit key = match key_name key with "enter" | "ctrl+q" -> true | _ -> false
let is_quit key = match key_name key with "q" | "esc" -> true | _ -> false

let make_model (options : options) ~headers ~rows ~padding =
  let widths = resolve_widths options headers rows in
  let columns =
    List.mapi
      (fun index title -> { Charm_bubbles.Table.title; width = List.nth widths index })
      headers
  in
  let styles =
    {
      Charm_bubbles.Table.header = Gum_style.to_style options.header_style;
      cell = Gum_style.to_style options.cell_style;
      selected = Gum_style.to_style options.selected_style;
    }
  in
  let height =
    if options.height > 0 then
      Charm_lipgloss.Sides.(max 1 (options.height - padding.top - padding.bottom))
    else 0
  in
  let table =
    if options.height > 0 then
      Charm_bubbles.Table.v ~columns ~rows ~height ~focused:true ~styles ()
    else Charm_bubbles.Table.v ~columns ~rows ~focused:true ~styles ()
  in
  {
    table;
    status = Running;
    show_help = options.show_help;
    hide_count = options.hide_count;
    padding;
    border_style = Gum_style.to_style options.border_style;
  }

let table_view model =
  let base = Charm_bubbles.Table.view model.table in
  let help =
    if model.show_help then "\n" ^ Charm_bubbles.Table.help_view model.table else ""
  in
  let count =
    if model.hide_count then ""
    else
      let total = List.length (Charm_bubbles.Table.rows model.table) in
      if total = 0 then ""
      else Fmt.str "\n%d/%d" (Charm_bubbles.Table.cursor model.table + 1) total
  in
  let content = base ^ count ^ help in
  let content = Charm_lipgloss.Style.render model.border_style content in
  Charm_lipgloss.Style.render
    (Charm_lipgloss.Style.padding model.padding Charm_lipgloss.Style.empty)
    content

let make_app model =
  let update message model =
    match message with
    | Key key when is_abort key ->
        ({ model with status = Aborted }, Charm_tea.Cmd.interrupt)
    | Key key when is_submit key -> (
        match Charm_bubbles.Table.selected_row model.table with
        | Some row -> ({ model with status = Selected row }, Charm_tea.Cmd.quit)
        | None -> ({ model with status = Quit }, Charm_tea.Cmd.quit))
    | Key key when is_quit key -> ({ model with status = Quit }, Charm_tea.Cmd.quit)
    | Key key -> (
        match Charm_bubbles.Table.key model.table key with
        | None -> (model, Charm_tea.Cmd.none)
        | Some message ->
            let table, command = Charm_bubbles.Table.update message model.table in
            ({ model with table }, Charm_tea.Cmd.map (fun msg -> Table msg) command))
    | Table message ->
        let table, command = Charm_bubbles.Table.update message model.table in
        ({ model with table }, Charm_tea.Cmd.map (fun msg -> Table msg) command)
  in
  let view model = Charm_tea.View.v ~alt_screen:false (table_view model) in
  let subscriptions _ = Charm_tea.Sub.key (fun key -> Key key) in
  {
    Charm_tea.init = (fun () -> (model, Charm_tea.Cmd.none));
    update;
    view;
    subscriptions;
  }

let write_result env ~separator row =
  let text = Csv.write_row ~separator row in
  Eio.Flow.copy_string text env#stdout

let read_input env (options : options) =
  if options.file <> "" then
    try Ok (Eio.Path.load Eio.Path.(env#fs / options.file))
    with Eio.Io _ -> Error (Fmt.str "could not render file: %s" options.file)
  else
    match Gum_io.read_stdin ~strip_ansi:false env with
    | Ok text -> Ok text
    | Error `Empty -> Error "no data provided"
    | Error (`Read text) -> Ok text

let run env (options : options) =
  let input =
    match read_input env options with
    | Ok input -> input
    | Error message -> Charm_cli.error message
  in
  let headers, rows =
    match parse_input options input with
    | Ok value -> value
    | Error error -> Charm_cli.error (error_message error)
  in
  if options.print then
    match render_static options ~headers ~rows with
    | Ok rendered -> Gum_io.println env rendered
    | Error error -> Charm_cli.error (error_message error)
  else
    let padding =
      match Gum_flag.parse_padding options.padding with
      | Ok value -> value
      | Error (`Msg message) -> Charm_cli.error message
    in
    let model = make_model options ~headers ~rows ~padding in
    let model =
      try
        Gum_run.run ?timeout:options.timeout env (make_app model) ~finished:(fun model ->
            match model.status with
            | Selected _ -> Gum_run.Submitted
            | Quit -> Gum_run.Quit
            | Aborted -> Gum_run.Aborted
            | Running -> Gum_run.Quit)
      with Gum_io.No_tty -> Charm_cli.error "table: requires a terminal"
    in
    match model.status with
    | Selected row ->
        let separator = Result.get_ok (separator_char options.separator) in
        let row =
          if options.return_column = 0 then row
          else if options.return_column > 0 && options.return_column <= List.length row
          then [ List.nth row (options.return_column - 1) ]
          else
            Charm_cli.error (error_message (`Invalid_return_column options.return_column))
        in
        write_result env ~separator row
    | Quit | Running -> Charm_cli.error "nothing selected"
    | Aborted -> Charm_cli.exit 130

let options separator columns widths height print file border show_help hide_count
    lazy_quotes fields_per_record return_column timeout padding border_style cell_style
    header_style selected_style =
  {
    separator;
    columns;
    widths;
    height;
    print;
    file;
    border;
    show_help;
    hide_count;
    lazy_quotes;
    fields_per_record;
    return_column;
    timeout;
    padding;
    border_style;
    cell_style;
    header_style;
    selected_style;
  }

let cmd env =
  let open Cmdliner in
  let separator =
    Gum_flag.delimiter ~cmd:"table" ~default:"," ~doc:"CSV separator." "separator"
  in
  let columns =
    Arg.(
      value
        (opt_all string []
           (info [ "columns"; "c" ]
              ~env:(Gum_flag.env ~cmd:"table" "columns")
              ~doc:"Column names.")))
  in
  let widths =
    Arg.(
      value
        (opt_all int []
           (info [ "widths"; "w" ]
              ~env:(Gum_flag.env ~cmd:"table" "widths")
              ~doc:"Column widths.")))
  in
  let height =
    Arg.(
      value
        (opt int 0
           (info [ "height" ]
              ~env:(Gum_flag.env ~cmd:"table" "height")
              ~doc:"Visible rows.")))
  in
  let print =
    Gum_flag.flag ~cmd:"table" ~default:false ~doc:"Print without opening a terminal."
      "print"
  in
  let file =
    Arg.(
      value
        (opt string ""
           (info [ "file"; "f" ] ~env:(Gum_flag.env ~cmd:"table" "file") ~doc:"CSV file.")))
  in
  let border =
    Arg.(
      value
        (opt
           (Gum_flag.enum ~docv:"BORDER"
              [
                ("rounded", "rounded");
                ("thick", "thick");
                ("normal", "normal");
                ("hidden", "hidden");
                ("double", "double");
                ("none", "none");
              ])
           "rounded"
           (info [ "border"; "b" ]
              ~env:(Gum_flag.env ~cmd:"table" "border")
              ~doc:"Border style.")))
  in
  let show_help =
    Gum_flag.negatable ~cmd:"table" ~default:true ~doc:"Show help." "show-help"
  in
  let hide_count =
    Gum_flag.negatable ~cmd:"table" ~default:false ~doc:"Hide cursor count." "hide-count"
  in
  let lazy_quotes =
    Gum_flag.flag ~cmd:"table" ~default:false ~doc:"Accept lazy CSV quotes." "lazy-quotes"
  in
  let fields_per_record =
    Arg.(
      value
        (opt int 0
           (info [ "fields-per-record" ]
              ~env:(Gum_flag.env ~cmd:"table" "fields-per-record")
              ~doc:"Expected fields per row.")))
  in
  let return_column =
    Arg.(
      value
        (opt int 0
           (info [ "return-column"; "r" ]
              ~env:(Gum_flag.env ~cmd:"table" "return-column")
              ~doc:"Return one column.")))
  in
  let timeout =
    Gum_flag.seconds ~cmd:"table" ~doc:"Abort after this duration." "timeout"
  in
  let typed_padding =
    let parse value =
      match Gum_flag.parse_padding value with
      | Ok _ -> Ok value
      | Error (`Msg message) -> Error (`Msg message)
    in
    let padding_conv =
      Arg.conv (parse, fun formatter _ -> Stdlib.Format.pp_print_string formatter "")
    in
    Arg.(
      value
        (opt padding_conv "0 0"
           (info [ "padding" ] ~env:(Gum_flag.env ~cmd:"table" "padding") ~doc:"Padding.")))
  in
  let border_style =
    Gum_style.term ~cmd:"table" ~prefix:"border" ~defaults:default_options.border_style ()
  in
  let cell_style =
    Gum_style.term ~cmd:"table" ~prefix:"cell" ~defaults:default_options.cell_style ()
  in
  let header_style =
    Gum_style.term ~cmd:"table" ~prefix:"header" ~defaults:default_options.header_style ()
  in
  let selected_style =
    Gum_style.term ~cmd:"table" ~prefix:"selected"
      ~defaults:default_options.selected_style ()
  in
  let action separator columns widths height print file border show_help hide_count
      lazy_quotes fields_per_record return_column timeout padding border_style cell_style
      header_style selected_style =
    run env
      (options separator columns widths height print file border show_help hide_count
         lazy_quotes fields_per_record return_column timeout padding border_style
         cell_style header_style selected_style)
  in
  let term =
    Term.(
      const action $ separator $ columns $ widths $ height $ print $ file $ border
      $ show_help $ hide_count $ lazy_quotes $ fields_per_record $ return_column $ timeout
      $ typed_padding $ border_style $ cell_style $ header_style $ selected_style)
  in
  Cmd.v (Cmd.info "table" ~doc:"Select a row from CSV data.") term
