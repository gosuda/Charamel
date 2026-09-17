open Result.Syntax

type border = { radius : float; width : float; color : string }
type shadow = { blur : float; x : float; y : float }
type font = { family : string; file : string; size : float; ligatures : bool }

type t = {
  input : string;
  background : string;
  margin : float list;
  padding : float list;
  window : bool;
  width : float;
  height : float;
  config : string;
  interactive : bool;
  language : string;
  theme : string;
  wrap : int;
  output : string;
  execute : string;
  execute_timeout : float;
  border : border;
  shadow : shadow;
  font : font;
  line_height : float;
  lines : int list;
  show_line_numbers : bool;
}

type cli = {
  input : string;
  background : string option;
  margin : float list option;
  padding : float list option;
  window : bool option;
  width : float option;
  height : float option;
  config : string;
  interactive : bool;
  language : string option;
  theme : string option;
  wrap : int option;
  output : string option;
  execute : string option;
  execute_timeout : float option;
  border_radius : float option;
  border_width : float option;
  border_color : string option;
  shadow_blur : float option;
  shadow_x : float option;
  shadow_y : float option;
  font_family : string option;
  font_file : string option;
  font_size : float option;
  font_ligatures : bool option;
  line_height : float option;
  lines : int list option;
  show_line_numbers : bool;
}

let default_border = { radius = 0.; width = 0.; color = "#515151" }
let default_shadow = { blur = 0.; x = 0.; y = 0. }
let default_font = { family = "JetBrains Mono"; file = ""; size = 14.; ligatures = true }

let default =
  {
    input = "";
    background = "#171717";
    margin = [ 0. ];
    padding = [ 20.; 40.; 20.; 20. ];
    window = false;
    width = 0.;
    height = 0.;
    config = "default";
    interactive = false;
    language = "";
    theme = "charm";
    wrap = 0;
    output = "";
    execute = "";
    execute_timeout = 10.;
    border = default_border;
    shadow = default_shadow;
    font = default_font;
    line_height = 1.2;
    lines = [];
    show_line_numbers = false;
  }

let expand_sides ~scale values =
  match values with
  | [ x ] -> [| x *. scale; x *. scale; x *. scale; x *. scale |]
  | [ y; x ] -> [| y *. scale; x *. scale; y *. scale; x *. scale |]
  | [ top; right; bottom; left ] ->
      [| top *. scale; right *. scale; bottom *. scale; left *. scale |]
  | _ -> [| 0.; 0.; 0.; 0. |]

let parse_list ~name parser s =
  let pieces = String.split_on_char ',' s |> List.map String.trim in
  if pieces = [ "" ] then Error (Fmt.str "%s must not be empty" name)
  else
    let rec loop acc = function
      | [] -> Ok (List.rev acc)
      | value :: rest -> (
          match parser value with
          | Some value -> loop (value :: acc) rest
          | None -> Error (Fmt.str "invalid %s value %S" name value))
    in
    loop [] pieces

let float_list_conv =
  let open Cmdliner in
  Arg.Conv.make ~docv:"VALUES"
    ~parser:(parse_list ~name:"side" Float.of_string_opt)
    ~pp:(fun ppf values ->
      Fmt.pf ppf "%s" (String.concat "," (List.map Float.to_string values)))
    ()

let int_list_conv =
  let open Cmdliner in
  Arg.Conv.make ~docv:"START,END"
    ~parser:(parse_list ~name:"line" int_of_string_opt)
    ~pp:(fun ppf values ->
      Fmt.pf ppf "%s" (String.concat "," (List.map Int.to_string values)))
    ()

let option_string names doc =
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt
       (Cmdliner.Arg.some Cmdliner.Arg.string)
       None
       (Cmdliner.Arg.info names ~doc))

let option_float names doc =
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt
       (Cmdliner.Arg.some Cmdliner.Arg.float)
       None
       (Cmdliner.Arg.info names ~doc))

let option_int names doc =
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt
       (Cmdliner.Arg.some Cmdliner.Arg.int)
       None
       (Cmdliner.Arg.info names ~doc))

let duration_conv =
  let parse raw =
    let raw = String.trim raw in
    let invalid () = Error (Fmt.str "invalid duration %S" raw) in
    let suffixes = [ ("ms", 0.001); ("s", 1.); ("m", 60.); ("h", 3600.) ] in
    let parse_number text =
      match Float.of_string_opt text with
      | Some value when Float.is_finite value -> Ok value
      | _ -> invalid ()
    in
    let rec find = function
      | [] -> parse_number raw
      | (suffix, factor) :: rest ->
          if String.ends_with ~suffix raw && String.length raw > String.length suffix then
            let number = String.sub raw 0 (String.length raw - String.length suffix) in
            match parse_number number with
            | Ok value -> Ok (value *. factor)
            | Error _ as error -> error
          else find rest
    in
    find suffixes
  in
  Cmdliner.Arg.Conv.make ~docv:"DURATION" ~parser:parse
    ~pp:(fun ppf value -> Fmt.pf ppf "%.3gs" value)
    ()

let cli_term =
  let open Cmdliner in
  let open Term.Syntax in
  let+ input =
    Arg.(
      value
        (pos 0 string ""
           (info [] ~docv:"FILE" ~doc:"Code or terminal output to capture.")))
  and+ background = option_string [ "b"; "background" ] "Apply a background fill."
  and+ margin =
    Arg.value
      (Arg.opt (Arg.some float_list_conv) None
         (Arg.info [ "m"; "margin" ] ~doc:"Apply margin to the window."))
  and+ padding =
    Arg.value
      (Arg.opt (Arg.some float_list_conv) None
         (Arg.info [ "p"; "padding" ] ~doc:"Apply padding to the code."))
  and+ window =
    Arg.value
      (Arg.opt ~vopt:(Some true) (Arg.some Arg.bool) None
         (Arg.info [ "window" ] ~doc:"Display window controls."))
  and+ width = option_float [ "W"; "width" ] "Width of terminal window."
  and+ height = option_float [ "H"; "height" ] "Height of terminal window."
  and+ config =
    Arg.(
      value
        (opt string "default"
           (info [ "c"; "config" ] ~doc:"Base configuration file or preset.")))
  and+ interactive =
    Arg.value
      (Arg.flag (Arg.info [ "i"; "interactive" ] ~doc:"Prompt for configuration values."))
  and+ language = option_string [ "l"; "language" ] "Language of code file."
  and+ theme = option_string [ "t"; "theme" ] "Theme to use for syntax highlighting."
  and+ wrap = option_int [ "w"; "wrap" ] "Wrap lines at a specific width."
  and+ output = option_string [ "o"; "output" ] "Output SVG or PNG path."
  and+ execute = option_string [ "x"; "execute" ] "Capture output of a command in a PTY."
  and+ execute_timeout =
    Arg.value
      (Arg.opt (Arg.some duration_conv) None
         (Arg.info [ "execute.timeout" ] ~doc:"Execution timeout (for example 10s)."))
  and+ border_radius = option_float [ "r"; "border.radius" ] "Corner radius of window."
  and+ border_width = option_float [ "border.width" ] "Border width thickness."
  and+ border_color = option_string [ "border.color" ] "Border color."
  and+ shadow_blur = option_float [ "shadow.blur" ] "Shadow Gaussian blur."
  and+ shadow_x = option_float [ "shadow.x" ] "Shadow horizontal offset."
  and+ shadow_y = option_float [ "shadow.y" ] "Shadow vertical offset."
  and+ font_family = option_string [ "font.family" ] "Font family to use for code."
  and+ font_file = option_string [ "font.file" ] "Font file to embed."
  and+ font_size = option_float [ "font.size" ] "Font size to use for code."
  and+ font_ligatures =
    Arg.value
      (Arg.opt ~vopt:(Some true) (Arg.some Arg.bool) None
         (Arg.info [ "font.ligatures" ] ~doc:"Use font ligatures."))
  and+ line_height = option_float [ "line-height" ] "Line height relative to font size."
  and+ lines =
    Arg.value
      (Arg.opt (Arg.some int_list_conv) None
         (Arg.info [ "lines" ] ~doc:"Lines to capture (start,end)."))
  and+ show_line_numbers =
    Arg.value
      (Arg.flag (Arg.info [ "show-line-numbers" ] ~doc:"Show source line numbers."))
  in
  {
    input;
    background;
    margin;
    padding;
    window;
    width;
    height;
    config;
    interactive;
    language;
    theme;
    wrap;
    output;
    execute;
    execute_timeout;
    border_radius;
    border_width;
    border_color;
    shadow_blur;
    shadow_x;
    shadow_y;
    font_family;
    font_file;
    font_size;
    font_ligatures;
    line_height;
    lines;
    show_line_numbers;
  }

let member name members = Option.map snd (Jsont.Json.find_mem name members)
let as_string = function Jsont.String (value, _) -> Some value | _ -> None
let as_bool = function Jsont.Bool (value, _) -> Some value | _ -> None

let as_float = function
  | Jsont.Number (value, _) when Float.is_finite value -> Some value
  | Jsont.String (value, _) -> Float.of_string_opt value
  | _ -> None

let as_int value =
  match as_float value with
  | Some value
    when Float.is_integer value
         && value >= float_of_int min_int
         && value <= float_of_int max_int ->
      Some (int_of_float value)
  | _ -> None

let as_float_list = function
  | Jsont.Array (values, _) ->
      let rec decode acc = function
        | [] -> Some (List.rev acc)
        | value :: rest -> (
            match as_float value with
            | Some value -> decode (value :: acc) rest
            | None -> None)
      in
      decode [] values
  | Jsont.String (value, _) ->
      parse_list ~name:"side" Float.of_string_opt value |> Result.to_option
  | Jsont.Number _ as value -> Option.map (fun value -> [ value ]) (as_float value)
  | _ -> None

let with_root value (config : t) =
  match value with
  | Jsont.Object (members, _) ->
      let value name decoder = Option.bind (member name members) decoder in
      let string name old = Option.value (value name as_string) ~default:old in
      let bool name old = Option.value (value name as_bool) ~default:old in
      let float name old = Option.value (value name as_float) ~default:old in
      let integer name old = Option.value (value name as_int) ~default:old in
      let list name old = Option.value (value name as_float_list) ~default:old in
      let nested name =
        Option.bind (member name members) (function
          | Jsont.Object _ as value -> Some value
          | _ -> None)
      in
      let border : border =
        match nested "border" with
        | Some (Jsont.Object (fields, _)) ->
            let value name decoder = Option.bind (member name fields) decoder in
            {
              radius =
                Option.value (value "radius" as_float) ~default:config.border.radius;
              width = Option.value (value "width" as_float) ~default:config.border.width;
              color = Option.value (value "color" as_string) ~default:config.border.color;
            }
        | _ -> config.border
      in
      let shadow : shadow =
        match nested "shadow" with
        | Some (Jsont.Object (fields, _)) ->
            let value name decoder = Option.bind (member name fields) decoder in
            {
              blur = Option.value (value "blur" as_float) ~default:config.shadow.blur;
              x = Option.value (value "x" as_float) ~default:config.shadow.x;
              y = Option.value (value "y" as_float) ~default:config.shadow.y;
            }
        | _ -> config.shadow
      in
      let font : font =
        match nested "font" with
        | Some (Jsont.Object (fields, _)) ->
            let value name decoder = Option.bind (member name fields) decoder in
            {
              family = Option.value (value "family" as_string) ~default:config.font.family;
              file = Option.value (value "file" as_string) ~default:config.font.file;
              size = Option.value (value "size" as_float) ~default:config.font.size;
              ligatures =
                Option.value (value "ligatures" as_bool) ~default:config.font.ligatures;
            }
        | _ -> config.font
      in
      ({
         config with
         background = string "background" config.background;
         margin = list "margin" config.margin;
         padding = list "padding" config.padding;
         window = bool "window" config.window;
         width = float "width" config.width;
         height = float "height" config.height;
         language = string "language" config.language;
         theme = string "theme" config.theme;
         output = string "output" config.output;
         wrap = integer "wrap" config.wrap;
         line_height = float "line_height" config.line_height;
         show_line_numbers = bool "show_line_numbers" config.show_line_numbers;
         border;
         shadow;
         font;
       }
        : t)
  | _ -> config

let decode_json text = Jsont_bytesrw.decode_string Jsont.json text

let base_json =
  "{\"window\":false,\"theme\":\"charm\",\"border\":{\"radius\":0,\"width\":0,\"color\":\"#515151\"},\"shadow\":{\"blur\":0,\"x\":0,\"y\":0},\"padding\":[20,40,20,20],\"margin\":\"0\",\"background\":\"#171717\",\"font\":{\"family\":\"JetBrains \
   Mono\",\"size\":14,\"ligatures\":true},\"line_height\":1.2}"

let full_json =
  "{\"window\":true,\"theme\":\"charm\",\"border\":{\"radius\":8,\"width\":1,\"color\":\"#515151\"},\"shadow\":{\"blur\":24,\"x\":0,\"y\":12},\"padding\":[20,40,20,20],\"margin\":[50,60,70,60],\"background\":\"#171717\",\"font\":{\"family\":\"JetBrains \
   Mono\",\"size\":14,\"ligatures\":true},\"line_height\":1.2}"

let read_path fs path =
  try Ok Eio.Path.(load (fs / path)) with
  | Eio.Io _ -> Error (Fmt.str "cannot read configuration %s" path)
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (Fmt.str "cannot read configuration %s: %s (%s %s)" path
           (Unix.error_message error) function_name argument)

let load ~fs ~name =
  let source =
    match String.lowercase_ascii name with
    | "default" | "base" -> Ok base_json
    | "full" -> Ok full_json
    | "user" -> (
        let path = Filename.concat (Charm_cli.Xdg.config_dir ~app:"freeze") "user.json" in
        match read_path fs path with Ok source -> Ok source | Error _ -> Ok base_json)
    | _ -> (
        match read_path fs name with Ok _ as value -> value | Error _ -> Ok base_json)
  in
  let* source = source in
  match decode_json source with
  | Error message -> Error (Fmt.str "invalid configuration JSON: %s" message)
  | Ok value -> Ok (with_root value { default with config = name })

let apply_cli (config : t) (cli : cli) =
  let border : border =
    {
      radius = Option.value cli.border_radius ~default:config.border.radius;
      width = Option.value cli.border_width ~default:config.border.width;
      color = Option.value cli.border_color ~default:config.border.color;
    }
  in
  let shadow : shadow =
    {
      blur = Option.value cli.shadow_blur ~default:config.shadow.blur;
      x = Option.value cli.shadow_x ~default:config.shadow.x;
      y = Option.value cli.shadow_y ~default:config.shadow.y;
    }
  in
  let font : font =
    {
      family = Option.value cli.font_family ~default:config.font.family;
      file = Option.value cli.font_file ~default:config.font.file;
      size = Option.value cli.font_size ~default:config.font.size;
      ligatures = Option.value cli.font_ligatures ~default:config.font.ligatures;
    }
  in
  ({
     input = cli.input;
     background = Option.value cli.background ~default:config.background;
     margin = Option.value cli.margin ~default:config.margin;
     padding = Option.value cli.padding ~default:config.padding;
     window = Option.value cli.window ~default:config.window;
     width = Option.value cli.width ~default:config.width;
     height = Option.value cli.height ~default:config.height;
     config = cli.config;
     interactive = config.interactive || cli.interactive;
     language = Option.value cli.language ~default:config.language;
     theme = Option.value cli.theme ~default:config.theme;
     wrap = Option.value cli.wrap ~default:config.wrap;
     output = Option.value cli.output ~default:config.output;
     execute = Option.value cli.execute ~default:config.execute;
     execute_timeout = Option.value cli.execute_timeout ~default:config.execute_timeout;
     border;
     shadow;
     font;
     line_height = Option.value cli.line_height ~default:config.line_height;
     lines = Option.value cli.lines ~default:config.lines;
     show_line_numbers = config.show_line_numbers || cli.show_line_numbers;
   }
    : t)

let json_string s = Jsont.Json.string s
let json_float f = Jsont.Json.number f
let json_bool b = Jsont.Json.bool b
let json_array values = Jsont.Json.list values
let member name value = Jsont.Json.mem (Jsont.Json.name name) value

let encode_json (config : t) =
  let sides values = json_array (List.map json_float values) in
  let border_obj =
    Jsont.Json.object'
      [
        member "radius" (json_float config.border.radius);
        member "width" (json_float config.border.width);
        member "color" (json_string config.border.color);
      ]
  in
  let shadow_obj =
    Jsont.Json.object'
      [
        member "blur" (json_float config.shadow.blur);
        member "x" (json_float config.shadow.x);
        member "y" (json_float config.shadow.y);
      ]
  in
  let font_obj =
    Jsont.Json.object'
      [
        member "family" (json_string config.font.family);
        member "file" (json_string config.font.file);
        member "size" (json_float config.font.size);
        member "ligatures" (json_bool config.font.ligatures);
      ]
  in
  Jsont.Json.object'
    [
      member "background" (json_string config.background);
      member "margin" (sides config.margin);
      member "padding" (sides config.padding);
      member "window" (json_bool config.window);
      member "width" (json_float config.width);
      member "height" (json_float config.height);
      member "theme" (json_string config.theme);
      member "wrap" (Jsont.Json.int config.wrap);
      member "border" border_obj;
      member "shadow" shadow_obj;
      member "font" font_obj;
      member "line_height" (json_float config.line_height);
      member "show_line_numbers" (json_bool config.show_line_numbers);
    ]

let save_user ~fs config =
  let path = Filename.concat (Charm_cli.Xdg.config_dir ~app:"freeze") "user.json" in
  let directory = Filename.dirname path in
  let encoded =
    match
      Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json (encode_json config)
    with
    | Ok value -> value
    | Error message -> Fmt.failwith "cannot encode user configuration: %s" message
  in
  try
    Eio.Path.(
      mkdirs ~exists_ok:true ~perm:0o700 (fs / directory);
      save ~create:(`Or_truncate 0o600) (fs / path) encoded);
    Ok ()
  with
  | Eio.Io _ -> Error "cannot write freeze user configuration"
  | Unix.Unix_error (error, function_name, argument) ->
      Error
        (Fmt.str "cannot write freeze user configuration: %s (%s %s)"
           (Unix.error_message error) function_name argument)
