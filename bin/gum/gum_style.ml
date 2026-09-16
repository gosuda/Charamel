type t = {
  foreground : Charm_ansi.Color.t option;
  background : Charm_ansi.Color.t option;
  border : Charm_lipgloss.Border.t;
  border_foreground : Charm_ansi.Color.t option;
  border_background : Charm_ansi.Color.t option;
  align : Charm_lipgloss.Position.t;
  height : int;
  width : int;
  margin : Charm_lipgloss.Sides.t;
  padding : Charm_lipgloss.Sides.t;
  bold : bool;
  faint : bool;
  italic : bool;
  strikethrough : bool;
  underline : bool;
}

let invalid what value = invalid_arg (Fmt.str "invalid %s: %s" what value)

let color_default name value =
  match Gum_flag.color value with
  | Ok color -> color
  | Error (`Msg message) -> invalid name message

let border_default value =
  match Gum_flag.border value with
  | Some border -> border
  | None -> invalid "border" value

let align_default value =
  match Gum_flag.align value with
  | Some position -> position
  | None -> invalid "alignment" value

let sides_default name value =
  match Gum_flag.parse_padding value with
  | Ok sides -> sides
  | Error (`Msg message) -> invalid name message

let neutral =
  {
    foreground = None;
    background = None;
    border = Charm_lipgloss.Border.none;
    border_foreground = None;
    border_background = None;
    align = Charm_lipgloss.Position.left;
    height = 0;
    width = 0;
    margin = Charm_lipgloss.Sides.all 0;
    padding = Charm_lipgloss.Sides.all 0;
    bold = false;
    faint = false;
    italic = false;
    strikethrough = false;
    underline = false;
  }

let empty = neutral

let defaults ?foreground ?background ?border ?border_foreground ?border_background ?align
    ?height ?width ?margin ?padding ?bold ?faint ?italic ?strikethrough ?underline () =
  let value option ~default = Option.value option ~default in
  {
    foreground = color_default "foreground" (value foreground ~default:"");
    background = color_default "background" (value background ~default:"");
    border = border_default (value border ~default:"none");
    border_foreground =
      color_default "border foreground" (value border_foreground ~default:"");
    border_background =
      color_default "border background" (value border_background ~default:"");
    align = align_default (value align ~default:"left");
    height = value height ~default:0;
    width = value width ~default:0;
    margin = sides_default "margin" (value margin ~default:"0 0");
    padding = sides_default "padding" (value padding ~default:"0 0");
    bold = value bold ~default:false;
    faint = value faint ~default:false;
    italic = value italic ~default:false;
    strikethrough = value strikethrough ~default:false;
    underline = value underline ~default:false;
  }

let field_name prefix field =
  if prefix = "" then field
  else if String.ends_with ~suffix:"." prefix then prefix ^ field
  else prefix ^ "." ^ field

let field_info ~cmd ~prefix ~env_prefix ~docs field doc =
  let option_name = field_name prefix field in
  let environment_name = field_name env_prefix field in
  Cmdliner.Arg.info [ option_name ] ~doc ~docs ~env:(Gum_flag.env ~cmd environment_name)

let environment_prefix prefix =
  let prefix =
    if String.ends_with ~suffix:"." prefix then
      String.sub prefix 0 (String.length prefix - 1)
    else prefix
  in
  match prefix with
  | "selected-indicator" -> "selected-prefix"
  | "match-highlight" -> "match-high"
  | _ -> prefix

let color_conv =
  Cmdliner.Arg.Conv.make ~docv:"COLOR"
    ~parser:(fun value ->
      match Gum_flag.color value with
      | Ok color -> Ok color
      | Error (`Msg message) -> Error message)
    ~pp:(fun ppf _ -> Fmt.string ppf "<color>")
    ()

let border_conv =
  let choices =
    [
      ("none", Charm_lipgloss.Border.none);
      ("hidden", Charm_lipgloss.Border.hidden);
      ("normal", Charm_lipgloss.Border.normal);
      ("rounded", Charm_lipgloss.Border.rounded);
      ("thick", Charm_lipgloss.Border.thick);
      ("double", Charm_lipgloss.Border.double);
    ]
  in
  Gum_flag.enum ~docv:"BORDER" choices

let align_conv =
  Cmdliner.Arg.Conv.make ~docv:"ALIGN"
    ~parser:(fun value ->
      match Gum_flag.align value with
      | Some position -> Ok position
      | None -> Error (Fmt.str "invalid alignment: %s" value))
    ~pp:(fun ppf _ -> Fmt.string ppf "<align>")
    ()

let sides_conv name =
  Cmdliner.Arg.Conv.make ~docv:(String.uppercase_ascii name)
    ~parser:(fun value ->
      match Gum_flag.parse_padding value with
      | Ok sides -> Ok sides
      | Error (`Msg message) -> Error message)
    ~pp:(fun ppf _ -> Fmt.string ppf "<padding>")
    ()

let term ~cmd ?(hidden = true) ?(prefix = "") ?env_prefix:custom_env_prefix ~defaults () =
  let env_prefix = Option.value custom_env_prefix ~default:(environment_prefix prefix) in
  let docs = if hidden then "STYLE FLAGS" else Cmdliner.Manpage.s_options in
  let open Cmdliner in
  let info field doc = field_info ~cmd ~prefix ~env_prefix ~docs field doc in
  let color field value =
    Arg.value (Arg.opt color_conv value (info field "Color value."))
  in
  let foreground = color "foreground" defaults.foreground in
  let background = color "background" defaults.background in
  let border =
    Arg.value (Arg.opt border_conv defaults.border (info "border" "Border shape."))
  in
  let border_foreground = color "border-foreground" defaults.border_foreground in
  let border_background = color "border-background" defaults.border_background in
  let align = Arg.value (Arg.opt align_conv defaults.align (info "align" "Alignment.")) in
  let height =
    Arg.value (Arg.opt Arg.int defaults.height (info "height" "Minimum height."))
  in
  let width =
    Arg.value (Arg.opt Arg.int defaults.width (info "width" "Minimum width."))
  in
  let margin =
    Arg.value
      (Arg.opt (sides_conv "margin") defaults.margin (info "margin" "Margin dimensions."))
  in
  let padding =
    Arg.value
      (Arg.opt (sides_conv "padding") defaults.padding
         (info "padding" "Padding dimensions."))
  in
  let bool field value doc =
    Gum_flag.negatable ~cmd ~env_name:(field_name env_prefix field) ~default:value ~doc
      (field_name prefix field)
  in
  let bold = bool "bold" defaults.bold "Bold text." in
  let faint = bool "faint" defaults.faint "Faint text." in
  let italic = bool "italic" defaults.italic "Italic text." in
  let strikethrough = bool "strikethrough" defaults.strikethrough "Strikethrough text." in
  let underline = bool "underline" defaults.underline "Underlined text." in
  let open Term.Syntax in
  let+ foreground = foreground
  and+ background = background
  and+ border = border
  and+ border_foreground = border_foreground
  and+ border_background = border_background
  and+ align = align
  and+ height = height
  and+ width = width
  and+ margin = margin
  and+ padding = padding
  and+ bold = bold
  and+ faint = faint
  and+ italic = italic
  and+ strikethrough = strikethrough
  and+ underline = underline in
  {
    foreground;
    background;
    border;
    border_foreground;
    border_background;
    align;
    height;
    width;
    margin;
    padding;
    bold;
    faint;
    italic;
    strikethrough;
    underline;
  }

let to_style t =
  let style = Charm_lipgloss.Style.empty in
  let style =
    match t.background with
    | Some color -> Charm_lipgloss.Style.background color style
    | None -> style
  in
  let style =
    match t.foreground with
    | Some color -> Charm_lipgloss.Style.foreground color style
    | None -> style
  in
  let style =
    match t.border_background with
    | Some color ->
        Charm_lipgloss.Style.border_background
          (Charm_lipgloss.Sides_color.all color)
          style
    | None -> style
  in
  let style =
    match t.border_foreground with
    | Some color ->
        Charm_lipgloss.Style.border_foreground
          (Charm_lipgloss.Sides_color.all color)
          style
    | None -> style
  in
  let style = Charm_lipgloss.Style.align t.align style in
  let style = Charm_lipgloss.Style.border t.border style in
  let style =
    if t.height > 0 then Charm_lipgloss.Style.height t.height style else style
  in
  let style = if t.width > 0 then Charm_lipgloss.Style.width t.width style else style in
  let style = Charm_lipgloss.Style.margin t.margin style in
  let style = Charm_lipgloss.Style.padding t.padding style in
  let style = Charm_lipgloss.Style.bold t.bold style in
  let style = Charm_lipgloss.Style.faint t.faint style in
  let style = Charm_lipgloss.Style.italic t.italic style in
  let style = Charm_lipgloss.Style.strikethrough t.strikethrough style in
  Charm_lipgloss.Style.underline t.underline style

let inline t = Charm_lipgloss.Style.inline true (to_style t)
let foreground t = t.foreground
