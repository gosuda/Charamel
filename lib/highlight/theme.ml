type t = Spec.kind -> Charm_lipgloss.Style.t

type palette = {
  keyword : string option;
  typ : string option;
  builtin : string option;
  constant : string option;
  string_ : string option;
  number : string option;
  comment : string option;
  operator : string option;
  punct : string option;
  ident : string option;
  attribute : string option;
  text : string option;
}

let style color =
  match Option.bind color Charm_lipgloss.Color.of_hex with
  | None -> Charm_lipgloss.Style.empty
  | Some color -> Charm_lipgloss.Style.foreground color Charm_lipgloss.Style.empty

let apply palette = function
  | Spec.Keyword -> style palette.keyword
  | Spec.Type -> style palette.typ
  | Spec.Builtin -> style palette.builtin
  | Spec.Constant -> style palette.constant
  | Spec.String -> style palette.string_
  | Spec.Number -> style palette.number
  | Spec.Comment -> style palette.comment
  | Spec.Operator -> style palette.operator
  | Spec.Punct -> style palette.punct
  | Spec.Ident -> style palette.ident
  | Spec.Attribute -> style palette.attribute
  | Spec.Text -> style palette.text

(* Charm values are transcribed from Glamour's MIT Chroma style tables. *)
let charm_dark =
  {
    keyword = Some "#00AAFF";
    typ = Some "#6E6ED8";
    builtin = Some "#FF8EC7";
    constant = Some "#C4C4C4";
    string_ = Some "#C69669";
    number = Some "#6EEFC0";
    comment = Some "#676767";
    operator = Some "#EF8080";
    punct = Some "#E8E8A8";
    ident = Some "#C4C4C4";
    attribute = Some "#7A7AE6";
    text = Some "#C4C4C4";
  }

let charm_light =
  {
    keyword = Some "#279EFC";
    typ = Some "#7049C2";
    builtin = Some "#0A1BB1";
    constant = Some "#581290";
    string_ = Some "#7E5B38";
    number = Some "#22CCAE";
    comment = Some "#8D8D8D";
    operator = Some "#FF2626";
    punct = Some "#FA7878";
    ident = Some "#2A2A2A";
    attribute = Some "#8362CB";
    text = Some "#2A2A2A";
  }

let dracula_palette =
  {
    keyword = Some "#FF79C6";
    typ = Some "#8BE9FD";
    builtin = Some "#8BE9FD";
    constant = Some "#BD93F9";
    string_ = Some "#F1FA8C";
    number = Some "#6EEFC0";
    comment = Some "#6272A4";
    operator = Some "#FF79C6";
    punct = Some "#F8F8F2";
    ident = Some "#8BE9FD";
    attribute = Some "#50FA7B";
    text = Some "#F8F8F2";
  }

let github_palette =
  {
    keyword = Some "#CF222E";
    typ = Some "#CF222E";
    builtin = Some "#6639BA";
    constant = Some "#0550AE";
    string_ = Some "#0A3069";
    number = Some "#0550AE";
    comment = Some "#57606A";
    operator = Some "#0550AE";
    punct = Some "#1F2328";
    ident = Some "#1F2328";
    attribute = Some "#0550AE";
    text = Some "#1F2328";
  }

let github_dark_palette =
  {
    keyword = Some "#FF7B72";
    typ = Some "#79C0FF";
    builtin = Some "#D2A8FF";
    constant = Some "#79C0FF";
    string_ = Some "#A5D6FF";
    number = Some "#79C0FF";
    comment = Some "#8B949E";
    operator = Some "#FF7B72";
    punct = Some "#E6EDF3";
    ident = Some "#E6EDF3";
    attribute = Some "#F0883E";
    text = Some "#E6EDF3";
  }

let monokai_palette =
  {
    keyword = Some "#66D9EF";
    typ = Some "#66D9EF";
    builtin = Some "#A6E22E";
    constant = Some "#66D9EF";
    string_ = Some "#E6DB74";
    number = Some "#AE81FF";
    comment = Some "#75715E";
    operator = Some "#F92672";
    punct = Some "#F8F8F2";
    ident = Some "#F8F8F2";
    attribute = Some "#A6E22E";
    text = Some "#F8F8F2";
  }

let nord_palette =
  {
    keyword = Some "#81A1C1";
    typ = Some "#81A1C1";
    builtin = Some "#81A1C1";
    constant = Some "#8FBCBB";
    string_ = Some "#A3BE8C";
    number = Some "#B48EAD";
    comment = Some "#616E87";
    operator = Some "#81A1C1";
    punct = Some "#ECEFF4";
    ident = Some "#D8DEE9";
    attribute = Some "#8FBCBB";
    text = Some "#D8DEE9";
  }

let solarized_dark_palette =
  {
    keyword = Some "#719E07";
    typ = Some "#DC322F";
    builtin = Some "#B58900";
    constant = Some "#CB4B16";
    string_ = Some "#2AA198";
    number = Some "#2AA198";
    comment = Some "#586E75";
    operator = Some "#719E07";
    punct = Some "#268BD2";
    ident = Some "#268BD2";
    attribute = Some "#93A1A1";
    text = Some "#93A1A1";
  }

let solarized_light_palette =
  {
    keyword = Some "#859900";
    typ = Some "#859900";
    builtin = Some "#268BD2";
    constant = Some "#CB4B16";
    string_ = Some "#2AA198";
    number = Some "#2AA198";
    comment = Some "#93A1A1";
    operator = Some "#859900";
    punct = Some "#586E75";
    ident = Some "#268BD2";
    attribute = Some "#268BD2";
    text = Some "#586E75";
  }

let charm ~is_dark = apply (if is_dark then charm_dark else charm_light)
let dracula = apply dracula_palette
let github ~is_dark = apply (if is_dark then github_dark_palette else github_palette)
let monokai = apply monokai_palette
let nord = apply nord_palette

let solarized ~is_dark =
  apply (if is_dark then solarized_dark_palette else solarized_light_palette)
