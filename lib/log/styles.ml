type t = {
  timestamp : Charamel_lipgloss.Style.t;
  caller : Charamel_lipgloss.Style.t;
  prefix : Charamel_lipgloss.Style.t;
  message : Charamel_lipgloss.Style.t;
  key : Charamel_lipgloss.Style.t;
  value : Charamel_lipgloss.Style.t;
  separator : Charamel_lipgloss.Style.t;
  levels : Logs.level -> Charamel_lipgloss.Style.t;
}

let level_style color =
  Charamel_lipgloss.Style.empty
  |> Charamel_lipgloss.Style.bold true
  |> Charamel_lipgloss.Style.width 5
  |> Charamel_lipgloss.Style.foreground (Charamel_lipgloss.Color.Indexed color)

let default =
  let debug = level_style 63 in
  let info = level_style 86 in
  let warning = level_style 192 in
  let error = level_style 204 in
  {
    timestamp = Charamel_lipgloss.Style.empty;
    caller = Charamel_lipgloss.Style.empty |> Charamel_lipgloss.Style.faint true;
    prefix =
      Charamel_lipgloss.Style.empty
      |> Charamel_lipgloss.Style.bold true
      |> Charamel_lipgloss.Style.faint true;
    message = Charamel_lipgloss.Style.empty;
    key = Charamel_lipgloss.Style.empty |> Charamel_lipgloss.Style.faint true;
    value = Charamel_lipgloss.Style.empty;
    separator = Charamel_lipgloss.Style.empty |> Charamel_lipgloss.Style.faint true;
    levels =
      (function
      | Logs.Debug -> debug
      | Logs.Info | Logs.App -> info
      | Logs.Warning -> warning
      | Logs.Error -> error);
  }
