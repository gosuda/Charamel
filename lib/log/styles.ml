type t = {
  timestamp : Charm_lipgloss.Style.t;
  caller : Charm_lipgloss.Style.t;
  prefix : Charm_lipgloss.Style.t;
  message : Charm_lipgloss.Style.t;
  key : Charm_lipgloss.Style.t;
  value : Charm_lipgloss.Style.t;
  separator : Charm_lipgloss.Style.t;
  levels : Logs.level -> Charm_lipgloss.Style.t;
}

let level_style color =
  Charm_lipgloss.Style.empty
  |> Charm_lipgloss.Style.bold true
  |> Charm_lipgloss.Style.width 5
  |> Charm_lipgloss.Style.foreground (Charm_lipgloss.Color.Indexed color)

let default =
  let debug = level_style 63 in
  let info = level_style 86 in
  let warning = level_style 192 in
  let error = level_style 204 in
  {
    timestamp = Charm_lipgloss.Style.empty;
    caller = Charm_lipgloss.Style.empty |> Charm_lipgloss.Style.faint true;
    prefix =
      Charm_lipgloss.Style.empty
      |> Charm_lipgloss.Style.bold true
      |> Charm_lipgloss.Style.faint true;
    message = Charm_lipgloss.Style.empty;
    key = Charm_lipgloss.Style.empty |> Charm_lipgloss.Style.faint true;
    value = Charm_lipgloss.Style.empty;
    separator = Charm_lipgloss.Style.empty |> Charm_lipgloss.Style.faint true;
    levels =
      (function
      | Logs.Debug -> debug
      | Logs.Info | Logs.App -> info
      | Logs.Warning -> warning
      | Logs.Error -> error);
  }
