type kind = Markdown | Template | Code | Emoji
type error = [ `Msg of string ]

let theme_value ~env name =
  match String.lowercase_ascii name with
  | "dark" -> Ok Charm_glamour.Theme.dark
  | "light" -> Ok Charm_glamour.Theme.light
  | "dracula" -> Ok Charm_glamour.Theme.dracula
  | "tokyo-night" -> Ok Charm_glamour.Theme.tokyo_night
  | "pink" -> Ok Charm_glamour.Theme.pink
  | "ascii" -> Ok Charm_glamour.Theme.ascii
  | "notty" -> Ok Charm_glamour.Theme.notty
  | "auto" -> Ok (Charm_glamour.Theme.auto ~is_dark:(Charm_cli.is_dark ~env))
  | value -> Error (`Msg (Fmt.str "unknown theme: %s" value))

let render ?(theme = "pink") ?(language = "") ?(strip_ansi = false) kind input =
  let input = if strip_ansi then Charm_ansi.Text.strip input else input in
  match kind with
  | Template -> Template.render input
  | Markdown -> (
      match theme_value ~env:Sys.getenv_opt theme with
      | Error _ as error -> error
      | Ok theme -> Ok (Charm_glamour.render ~width:0 ~theme input))
  | Code -> (
      match theme_value ~env:Sys.getenv_opt theme with
      | Error _ as error -> error
      | Ok theme ->
          let fenced = "```" ^ language ^ "\n" ^ input ^ "\n```" in
          Ok (Charm_glamour.render ~width:0 ~theme fenced))
  | Emoji -> Ok (Charm_glamour.render ~width:0 ~emoji:true input)

let read_input env ~strip_ansi texts =
  match texts with
  | _ :: _ -> String.concat "\n" texts
  | [] -> (
      match Gum_io.read_stdin ~strip_ansi env with
      | Ok text -> text
      | Error `Empty -> ""
      | Error (`Read message) -> Charm_cli.error message)

let kind_conv =
  Gum_flag.enum ~docv:"TYPE"
    [ ("markdown", Markdown); ("template", Template); ("code", Code); ("emoji", Emoji) ]

let theme_conv =
  Gum_flag.enum ~docv:"THEME"
    [
      ("dark", "dark");
      ("light", "light");
      ("dracula", "dracula");
      ("tokyo-night", "tokyo-night");
      ("pink", "pink");
      ("ascii", "ascii");
      ("notty", "notty");
      ("auto", "auto");
    ]

let command_info name doc = Cmdliner.Cmd.info name ~doc

let cmd env =
  let open Cmdliner in
  let kind =
    Arg.value
      (Arg.opt kind_conv Markdown
         (Arg.info [ "type"; "t" ] ~doc:"Formatting mode."
            ~env:(Gum_flag.env ~cmd:"format" "type")))
  in
  let theme =
    Arg.value
      (Arg.opt theme_conv "pink"
         (Arg.info [ "theme" ] ~doc:"Glamour theme."
            ~env:(Gum_flag.env ~cmd:"format" "theme")))
  in
  let language =
    Arg.value
      (Arg.opt Arg.string ""
         (Arg.info [ "language"; "l" ] ~doc:"Programming language."
            ~env:(Gum_flag.env ~cmd:"format" "language")))
  in
  let strip_ansi =
    Gum_flag.negatable ~cmd:"format" ~default:true
      ~doc:"Strip ANSI sequences read from standard input." "strip-ansi"
  in
  let texts =
    Arg.(value (pos_all string [] (info [] ~docv:"TEXT" ~doc:"Text to format.")))
  in
  let term =
    let open Term.Syntax in
    let+ kind = kind
    and+ theme = theme
    and+ language = language
    and+ strip_ansi = strip_ansi
    and+ texts = texts in
    let input = read_input env ~strip_ansi texts in
    match render ~theme ~language kind input with
    | Ok output -> Gum_io.println env output
    | Error (`Msg message) ->
        let message =
          match kind with
          | Template -> Fmt.str "unable to parse template: %s" message
          | _ -> Fmt.str "unable to render: %s" message
        in
        Charm_cli.error message
  in
  Cmd.v
    (command_info "format" "Format text as markdown, code, emoji, or a template.")
    term
