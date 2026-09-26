let trim_lines text =
  text |> String.split_on_char '\n' |> List.map String.trim |> String.concat "\n"

let render style ~trim text =
  let text = if trim then trim_lines text else text in
  Charamel_lipgloss.Style.render (Gum_style.to_style style) text

let read_input env ~strip_ansi texts =
  match texts with
  | _ :: _ -> Lwt.return (Ok (String.concat "\n" texts))
  | [] ->
      Lwt.map
        (function
          | Ok text -> Ok text
          | Error `Empty -> Error (`Msg "no input provided, see `gum style --help`")
          | Error (`Read message) -> Error (`Msg message))
        (Gum_io.read_stdin ~strip_ansi env)

let cmd env =
  let open Cmdliner in
  let trim =
    Gum_flag.flag ~cmd:"style" ~doc:"Trim whitespace on every input line." "trim"
  in
  let strip_ansi =
    Gum_flag.negatable ~cmd:"style" ~default:true
      ~doc:"Strip ANSI sequences when reading standard input." "strip-ansi"
  in
  let style = Gum_style.term ~cmd:"style" ~hidden:false ~defaults:Gum_style.empty () in
  let texts =
    Arg.(value (pos_all string [] (info [] ~docv:"TEXT" ~doc:"Text to style.")))
  in
  let term =
    let open Term.Syntax in
    let+ trim = trim and+ strip_ansi = strip_ansi and+ style = style and+ texts = texts in
    Lwt.bind (read_input env ~strip_ansi texts) (function
      | Error (`Msg message) -> Charamel_cli.error message
      | Ok text when text = "" ->
          Charamel_cli.error "no input provided, see `gum style --help`"
      | Ok text -> Gum_io.println env (render style ~trim text))
  in
  Cmd.v (Cmd.info "style" ~doc:"Apply terminal styles to text.") term
