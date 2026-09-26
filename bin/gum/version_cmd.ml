let check ~current constraint_text =
  match Semver.parse current with
  | Error (`Msg message) ->
      Error (`Msg (Fmt.str "could not parse version %S: %s" current message))
  | Ok version -> (
      match Semver.parse_constraint constraint_text with
      | Error (`Msg message) ->
          Error (`Msg (Fmt.str "could not parse range %S: %s" constraint_text message))
      | Ok constraint_ ->
          if Semver.satisfies constraint_ version then Ok ()
          else
            Error
              (`Msg
                 (Fmt.str "gum version %S is not within given range %S" current
                    constraint_text)))

let display ~current constraint_ =
  match constraint_ with
  | None -> Ok current
  | Some constraint_text -> Result.map (fun () -> "") (check ~current constraint_text)

let cmd env =
  let open Cmdliner in
  let constraint_ =
    Arg.value
      (Arg.pos 0 (Arg.some Arg.string) None
         (Arg.info [] ~docv:"CONSTRAINT" ~doc:"Semantic version constraint."))
  in
  let term =
    let open Term.Syntax in
    let+ constraint_ = constraint_ in
    match display ~current:Charamel_cli.Version.current constraint_ with
    | Ok output when output <> "" -> Gum_io.println env output
    | Ok _ -> Lwt.return_unit
    | Error (`Msg message) -> Charamel_cli.error message
  in
  Cmd.v (Cmd.info "version" ~doc:"Print or check the gum version.") term
