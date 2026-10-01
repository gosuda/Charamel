let errorf fmt = Fmt.str fmt

let join ?(align = "left") ?horizontal:(_ = false) ?(vertical = false) texts =
  match texts with
  | [] -> Error (`Msg "no text provided")
  | _ -> (
      match Gum_flag.align align with
      | None -> Error (`Msg (errorf "invalid alignment: %s" align))
      | Some position ->
          Ok
            (if vertical then Charamel_lipgloss.Layout.join_vertical ~pos:position texts
             else Charamel_lipgloss.Layout.join_horizontal ~pos:position texts))

let cmd env =
  let open Cmdliner in
  let align =
    Arg.value
      (Arg.opt
         (Gum_flag.enum ~docv:"ALIGN"
            [
              ("left", "left");
              ("center", "center");
              ("right", "right");
              ("bottom", "bottom");
              ("middle", "middle");
              ("top", "top");
            ])
         "left"
         (Arg.info [ "align" ] ~doc:"Alignment of joined blocks."
            ~env:(Gum_flag.env ~cmd:"join" "align")
            ~docv:"ALIGN"))
  in
  let horizontal =
    Gum_flag.flag ~cmd:"join" ~doc:"Join blocks horizontally." "horizontal"
  in
  let vertical = Gum_flag.flag ~cmd:"join" ~doc:"Join blocks vertically." "vertical" in
  let texts =
    Arg.non_empty
      (Arg.pos_all Arg.string [] (Arg.info [] ~docv:"TEXT" ~doc:"Text blocks to join."))
  in
  let term =
    let open Term.Syntax in
    let+ align = align
    and+ horizontal = horizontal
    and+ vertical = vertical
    and+ texts = texts in
    match join ~align ~horizontal ~vertical texts with
    | Ok output -> Gum_io.println env output
    | Error (`Msg message) -> Charamel_cli.error message
  in
  Cmd.v (Cmd.info "join" ~doc:"Join multi-line text blocks.") term
