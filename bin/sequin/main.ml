type mode = Explain | Raw | Width

module Env = Charamel_cli.Env
open Lwt.Syntax

let read_input env = function
  | None | Some "-" -> Lwt_io.read env.Env.stdin
  | Some path ->
      Lwt.catch
        (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.input path (fun channel -> Lwt_io.read channel))
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) ->
              Charamel_cli.error (Fmt.str "sequin: %s: no such file or directory" path)
          | Unix.Unix_error ((Unix.EACCES | Unix.EPERM), _, _) ->
              Charamel_cli.error (Fmt.str "sequin: %s: permission denied" path)
          | exn ->
              Charamel_cli.error
                (Fmt.str "sequin: cannot read %s: %s" path (Printexc.to_string exn)))

let render mode input =
  match mode with
  | Explain -> Sequin_core.Explain.explain input
  | Raw -> String.escaped input
  | Width -> Fmt.str "%d\n" (Charamel_ansi.Text.width input)

let run env file raw width =
  let mode =
    if raw && width then
      Charamel_cli.error ~code:2 "sequin: --raw and --width are mutually exclusive"
    else if raw then Raw
    else if width then Width
    else Explain
  in
  let* input = read_input env file in
  Lwt_io.write env.Env.stdout (render mode input)

let file_arg =
  let doc =
    "Read ANSI bytes from FILE instead of standard input; use - for standard input."
  in
  Cmdliner.Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let raw_arg =
  let doc = "Print the input as escaped bytes." in
  Cmdliner.Arg.(value & flag & info [ "raw"; "r" ] ~doc)

let width_arg =
  let doc = "Print the display width of the input as one integer." in
  Cmdliner.Arg.(value & flag & info [ "width" ] ~doc)

let default env =
  let action = run env in
  Cmdliner.Term.(const action $ file_arg $ raw_arg $ width_arg)

let () =
  Charamel_cli.run ~name:"sequin" ~version:Charamel_cli.Version.current
    ~doc:"Inspect ANSI terminal escape sequences." ~default []
