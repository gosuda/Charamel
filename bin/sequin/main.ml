type mode = Explain | Raw | Width

let ( / ) = Eio.Path.( / )

let read_all source =
  let contents = Buffer.create 4096 in
  Eio.Flow.copy source (Eio.Flow.buffer_sink contents);
  Buffer.contents contents

let read_input env = function
  | None -> read_all env#stdin
  | Some "-" -> read_all env#stdin
  | Some path -> (
      (* [env#fs] handles both relative and absolute [path]s and reports real
         kernel errors, unlike [env#cwd]'s sandboxed resolution, which treats
         any absolute argument as escaping the sandbox and refuses it with
         [Permission_denied] before the filesystem is even consulted. *)
      try Eio.Path.load (env#fs / path) with
      | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
          Charm_cli.error (Fmt.str "sequin: %s: no such file or directory" path)
      | Eio.Io (Eio.Fs.E (Eio.Fs.Permission_denied _), _) ->
          Charm_cli.error (Fmt.str "sequin: %s: permission denied" path)
      | Eio.Io _ as exn ->
          Charm_cli.error
            (Fmt.str "sequin: cannot read %s: %s" path (Printexc.to_string exn)))

let render mode input =
  match mode with
  | Explain -> Sequin_core.Explain.explain input
  | Raw -> String.escaped input
  | Width -> Fmt.str "%d\n" (Charm_ansi.Text.width input)

let run env file raw width =
  let mode =
    if raw && width then
      Charm_cli.error ~code:2 "sequin: --raw and --width are mutually exclusive"
    else if raw then Raw
    else if width then Width
    else Explain
  in
  let input = read_input env file in
  Eio.Flow.copy_string (render mode input) env#stdout

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
  Charm_cli.run ~name:"sequin" ~version:Charm_cli.Version.current
    ~doc:"Inspect ANSI terminal escape sequences." ~default []
