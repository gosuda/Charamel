module Name = Hotdiva_core.Name

let count_arg =
  Cmdliner.Arg.(
    value
      (opt int 1
         (info [ "n" ] ~docv:"COUNT" ~doc:"Number of names to generate (default: 1).")))

let separator_arg =
  Cmdliner.Arg.(
    value
      (opt string "-"
         (info [ "separator" ] ~docv:"SEPARATOR"
            ~doc:"Separator between words (default: -).")))

let tokens_arg =
  Cmdliner.Arg.(
    value
      (opt int 2
         (info [ "tokens" ] ~docv:"TOKENS"
            ~doc:"Number of words in each name (default: 2).")))

let write_line env name = Eio.Flow.copy_string (name ^ "\n") env#stdout

let generate env count separator tokens =
  let names =
    try Name.generate_many ~random:Mirage_crypto_rng.generate ~count ~separator ~tokens ()
    with Invalid_argument message -> Charm_cli.error message
  in
  try List.iter (write_line env) names with Unix.Unix_error (Unix.EPIPE, _, _) -> ()

let default env =
  let action = generate env in
  Cmdliner.Term.(const action $ count_arg $ separator_arg $ tokens_arg)

let run () =
  Mirage_crypto_rng_unix.use_default ();
  Charm_cli.run ~name:"hotdiva2000" ~version:Charm_cli.Version.current
    ~doc:"Generate memorable random names from the command line." ~default []

let () = run ()
