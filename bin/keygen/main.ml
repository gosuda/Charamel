module Key = Charamel_ssh_keygen
module Keygen = Keygen_core.Keygen
module Env = Charamel_cli.Env
open Lwt.Infix

let run env algorithm path comment force =
  let path_result =
    if String.equal path "" then Keygen.default_path algorithm else Ok path
  in
  match path_result with
  | Error error -> Charamel_cli.error (Fmt.str "%a" Keygen.pp_error error)
  | Ok path -> (
      Keygen.generate ~fs_root:env.Env.fs_root ~path ~algorithm ~comment ~force ()
      >>= function
      | Error error -> Charamel_cli.error (Fmt.str "%a" Keygen.pp_error error)
      | Ok fingerprint -> Lwt_io.write env.Env.stdout (fingerprint ^ "\n"))

let algorithm_arg () =
  let open Cmdliner in
  Arg.(
    value
      (opt
         (enum
            [
              ("ed25519", Key.Ed25519);
              ("ecdsa-p256", Key.Ecdsa_p256);
              ("ecdsa-p384", Key.Ecdsa_p384);
              ("ecdsa-p521", Key.Ecdsa_p521);
            ])
         Key.Ed25519
         (info [ "t"; "type" ] ~docv:"ALGORITHM"
            ~doc:"Key algorithm. The default is ed25519.")))

let path_arg () =
  let open Cmdliner in
  Arg.(
    value
      (opt string ""
         (info [ "f"; "file" ] ~docv:"PATH"
            ~doc:"Private-key path. The default is ~/.ssh/id_<algorithm>.")))

let comment_arg () =
  let open Cmdliner in
  Arg.(
    value (opt string "" (info [ "C"; "comment" ] ~docv:"COMMENT" ~doc:"Key comment.")))

let force_arg () =
  let open Cmdliner in
  Arg.(value (flag (info [ "force" ] ~doc:"Replace existing key files.")))

let term env =
  let open Cmdliner.Term.Syntax in
  let+ algorithm = algorithm_arg ()
  and+ path = path_arg ()
  and+ comment = comment_arg ()
  and+ force = force_arg () in
  run env algorithm path comment force

let () =
  Mirage_crypto_rng_unix.use_default ();
  Charamel_cli.run ~name:"keygen" ~version:Charamel_cli.Version.current
    ~doc:"Generate an OpenSSH key pair."
    ~default:(fun env -> term env)
    []
