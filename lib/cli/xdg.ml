let qualifies p = p <> "" && not (Filename.is_relative p)

(* XDG Base Directory specification: a variable that does not qualify is
   ignored in favour of the $HOME fallback, and an environment without a
   usable $HOME is a broken environment, not a directory to fabricate. *)
let base_dir ~var ~fallback =
  match Sys.getenv_opt var with
  | Some dir when qualifies dir -> dir
  | _ -> (
      match Sys.getenv_opt "HOME" with
      | Some home when qualifies home -> Filename.concat home fallback
      | _ ->
          invalid_arg
            ("Xdg: neither $" ^ var ^ " nor $HOME is set to a nonempty absolute directory")
      )

let app_dir ~var ~fallback ~app = Filename.concat (base_dir ~var ~fallback) app
let config_dir ~app = app_dir ~var:"XDG_CONFIG_HOME" ~fallback:".config" ~app
let data_dir ~app = app_dir ~var:"XDG_DATA_HOME" ~fallback:".local/share" ~app
let state_dir ~app = app_dir ~var:"XDG_STATE_HOME" ~fallback:".local/state" ~app
let cache_dir ~app = app_dir ~var:"XDG_CACHE_HOME" ~fallback:".cache" ~app
