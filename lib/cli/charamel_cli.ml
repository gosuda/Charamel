open Lwt.Syntax
module Env = Env
module Xdg = Xdg
module Version = Version

exception Controlled_exit of int * string option

let check_code code =
  if code < 0 || code > 255 then
    invalid_arg (Fmt.str "Charamel_cli: exit code %d is outside 0..255" code);
  code

let error ?(code = 1) message = raise (Controlled_exit (check_code code, Some message))
let exit code = raise (Controlled_exit (check_code code, None))

let colorfgbg_color value =
  match List.rev (String.split_on_char ';' value) with
  | field :: _ -> (
      match int_of_string_opt (String.trim field) with
      | Some index -> Charamel_ansi.Color.indexed index
      | None -> None)
  | [] -> None

let is_dark ~env =
  match Option.bind (env "COLORFGBG") colorfgbg_color with
  | Some color -> Charamel_ansi.Color.is_dark color
  | None -> true

let verbosity_args argv =
  let rec loop index kept verbose quiet passthrough =
    if index = Array.length argv then (Array.of_list (List.rev kept), verbose, quiet)
    else
      let argument = argv.(index) in
      if passthrough then loop (index + 1) (argument :: kept) verbose quiet true
      else if argument = "--" then loop (index + 1) (argument :: kept) verbose quiet true
      else if argument = "-v" || argument = "--verbose" then
        loop (index + 1) kept (verbose + 1) quiet false
      else if argument = "-q" || argument = "--quiet" then
        loop (index + 1) kept verbose (quiet + 1) false
      else if
        String.length argument > 2
        && argument.[0] = '-'
        && argument.[1] <> '-'
        && String.for_all
             (fun c -> c = 'v' || c = 'q')
             (String.sub argument 1 (String.length argument - 1))
      then
        let v, q =
          String.fold_left
            (fun (v, q) c -> if c = 'v' then (v + 1, q) else (v, q + 1))
            (verbose, quiet)
            (String.sub argument 1 (String.length argument - 1))
        in
        loop (index + 1) kept v q false
      else loop (index + 1) (argument :: kept) verbose quiet false
  in
  loop 1 [ argv.(0) ] 0 0 false

let log_level ~verbose ~quiet =
  if quiet > 0 then
    Some (if quiet = 1 then Logs.Warning else if quiet = 2 then Logs.Error else Logs.App)
  else if verbose > 0 then Some Logs.Debug
  else Some Logs.Warning

let is_tty channel =
  match Unix.isatty channel with
  | flag -> flag
  | exception Unix.Unix_error (_, _, _) -> false

let path_for ~cwd path =
  if Filename.is_relative path then Filename.concat cwd path else path

let diagnostic ppf ~profile message =
  let styled =
    match profile with
    | Charamel_colorprofile.No_tty | Charamel_colorprofile.Ascii -> false
    | Charamel_colorprofile.Ansi | Charamel_colorprofile.Ansi256
    | Charamel_colorprofile.True_color ->
        true
  in
  if styled then Fmt.pf ppf "\027[1;31mERROR\027[0m %s@.Try --help for usage.@." message
  else Fmt.pf ppf "ERROR: %s@.Try --help for usage.@." message

let exits =
  [
    Cmdliner.Cmd.Exit.info 0 ~doc:"The command completed successfully.";
    Cmdliner.Cmd.Exit.info 1 ~doc:"The command reported an error.";
    Cmdliner.Cmd.Exit.info 2 ~doc:"The command line was invalid.";
    Cmdliner.Cmd.Exit.info 124 ~doc:"The command timed out.";
    Cmdliner.Cmd.Exit.info 130 ~doc:"The command was interrupted.";
  ]

let write_buffer channel buffer =
  let text = Buffer.contents buffer in
  if text = "" then Lwt.return_unit else Lwt_io.write channel text

let run ~name ~version ~doc ?default commands =
  let argv, verbose, quiet = verbosity_args Sys.argv in
  Sys.catch_break true;
  let output_buffer = Buffer.create 256 in
  let error_buffer = Buffer.create 256 in
  let output_ppf = Format.formatter_of_buffer output_buffer in
  let error_ppf = Format.formatter_of_buffer error_buffer in
  let profile =
    Charamel_colorprofile.detect ~is_tty:(is_tty Unix.stderr) ~env:Sys.getenv_opt
  in
  let env =
    {
      Env.cwd = Sys.getcwd ();
      Env.fs_root = Filename.dir_sep;
      Env.stdin = Lwt_io.stdin;
      Env.stdout = Lwt_io.stdout;
      Env.stderr = Lwt_io.stderr;
      Env.clock = Charamel_os.Time.lwt;
    }
  in
  let info = Cmdliner.Cmd.info ~version ~doc ~exits name in
  let children = List.map (fun make -> make env) commands in
  let command =
    match (default, children) with
    | Some make, [] -> Cmdliner.Cmd.v info (make env)
    | Some make, children ->
        (* A group's default term is reached only for an empty or
         option-first command line: a positional first token always
         runs the subcommand trie and a miss is fatal. Route a
         positional first token that names no child straight to the
         default term so [app FILE] parses as the default's argument. *)
        let child_names = List.map Cmdliner.Cmd.name children in
        let args = match Array.to_list argv with _ :: rest -> rest | [] -> [] in
        let routes_to_default =
          match args with
          | token :: _ when token = "" || token.[0] <> '-' ->
              not (List.exists (fun name -> String.equal token name) child_names)
          | _ -> false
        in
        if routes_to_default then Cmdliner.Cmd.v info (make env)
        else Cmdliner.Cmd.group ~default:(make env) info children
    | None, [] -> invalid_arg "charamel_cli.run needs a default term or commands"
    | None, children -> Cmdliner.Cmd.group info children
  in
  let old_reporter = Logs.reporter () in
  let old_level = Logs.level () in
  Logs.set_level (log_level ~verbose ~quiet);
  Logs.set_reporter (Charamel_log.reporter ~clock:env.Env.clock ~profile error_ppf);
  let evaluate () =
    match
      Cmdliner.Cmd.eval_value ~help:output_ppf ~err:error_ppf ~catch:false
        ~env:Sys.getenv_opt ~argv command
    with
    | Ok (`Ok action) ->
        let* () = action in
        Lwt.return 0
    | Ok `Help | Ok `Version -> Lwt.return 0
    | Error `Parse | Error `Term | Error `Exn -> Lwt.return 2
  in
  let report exn =
    Lwt.return
      (match exn with
      | Controlled_exit (code, Some message) ->
          diagnostic error_ppf ~profile message;
          check_code code
      | Controlled_exit (code, None) -> check_code code
      | Lwt_unix.Timeout ->
          diagnostic error_ppf ~profile "operation timed out";
          124
      | Sys.Break -> 130
      | exn ->
          diagnostic error_ppf ~profile (Printexc.to_string exn);
          1)
  in
  let cleanup () =
    Format.pp_print_flush output_ppf ();
    Format.pp_print_flush error_ppf ();
    Logs.set_reporter old_reporter;
    Logs.set_level old_level;
    let* () = write_buffer Lwt_io.stdout output_buffer in
    let* () = write_buffer Lwt_io.stderr error_buffer in
    Lwt.join [ Lwt_io.flush Lwt_io.stdout; Lwt_io.flush Lwt_io.stderr ]
  in
  let code = Lwt_main.run (Lwt.finalize (fun () -> Lwt.catch evaluate report) cleanup) in
  Stdlib.exit code
