module Xdg = Xdg
module Version = Version

exception Controlled_exit of int * string option

let check_code code =
  if code < 0 || code > 255 then
    invalid_arg (Fmt.str "Charm_cli: exit code %d is outside 0..255" code);
  code

let error ?(code = 1) message = raise (Controlled_exit (check_code code, Some message))
let exit code = raise (Controlled_exit (check_code code, None))

let ansi_index_is_dark index =
  match Charm_ansi.Color.indexed index with
  | None -> true
  | Some color -> (
      match Charm_ansi.Color.to_rgb color with
      | None -> true
      | Some (red, green, blue) ->
          (* The ITU-R BT.601 integer luma approximation agrees with the
             conventional xterm palette boundary: index 8 (dark gray) remains
             dark while index 7 (light gray) is light. *)
          (299 * red) + (587 * green) + (114 * blue) <= 128_000)

let colorfgbg_index value =
  match List.rev (String.split_on_char ';' value) with
  | field :: _ -> (
      match int_of_string_opt (String.trim field) with
      | Some index when index >= 0 && index <= 255 -> Some index
      | Some _ | None -> None)
  | [] -> None

let is_dark ~env =
  match env "COLORFGBG" with
  | Some value -> (
      match colorfgbg_index value with
      | Some index -> ansi_index_is_dark index
      | None -> true)
  | None -> true

let distance_bounded ~limit left right =
  let left_length = String.length left in
  let right_length = String.length right in
  if abs (left_length - right_length) > limit then None
  else
    let previous = Array.init (right_length + 1) Fun.id in
    let current = Array.make (right_length + 1) 0 in
    let rec rows row =
      if row > left_length then Some previous.(right_length)
      else begin
        current.(0) <- row;
        let row_min = ref current.(0) in
        for column = 1 to right_length do
          let substitution =
            previous.(column - 1) + if left.[row - 1] = right.[column - 1] then 0 else 1
          in
          let insertion = current.(column - 1) + 1 in
          let deletion = previous.(column) + 1 in
          let value = min substitution (min insertion deletion) in
          current.(column) <- value;
          if value < !row_min then row_min := value
        done;
        if !row_min > limit then None
        else begin
          Array.blit current 0 previous 0 (right_length + 1);
          rows (row + 1)
        end
      end
    in
    rows 1

let nearest_candidate ~candidates query =
  let rec choose best best_distance = function
    | [] -> best
    | candidate :: rest ->
        let next_best, next_distance =
          match distance_bounded ~limit:3 query candidate with
          | Some distance when distance < best_distance -> (Some candidate, distance)
          | Some _ | None -> (best, best_distance)
        in
        choose next_best next_distance rest
  in
  choose None 4 candidates

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

let is_tty flow =
  try Eio_unix.Fd.use_exn "isatty" (Eio_unix.Resource.fd flow) Unix.isatty
  with Eio.Io _ | Unix.Unix_error (_, _, _) -> false

let diagnostic ppf ~profile message =
  let styled =
    match profile with
    | Charm_colorprofile.No_tty | Charm_colorprofile.Ascii -> false
    | Charm_colorprofile.Ansi | Charm_colorprofile.Ansi256 | Charm_colorprofile.True_color
      ->
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

let run ~name ~version ~doc ?default commands =
  let argv, verbose, quiet = verbosity_args Sys.argv in
  Sys.catch_break true;
  let result =
    Eio_main.run (fun env ->
        Eio.Buf_write.with_flow env#stdout (fun stdout ->
            Eio.Buf_write.with_flow env#stderr (fun stderr ->
                let output_ppf = Eio.Buf_write.make_formatter stdout in
                let error_ppf = Eio.Buf_write.make_formatter stderr in
                let profile =
                  Charm_colorprofile.detect ~is_tty:(is_tty env#stderr)
                    ~env:Sys.getenv_opt
                in
                let old_reporter = Logs.reporter () in
                let old_level = Logs.level () in
                Logs.set_level (log_level ~verbose ~quiet);
                Logs.set_reporter (Charm_log.reporter ~clock:env#clock ~profile error_ppf);
                let restore () =
                  Format.pp_print_flush output_ppf ();
                  Format.pp_print_flush error_ppf ();
                  Logs.set_reporter old_reporter;
                  Logs.set_level old_level
                in
                Fun.protect ~finally:restore (fun () ->
                    let finish () =
                      let info = Cmdliner.Cmd.info ~version ~doc ~exits name in
                      let children = List.map (fun make -> make env) commands in
                      match (default, children) with
                      | Some make, [] -> Cmdliner.Cmd.v info (make env)
                      | Some make, children ->
                          (* A group's default term is reached only for an empty or
                           option-first command line: a positional first token always
                           runs the subcommand trie and a miss is fatal. Route a
                           positional first token that names no child straight to the
                           default term so [app FILE] parses as the default's argument. *)
                          let child_names = List.map Cmdliner.Cmd.name children in
                          let args =
                            match Array.to_list argv with _ :: rest -> rest | [] -> []
                          in
                          let routes_to_default =
                            match args with
                            | token :: _ when token = "" || token.[0] <> '-' ->
                                not
                                  (List.exists
                                     (fun name -> String.equal token name)
                                     child_names)
                            | _ -> false
                          in
                          if routes_to_default then Cmdliner.Cmd.v info (make env)
                          else Cmdliner.Cmd.group ~default:(make env) info children
                      | None, [] ->
                          invalid_arg "charm_cli.run needs a default term or commands"
                      | None, children -> Cmdliner.Cmd.group info children
                    in
                    try
                      match
                        Cmdliner.Cmd.eval_value ~help:output_ppf ~err:error_ppf
                          ~catch:false ~env:Sys.getenv_opt ~argv (finish ())
                      with
                      | Ok (`Ok ()) | Ok `Help | Ok `Version -> 0
                      | Error `Parse | Error `Term | Error `Exn -> 2
                    with
                    | Controlled_exit (code, Some message) ->
                        diagnostic error_ppf ~profile message;
                        check_code code
                    | Controlled_exit (code, None) -> check_code code
                    | Eio.Time.Timeout ->
                        diagnostic error_ppf ~profile "operation timed out";
                        124
                    | Sys.Break -> 130
                    | exn ->
                        diagnostic error_ppf ~profile (Printexc.to_string exn);
                        1))))
  in
  Stdlib.exit result
