let env_name ~cmd name =
  let normalized =
    name |> String.uppercase_ascii
    |> String.map (fun c -> if c = '-' || c = '.' then '_' else c)
  in
  "GUM_" ^ String.uppercase_ascii cmd ^ "_" ^ normalized

let env ~cmd name = Cmdliner.Cmd.Env.info (env_name ~cmd name)
let env_info = env

let env_bool value =
  match String.lowercase_ascii (String.trim value) with
  | "1" | "true" | "yes" | "on" -> Ok true
  | "0" | "false" | "no" | "off" -> Ok false
  | _ -> Error (Fmt.str "expected a boolean value, got %S" value)

let bool_term ~cmd ?(use_env = true) ~default ~names ~doc () =
  let env_variable = env_name ~cmd (List.hd (List.rev names)) in
  let info =
    Cmdliner.Arg.info names ~doc
      ?env:(if use_env then Some (env_info ~cmd (List.hd (List.rev names))) else None)
  in
  let cli = Cmdliner.Arg.value (Cmdliner.Arg.flag_all info) in
  let resolve lookup occurrences =
    match List.rev occurrences with
    | value :: _ -> Ok value
    | [] ->
        if use_env then
          match lookup env_variable with
          | Some value -> (
              match env_bool value with
              | Ok value -> Ok value
              | Error message -> Error (`Msg message))
          | None -> Ok default
        else Ok default
  in
  let parsed = Cmdliner.Term.(const resolve $ Cmdliner.Term.env $ cli) in
  Cmdliner.Term.term_result parsed

let negatable ~cmd ?(env = true) ?env_name:custom_env ?short ~default ~doc name =
  let variable = env_name ~cmd (Option.value custom_env ~default:name) in
  let positive_names =
    match short with None -> [ name ] | Some c -> [ String.make 1 c; name ]
  in
  let environment_info =
    if env then Some (env_info ~cmd (Option.value custom_env ~default:name)) else None
  in
  let positive = Cmdliner.Arg.info positive_names ~doc ?env:environment_info in
  let negative = Cmdliner.Arg.info [ "no-" ^ name ] ~doc ?env:environment_info in
  let cli =
    Cmdliner.Arg.value (Cmdliner.Arg.vflag_all [] [ (true, positive); (false, negative) ])
  in
  let resolve lookup occurrences =
    match List.rev occurrences with
    | value :: _ -> Ok value
    | [] ->
        if env then
          match lookup variable with
          | Some value -> (
              match env_bool value with
              | Ok value -> Ok value
              | Error message -> Error (`Msg message))
          | None -> Ok default
        else Ok default
  in
  let parsed = Cmdliner.Term.(const resolve $ Cmdliner.Term.env $ cli) in
  Cmdliner.Term.term_result parsed

let flag ~cmd ?(env = true) ?short ?(default = false) ~doc name =
  let names = match short with None -> [ name ] | Some c -> [ String.make 1 c; name ] in
  bool_term ~cmd ~use_env:env ~default ~names ~doc ()

let duration_parser raw =
  let source = String.trim raw in
  if source = "" then Error "duration cannot be empty"
  else
    let len = String.length source in
    let position = ref 0 in
    let total = ref 0. in
    let found = ref false in
    let parse_number () =
      let start = !position in
      let saw_digit = ref false in
      while
        !position < len
        &&
        let c = source.[!position] in
        if c >= '0' && c <= '9' then (
          saw_digit := true;
          true)
        else c = '.'
      do
        incr position
      done;
      if not !saw_digit then None
      else float_of_string_opt (String.sub source start (!position - start))
    in
    let parse_unit () =
      if !position + 1 < len && String.sub source !position 2 = "ms" then begin
        position := !position + 2;
        Some 0.001
      end
      else if !position < len then
        begin match source.[!position] with
        | 's' ->
            incr position;
            Some 1.
        | 'm' ->
            incr position;
            Some 60.
        | 'h' ->
            incr position;
            Some 3600.
        | _ -> Some 1.
        end
      else Some 1.
    in
    let rec loop () =
      if !position >= len then Ok ()
      else
        match parse_number () with
        | None -> Error "invalid number"
        | Some number -> (
            match parse_unit () with
            | None -> Error "invalid unit"
            | Some multiplier ->
                found := true;
                total := !total +. (number *. multiplier);
                loop ())
    in
    match loop () with
    | Error _ as error -> error
    | Ok () when not !found -> Error "invalid duration"
    | Ok () when !total < 0. -> Error "duration must not be negative"
    | Ok () -> Ok !total

let seconds ~cmd ~doc name =
  let conv =
    Cmdliner.Arg.Conv.make ~docv:"SECONDS"
      ~parser:(fun raw ->
        match duration_parser raw with
        | Ok value -> Ok value
        | Error message -> Error (Fmt.str "invalid duration %S: %s" raw message))
      ~pp:(fun ppf value -> Fmt.pf ppf "%.6gs" value)
      ()
  in
  let parsed =
    Cmdliner.Arg.value
      (Cmdliner.Arg.opt (Cmdliner.Arg.some conv) None
         (Cmdliner.Arg.info [ name ] ~doc ?env:(Some (env ~cmd name))))
  in
  Cmdliner.Term.(
    const (function Some value when value <= 0. -> None | value -> value) $ parsed)

let decode_delimiter raw =
  let b = Buffer.create (String.length raw) in
  let rec loop index =
    if index >= String.length raw then ()
    else if raw.[index] <> '\\' || index + 1 >= String.length raw then begin
      Buffer.add_char b raw.[index];
      loop (index + 1)
    end
    else begin
      (match raw.[index + 1] with
      | 'n' -> Buffer.add_char b '\n'
      | 't' -> Buffer.add_char b '\t'
      | '0' -> Buffer.add_char b '\000'
      | '\\' -> Buffer.add_char b '\\'
      | other ->
          Buffer.add_char b '\\';
          Buffer.add_char b other);
      loop (index + 2)
    end
  in
  loop 0;
  Buffer.contents b

let delimiter ~cmd ~default ~doc name =
  let conv =
    Cmdliner.Arg.Conv.make ~docv:"DELIMITER"
      ~parser:(fun raw -> Ok (decode_delimiter raw))
      ~pp:(fun ppf value -> Fmt.string ppf value)
      ()
  in
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt conv (decode_delimiter default)
       (Cmdliner.Arg.info [ name ] ~doc ?env:(Some (env ~cmd name))))

let parse_padding text =
  let text = String.trim text in
  let tokens =
    text
    |> String.map (fun c -> if c = ',' then ' ' else c)
    |> String.split_on_char ' '
    |> List.filter (fun token -> token <> "")
  in
  let int token =
    match int_of_string_opt token with
    | Some value -> Ok value
    | None -> Error (`Msg (Fmt.str "invalid padding value %S" token))
  in
  let values_result =
    List.fold_left
      (fun acc token ->
        match (int token, acc) with
        | Ok value, Ok values -> Ok (value :: values)
        | Error error, _ | _, Error error -> Error error)
      (Ok []) tokens
    |> Result.map List.rev
  in
  match values_result with
  | Error _ as error -> error
  | Ok [ value ] -> Ok (Charm_lipgloss.Sides.all value)
  | Ok [ vertical; horizontal ] ->
      Ok
        (Charm_lipgloss.Sides.v ~top:vertical ~right:horizontal ~bottom:vertical
           ~left:horizontal ())
  | Ok [ top; horizontal; bottom ] ->
      Ok (Charm_lipgloss.Sides.v ~top ~right:horizontal ~bottom ~left:horizontal ())
  | Ok [ top; right; bottom; left ] ->
      Ok (Charm_lipgloss.Sides.v ~top ~right ~bottom ~left ())
  | Ok [] -> Error (`Msg "padding requires one to four integer values")
  | Ok _ -> Error (`Msg "padding requires one to four integer values")

let padding ~cmd =
  let conv =
    Cmdliner.Arg.Conv.make ~docv:"PADDING"
      ~parser:(fun raw ->
        match parse_padding raw with
        | Ok _ -> Ok raw
        | Error (`Msg message) -> Error message)
      ~pp:(fun ppf value -> Fmt.string ppf value)
      ()
  in
  Cmdliner.Arg.value
    (Cmdliner.Arg.opt conv "0 0"
       (Cmdliner.Arg.info [ "padding" ] ~doc:"Padding as one to four integers."
          ?env:(Some (env ~cmd "padding"))))

let align text =
  match String.lowercase_ascii (String.trim text) with
  | "left" | "top" -> Some Charm_lipgloss.Position.left
  | "center" | "middle" -> Some Charm_lipgloss.Position.center
  | "right" | "bottom" -> Some Charm_lipgloss.Position.right
  | _ -> None

let border text =
  match String.lowercase_ascii (String.trim text) with
  | "none" -> Some Charm_lipgloss.Border.none
  | "hidden" -> Some Charm_lipgloss.Border.hidden
  | "normal" -> Some Charm_lipgloss.Border.normal
  | "rounded" -> Some Charm_lipgloss.Border.rounded
  | "thick" -> Some Charm_lipgloss.Border.thick
  | "double" -> Some Charm_lipgloss.Border.double
  | _ -> None

let color text =
  let text = String.trim text in
  if text = "" then Ok None
  else
    match Charm_ansi.Color.of_hex text with
    | Some color -> Ok (Some color)
    | None -> (
        match int_of_string_opt text with
        | Some index -> (
            match Charm_ansi.Color.indexed index with
            | Some color -> Ok (Some color)
            | None -> Error (`Msg (Fmt.str "invalid color: %s" text)))
        | None -> Error (`Msg (Fmt.str "invalid color: %s" text)))

let enum ~docv choices =
  let expected = String.concat ", " (List.map fst choices) in
  Cmdliner.Arg.Conv.make ~docv
    ~parser:(fun value ->
      match List.assoc_opt value choices with
      | Some parsed -> Ok parsed
      | None -> Error (Fmt.str "%s must be one of: %s" docv expected))
    ~pp:(fun ppf _ -> Fmt.string ppf "<value>")
    ()
