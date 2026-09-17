module Key = Charamel_tea.Key
module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module View = Charamel_tea.View
module Style = Charamel_lipgloss.Style
module Layout = Charamel_lipgloss.Layout

let key name =
  match Key.of_string name with
  | Ok value -> value
  | Error (`Msg message) ->
      invalid_arg (Fmt.str "invalid confirm key %s: %s" name message)

let k_ctrl_c = key "ctrl+c"
let k_escape = key "esc"
let k_n = key "n"
let k_shift_n = key "N"
let k_q = key "q"
let k_y = key "y"
let k_shift_y = key "Y"
let k_left = key "left"
let k_h = key "h"
let k_ctrl_n = key "ctrl+n"
let k_shift_tab = key "shift+tab"
let k_right = key "right"
let k_l = key "l"
let k_ctrl_p = key "ctrl+p"
let k_tab = key "tab"
let k_enter = key "enter"
let is_key actual expected = Key.matches actual expected
let any_key actual expected = List.exists (is_key actual) expected

type options = {
  default : bool;
  show_output : bool;
  affirmative : string;
  negative : string;
  prompt : string;
  show_help : bool;
  timeout : float option;
  padding : string;
  prompt_style : Gum_style.t;
  selected_style : Gum_style.t;
  unselected_style : Gum_style.t;
}

type msg = Key of Key.t

type model = {
  options : options;
  confirmation : bool;
  submitted : bool;
  quitting : bool;
  padding : Charamel_lipgloss.Sides.t;
}

let default_options =
  {
    default = true;
    show_output = false;
    affirmative = "Yes";
    negative = "No";
    prompt = "Are you sure?";
    show_help = true;
    timeout = None;
    padding = "0 0";
    prompt_style =
      Gum_style.defaults ~margin:"0 0 0 1" ~foreground:"#7571F9" ~bold:true ();
    selected_style =
      Gum_style.defaults ~background:"212" ~foreground:"230" ~padding:"0 3" ~margin:"0 1"
        ();
    unselected_style =
      Gum_style.defaults ~background:"235" ~foreground:"254" ~padding:"0 3" ~margin:"0 1"
        ();
  }

let make (options : options) =
  {
    options;
    confirmation = options.default;
    submitted = false;
    quitting = false;
    padding = Gum_flag.parsed_padding options.padding;
  }

let answer model = model.confirmation
let submitted model = model.submitted

let handle_key model key =
  if is_key key k_ctrl_c then ({ model with quitting = true }, Cmd.interrupt)
  else if is_key key k_escape then
    ({ model with quitting = true; submitted = true; confirmation = false }, Cmd.quit)
  else if any_key key [ k_n; k_shift_n; k_q ] then
    ({ model with quitting = true; submitted = true; confirmation = false }, Cmd.quit)
  else if any_key key [ k_y; k_shift_y ] then
    ({ model with quitting = true; submitted = true; confirmation = true }, Cmd.quit)
  else if
    any_key key [ k_left; k_h; k_ctrl_n; k_shift_tab; k_right; k_l; k_ctrl_p; k_tab ]
  then
    if model.options.negative = "" then (model, Cmd.none)
    else ({ model with confirmation = not model.confirmation }, Cmd.none)
  else if is_key key k_enter then
    ({ model with quitting = true; submitted = true }, Cmd.quit)
  else (model, Cmd.none)

let render model =
  if model.quitting then ""
  else
    let prompt =
      Style.render (Gum_style.to_style model.options.prompt_style) model.options.prompt
    in
    let affirmative_style, negative_style =
      if model.confirmation then
        ( Gum_style.to_style model.options.selected_style,
          Gum_style.to_style model.options.unselected_style )
      else
        ( Gum_style.to_style model.options.unselected_style,
          Gum_style.to_style model.options.selected_style )
    in
    let affirmative = Style.render affirmative_style model.options.affirmative in
    let negative =
      if model.options.negative = "" then ""
      else Style.render negative_style model.options.negative
    in
    let buttons =
      if negative = "" then affirmative
      else Layout.join_horizontal [ affirmative; negative ]
    in
    let body = prompt ^ "\n" ^ buttons in
    let body =
      if model.options.show_help then body ^ "\n\n←→ toggle • enter submit • esc quit"
      else body
    in
    Style.render (Style.padding model.padding Style.empty) body

let update message model = match message with Key key -> handle_key model key

let app options : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> (make options, Cmd.none));
    update = (fun message model -> update message model);
    view = (fun model -> View.v (render model));
    subscriptions = (fun _ -> Sub.key (fun key -> Key key));
  }

let piped_answer env =
  if Gum_io.stdin_is_empty env then None
  else
    match Gum_io.read_stdin ~single_line:true env with
    | Error `Empty -> None
    | Error (`Read value) -> Some value
    | Ok value -> Some value

let print_answer env (options : options) confirmation =
  if options.show_output then
    let label = if confirmation then options.affirmative else options.negative in
    Gum_io.print_raw env (options.prompt ^ " " ^ label)

let run env (options : options) =
  match piped_answer env with
  | Some value ->
      let confirmation = value = "yes" || value = "y" in
      print_answer env options confirmation;
      if confirmation then () else Charamel_cli.exit 1
  | None ->
      let execute () =
        try
          let model =
            Gum_run.run env (app options) ~finished:(fun model ->
                if submitted model then Gum_run.Submitted else Gum_run.Quit)
          in
          answer model
        with Gum_io.No_tty -> Charamel_cli.error "confirm: requires a terminal"
      in
      let confirmation =
        match options.timeout with
        | Some seconds when seconds > 0. -> (
            try Eio.Time.with_timeout_exn env#clock seconds execute
            with Eio.Time.Timeout -> options.default)
        | _ -> execute ()
      in
      print_answer env options confirmation;
      if confirmation then () else Charamel_cli.exit 1

let string_arg ~cmd names ~default ~doc =
  Cmdliner.Arg.(
    value (opt string default (info [ names ] ~doc ~env:(Gum_flag.env ~cmd names))))

let cmd env =
  let open Cmdliner in
  let open Term.Syntax in
  let prompt =
    Arg.(
      value
        (pos 0 string "Are you sure?" (info [] ~docv:"PROMPT" ~doc:"Prompt to display.")))
  in
  let prompt_style =
    Gum_style.term ~cmd:"confirm" ~prefix:"prompt."
      ~defaults:(Gum_style.defaults ~margin:"0 0 0 1" ~foreground:"#7571F9" ~bold:true ())
      ()
  in
  let selected_style =
    Gum_style.term ~cmd:"confirm" ~prefix:"selected."
      ~defaults:
        (Gum_style.defaults ~background:"212" ~foreground:"230" ~padding:"0 3"
           ~margin:"0 1" ())
      ()
  in
  let unselected_style =
    Gum_style.term ~cmd:"confirm" ~prefix:"unselected."
      ~defaults:
        (Gum_style.defaults ~background:"235" ~foreground:"254" ~padding:"0 3"
           ~margin:"0 1" ())
      ()
  in
  let term =
    let+ default =
      Gum_flag.negatable ~cmd:"confirm" ~default:true ~doc:"Default answer." "default"
    and+ show_output =
      Gum_flag.flag ~cmd:"confirm" ~doc:"Print prompt and answer." "show-output"
    and+ affirmative =
      string_arg ~cmd:"confirm" "affirmative" ~default:"Yes" ~doc:"Affirmative label."
    and+ negative =
      string_arg ~cmd:"confirm" "negative" ~default:"No" ~doc:"Negative label."
    and+ prompt = prompt
    and+ show_help =
      Gum_flag.negatable ~cmd:"confirm" ~default:true ~doc:"Show help keybinds."
        "show-help"
    and+ timeout =
      Gum_flag.seconds ~cmd:"confirm" ~doc:"Timeout until confirmation." "timeout"
    and+ padding = Gum_flag.validated_padding_term ~cmd:"confirm" ()
    and+ prompt_style = prompt_style
    and+ selected_style = selected_style
    and+ unselected_style = unselected_style in
    run env
      {
        default;
        show_output;
        affirmative;
        negative;
        prompt;
        show_help;
        timeout;
        padding;
        prompt_style;
        selected_style;
        unselected_style;
      }
  in
  Cmd.v (Cmd.info "confirm" ~doc:"Ask for an affirmative or negative answer.") term
