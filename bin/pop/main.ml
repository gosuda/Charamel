open Pop_core

let string_list_option names doc =
  Cmdliner.Arg.(value (opt_all string [] (info names ~doc ~docv:"ADDRESS")))

let error_message pp error = Fmt.str "%a" pp error

let to_arg =
  string_list_option [ "t"; "to" ]
    "Recipient address. The option may be repeated or comma separated."

let cc_arg = string_list_option [ "cc" ] "Carbon-copy recipient address."
let bcc_arg = string_list_option [ "bcc" ] "Blind-carbon-copy recipient address."

let from_arg =
  Cmdliner.Arg.(
    value
      (opt (some string) None
         (info [ "f"; "from" ] ~docv:"ADDRESS"
            ~env:(Cmdliner.Cmd.Env.info "POP_FROM")
            ~doc:"Sender address.")))

let subject_arg =
  Cmdliner.Arg.(
    value
      (opt (some string) None
         (info [ "s"; "subject" ] ~docv:"TEXT" ~doc:"Message subject.")))

let body_arg =
  Cmdliner.Arg.(
    value
      (opt (some string) None
         (info [ "b"; "body" ] ~docv:"TEXT" ~doc:"Markdown message body.")))

let body_file_arg =
  Cmdliner.Arg.(
    value
      (opt (some string) None
         (info [ "body-file" ] ~docv:"PATH" ~doc:"Read the Markdown body from PATH.")))

let attachment_arg =
  Cmdliner.Arg.(
    value
      (opt_all string []
         (info [ "a"; "attach" ] ~docv:"PATH"
            ~doc:"Attach PATH. The option may be repeated.")))

let signature_arg =
  Cmdliner.Arg.(
    value
      (opt (some string) None
         (info [ "x"; "signature" ] ~docv:"TEXT"
            ~env:(Cmdliner.Cmd.Env.info "POP_SIGNATURE")
            ~doc:"Append TEXT to the body as a signature.")))

let preview_arg =
  Cmdliner.Arg.(
    value & flag
    & info [ "preview" ] ~doc:"Print the composed MIME message without delivering it.")

let env_truthy = function
  | Some value -> (
      match String.lowercase_ascii (String.trim value) with
      | "1" | "true" | "yes" | "on" -> true
      | _ -> false)
  | None -> false

let unsafe_html_arg =
  let flag =
    Cmdliner.Arg.(
      value & flag
      & info [ "unsafe-html" ] ~doc:"Allow raw HTML and unsafe links in the HTML part.")
  in
  Cmdliner.Term.(
    const (fun selected env -> selected || env_truthy (env "POP_UNSAFE_HTML"))
    $ flag $ env)

type cli = {
  to_ : string list;
  cc : string list;
  bcc : string list;
  from : string option;
  subject : string option;
  body : string option;
  body_file : string option;
  attachments : string list;
  signature : string option;
  preview : bool;
  unsafe_html : bool;
}

let cli_term =
  let open Cmdliner.Term in
  const
    (fun
      to_ cc bcc from subject body body_file attachments signature preview unsafe_html ->
      {
        to_;
        cc;
        bcc;
        from;
        subject;
        body;
        body_file;
        attachments;
        signature;
        preview;
        unsafe_html;
      })
  $ to_arg $ cc_arg $ bcc_arg $ from_arg $ subject_arg $ body_arg $ body_file_arg
  $ attachment_arg $ signature_arg $ preview_arg $ unsafe_html_arg

let options_of_cli (cli : cli) : Pop_lib.options =
  {
    to_ = cli.to_;
    cc = cli.cc;
    bcc = cli.bcc;
    from = cli.from;
    subject = cli.subject;
    body = cli.body;
    body_file = cli.body_file;
    attachments = cli.attachments;
    signature = cli.signature;
    unsafe_html = cli.unsafe_html;
  }

let form_values_of_options ~cwd (options : Pop_lib.options) : Forms.values =
  let body =
    match (options.body, options.body_file) with
    | Some body, _ -> body
    | None, None -> ""
    | None, Some path -> (
        match Pop_lib.read_file ~cwd path with
        | Ok body -> body
        | Error error -> Charm_cli.error (error_message Pop_lib.pp_error error))
  in
  {
    to_ = String.concat ", " (Pop_lib.split_addresses options.to_);
    cc = String.concat ", " (Pop_lib.split_addresses options.cc);
    bcc = String.concat ", " (Pop_lib.split_addresses options.bcc);
    from = Option.value options.from ~default:"";
    subject = Option.value options.subject ~default:"";
    body;
  }

let options_of_form (options : Pop_lib.options) (values : Forms.values) : Pop_lib.options
    =
  {
    options with
    to_ = [ values.to_ ];
    cc = [ values.cc ];
    bcc = [ values.bcc ];
    from = Some values.from;
    subject = Some values.subject;
    body = Some values.body;
    body_file = None;
  }

let prepare_with_form ~sw env options =
  match Pop_lib.prepare ~sw ~clock:env#clock ~cwd:env#cwd ~stdin:env#stdin options with
  | Ok prepared -> Ok prepared
  | Error (`Missing _) -> (
      match
        Forms.run ~clock:env#clock env
          ~initial:(form_values_of_options ~cwd:env#cwd options)
      with
      | Error error -> Error (`Form error)
      | Ok values -> (
          let options = options_of_form options values in
          match
            Pop_lib.prepare ~sw ~clock:env#clock ~cwd:env#cwd ~stdin:env#stdin options
          with
          | Ok prepared -> Ok prepared
          | Error error -> Error (`Compose error)))
  | Error error -> Error (`Compose error)

let summary message =
  let recipients = List.map Mime.Address.addr message.Mime.to_ in
  Fmt.str "Email %S sent to %s\n" message.Mime.subject (String.concat ", " recipients)

let run env cli =
  let options = options_of_cli cli in
  Eio.Switch.run (fun sw ->
      match prepare_with_form ~sw env options with
      | Error (`Form error) ->
          Charm_cli.error
            ~code:(match error with `Timeout -> 124 | `Aborted -> 130)
            (error_message Forms.pp_error error)
      | Error (`Compose error) -> Charm_cli.error (error_message Pop_lib.pp_error error)
      | Ok prepared -> (
          if cli.preview then Preview.write env#stdout prepared.message
          else
            let resend_key =
              match Sys.getenv_opt "RESEND_API_KEY" with
              | Some key when String.trim key <> "" -> Some (String.trim key)
              | _ -> None
            in
            let smtp =
              match resend_key with
              | Some _ -> None
              | None -> (
                  match Pop_lib.config_of_env ~env:Sys.getenv_opt with
                  | Ok config -> Some config
                  | Error error -> Charm_cli.error (error_message Pop_lib.pp_error error))
            in
            match
              Send.deliver ~sw ~clock:env#clock ~net:env#net ~resend_key ~smtp
                prepared.message
            with
            | Ok () -> Eio.Flow.copy_string (summary prepared.message) env#stdout
            | Error error -> Charm_cli.error (error_message Send.pp_error error)))

let default env =
  let action = run env in
  Cmdliner.Term.(const action $ cli_term)

let () =
  Mirage_crypto_rng_unix.use_default ();
  Charm_cli.run ~name:"pop" ~version:Charm_cli.Version.current
    ~doc:"Send Markdown email from the command line." ~default []
