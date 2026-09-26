open Pop_core
open Charamel_cli

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

let unsafe_html_arg =
  Cmdliner.Arg.(
    value & flag
    & info [ "unsafe-html" ]
        ~env:(Cmdliner.Cmd.Env.info "POP_UNSAFE_HTML")
        ~doc:
          "Allow raw HTML and unsafe links in the HTML part. When the option is absent, \
           the POP_UNSAFE_HTML environment variable decides, taken exactly and not \
           trimmed: true, yes, y or 1 enable it and false, no, n, 0 or an empty value \
           disable it; any other value is a usage error.")

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

let form_values_of_options ~cwd (options : Pop_lib.options) =
  let body_lwt =
    match (options.Pop_lib.body, options.Pop_lib.body_file) with
    | Some body, _ -> Lwt.return body
    | None, None -> Lwt.return ""
    | None, Some path ->
        Lwt.bind (Pop_lib.read_file ~cwd path) (function
          | Ok body -> Lwt.return body
          | Error error -> Charamel_cli.error (error_message Pop_lib.pp_error error))
  in
  Lwt.bind body_lwt (fun body ->
      Lwt.return
        {
          Forms.to_ = String.concat ", " (Pop_lib.split_addresses options.Pop_lib.to_);
          cc = String.concat ", " (Pop_lib.split_addresses options.Pop_lib.cc);
          bcc = String.concat ", " (Pop_lib.split_addresses options.Pop_lib.bcc);
          from = Option.value options.Pop_lib.from ~default:"";
          subject = Option.value options.Pop_lib.subject ~default:"";
          body;
        })

let options_of_form (options : Pop_lib.options) (values : Forms.values) : Pop_lib.options
    =
  {
    options with
    to_ = [ values.Forms.to_ ];
    cc = [ values.Forms.cc ];
    bcc = [ values.Forms.bcc ];
    from = Some values.Forms.from;
    subject = Some values.Forms.subject;
    body = Some values.Forms.body;
    body_file = None;
  }

let prepare_with_form (env : Charamel_cli.Env.t) options =
  Lwt.bind (Pop_lib.prepare ~cwd:env.Env.cwd ~stdin:env.Env.stdin options) (function
    | Ok message -> Lwt.return (Ok message)
    | Error (`Missing _) ->
        Lwt.bind (form_values_of_options ~cwd:env.Env.cwd options) (fun initial ->
            Lwt.bind
              (Forms.run ~clock:env.Env.clock ~fs_root:env.Env.fs_root
                 ~temp_dir:env.Env.fs_root ~initial) (function
              | Error error -> Lwt.return (Error (`Form error))
              | Ok values ->
                  let options = options_of_form options values in
                  Lwt.bind (Pop_lib.prepare ~cwd:env.Env.cwd ~stdin:env.Env.stdin options)
                    (function
                    | Ok message -> Lwt.return (Ok message)
                    | Error error -> Lwt.return (Error (`Compose error)))))
    | Error error -> Lwt.return (Error (`Compose error)))

let summary message =
  let recipients = List.map Mime.Address.addr message.Mime.to_ in
  Fmt.str "Email %S sent to %s\n" message.Mime.subject (String.concat ", " recipients)

let run (env : Charamel_cli.Env.t) cli =
  let options = options_of_cli cli in
  Lwt.bind (prepare_with_form env options) (function
    | Error (`Form error) ->
        Charamel_cli.error
          ~code:(match error with `Timeout -> 124 | `Aborted -> 130)
          (error_message Forms.pp_error error)
    | Error (`Compose error) -> Charamel_cli.error (error_message Pop_lib.pp_error error)
    | Ok message ->
        if cli.preview then Lwt_io.write env.Env.stdout (Mime.serialise message)
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
                | Error error -> Charamel_cli.error (error_message Pop_lib.pp_error error)
                )
          in
          Lwt.bind (Send.deliver ~resend_key ~smtp message) (function
            | Ok () -> Lwt_io.write env.Env.stdout (summary message)
            | Error error -> Charamel_cli.error (error_message Send.pp_error error)))

let default env =
  let action = run env in
  Cmdliner.Term.(const action $ cli_term)

let () =
  Mirage_crypto_rng_unix.use_default ();
  Charamel_cli.run ~name:"pop" ~version:Charamel_cli.Version.current
    ~doc:"Send Markdown email from the command line." ~default []
