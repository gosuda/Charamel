type values = {
  to_ : string;
  cc : string;
  bcc : string;
  from : string;
  subject : string;
  body : string;
}

type error = [ `Aborted | `Timeout ]

let pp_error ppf = function
  | `Aborted -> Fmt.string ppf "form aborted"
  | `Timeout -> Fmt.string ppf "form timed out"

let dyn text = Charamel_huh.Dyn.Const text

let nonempty value =
  if String.trim value = "" then Error "this value is required" else Ok ()

let get_string results key default =
  match Charamel_huh.Results.get key results with Some value -> value | None -> default

let run ~clock env ~initial =
  let to_key = Charamel_huh.Key.v "to" in
  let cc_key = Charamel_huh.Key.v "cc" in
  let bcc_key = Charamel_huh.Key.v "bcc" in
  let from_key = Charamel_huh.Key.v "from" in
  let subject_key = Charamel_huh.Key.v "subject" in
  let body_key = Charamel_huh.Key.v "body" in
  let send_key = Charamel_huh.Key.v "send" in
  let required_input title default key =
    Charamel_huh.Field.input ~title:(dyn title) ~default ~validate:nonempty key
  in
  let fields =
    [
      required_input "To addresses" initial.to_ to_key;
      Charamel_huh.Field.input ~title:(dyn "Cc addresses") ~default:initial.cc cc_key;
      Charamel_huh.Field.input ~title:(dyn "Bcc addresses") ~default:initial.bcc bcc_key;
      required_input "From address" initial.from from_key;
      required_input "Subject" initial.subject subject_key;
      Charamel_huh.Field.text ~title:(dyn "Message body") ~lines:8 ~default:initial.body
        ~validate:nonempty body_key;
      Charamel_huh.Field.confirm ~title:(dyn "Send this email?") ~default:true send_key;
    ]
  in
  let form =
    Charamel_huh.Form.v ~show_help:true ~show_errors:true
      [ Charamel_huh.Group.v ~title:"Compose email" fields ]
  in
  let form_env = Charamel_huh.Form.Env.v ~fs:env#fs ~temp_dir:env#fs ~editor:[] in
  match Charamel_huh.run ~env:form_env ~clock form env with
  | Error ((`Aborted | `Timeout) as error) -> Error error
  | Ok results ->
      let send =
        match Charamel_huh.Results.get send_key results with
        | Some value -> value
        | None -> true
      in
      if not send then Error `Aborted
      else
        Ok
          {
            to_ = get_string results to_key initial.to_;
            cc = get_string results cc_key initial.cc;
            bcc = get_string results bcc_key initial.bcc;
            from = get_string results from_key initial.from;
            subject = get_string results subject_key initial.subject;
            body = get_string results body_key initial.body;
          }
