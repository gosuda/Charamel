open Lwt.Infix

let default_profile = Charamel_colorprofile.detect ~is_tty:false ~env:Sys.getenv_opt

let print ?(profile = default_profile) ?(sink = Lwt_io.stdout) text =
  let writer = Charamel_colorprofile.Writer.create ~profile sink in
  Charamel_colorprofile.Writer.write writer text

let println ?profile ?sink text = print ?profile ?sink (text ^ "\n")

let capture profile text =
  let buffer = Buffer.create (String.length text + 16) in
  let sink =
    Lwt_io.make ~mode:Lwt_io.Output (fun bytes offset length ->
        Buffer.add_subbytes buffer (Lwt_bytes.to_bytes bytes) offset length;
        Lwt.return length)
  in
  let writer = Charamel_colorprofile.Writer.create ~profile sink in
  Charamel_colorprofile.Writer.write writer text >|= fun () -> Buffer.contents buffer

let sprint ?(profile = default_profile) text = capture profile text
let sprintln ?profile text = sprint ?profile (text ^ "\n")
