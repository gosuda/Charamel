(** Capabilities used by form fields.

    The form never consults process-global filesystem or editor settings. *)

type t = {
  fs : Eio.Fs.dir_ty Eio.Path.t;
  temp_dir : Eio.Fs.dir_ty Eio.Path.t;
  editor : string list;
}

val v :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  temp_dir:Eio.Fs.dir_ty Eio.Path.t ->
  editor:string list ->
  t
(** [v ~fs ~temp_dir ~editor] creates the explicit field capabilities. An empty [editor]
    disables the editor action. *)

val editor_of_string : string option -> string list
(** [editor_of_string value] splits an editor command on ASCII spaces and tabs. Empty or
    absent values return [["nano"]]. *)
