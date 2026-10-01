(** Capabilities used by form fields.

    The form never consults process-global filesystem or editor settings. *)

type t = {
  fs_root : string;
  temp_dir : string;
  editor : string list option;
  clock : Charamel_os.Time.clock;
}

val v :
  fs_root:string ->
  temp_dir:string ->
  editor:string list option ->
  clock:Charamel_os.Time.clock ->
  t
(** [v ~fs_root ~temp_dir ~editor ~clock] creates the explicit field capabilities.
    [fs_root] anchors every path a field resolves, [temp_dir] holds editor scratch files,
    and [clock] times field timers. [editor] is the command that the editor action runs;
    [None], or an empty argument list, disables the editor action. *)

val editor_of_string : string option -> string list
(** [editor_of_string value] splits an editor command on ASCII spaces and tabs. Empty or
    absent values return [["nano"]]. *)
