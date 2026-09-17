(** Filesystem file picker.

    [t] is an immutable picker model. Directory reads are issued by [init] and [update] as
    commands through the explicitly supplied Eio filesystem path. *)

type keymap = {
  go_to_top : Key_binding.t;
  go_to_last : Key_binding.t;
  down : Key_binding.t;
  up : Key_binding.t;
  page_up : Key_binding.t;
  page_down : Key_binding.t;
  back : Key_binding.t;
  open_ : Key_binding.t;
  select : Key_binding.t;
}

val default_keymap : keymap
(** [default_keymap] is the standard file-picker navigation map. *)

type styles = {
  disabled_cursor : Charamel_lipgloss.Style.t;
  cursor : Charamel_lipgloss.Style.t;
  symlink : Charamel_lipgloss.Style.t;
  directory : Charamel_lipgloss.Style.t;
  file : Charamel_lipgloss.Style.t;
  disabled_file : Charamel_lipgloss.Style.t;
  permission : Charamel_lipgloss.Style.t;
  selected : Charamel_lipgloss.Style.t;
  disabled_selected : Charamel_lipgloss.Style.t;
  file_size : Charamel_lipgloss.Style.t;
  empty_directory : Charamel_lipgloss.Style.t;
}

val default_styles : styles
(** [default_styles] is the standard palette and layout. *)

type entry = {
  name : string;
  is_dir : bool;
  is_symlink : bool;
  symlink_target : string;
  perm : string;
  size : int;
}
(** [entry] is the metadata read for one directory entry. *)

type msg =
  | Go_to_top
  | Go_to_last
  | Down
  | Up
  | Page_up
  | Page_down
  | Back
  | Open
  | Select
  | Read_dir of { path : string; entries : (entry list, string) result }
  | Resize of int

type t

val v :
  fs:Eio.Fs.dir_ty Eio.Path.t ->
  ?current_directory:string ->
  ?allowed_types:string list ->
  ?show_permissions:bool ->
  ?show_size:bool ->
  ?show_hidden:bool ->
  ?dir_allowed:bool ->
  ?file_allowed:bool ->
  ?auto_height:bool ->
  ?height:int ->
  ?cursor:string ->
  ?keymap:keymap ->
  ?styles:styles ->
  unit ->
  t
(** [v ~fs ()] creates a picker rooted at [current_directory] (default [.]). It uses [fs]
    for every directory and metadata operation. [allowed_types] contains filename
    suffixes; an empty list allows every file. Defaults are permissions and sizes shown,
    hidden entries omitted, files allowed, directories disallowed, automatic height
    enabled, height [0], and cursor [">"]. *)

val init : t -> t * msg Charamel_tea.Cmd.t
(** [init t] schedules a real read of [current_directory]. *)

val update : msg -> t -> t * msg Charamel_tea.Cmd.t
(** [update msg t] applies navigation, selection, resize, or a directory-read result. A
    read result for a stale path is ignored. Directory failures are retained in the model
    and rendered visibly. *)

val view : t -> string
(** [view t] renders the visible rows and pads to the configured height. *)

val key : t -> Charamel_tea.Key.t -> msg option
(** [key t key] returns the enabled action bound to [key], if any. *)

val subscriptions : t -> msg Charamel_tea.Sub.t
(** [subscriptions t] is {!Charamel_tea.Sub.none}; reads are commands. *)

val did_select_file : msg -> t -> string option
(** [did_select_file msg t] reports a selectable highlighted path before [msg] is passed
    to [update]. *)

val did_select_disabled_file : msg -> t -> string option
(** [did_select_disabled_file msg t] reports a highlighted file rejected by
    [allowed_types] or the file/dir policy. *)

val path : t -> string
val current_directory : t -> string
val highlighted_path : t -> string
val height : t -> int
val set_height : int -> t -> t
val set_current_directory : string -> t -> t
val set_allowed_types : string list -> t -> t
val set_show_hidden : bool -> t -> t
val set_styles : styles -> t -> t
val set_keymap : keymap -> t -> t
val entries : t -> entry list
val cursor : t -> int
