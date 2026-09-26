module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub
module Style = Charamel_lipgloss.Style
module Color = Charamel_ansi.Color
module Position = Charamel_lipgloss.Position
module Sides = Charamel_lipgloss.Sides

let clamp n lo hi = max lo (min hi n)
let key_binding ?help names = Key_binding.v ?help names

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

type styles = {
  disabled_cursor : Style.t;
  cursor : Style.t;
  symlink : Style.t;
  directory : Style.t;
  file : Style.t;
  disabled_file : Style.t;
  permission : Style.t;
  selected : Style.t;
  disabled_selected : Style.t;
  file_size : Style.t;
  empty_directory : Style.t;
}

type entry = {
  name : string;
  is_dir : bool;
  is_symlink : bool;
  symlink_target : string;
  perm : string;
  size : int;
}

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

let default_keymap =
  {
    go_to_top = key_binding ~help:("g", "first") [ "g" ];
    go_to_last = key_binding ~help:("G", "last") [ "G" ];
    down = key_binding ~help:("j", "down") [ "j"; "down"; "ctrl+n" ];
    up = key_binding ~help:("k", "up") [ "k"; "up"; "ctrl+p" ];
    page_up = key_binding ~help:("pgup", "page up") [ "K"; "pgup" ];
    page_down = key_binding ~help:("pgdown", "page down") [ "J"; "pgdown" ];
    back = key_binding ~help:("h", "back") [ "h"; "backspace"; "left"; "esc" ];
    open_ = key_binding ~help:("l", "open") [ "l"; "right"; "enter" ];
    select = key_binding ~help:("enter", "select") [ "enter" ];
  }

let default_styles =
  let foreground index = Style.foreground (Color.Indexed index) Style.empty in
  {
    disabled_cursor = foreground 247;
    cursor = foreground 212;
    symlink = foreground 36;
    directory = foreground 99;
    file = Style.empty;
    disabled_file = foreground 243;
    permission = foreground 244;
    selected = Style.bold true (foreground 212);
    disabled_selected = foreground 247;
    file_size = Style.align_horizontal Position.right (Style.width 7 (foreground 240));
    empty_directory = Style.padding (Sides.v ~left:2 ()) (foreground 240);
  }

type directory_state = { selected : int; min_idx : int; max_idx : int }

type t = {
  root : string;
  path : string;
  current_directory : string;
  allowed_types : string list;
  show_permissions : bool;
  show_size : bool;
  show_hidden : bool;
  dir_allowed : bool;
  file_allowed : bool;
  auto_height : bool;
  height : int;
  cursor_character : string;
  keymap : keymap;
  styles : styles;
  entries : entry list;
  selected : int;
  min_idx : int;
  max_idx : int;
  history : directory_state list;
  error : string option;
}

let v ~root ?(current_directory = ".") ?(allowed_types = []) ?(show_permissions = true)
    ?(show_size = true) ?(show_hidden = false) ?(dir_allowed = false)
    ?(file_allowed = true) ?(auto_height = true) ?(height = 0) ?(cursor = ">")
    ?(keymap = default_keymap) ?(styles = default_styles) () =
  {
    root;
    path = "";
    current_directory;
    allowed_types;
    show_permissions;
    show_size;
    show_hidden;
    dir_allowed;
    file_allowed;
    auto_height;
    height = max 0 height;
    cursor_character = cursor;
    keymap;
    styles;
    entries = [];
    selected = 0;
    min_idx = 0;
    max_idx = 0;
    history = [];
    error = None;
  }

let path_join dir name =
  if dir = "" || dir = "." then name
  else if String.ends_with ~suffix:"/" dir then dir ^ name
  else dir ^ "/" ^ name

let resolve root path =
  if path = "" || path = "." then root
  else if Filename.is_relative path then Filename.concat root path
  else path

let parent_directory path =
  if path = "/" then "/"
  else
    match String.rindex_opt path '/' with
    | None -> "."
    | Some 0 -> "/"
    | Some i ->
        let parent = String.sub path 0 i in
        if parent = "" then "/" else parent

let io_error path message = Fmt.str "%s: %s" path message

let permission_string kind perm =
  let type_char =
    match kind with
    | Unix.S_DIR -> 'd'
    | Unix.S_LNK -> 'l'
    | Unix.S_BLK -> 'b'
    | Unix.S_CHR -> 'c'
    | Unix.S_FIFO -> 'p'
    | Unix.S_SOCK -> 's'
    | Unix.S_REG -> '-'
  in
  let chars = Array.make 10 '-' in
  chars.(0) <- type_char;
  let bits =
    [
      (0o400, 1, 'r');
      (0o200, 2, 'w');
      (0o100, 3, 'x');
      (0o040, 4, 'r');
      (0o020, 5, 'w');
      (0o010, 6, 'x');
      (0o004, 7, 'r');
      (0o002, 8, 'w');
      (0o001, 9, 'x');
    ]
  in
  Stdlib.List.iter
    (fun (mask, index, mark) -> if perm land mask <> 0 then chars.(index) <- mark)
    bits;
  Bytes.to_string (Bytes.init 10 (fun i -> chars.(i)))

let entry_of_name root current_directory name =
  let path = resolve root (path_join current_directory name) in
  let lstat = Unix.lstat path in
  let is_symlink = lstat.Unix.st_kind = Unix.S_LNK in
  let target = if is_symlink then Unix.readlink path else "" in
  let stat = if is_symlink then Unix.stat path else lstat in
  let is_dir = stat.Unix.st_kind = Unix.S_DIR in
  {
    name;
    is_dir;
    is_symlink;
    symlink_target = target;
    perm = permission_string lstat.Unix.st_kind lstat.Unix.st_perm;
    size = lstat.Unix.st_size;
  }

let read_directory m path =
  try
    let raw = Array.to_list (Sys.readdir (resolve m.root path)) in
    let visible =
      if m.show_hidden then raw
      else Stdlib.List.filter (fun name -> name = "" || name.[0] <> '.') raw
    in
    let entries = Stdlib.List.map (entry_of_name m.root path) visible in
    let entries =
      Stdlib.List.sort
        (fun left right ->
          match (left.is_dir, right.is_dir) with
          | true, false -> -1
          | false, true -> 1
          | _ -> String.compare left.name right.name)
        entries
    in
    Ok entries
  with
  | Sys_error message -> Error (io_error path message)
  | Unix.Unix_error (kind, _, _) -> Error (io_error path (Unix.error_message kind))

let read_command m path =
  Cmd.perform (fun () ->
      match read_directory m path with
      | Ok entries -> Read_dir { path; entries = Ok entries }
      | Error error -> Read_dir { path; entries = Error error })

let bottom_idx m top = if m.height < 1 then top else top + m.height - 1

let normalize_indices m =
  let length = Stdlib.List.length m.entries in
  if length = 0 then { m with selected = 0; min_idx = 0; max_idx = 0 }
  else
    let selected = clamp m.selected 0 (length - 1) in
    let visible_height = max 1 m.height in
    let min_idx = clamp m.min_idx 0 (max 0 (length - visible_height)) in
    let max_idx = min (length - 1) (max (bottom_idx m min_idx) selected) in
    let min_idx = if selected < min_idx then selected else min_idx in
    let max_idx = if selected > max_idx then selected else max_idx in
    { m with selected; min_idx; max_idx }

let set_height height m =
  let m = { m with height = max 0 height } in
  normalize_indices m

let init m = (m, read_command m m.current_directory)

let selected_entry m =
  if m.selected < 0 then None else Stdlib.List.nth_opt m.entries m.selected

let entry_path m entry = path_join m.current_directory entry.name

let allowed_type m name =
  m.allowed_types = []
  || Stdlib.List.exists (fun suffix -> String.ends_with ~suffix name) m.allowed_types

let selectable m entry =
  if entry.is_dir then m.dir_allowed && allowed_type m entry.name
  else m.file_allowed && allowed_type m entry.name

let action_message m key =
  if Key_binding.matches key m.keymap.go_to_top then Some Go_to_top
  else if Key_binding.matches key m.keymap.go_to_last then Some Go_to_last
  else if Key_binding.matches key m.keymap.down then Some Down
  else if Key_binding.matches key m.keymap.up then Some Up
  else if Key_binding.matches key m.keymap.page_up then Some Page_up
  else if Key_binding.matches key m.keymap.page_down then Some Page_down
  else if Key_binding.matches key m.keymap.back then Some Back
  else if Key_binding.matches key m.keymap.open_ then Some Open
  else if Key_binding.matches key m.keymap.select then Some Select
  else None

let key m key = action_message m key
let subscriptions _ = Sub.none

let move_down m amount =
  let length = Stdlib.List.length m.entries in
  if length = 0 then m
  else
    let selected = clamp (m.selected + amount) 0 (length - 1) in
    let min_idx, max_idx =
      if selected > m.max_idx then
        let min_idx = min selected (max 0 (length - max 1 m.height)) in
        (min_idx, min (length - 1) (bottom_idx m min_idx))
      else (m.min_idx, m.max_idx)
    in
    { m with selected; min_idx; max_idx }

let move_up m amount =
  let length = Stdlib.List.length m.entries in
  if length = 0 then m
  else
    let selected = clamp (m.selected - amount) 0 (length - 1) in
    let min_idx, max_idx =
      if selected < m.min_idx then (selected, min (length - 1) (bottom_idx m selected))
      else (m.min_idx, m.max_idx)
    in
    { m with selected; min_idx; max_idx }

let update message m =
  match message with
  | Read_dir { path; entries } when path = m.current_directory ->
      ( (match entries with
        | Ok entries -> normalize_indices { m with entries; error = None }
        | Error error ->
            {
              (normalize_indices { m with entries = []; error = Some error }) with
              selected = 0;
            }),
        Cmd.none )
  | Read_dir _ -> (m, Cmd.none)
  | Resize rows when m.auto_height -> (set_height (max 0 (rows - 5)) m, Cmd.none)
  | Resize _ -> (m, Cmd.none)
  | Go_to_top ->
      ( normalize_indices { m with selected = 0; min_idx = 0; max_idx = bottom_idx m 0 },
        Cmd.none )
  | Go_to_last ->
      let last = max 0 (Stdlib.List.length m.entries - 1) in
      let min_idx = max 0 (last - max 1 m.height + 1) in
      ({ m with selected = last; min_idx; max_idx = last }, Cmd.none)
  | Down -> (move_down m 1, Cmd.none)
  | Up -> (move_up m 1, Cmd.none)
  | Page_down -> (move_down m (max 1 m.height), Cmd.none)
  | Page_up -> (move_up m (max 1 m.height), Cmd.none)
  | Select -> (
      match selected_entry m with
      | Some entry when selectable m entry ->
          ({ m with path = entry_path m entry }, Cmd.none)
      | _ -> (m, Cmd.none))
  | Back ->
      let parent = parent_directory m.current_directory in
      let selected, min_idx, max_idx, history =
        match m.history with
        | previous :: rest -> (previous.selected, previous.min_idx, previous.max_idx, rest)
        | [] -> (0, 0, bottom_idx m 0, [])
      in
      let next =
        {
          m with
          current_directory = parent;
          selected;
          min_idx;
          max_idx;
          history;
          entries = [];
          error = None;
        }
      in
      (next, read_command next parent)
  | Open -> (
      match selected_entry m with
      | None -> (m, Cmd.none)
      | Some entry when entry.is_dir ->
          let directory = entry_path m entry in
          let next =
            {
              m with
              current_directory = directory;
              selected = 0;
              min_idx = 0;
              max_idx = 0;
              history =
                { selected = m.selected; min_idx = m.min_idx; max_idx = m.max_idx }
                :: m.history;
              entries = [];
              error = None;
            }
          in
          (next, read_command next directory)
      | Some entry when m.file_allowed && allowed_type m entry.name ->
          ({ m with path = entry_path m entry }, Cmd.none)
      | Some _ -> (m, Cmd.none))

let set_current_directory current_directory =
 fun m ->
  {
    m with
    current_directory;
    entries = [];
    selected = 0;
    min_idx = 0;
    max_idx = 0;
    error = None;
  }

let human_size value =
  let f = float value in
  if value < 1000 then Fmt.str "%dB" value
  else if value < 1_000_000 then
    if value < 10_000 then Fmt.str "%.1fkB" (f /. 1000.) else Fmt.str "%.0fkB" (f /. 1000.)
  else if value < 1_000_000_000 then
    if value < 10_000_000 then Fmt.str "%.1fMB" (f /. 1_000_000.)
    else Fmt.str "%.0fMB" (f /. 1_000_000.)
  else if value < 1_000_000_000_000 then
    if value < 10_000_000_000 then Fmt.str "%.1fGB" (f /. 1_000_000_000.)
    else Fmt.str "%.0fGB" (f /. 1_000_000_000.)
  else if value < 10_000_000_000_000 then Fmt.str "%.1fTB" (f /. 1_000_000_000_000.)
  else Fmt.str "%.0fTB" (f /. 1_000_000_000_000.)

let render_padded m body =
  let lines =
    if body = "" then 0
    else 1 + String.fold_left (fun n c -> if c = '\n' then n + 1 else n) 0 body
  in
  if m.height <= lines then body else body ^ String.make (m.height - lines) '\n'

let view m =
  match m.error with
  | Some error ->
      render_padded m (Style.render m.styles.empty_directory (Fmt.str "Error: %s" error))
  | None when m.entries = [] ->
      render_padded m (Style.render m.styles.empty_directory "Bummer. No Files Found.")
  | None ->
      let buffer = Buffer.create 256 in
      let first = max 0 m.min_idx in
      let last = min (Stdlib.List.length m.entries - 1) m.max_idx in
      let visible =
        if first > last then []
        else
          let rec take index xs acc =
            if index > last then Stdlib.List.rev acc
            else
              match xs with
              | [] -> Stdlib.List.rev acc
              | _ :: rest when index < first -> take (index + 1) rest acc
              | x :: rest -> take (index + 1) rest (x :: acc)
          in
          take 0 m.entries []
      in
      Stdlib.List.iteri
        (fun offset entry ->
          let index = first + offset in
          let selectable = selectable m entry in
          let disabled = (not entry.is_dir) && not selectable in
          let target = if entry.is_symlink then " → " ^ entry.symlink_target else "" in
          let size =
            if m.show_size then Style.render m.styles.file_size (human_size entry.size)
            else ""
          in
          let permission =
            if m.show_permissions then " " ^ Style.render m.styles.permission entry.perm
            else ""
          in
          if index = m.selected then begin
            let selected_style =
              if disabled then m.styles.disabled_selected else m.styles.selected
            in
            let cursor_style =
              if disabled then m.styles.disabled_cursor else m.styles.cursor
            in
            let row = permission ^ size ^ " " ^ entry.name ^ target in
            Buffer.add_string buffer (Style.render cursor_style m.cursor_character);
            Buffer.add_string buffer (Style.render selected_style row)
          end
          else begin
            let name_style =
              if entry.is_dir then m.styles.directory
              else if entry.is_symlink then m.styles.symlink
              else if disabled then m.styles.disabled_file
              else m.styles.file
            in
            Buffer.add_string buffer (Style.render m.styles.cursor " ");
            Buffer.add_string buffer permission;
            Buffer.add_string buffer size;
            Buffer.add_char buffer ' ';
            Buffer.add_string buffer (Style.render name_style entry.name);
            Buffer.add_string buffer target
          end;
          Buffer.add_char buffer '\n')
        visible;
      render_padded m (Buffer.contents buffer)

let did_select_action = function Select | Open -> true | _ -> false

let did_select_file message m =
  if not (did_select_action message) then None
  else
    match selected_entry m with
    | Some entry when selectable m entry -> Some (entry_path m entry)
    | _ -> None

let did_select_disabled_file message m =
  if not (did_select_action message) then None
  else
    match selected_entry m with
    | Some entry when not (selectable m entry) -> Some (entry_path m entry)
    | _ -> None

let path m = m.path
let current_directory m = m.current_directory

let highlighted_path m =
  match selected_entry m with None -> "" | Some entry -> entry_path m entry

let height m = m.height
let entries m = m.entries
let cursor m = m.selected
let set_allowed_types allowed_types m = { m with allowed_types }
let set_show_hidden show_hidden m = { m with show_hidden }
let set_styles styles m = { m with styles }
let set_keymap keymap m = { m with keymap }
