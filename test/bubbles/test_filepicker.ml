open Lwt.Syntax
module Filepicker = Charamel_bubbles.Filepicker

let path_join dir name =
  if dir = "" || dir = "." then name
  else if String.ends_with ~suffix:"/" dir then dir ^ name
  else dir ^ "/" ^ name

let resolve root path =
  if path = "" || path = "." then root
  else if Filename.is_relative path then Filename.concat root path
  else path

let read_real_entries root path =
  try
    let directory = resolve root path in
    let entries =
      Array.to_list (Sys.readdir directory)
      |> Stdlib.List.filter_map (fun name ->
          if name <> "" && name.[0] = '.' then None
          else
            let entry_path = path_join directory name in
            let lstat = Unix.lstat entry_path in
            let is_symlink = lstat.Unix.st_kind = Unix.S_LNK in
            let target = if is_symlink then Unix.readlink entry_path else "" in
            let stat = if is_symlink then Unix.stat entry_path else lstat in
            Some
              {
                Filepicker.name;
                is_dir = stat.Unix.st_kind = Unix.S_DIR;
                is_symlink;
                symlink_target = target;
                perm = "";
                size = stat.Unix.st_size;
              })
    in
    let entries =
      Stdlib.List.sort
        (fun left right ->
          match (left.Filepicker.is_dir, right.Filepicker.is_dir) with
          | true, false -> -1
          | false, true -> 1
          | _ -> String.compare left.Filepicker.name right.Filepicker.name)
        entries
    in
    Ok entries
  with
  | Sys_error message -> Error (Fmt.str "cannot read %s: %s" path message)
  | Unix.Unix_error (kind, _, _) ->
      Error (Fmt.str "cannot read %s: %s" path (Unix.error_message kind))

let write_file path text =
  Charamel_os.Fs.with_open_out ~perm:0o600 path (fun channel -> Lwt_io.write channel text)

let make_directory path = Lwt.map Result.get_ok (Charamel_os.Fs.mkdir_p path)

let read_model root path picker =
  let picker, command = Filepicker.init picker in
  ignore command;
  let entries = read_real_entries root path in
  Filepicker.update (Filepicker.Read_dir { path; entries }) picker

let names entries =
  Stdlib.List.map (fun (entry : Filepicker.entry) -> entry.Filepicker.name) entries

let navigation_and_real_io () =
  Test_support.with_temp_dir @@ fun root ->
  let* () = make_directory (path_join root "subdir") in
  let* () = write_file (path_join root "root.txt") "root" in
  let* () = write_file (path_join root "subdir/file.go") "package main\n" in
  let* () = write_file (path_join root ".hidden") "hidden" in
  let picker = Filepicker.v ~root ~current_directory:root ~allowed_types:[ ".go" ] () in
  let picker, _ = read_model root root picker in
  Alcotest.(check string) "root directory" root (Filepicker.current_directory picker);
  Alcotest.(check bool)
    "hidden omitted" false
    (Stdlib.List.mem ".hidden" (names (Filepicker.entries picker)));
  Alcotest.(check bool)
    "directory first" true
    (match Filepicker.entries picker with
    | first :: _ -> first.Filepicker.is_dir
    | [] -> false);
  let picker, _ = Filepicker.update Filepicker.Open picker in
  let entered = Filepicker.current_directory picker in
  let picker, _ = read_model root entered picker in
  Alcotest.(check string)
    "entered directory" (root ^ "/subdir")
    (Filepicker.current_directory picker);
  Alcotest.(check bool)
    "child discovered" true
    (Stdlib.List.mem "file.go" (names (Filepicker.entries picker)));
  let selected = Filepicker.did_select_file Filepicker.Open picker in
  Alcotest.(check string) "select path" (root ^ "/subdir/file.go") (Option.get selected);
  Alcotest.(check int) "cursor starts at first child" 0 (Filepicker.cursor picker);
  Lwt.return_unit

let filtering_and_disabled_selection () =
  Test_support.with_temp_dir @@ fun root ->
  let* () = write_file (path_join root "good.go") "go" in
  let* () = write_file (path_join root "bad.txt") "txt" in
  let picker = Filepicker.v ~root ~current_directory:root ~allowed_types:[ ".go" ] () in
  let picker, _ = read_model root root picker in
  let picker, _ = Filepicker.update Filepicker.Go_to_top picker in
  Alcotest.(check bool)
    "disallowed file has no selection" false
    (Option.is_some (Filepicker.did_select_file Filepicker.Open picker));
  Alcotest.(check string)
    "disabled path is reported" (root ^ "/bad.txt")
    (Option.get (Filepicker.did_select_disabled_file Filepicker.Open picker));
  let picker = Filepicker.set_allowed_types [] picker in
  Alcotest.(check bool)
    "allowed types can be reset" true
    (Option.is_some (Filepicker.did_select_file Filepicker.Open picker));
  Lwt.return_unit

let directory_error_is_visible () =
  Test_support.with_temp_dir @@ fun root ->
  let picker = Filepicker.v ~root ~current_directory:root () in
  let picker, _ = read_model root root picker in
  let missing = root ^ "/missing" in
  let picker = Filepicker.set_current_directory missing picker in
  let picker, _ = read_model root missing picker in
  let view = Charamel_ansi.Text.strip (Filepicker.view picker) in
  Alcotest.(check bool)
    "directory failure names the error" true
    (Test_support.contains ~needle:"Error:" ~haystack:view);
  Alcotest.(check bool)
    "directory failure names the path" true
    (Test_support.contains ~needle:"missing" ~haystack:view);
  Alcotest.(check int)
    "failed read clears entries" 0
    (Stdlib.List.length (Filepicker.entries picker));
  Lwt.return_unit

let auto_height_and_resize () =
  Test_support.with_temp_dir @@ fun root ->
  let picker = Filepicker.v ~root ~current_directory:root ~height:10 () in
  let picker, _ = Filepicker.update (Filepicker.Resize 8) picker in
  Alcotest.(check int) "auto height leaves bottom margin" 3 (Filepicker.height picker);
  let picker =
    Filepicker.v ~root ~current_directory:root ~auto_height:false ~height:4 ()
  in
  let picker, _ = Filepicker.update (Filepicker.Resize 1) picker in
  Alcotest.(check int) "manual height unchanged" 4 (Filepicker.height picker);
  Lwt.return_unit

let cursor_row cursor =
  match cursor with None -> -1 | Some (cursor : Charamel_tea.Cursor.t) -> cursor.row

let selection_cursor () =
  Test_support.with_temp_dir @@ fun root ->
  let* () = write_file (path_join root "a.txt") "a" in
  let* () = write_file (path_join root "b.txt") "b" in
  let picker =
    Filepicker.v ~root ~current_directory:root ~auto_height:false ~height:2
      ~show_permissions:false ~show_size:false ()
  in
  let picker, _ = read_model root root picker in
  Alcotest.(check bool)
    "a populated picker asks for a cursor" true
    (Filepicker.selection_cursor picker <> None);
  Alcotest.(check int)
    "the selected entry is the first line" 0
    (cursor_row (Filepicker.selection_cursor picker));
  Alcotest.(check bool)
    "the column clears the cursor marker" true
    (match Filepicker.selection_cursor picker with
    | Some (cursor : Charamel_tea.Cursor.t) -> cursor.col > 0
    | None -> false);
  let picker, _ = Filepicker.update Filepicker.Down picker in
  Alcotest.(check int)
    "the cursor follows the selection" 1
    (cursor_row (Filepicker.selection_cursor picker));
  let empty = Filepicker.v ~root:"/proc/self/non-directory" () in
  Alcotest.(check bool)
    "an empty picker asks for no cursor" true
    (Filepicker.selection_cursor empty = None);
  Lwt.return_unit

let back_restores_the_parent_cursor () =
  Test_support.with_temp_dir @@ fun root ->
  let* () = make_directory (path_join root "alpha") in
  let* () = make_directory (path_join root "beta") in
  let* () = write_file (path_join root "one.txt") "1" in
  let* () = write_file (path_join root "two.txt") "2" in
  let* () = make_directory (path_join root "beta/sub") in
  let* () = write_file (path_join root "beta/x.txt") "x" in
  let* () = write_file (path_join root "beta/y.txt") "y" in
  let picker =
    Filepicker.v ~root ~current_directory:root ~auto_height:false ~height:5
      ~show_permissions:false ~show_size:false ()
  in
  let picker, _ = read_model root root picker in
  (* Directories sort first: alpha, beta, one.txt, two.txt. *)
  let picker, _ = Filepicker.update Filepicker.Down picker in
  Alcotest.(check int)
    "the cursor sits on the second directory" 1 (Filepicker.cursor picker);
  let picker, _ = Filepicker.update Filepicker.Open picker in
  Alcotest.(check string)
    "open enters the directory" (root ^ "/beta")
    (Filepicker.current_directory picker);
  let picker, _ = read_model root (path_join root "beta") picker in
  let picker, _ = Filepicker.update Filepicker.Down picker in
  let picker, _ = Filepicker.update Filepicker.Down picker in
  Alcotest.(check int)
    "the child directory keeps its own cursor" 2 (Filepicker.cursor picker);
  let picker, _ = Filepicker.update Filepicker.Back picker in
  Alcotest.(check string)
    "back returns to the parent" root
    (Filepicker.current_directory picker);
  Alcotest.(check int)
    "back restores the parent cursor, not zero" 1 (Filepicker.cursor picker);
  let picker, _ = read_model root root picker in
  Alcotest.(check int)
    "the reloaded parent keeps the restored cursor" 1 (Filepicker.cursor picker);
  Lwt.return_unit

let cases =
  [
    Alcotest_lwt.test_case "selection cursor" `Quick (fun _switch () ->
        selection_cursor ());
    Alcotest_lwt.test_case "navigation and real filesystem" `Quick (fun _switch () ->
        navigation_and_real_io ());
    Alcotest_lwt.test_case "filtering and disabled selection" `Quick (fun _switch () ->
        filtering_and_disabled_selection ());
    Alcotest_lwt.test_case "directory failures are visible" `Quick (fun _switch () ->
        directory_error_is_visible ());
    Alcotest_lwt.test_case "automatic height" `Quick (fun _switch () ->
        auto_height_and_resize ());
    Alcotest_lwt.test_case "back restores the cursor" `Quick (fun _switch () ->
        back_restores_the_parent_cursor ());
  ]
