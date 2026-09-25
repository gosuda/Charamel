module Filepicker = Charamel_bubbles.Filepicker

let check_string name expected actual =
  Alcotest.check Alcotest.string name expected actual

let check_bool name expected actual = Alcotest.check Alcotest.bool name expected actual
let check_int name expected actual = Alcotest.check Alcotest.int name expected actual
let path_append = Eio.Path.( / )

let path_join dir name =
  if dir = "" || dir = "." then name
  else if String.ends_with ~suffix:"/" dir then dir ^ name
  else dir ^ "/" ^ name

let read_real_entries fs path =
  try
    let raw = Eio.Path.read_dir_entries (path_append fs path) in
    let entries =
      raw
      |> Stdlib.List.filter_map (fun (kind, name) ->
          if name <> "" && name.[0] = '.' then None
          else
            let entry_path = path_append fs (path_join path name) in
            let lstat = Eio.Path.stat ~follow:false entry_path in
            let is_symlink =
              kind = `Symbolic_link || lstat.Eio.File.Stat.kind = `Symbolic_link
            in
            let target = if is_symlink then Eio.Path.read_link entry_path else "" in
            let stat =
              if is_symlink then Eio.Path.stat ~follow:true entry_path else lstat
            in
            Some
              {
                Filepicker.name;
                is_dir = stat.Eio.File.Stat.kind = `Directory;
                is_symlink;
                symlink_target = target;
                perm = "";
                size = Optint.Int63.to_int stat.Eio.File.Stat.size;
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
  with Eio.Io (Eio.Fs.E _, _) -> Error (Fmt.str "cannot read %s" path)

let with_directory f =
  Test_support.with_temp_dir (fun fs -> f fs (Eio.Path.native_exn fs))

let read_model fs path picker =
  let picker, command = Filepicker.init picker in
  ignore command;
  let entries = read_real_entries fs path in
  Filepicker.update (Filepicker.Read_dir { path; entries }) picker

let names entries =
  Stdlib.List.map (fun (entry : Filepicker.entry) -> entry.Filepicker.name) entries

let navigation_and_real_io () =
  with_directory @@ fun fs name ->
  let sub = path_append fs "subdir" in
  Eio.Path.mkdir ~perm:0o700 sub;
  Eio.Path.save ~create:(`Exclusive 0o600) (path_append fs "root.txt") "root";
  Eio.Path.save ~create:(`Exclusive 0o600) (path_append sub "file.go") "package main\n";
  Eio.Path.save ~create:(`Exclusive 0o600) (path_append fs ".hidden") "hidden";
  let picker = Filepicker.v ~fs ~current_directory:name ~allowed_types:[ ".go" ] () in
  let picker, _ = read_model fs name picker in
  check_string "root directory" name (Filepicker.current_directory picker);
  check_bool "hidden omitted" false
    (Stdlib.List.mem ".hidden" (names (Filepicker.entries picker)));
  check_bool "directory first" true
    (match Filepicker.entries picker with
    | first :: _ -> first.Filepicker.is_dir
    | [] -> false);
  let picker, _ = Filepicker.update Filepicker.Open picker in
  let picker, _ = read_model fs (Filepicker.current_directory picker) picker in
  check_string "entered directory" (name ^ "/subdir")
    (Filepicker.current_directory picker);
  check_bool "child discovered" true
    (Stdlib.List.mem "file.go" (names (Filepicker.entries picker)));
  let selected = Filepicker.did_select_file Filepicker.Open picker in
  check_string "select path" (name ^ "/subdir/file.go") (Option.get selected);
  check_int "cursor starts at first child" 0 (Filepicker.cursor picker)

let filtering_and_disabled_selection () =
  with_directory @@ fun fs name ->
  Eio.Path.save ~create:(`Exclusive 0o600) (path_append fs "good.go") "go";
  Eio.Path.save ~create:(`Exclusive 0o600) (path_append fs "bad.txt") "txt";
  let picker = Filepicker.v ~fs ~current_directory:name ~allowed_types:[ ".go" ] () in
  let picker, _ = read_model fs name picker in
  let picker, _ = Filepicker.update Filepicker.Go_to_top picker in
  check_bool "disallowed file has no selection" false
    (Option.is_some (Filepicker.did_select_file Filepicker.Open picker));
  check_string "disabled path is reported" (name ^ "/bad.txt")
    (Option.get (Filepicker.did_select_disabled_file Filepicker.Open picker));
  let picker = Filepicker.set_allowed_types [] picker in
  check_bool "allowed types can be reset" true
    (Option.is_some (Filepicker.did_select_file Filepicker.Open picker))

let directory_error_is_visible () =
  with_directory @@ fun fs name ->
  let picker = Filepicker.v ~fs ~current_directory:name () in
  let picker, _ = read_model fs name picker in
  let missing = name ^ "/missing" in
  let picker = Filepicker.set_current_directory missing picker in
  let picker, _ = read_model fs missing picker in
  let view = Charamel_ansi.Text.strip (Filepicker.view picker) in
  check_bool "directory failure visible" true
    (String.length view > 0 && String.contains view 'E');
  check_int "failed read clears entries" 0
    (Stdlib.List.length (Filepicker.entries picker))

let auto_height_and_resize () =
  with_directory @@ fun fs name ->
  let picker = Filepicker.v ~fs ~current_directory:name ~height:10 () in
  let picker, _ = Filepicker.update (Filepicker.Resize 8) picker in
  check_int "auto height leaves bottom margin" 3 (Filepicker.height picker);
  let picker = Filepicker.v ~fs ~current_directory:name ~auto_height:false ~height:4 () in
  let picker, _ = Filepicker.update (Filepicker.Resize 1) picker in
  check_int "manual height unchanged" 4 (Filepicker.height picker)

let cases =
  [
    Alcotest.test_case "navigation and real filesystem" `Quick navigation_and_real_io;
    Alcotest.test_case "filtering and disabled selection" `Quick
      filtering_and_disabled_selection;
    Alcotest.test_case "directory failures are visible" `Quick directory_error_is_visible;
    Alcotest.test_case "automatic height" `Quick auto_height_and_resize;
  ]
