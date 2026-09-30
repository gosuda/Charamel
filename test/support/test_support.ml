open Lwt.Infix
open Lwt.Syntax

let contains ~needle ~haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search index =
    index + needle_length <= haystack_length
    && (String.sub haystack index needle_length = needle || search (index + 1))
  in
  needle_length = 0 || search 0

let feed_stdin channel text =
  let write () = Lwt_io.write channel text >>= fun () -> Lwt_io.close channel in
  if text = "" then Lwt_io.close channel
  else
    Lwt.catch write (function
      | Unix.Unix_error ((EPIPE | EBADF), _, _) | Lwt_io.Channel_closed _ ->
          Lwt.return_unit
      | exn -> Lwt.fail exn)

let exit_status = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal

(* Windows needs a working system environment to launch a child at all:
   CreateProcess resolves console support and system DLLs through variables
   like SystemRoot and PATH, and a replaced environment that omits them fails
   with EINVAL before the child runs. POSIX spawns carry no such requirement,
   so the test's custom environment is used verbatim there. *)
let windows_inherited =
  [
    "APPDATA";
    "COMSPEC";
    "HOMEDRIVE";
    "HOMEPATH";
    "LOCALAPPDATA";
    "PATH";
    "PATHEXT";
    "PROGRAMDATA";
    "PUBLIC";
    "SYSTEMDRIVE";
    "SYSTEMROOT";
    "TEMP";
    "TMP";
    "USERPROFILE";
    "WINDIR";
  ]

let complete_env env =
  match env with
  | Some provided when Sys.win32 ->
      let names provided =
        Array.map
          (fun kv ->
            match String.index_opt kv '=' with
            | Some i -> String.uppercase_ascii (String.sub kv 0 i)
            | None -> String.uppercase_ascii kv)
          provided
      in
      let present = names provided in
      let missing =
        List.filter_map
          (fun name ->
            if Array.exists (String.equal name) present then None
            else
              match Sys.getenv_opt name with
              | Some value -> Some (name ^ "=" ^ value)
              | None -> None)
          windows_inherited
      in
      Some (Array.append provided (Array.of_list missing))
  | _ -> env

let run_cli ~exe ?env ?cwd ?(timeout = 10.) ?(stdin = "") args =
  if not Sys.win32 then ignore (Sys.set_signal Sys.sigpipe Sys.Signal_ignore);
  let env = complete_env env in
  let argv = Array.of_list (exe :: args) in
  Lwt_process.with_process_full ?env ?cwd (exe, argv) (fun process ->
      let collect =
        Lwt.protected (Lwt.both (Lwt_io.read process#stdout) (Lwt_io.read process#stderr))
      in
      let* () = feed_stdin process#stdin stdin in
      let* () =
        Lwt.catch
          (fun () -> Lwt_unix.with_timeout timeout (fun () -> Lwt.map ignore collect))
          (function
            | Lwt_unix.Timeout ->
                process#kill Sys.sigkill;
                Lwt.return_unit
            | exn -> Lwt.fail exn)
      in
      let* stdout_text, stderr_text = collect in
      let* status = process#status in
      Lwt.return (exit_status status, stdout_text, stderr_text))

let entry_kind path =
  match try Some (Unix.lstat path) with Unix.Unix_error _ -> None with
  | None -> `Missing
  | Some { Unix.st_kind = Unix.S_DIR; _ } -> `Directory
  | Some _ -> `File

let try_unix action = match action () with () -> () | exception Unix.Unix_error _ -> ()

let rec remove_tree path =
  match entry_kind path with
  | `Missing -> ()
  | `File -> try_unix (fun () -> Unix.unlink path)
  | `Directory ->
      let names =
        match try Some (Sys.readdir path) with Unix.Unix_error _ -> None with
        | Some names -> names
        | None -> [||]
      in
      Array.iter (fun name -> remove_tree (Filename.concat path name)) names;
      try_unix (fun () -> Unix.rmdir path)

let with_temp_dir f =
  let path = Filename.temp_file "charamel-test-" ".dir" in
  try_unix (fun () -> Unix.unlink path);
  Unix.mkdir path 0o700;
  Lwt.finalize
    (fun () -> f path)
    (fun () ->
      remove_tree path;
      Lwt.return_unit)

let run_lwt name suites = Lwt_main.run (Alcotest_lwt.run name suites)

let corpus =
  [
    ("\u{D55C}", 2, 1);
    ("\u{1112}\u{1161}\u{11AB}", 2, 1);
    ("\u{304B}\u{306A}", 4, 2);
    ("\u{FF76}\u{FF77}", 2, 2);
    ("\u{FF76}\u{FF9E}", 1, 1);
    ("\u{4E16}", 2, 1);
    ("\u{FF21}", 2, 1);
    ("\u{3001}\u{3002}", 4, 2);
    ("\u{26A0}\u{FE0F}", 2, 1);
    ("\u{2639}\u{FE0E}", 1, 1);
    ("\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F466}", 2, 1);
    ("\u{1F1FA}\u{1F1F8}", 2, 1);
    ("\u{4E16}\u{0301}", 2, 1);
    ("\u{00A7}", 1, 1);
    ("\u{0410}", 1, 1);
    ("\u{D55C}\u{AD6D}\u{C5B4}\u{D14D}\u{C2A4}\u{D2B8}\u{D3B8}\u{C9D1}\u{AE30}", 18, 9);
    ("\u{4F60}\u{597D}\u{FF0C}\u{4E16}\u{754C}\u{FF01}", 12, 6);
  ]

let cjk_gen =
  let open QCheck2.Gen in
  let piece =
    oneof_weighted
      [
        (2, string_size ~gen:(map Char.chr (int_range 32 126)) (int_range 1 6));
        (2, map (fun (text, _, _) -> text) (oneof_list corpus));
      ]
  in
  map (String.concat "") (list_size (int_range 0 6) piece)
