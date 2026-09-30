open Lwt.Infix

type value = Text of string | Binary of string

type error =
  [ `No_such_db of string
  | `No_such_key of string
  | `Corrupt of string
  | `Io of string
  | `Invalid_db of string ]

let pp_error fmt = function
  | `No_such_db db -> Fmt.pf fmt "no such database: %s" db
  | `No_such_key key -> Fmt.pf fmt "no such key: %s" key
  | `Corrupt msg -> Fmt.pf fmt "corrupt database: %s" msg
  | `Io msg -> Fmt.pf fmt "%s" msg
  | `Invalid_db db -> Fmt.pf fmt "invalid database name: %s" db

(* A database name is one filename component: rejecting '/' keeps it from
   introducing a path segment, and rejecting ".." keeps it from spelling the
   parent-directory entry even as a full name (a lone ".." would otherwise
   become the sibling temp file "...json.tmp...", which is confusing but not
   an escape; this module refuses it anyway rather than rely on that). *)
let has_dotdot s =
  let n = String.length s in
  let rec loop i = i < n - 1 && ((s.[i] = '.' && s.[i + 1] = '.') || loop (i + 1)) in
  n >= 2 && loop 0

let is_valid_db db =
  db <> "" && db <> "." && db <> ".."
  && (not (String.contains db '/'))
  && (not (String.contains db '\000'))
  && not (has_dotdot db)

let validate_db db = if is_valid_db db then Ok () else Error (`Invalid_db db)

(* Map a database name to one filename component injectively, so names that
   differ only in case stay distinct on case-insensitive filesystems such
   as macOS volumes. '_' escapes as "__" and an uppercase letter as '_'
   followed by its lowercase form; the map is injective because every '_'
   in an encoded name starts an escape. *)
let encode_db db =
  let buf = Buffer.create (String.length db) in
  String.iter
    (fun c ->
      match c with
      | '_' -> Buffer.add_string buf "__"
      | 'A' .. 'Z' ->
          Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
      | _ -> Buffer.add_char buf c)
    db;
  Buffer.contents buf

(* The inverse of [encode_db] on the names it produces: every '_' in an encoded
   component starts an escape, so [__] is a literal underscore and [_a] is [A]. An
   encoded component never holds an uppercase letter. A component no name encodes
   to is [None]; the caller drops it rather than list a name the store would
   refuse. *)
let decode_db name =
  let buf = Buffer.create (String.length name) in
  let rec loop i =
    if i = String.length name then Some (Buffer.contents buf)
    else
      match name.[i] with
      | '_' when i + 1 = String.length name -> None
      | '_' -> (
          match name.[i + 1] with
          | '_' ->
              Buffer.add_char buf '_';
              loop (i + 2)
          | 'a' .. 'z' as c ->
              Buffer.add_char buf (Char.uppercase_ascii c);
              loop (i + 2)
          | _ -> None)
      | 'A' .. 'Z' -> None
      | c ->
          Buffer.add_char buf c;
          loop (i + 1)
  in
  loop 0

let db_path root db = Filename.concat root (encode_db db ^ ".json")

(* Generic JSON value helpers, matching the [Jsont.Json] convention already
   used elsewhere in this project (see lib/fantasy/anthropic_codec.ml). *)
let jmem k v = Jsont.Json.mem (Jsont.Json.name k) v
let jobj ms = Jsont.Json.object' ms
let jstr s = Jsont.Json.string s
let jbool b = Jsont.Json.bool b

let encode_value = function
  | Text s -> jobj [ jmem "v" (jstr s); jmem "b" (jbool false) ]
  | Binary raw ->
      jobj [ jmem "v" (jstr (Base64.encode_string raw)); jmem "b" (jbool true) ]

let decode_value ~db ~key j =
  match j with
  | Jsont.Object (ms, _) -> (
      match (Jsont.Json.find_mem "v" ms, Jsont.Json.find_mem "b" ms) with
      | Some (_, Jsont.String (v, _)), Some (_, Jsont.Bool (b, _)) -> (
          if not b then Ok (Text v)
          else
            match Base64.decode v with
            | Ok raw -> Ok (Binary raw)
            | Error (`Msg m) ->
                Error (`Corrupt (Fmt.str "%s: entry %S has invalid base64: %s" db key m)))
      | _ ->
          Error
            (`Corrupt
               (Fmt.str "%s: entry %S is missing a string \"v\" or boolean \"b\"" db key))
      )
  | _ -> Error (`Corrupt (Fmt.str "%s: entry %S is not a JSON object" db key))

let table_of_members ms =
  let t = Hashtbl.create (List.length ms + 8) in
  List.iter (fun ((name, _), v) -> Hashtbl.replace t name v) ms;
  t

let members_of_table t =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) t []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  |> List.map (fun (k, v) -> jmem k v)

(* Generous but bounded: protects against a runaway or hostile file without
   ever truncating a database this application will realistically produce. *)
let max_db_bytes = 64 * 1024 * 1024

(* The diagnostic one filesystem failure renders as. The store's [Io] contract
   carries a message string, and the errno triple is what distinguishes a
   refusal from a missing component on disk. *)
let io_message = function
  | Unix.Unix_error (code, name, arg) ->
      Fmt.str "%s (%s %s)" (Unix.error_message code) name arg
  | exn -> Printexc.to_string exn

let read_file path =
  Lwt.catch
    (fun () ->
      Lwt_io.with_file ~mode:Lwt_io.input path (fun channel ->
          Lwt_io.read ~count:(max_db_bytes + 1) channel)
      >|= fun body -> Some body)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_none
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn -> Lwt.fail exn)
  >|= function
  | None -> Ok None
  | Some body when String.length body > max_db_bytes ->
      Error (`Io "database file exceeds the read size limit")
  | Some body -> Ok (Some body)

type presence = Absent | Present of (string, Jsont.json) Hashtbl.t

let load_table ~db path =
  read_file path >>= function
  | Error _ as e -> Lwt.return e
  | Ok None -> Lwt.return (Ok Absent)
  | Ok (Some body) -> (
      match Jsont_bytesrw.decode_string Jsont.json body with
      | Error message -> Lwt.return (Error (`Corrupt (Fmt.str "%s: %s" db message)))
      | Ok (Jsont.Object (ms, _)) ->
          let rec validate = function
            | [] -> Ok (Present (table_of_members ms))
            | ((key, _), value) :: rest -> (
                match decode_value ~db ~key value with
                | Error _ as e -> e
                | Ok _ -> validate rest)
          in
          Lwt.return (validate ms)
      | Ok _ ->
          Lwt.return
            (Error (`Corrupt (Fmt.str "%s: top-level JSON value is not an object" db))))

let serialize t =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json
      (jobj (members_of_table t))
  with
  | Ok s -> Ok s
  | Error message -> Error (`Io (Fmt.str "database JSON encoding failed: %s" message))

(* A single lock also serializes callers using different paths to the same root. *)
let write_lock = Lwt_mutex.create ()
let max_temp_attempts = 8

(* A per-process random tag plus a monotonic counter: not a cryptographic
   requirement, just good enough that [`Exclusive] creation rarely collides.
   Atomicity itself comes from the filesystem's Unix.O_EXCL semantics, not from
   this tag being unique: a collision, ours or a second process's, is simply
   retried with a new candidate name. *)
let process_tag = Random.State.bits (Random.State.make_self_init ())
let temp_counter = ref 0

let next_temp_name base =
  incr temp_counter;
  Fmt.str "%s.tmp.%x.%x" base process_tag !temp_counter

let save_atomic dest body =
  let rec attempt dir base remaining =
    if remaining = 0 then
      Lwt.return (Error (`Io "could not create a unique temporary file"))
    else
      let tmp = Filename.concat dir (next_temp_name base) in
      let owned = ref false in
      Lwt.catch
        (fun () ->
          Lwt_unix.openfile tmp
            ([ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL ] @ Charamel_os.Fs.binary_flags)
            0o600
          >>= fun fd ->
          owned := true;
          let channel = Lwt_io.of_fd ~mode:Lwt_io.output fd in
          Lwt.finalize
            (fun () -> Lwt_io.write channel body)
            (fun () -> Lwt_io.close channel)
          >>= fun () ->
          Lwt_unix.rename tmp dest >|= fun () -> Ok ())
        (function
          | Unix.Unix_error (Unix.EEXIST, _, _) when not !owned ->
              attempt dir base (remaining - 1)
          | Lwt.Canceled -> Lwt.fail Lwt.Canceled
          | exn -> Lwt.return (Error (`Io (io_message exn))))
      >>= fun outcome ->
      if not !owned then Lwt.return outcome
      else
        Lwt.catch
          (fun () -> Lwt_unix.unlink tmp >|= fun () -> outcome)
          (function
            | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return outcome
            | Lwt.Canceled -> Lwt.fail Lwt.Canceled
            | exn ->
                Lwt.return
                  (Error
                     (`Io (Fmt.str "temporary file cleanup failed: %s" (io_message exn)))))
  in
  let dir, base = (Filename.dirname dest, Filename.basename dest) in
  attempt dir base max_temp_attempts

(* Create [path] and every missing ancestor with [perm]; a directory already
   being in place is fine, anything else occupying the name is an error. *)
let mkdir_exists perm path =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir path perm >|= fun () -> Ok ())
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
          Lwt.catch
            (fun () ->
              Lwt_unix.stat path >|= fun stats ->
              if stats.Unix.st_kind = Unix.S_DIR then Ok () else Error Unix.ENOTDIR)
            (fun _ -> Lwt.return (Error Unix.ENOTDIR))
      | Unix.Unix_error (code, _, _) -> Lwt.return (Error code)
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn -> Lwt.fail exn)

let ensure_dir ~perm path =
  let rec up path =
    if path = Filename.dir_sep || path = "." then Lwt.return (Ok ())
    else
      mkdir_exists perm path >>= function
      | Ok () -> Lwt.return (Ok ())
      | Error Unix.ENOENT when not (String.equal (Filename.dirname path) path) -> (
          up (Filename.dirname path) >>= function
          | Ok () -> mkdir_exists perm path
          | Error _ as e -> Lwt.return e)
      | Error code -> Lwt.return (Error code)
  in
  up path

let ensure_root root =
  ensure_dir ~perm:0o700 root >>= function
  | Ok () -> Lwt.return (Ok ())
  | Error code -> Lwt.return (Error (`Io (Unix.error_message code)))

(* The [b] flag is determined solely by [String.is_valid_utf_8]; callers never
   choose it (see the .mli). *)
let classify data = if String.is_valid_utf_8 data then Text data else Binary data

let set ~root ~db key data =
  match validate_db db with
  | Error _ as e -> Lwt.return e
  | Ok () ->
      let path = db_path root db in
      Lwt_mutex.with_lock write_lock (fun () ->
          ensure_root root >>= function
          | Error _ as e -> Lwt.return e
          | Ok () -> (
              load_table ~db path >>= function
              | Error _ as e -> Lwt.return e
              | Ok presence -> (
                  let t =
                    match presence with Absent -> Hashtbl.create 8 | Present t -> t
                  in
                  Hashtbl.replace t key (encode_value (classify data));
                  match serialize t with
                  | Ok body -> save_atomic path body
                  | Error _ as e -> Lwt.return e)))

let get ~root ~db key =
  match validate_db db with
  | Error _ as e -> Lwt.return e
  | Ok () -> (
      load_table ~db (db_path root db) >>= function
      | Error _ as e -> Lwt.return e
      | Ok Absent -> Lwt.return (Error (`No_such_db db))
      | Ok (Present t) -> (
          match Hashtbl.find_opt t key with
          | None -> Lwt.return (Error (`No_such_key key))
          | Some j -> Lwt.return (decode_value ~db ~key j)))

let delete ~root ~db key =
  match validate_db db with
  | Error _ as e -> Lwt.return e
  | Ok () ->
      let path = db_path root db in
      Lwt_mutex.with_lock write_lock (fun () ->
          load_table ~db path >>= function
          | Error _ as e -> Lwt.return e
          | Ok Absent -> Lwt.return (Error (`No_such_db db))
          | Ok (Present t) ->
              if not (Hashtbl.mem t key) then Lwt.return (Error (`No_such_key key))
              else (
                Hashtbl.remove t key;
                match serialize t with
                | Ok body -> save_atomic path body
                | Error _ as e -> Lwt.return e))

let list ~root ~db =
  match validate_db db with
  | Error _ as e -> Lwt.return e
  | Ok () -> (
      load_table ~db (db_path root db) >>= function
      | Error _ as e -> Lwt.return e
      | Ok Absent -> Lwt.return (Ok [])
      | Ok (Present t) ->
          let entries = Hashtbl.fold (fun k j acc -> (k, j) :: acc) t [] in
          let rec collect acc = function
            | [] -> Lwt.return (Ok (List.rev acc))
            | (k, j) :: rest -> (
                match decode_value ~db ~key:k j with
                | Error _ as e -> Lwt.return e
                | Ok v -> collect ((k, v) :: acc) rest)
          in
          collect [] entries
          >|= Result.map (List.sort (fun (a, _) (b, _) -> String.compare a b)))

let delete_db ~root ~db =
  match validate_db db with
  | Error _ as e -> Lwt.return e
  | Ok () ->
      let path = db_path root db in
      Lwt_mutex.with_lock write_lock (fun () ->
          Lwt.catch
            (fun () -> Lwt_unix.unlink path >|= fun () -> Ok ())
            (function
              | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Error (`No_such_db db))
              | Lwt.Canceled -> Lwt.fail Lwt.Canceled
              | exn -> Lwt.return (Error (`Io (io_message exn)))))

let database_of_file name =
  match Filename.chop_suffix_opt ~suffix:".json" name with
  | None -> None
  | Some stem -> (
      match decode_db stem with Some db when is_valid_db db -> Some db | _ -> None)

let dbs ~root =
  Lwt.catch
    (fun () ->
      Lwt_stream.to_list (Lwt_unix.files_of_directory root) >|= fun names ->
      Ok (List.sort String.compare (List.filter_map database_of_file names)))
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return (Ok [])
      | Lwt.Canceled -> Lwt.fail Lwt.Canceled
      | exn -> Lwt.return (Error (`Io (io_message exn))))
