let ( let* ) = Result.bind

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

let db_path root db = Eio.Path.(root / (encode_db db ^ ".json"))

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

let read_file path =
  match
    Eio.Path.with_open_in path (fun flow ->
        Eio.Buf_read.parse ~max_size:(max_db_bytes + 1) Eio.Buf_read.take_all flow)
  with
  | Ok body when String.length body > max_db_bytes ->
      Error (`Io "database file exceeds the read size limit")
  | Ok body -> Ok (Some body)
  | Error (`Msg _) -> Error (`Io "database file exceeds the read size limit")
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok None
  | exception (Eio.Io (Eio.Fs.E _, _) as exn) ->
      Eio.Fiber.check ();
      Error (`Io (Fmt.str "%a" Eio.Exn.pp exn))

type presence = Absent | Present of (string, Jsont.json) Hashtbl.t

let load_table ~db path =
  match read_file path with
  | Error _ as e -> e
  | Ok None -> Ok Absent
  | Ok (Some body) -> (
      match Jsont_bytesrw.decode_string Jsont.json body with
      | Error message -> Error (`Corrupt (Fmt.str "%s: %s" db message))
      | Ok (Jsont.Object (ms, _)) ->
          let rec validate = function
            | [] -> Ok (Present (table_of_members ms))
            | ((key, _), value) :: rest ->
                let* _ = decode_value ~db ~key value in
                validate rest
          in
          validate ms
      | Ok _ -> Error (`Corrupt (Fmt.str "%s: top-level JSON value is not an object" db)))

let serialize t =
  match
    Jsont_bytesrw.encode_string ~format:Jsont.Indent Jsont.json
      (jobj (members_of_table t))
  with
  | Ok s -> Ok s
  | Error message -> Error (`Io (Fmt.str "database JSON encoding failed: %s" message))

(* A single lock also serializes callers using different paths to the same root. *)
let write_lock = Eio.Mutex.create ()
let max_temp_attempts = 8

(* A per-process random tag plus a monotonic counter: not a cryptographic
   requirement, just good enough that [`Exclusive] creation rarely collides.
   Atomicity itself comes from the filesystem's O_EXCL semantics, not from
   this tag being unique: a collision, ours or a second process's, is simply
   retried with a new candidate name. *)
let process_tag = Random.State.bits (Random.State.make_self_init ())
let temp_counter = ref 0

let next_temp_name base =
  incr temp_counter;
  Fmt.str "%s.tmp.%x.%x" base process_tag !temp_counter

let save_atomic dest body =
  let rec attempt dir base remaining =
    if remaining = 0 then Error (`Io "could not create a unique temporary file")
    else
      let tmp = Eio.Path.(dir / next_temp_name base) in
      let owned = ref false in
      let outcome =
        try
          Eio.Path.with_open_out ~create:(`Exclusive 0o600) tmp (fun flow ->
              owned := true;
              Eio.Flow.copy_string body flow);
          Eio.Path.rename tmp dest;
          owned := false;
          Ok ()
        with
        | Eio.Io (Eio.Fs.E (Already_exists _), _) when not !owned ->
            attempt dir base (remaining - 1)
        | Eio.Io (Eio.Fs.E _, _) as exn -> Error (`Io (Fmt.str "%a" Eio.Exn.pp exn))
      in
      if not !owned then outcome
      else
        match Eio.Path.unlink ~missing_ok:true tmp with
        | () -> outcome
        | exception (Eio.Io (Eio.Fs.E _, _) as exn) ->
            Error (`Io (Fmt.str "temporary file cleanup failed: %a" Eio.Exn.pp exn))
  in
  Eio.Cancel.protect (fun () ->
      match Eio.Path.split dest with
      | None -> Error (`Io "destination path has no basename")
      | Some (dir, base) -> attempt dir base max_temp_attempts)

let ensure_root root =
  try
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
    Ok ()
  with Eio.Io (Eio.Fs.E _, _) as exn ->
    Eio.Fiber.check ();
    Error (`Io (Fmt.str "%a" Eio.Exn.pp exn))

(* The [b] flag is determined solely by [String.is_valid_utf_8]; callers never
   choose it (see the .mli). *)
let classify data = if String.is_valid_utf_8 data then Text data else Binary data

let set ~root ~db key data =
  let* () = validate_db db in
  let path = db_path root db in
  Eio.Mutex.use_rw ~protect:true write_lock (fun () ->
      let* () = ensure_root root in
      let* presence = load_table ~db path in
      let t = match presence with Absent -> Hashtbl.create 8 | Present t -> t in
      Hashtbl.replace t key (encode_value (classify data));
      let* body = serialize t in
      save_atomic path body)

let get ~root ~db key =
  let* () = validate_db db in
  match load_table ~db (db_path root db) with
  | Error _ as e -> e
  | Ok Absent -> Error (`No_such_db db)
  | Ok (Present t) -> (
      match Hashtbl.find_opt t key with
      | None -> Error (`No_such_key key)
      | Some j -> decode_value ~db ~key j)

let delete ~root ~db key =
  let* () = validate_db db in
  let path = db_path root db in
  Eio.Mutex.use_rw ~protect:true write_lock (fun () ->
      match load_table ~db path with
      | Error _ as e -> e
      | Ok Absent -> Error (`No_such_db db)
      | Ok (Present t) ->
          if not (Hashtbl.mem t key) then Error (`No_such_key key)
          else (
            Hashtbl.remove t key;
            let* body = serialize t in
            save_atomic path body))

let list ~root ~db =
  let* () = validate_db db in
  match load_table ~db (db_path root db) with
  | Error _ as e -> e
  | Ok Absent -> Ok []
  | Ok (Present t) ->
      let entries = Hashtbl.fold (fun k j acc -> (k, j) :: acc) t [] in
      let rec collect acc = function
        | [] -> Ok (List.rev acc)
        | (k, j) :: rest -> (
            match decode_value ~db ~key:k j with
            | Error _ as e -> e
            | Ok v -> collect ((k, v) :: acc) rest)
      in
      Result.map
        (List.sort (fun (a, _) (b, _) -> String.compare a b))
        (collect [] entries)

let delete_db ~root ~db =
  let* () = validate_db db in
  let path = db_path root db in
  Eio.Mutex.use_rw ~protect:true write_lock (fun () ->
      try
        Eio.Path.unlink ~missing_ok:false path;
        Ok ()
      with
      | Eio.Io (Eio.Fs.E (Not_found _), _) -> Error (`No_such_db db)
      | Eio.Io (Eio.Fs.E _, _) as exn ->
          Eio.Fiber.check ();
          Error (`Io (Fmt.str "%a" Eio.Exn.pp exn)))

let dbs ~root =
  let suffix = ".json" in
  let slen = String.length suffix in
  let strip_suffix name =
    let nlen = String.length name in
    if nlen > slen && String.equal (String.sub name (nlen - slen) slen) suffix then
      Some (String.sub name 0 (nlen - slen))
    else None
  in
  match Eio.Path.read_dir root with
  | names ->
      let rec collect acc = function
        | [] -> acc
        | name :: rest -> (
            match strip_suffix name with
            | None -> collect acc rest
            | Some db -> collect (db :: acc) rest)
      in
      Ok (List.sort String.compare (collect [] names))
  | exception Eio.Io (Eio.Fs.E (Not_found _), _) -> Ok []
  | exception (Eio.Io (Eio.Fs.E _, _) as exn) ->
      Eio.Fiber.check ();
      Error (`Io (Fmt.str "%a" Eio.Exn.pp exn))
