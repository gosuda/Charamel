type error = [ `Io of string * string ]

open Result.Syntax

let counter = Atomic.make 0
let path_text path = Option.value (Eio.Path.native path) ~default:"<state file>"
let pp_error ppf (`Io (path, message)) = Fmt.pf ppf "cannot replace %s: %s" path message

let error_of path exn =
  try raise exn with
  | Eio.Io (Eio.Fs.E _, _) as exn -> `Io (path, Fmt.str "%a" Eio.Exn.pp exn)
  | Unix.Unix_error (error, function_name, argument) ->
      `Io (path, Fmt.str "%s (%s %s)" (Unix.error_message error) function_name argument)

let cleanup path =
  Eio.Cancel.protect (fun () ->
      try Eio.Path.unlink ~missing_ok:true path with
      | Eio.Io (Eio.Fs.E _, _) -> ()
      | Unix.Unix_error _ -> ())

let make_temp parent basename attempt =
  let nonce = Atomic.fetch_and_add counter 1 in
  let name =
    Fmt.str ".%s.crush-state.%d.%d" basename (Unix.getpid ()) (nonce + attempt)
  in
  Eio.Path.(parent / name)

let create_temp parent basename contents =
  let rec attempt count =
    let temporary = make_temp parent basename count in
    let created = ref false in
    try
      Eio.Path.with_open_out ~create:(`Exclusive 0o600) temporary (fun flow ->
          created := true;
          Eio.Flow.copy_string contents flow);
      Ok temporary
    with
    | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> attempt (count + 1)
    | Eio.Io _ as exn ->
        if !created then cleanup temporary;
        raise exn
    | Unix.Unix_error _ as exn ->
        if !created then cleanup temporary;
        raise exn
  in
  attempt 0

let replace destination contents =
  let target = path_text destination in
  match Eio.Path.split destination with
  | None -> Error (`Io (target, "destination has no parent directory"))
  | Some (parent, basename) ->
      let temporary = ref None in
      let committed = ref false in
      let operation () =
        let* path = create_temp parent basename contents in
        temporary := Some path;
        Eio.Path.rename path destination;
        committed := true;
        Ok ()
      in
      Fun.protect
        ~finally:(fun () ->
          match (!temporary, !committed) with Some path, false -> cleanup path | _ -> ())
        (fun () ->
          try Eio.Cancel.protect operation with
          | Eio.Io (Eio.Fs.E _, _) as exn -> Error (error_of target exn)
          | Unix.Unix_error _ as exn -> Error (error_of target exn))
