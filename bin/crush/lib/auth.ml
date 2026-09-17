open Result.Syntax

type credential =
  | Api_key of string
  | Oauth of Charm_fantasy.Oauth.Credential.t
  | Disabled of { reason : string; at_ms : int }

type error = [ `Io of string * string | `Parse of string * string ]

type refresh_error =
  [ error | `Disabled of string | `No_credential | `Refresh of Charm_fantasy.Error.t ]

type login_error = [ error | `Oauth of string | `Timeout | `Aborted ]

type t = {
  path : Eio.Fs.dir_ty Eio.Path.t;
  parent : Eio.Fs.dir_ty Eio.Path.t;
  lock_path : Eio.Fs.dir_ty Eio.Path.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  mutable entries : (string * credential) list;
  mutex : Eio.Mutex.t;
  mutable persistence_fault : error option;
}

let pp_error ppf = function
  | `Io (path, message) -> Fmt.pf ppf "auth I/O error at %s: %s" path message
  | `Parse (path, message) -> Fmt.pf ppf "auth JSON error at %s: %s" path message

let pp_refresh_error ppf = function
  | (`Io _ | `Parse _) as error -> pp_error ppf error
  | `Disabled reason -> Fmt.pf ppf "provider credential disabled: %s" reason
  | `No_credential -> Fmt.string ppf "no credential"
  | `Refresh error -> Charm_fantasy.Error.pp ppf error

let path () = Filename.concat (Charm_cli.Xdg.config_dir ~app:"crush") "auth.json"

let path_text path =
  Option.value (Eio.Path.native path) ~default:(Fmt.str "%a" Eio.Path.pp path)

let json_object = function Jsont.Object (members, _) -> Some members | _ -> None

let credential_of_json json =
  match json_object json with
  | None -> Error "credential must be an object"
  | Some _ -> (
      match Jsonx.string_member "type" json with
      | Some "api_key" -> (
          match Jsonx.string_member "key" json with
          | Some key when String.trim key <> "" -> Ok (Api_key key)
          | _ -> Error "api_key credential requires a non-empty key")
      | Some "oauth" -> (
          match
            ( Jsonx.string_member "access" json,
              Jsonx.string_member "refresh" json,
              Jsonx.int_member "expires_at_ms" json,
              Jsonx.member "account" json )
          with
          | Some access, Some refresh, Some expires_at_ms, Some (Jsont.Null _) ->
              if access = "" || refresh = "" then Error "oauth tokens must be non-empty"
              else Ok (Oauth { access; refresh; expires_at_ms; account = None })
          | ( Some access,
              Some refresh,
              Some expires_at_ms,
              Some (Jsont.String (account, _)) )
            when access <> "" && refresh <> "" && account <> "" ->
              Ok (Oauth { access; refresh; expires_at_ms; account = Some account })
          | _ ->
              Error "oauth credential requires access, refresh, expires_at_ms and account"
          )
      | Some "disabled" -> (
          match (Jsonx.string_member "reason" json, Jsonx.int_member "at_ms" json) with
          | Some reason, Some at_ms when reason <> "" -> Ok (Disabled { reason; at_ms })
          | _ -> Error "disabled credential requires reason and at_ms")
      | Some kind -> Error (Fmt.str "unknown credential type %S" kind)
      | None -> Error "credential type is missing")

let member name value = Jsont.Json.mem (Jsont.Json.name name) value
let string value = Jsont.Json.string value
let number value = Jsont.Json.number (float_of_int value)

let credential_to_json = function
  | Api_key key ->
      Jsont.Json.object' [ member "type" (string "api_key"); member "key" (string key) ]
  | Oauth { access; refresh; expires_at_ms; account } ->
      let account =
        match account with None -> Jsont.Json.null () | Some value -> string value
      in
      Jsont.Json.object'
        [
          member "type" (string "oauth");
          member "access" (string access);
          member "refresh" (string refresh);
          member "expires_at_ms" (number expires_at_ms);
          member "account" account;
        ]
  | Disabled { reason; at_ms } ->
      Jsont.Json.object'
        [
          member "type" (string "disabled");
          member "reason" (string reason);
          member "at_ms" (number at_ms);
        ]

let decode_store json =
  match json with
  | Jsont.Object (members, _) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | ((name, _), value) :: rest -> (
            match credential_of_json value with
            | Ok credential -> loop ((name, credential) :: acc) rest
            | Error message -> Error (Fmt.str "provider %s: %s" name message))
      in
      loop [] members
  | _ -> Error "auth file must be a JSON object"

let encode_store entries =
  Jsont.Json.object'
    (List.map (fun (name, value) -> member name (credential_to_json value)) entries)

let jsont : (string * credential) list Jsont.t =
  Jsont.map
    ~dec:(fun json ->
      match decode_store json with
      | Ok entries -> entries
      | Error message -> Jsont.Error.msg Jsont.Meta.none message)
    ~enc:encode_store Jsont.json

let io_message exn =
  match exn with
  | Eio.Io _ -> Fmt.str "%a" Eio.Exn.pp exn
  | Unix.Unix_error (error, fn, arg) ->
      Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg
  | End_of_file -> "unexpected end of file"
  | Invalid_argument message -> message
  | Failure message -> message
  | exn -> Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())

let with_io_error path f =
  try f () with
  | Eio.Io _ as exn -> Error (`Io (path, io_message exn))
  | Unix.Unix_error (error, fn, arg) ->
      Error (`Io (path, Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg))
  | End_of_file -> Error (`Io (path, "unexpected end of file"))
  | Invalid_argument message -> Error (`Io (path, message))
  | Failure message -> Error (`Io (path, message))
  | exn -> Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())

let load_entries path =
  let filename = path_text path in
  with_io_error filename (fun () ->
      match Eio.Path.kind ~follow:false path with
      | `Not_found -> Ok []
      | `Regular_file -> (
          let stat = Eio.Path.stat ~follow:false path in
          if Optint.Int63.to_int stat.Eio.File.Stat.size > 1_048_576 then
            Error (`Parse (filename, "credential file exceeds 1 MiB"))
          else
            match Jsonx.decode jsont (Eio.Path.load path) with
            | Ok entries -> Ok entries
            | Error message -> Error (`Parse (filename, message)))
      | _ -> Error (`Io (filename, "credential path is not a regular file")))

let persist_entries t entries =
  let filename = path_text t.path in
  with_io_error filename (fun () ->
      let encoded = Jsonx.encode jsont entries in
      match State_file.replace t.path encoded with
      | Ok () -> Ok ()
      | Error (`Io (target, message)) -> Error (`Io (target, message)))

let close_fd fd =
  try Eio_unix.run_in_systhread (fun () -> Unix.close fd) with
  | Unix.Unix_error _ -> ()
  | Eio.Io _ -> ()

let release_file_lock fd =
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> close_fd fd))
    (fun () ->
      Eio.Cancel.protect (fun () ->
          Eio_unix.run_in_systhread (fun () -> Unix.lockf fd Unix.F_ULOCK 0)))

let release_file_lock_safely fd =
  try release_file_lock fd with Unix.Unix_error _ -> () | Eio.Io _ -> ()

let acquire_file_lock t =
  let filename = path_text t.lock_path in
  with_io_error filename (fun () ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 t.parent;
      match Eio.Path.native t.lock_path with
      | None -> Error (`Io (filename, "lock path is not native"))
      | Some native ->
          let fd =
            Eio_unix.run_in_systhread (fun () ->
                Unix.openfile native [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600)
          in
          let locked = ref false in
          Fun.protect
            ~finally:(fun () ->
              if not !locked then Eio.Cancel.protect (fun () -> close_fd fd))
            (fun () ->
              let rec wait_for_lock () =
                try
                  Eio_unix.run_in_systhread (fun () -> Unix.lockf fd Unix.F_TLOCK 0);
                  locked := true
                with
                | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
                    Eio.Time.sleep t.clock 0.01;
                    wait_for_lock ()
                | Unix.Unix_error (Unix.EINTR, _, _) -> wait_for_lock ()
              in
              wait_for_lock ();
              Ok fd))

let with_mutex t f =
  Eio.Mutex.lock t.mutex;
  Fun.protect
    ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Mutex.unlock t.mutex))
    f

let with_loaded t ~on_fault f =
  with_mutex t (fun () ->
      match t.persistence_fault with
      | Some error -> on_fault error
      | None -> (
          match acquire_file_lock t with
          | Error error -> on_fault error
          | Ok fd ->
              Fun.protect
                ~finally:(fun () -> release_file_lock_safely fd)
                (fun () ->
                  match load_entries t.path with
                  | Error error -> on_fault error
                  | Ok entries -> f ~previous:t.entries entries)))

let create ~path ~clock () =
  match Eio.Path.split path with
  | None -> Error (`Io (path_text path, "credential path has no parent directory"))
  | Some (parent, basename) ->
      let lock_path = Eio.Path.(parent / (basename ^ ".lock")) in
      let* entries = load_entries path in
      Ok
        {
          path;
          parent;
          lock_path;
          clock;
          entries;
          mutex = Eio.Mutex.create ();
          persistence_fault = None;
        }

let find t ~provider = List.assoc_opt provider t.entries
let providers t = List.map fst t.entries

let replace_entry provider credential entries =
  let rec loop seen = function
    | [] -> List.rev_append seen [ (provider, credential) ]
    | (name, _) :: rest when String.equal name provider ->
        List.rev_append seen ((provider, credential) :: rest)
    | item :: rest -> loop (item :: seen) rest
  in
  loop [] entries

let remove_entry provider entries =
  List.filter (fun (name, _) -> not (String.equal name provider)) entries

let to_fantasy = function
  | Api_key key -> Some (Charm_fantasy.Provider.Api_key key)
  | Oauth credential -> Some (Charm_fantasy.Provider.Oauth credential)
  | Disabled _ -> None

let nonempty = function
  | Some value when String.trim value <> "" -> Some value
  | _ -> None

let env_name provider =
  let b = Bytes.of_string (String.uppercase_ascii provider) in
  for index = 0 to Bytes.length b - 1 do
    if Bytes.get b index = '-' then Bytes.set b index '_'
  done;
  Bytes.to_string b ^ "_API_KEY"

let conventional_env provider =
  match String.lowercase_ascii provider with
  | "anthropic" -> [ "ANTHROPIC_API_KEY" ]
  | "openai" -> [ "OPENAI_API_KEY" ]
  | "google" | "gemini" -> [ "GEMINI_API_KEY" ]
  | "openrouter" -> [ "OPENROUTER_API_KEY" ]
  | _ -> []

let resolve_entries entries ~config ~env ~provider =
  match List.assoc_opt provider entries with
  | Some credential -> Some credential
  | None -> (
      let names = env_name provider :: conventional_env provider in
      let environment_key () =
        List.find_map
          (fun name -> Option.map (fun key -> Api_key key) (nonempty (env name)))
          names
      in
      match List.assoc_opt provider config.Config.providers with
      | Some configured -> (
          match nonempty configured.Config.api_key with
          | Some key -> Some (Api_key key)
          | None -> environment_key ())
      | None -> environment_key ())

let resolve t ~config ~env ~provider =
  with_loaded t
    ~on_fault:(fun error -> Error error)
    (fun ~previous:_ entries ->
      t.entries <- entries;
      Ok (resolve_entries entries ~config ~env ~provider))

let validate_credential t = function
  | Api_key key when String.trim key = "" ->
      Error (`Io (path_text t.path, "api_key credential requires a non-empty key"))
  | Oauth { access; refresh; _ } when access = "" || refresh = "" ->
      Error (`Io (path_text t.path, "oauth tokens must be non-empty"))
  | Disabled { reason; _ } when reason = "" ->
      Error (`Io (path_text t.path, "disabled credential requires a reason"))
  | _ -> Ok ()

let set t ~provider credential =
  let* () = validate_credential t credential in
  with_loaded t
    ~on_fault:(fun error -> Error error)
    (fun ~previous entries ->
      let updated = replace_entry provider credential entries in
      match Eio.Cancel.protect (fun () -> persist_entries t updated) with
      | Ok () ->
          t.entries <- updated;
          Ok ()
      | Error error ->
          t.entries <- previous;
          Error error)

let remove t ~provider =
  with_loaded t
    ~on_fault:(fun error -> Error error)
    (fun ~previous entries ->
      let updated = remove_entry provider entries in
      match Eio.Cancel.protect (fun () -> persist_entries t updated) with
      | Ok () ->
          t.entries <- updated;
          Ok ()
      | Error error ->
          t.entries <- previous;
          Error error)

let now_ms t = int_of_float (Eio.Time.now t.clock *. 1000.)

let definitive_refresh_error = function
  | (`Oauth_invalid_grant reason : Charm_fantasy.Error.t) -> Some reason
  | `Http ({ status = 401; message; _ } : Charm_fantasy.Error.http_error) -> Some message
  | _ -> None

let auth_equal left right = left = right

let refresh_oauth ~force ~sw ~net t ~provider ~entries old :
    (Charm_fantasy.Provider.auth, refresh_error) result =
  let outcome =
    if force then Charm_fantasy.Oauth.Anthropic.refresh ~sw ~clock:t.clock ~net old
    else Charm_fantasy.Oauth.Anthropic.ensure_fresh ~sw ~clock:t.clock ~net old
  in
  match outcome with
  | Ok fresh ->
      let updated = replace_entry provider (Oauth fresh) entries in
      if fresh = old then (
        t.entries <- entries;
        Ok (Charm_fantasy.Provider.Oauth fresh))
      else (
        t.entries <- updated;
        match Eio.Cancel.protect (fun () -> persist_entries t updated) with
        | Ok () -> Ok (Charm_fantasy.Provider.Oauth fresh)
        | Error error ->
            t.persistence_fault <- Some error;
            Error error)
  | Error oauth_error -> (
      match definitive_refresh_error oauth_error with
      | None -> Error (`Refresh oauth_error)
      | Some reason -> (
          let disabled =
            replace_entry provider (Disabled { reason; at_ms = now_ms t }) entries
          in
          t.entries <- disabled;
          match Eio.Cancel.protect (fun () -> persist_entries t disabled) with
          | Ok () -> Error (`Disabled reason)
          | Error error ->
              t.persistence_fault <- Some error;
              Error error))

let ensure_fresh ~sw ~net ~config ~env t ~provider :
    (Charm_fantasy.Provider.auth, refresh_error) result =
  let callback ~previous:_ entries : (_, refresh_error) result =
    t.entries <- entries;
    match resolve_entries entries ~config ~env ~provider with
    | None -> Error `No_credential
    | Some (Disabled { reason; _ }) -> Error (`Disabled reason)
    | Some (Api_key key) -> Ok (Charm_fantasy.Provider.Api_key key)
    | Some (Oauth old) ->
        Eio.Cancel.protect (fun () ->
            refresh_oauth ~force:false ~sw ~net t ~provider ~entries old)
  in
  with_loaded t ~on_fault:(fun error -> Error (error :> refresh_error)) callback

let refresh ~sw ~net ~config ~env t ~provider ~rejected :
    (Charm_fantasy.Provider.auth, refresh_error) result =
  let callback ~previous:_ entries : (_, refresh_error) result =
    t.entries <- entries;
    match resolve_entries entries ~config ~env ~provider with
    | None -> Error `No_credential
    | Some (Disabled { reason; _ }) -> Error (`Disabled reason)
    | Some (Api_key key) -> Ok (Charm_fantasy.Provider.Api_key key)
    | Some (Oauth old) ->
        let current = Charm_fantasy.Provider.Oauth old in
        let force = auth_equal current rejected in
        Eio.Cancel.protect (fun () ->
            refresh_oauth ~force ~sw ~net t ~provider ~entries old)
  in
  with_loaded t ~on_fault:(fun error -> Error (error :> refresh_error)) callback

let disable t ~provider ~rejected ~reason ~now_ms =
  if reason = "" then
    Error (`Io (path_text t.path, "disabled credential requires a reason"))
  else
    with_loaded t
      ~on_fault:(fun error -> Error error)
      (fun ~previous:_ entries ->
        t.entries <- entries;
        match List.assoc_opt provider entries with
        | None -> Ok ()
        | Some (Disabled _) -> Ok ()
        | Some current -> (
            match to_fantasy current with
            | Some current_auth when auth_equal current_auth rejected -> (
                let updated =
                  replace_entry provider (Disabled { reason; at_ms = now_ms }) entries
                in
                t.entries <- updated;
                match Eio.Cancel.protect (fun () -> persist_entries t updated) with
                | Ok () -> Ok ()
                | Error error ->
                    t.persistence_fault <- Some error;
                    Error error)
            | _ -> Ok ()))

module Login = struct
  let port = 54545
  let redirect_uri = "http://127.0.0.1:54545/callback"

  type race =
    | Callback of string
    | Paste of string
    | Aborted
    | Timeout
    | Accept_error of string

  let close_flow flow =
    try Eio.Cancel.protect (fun () -> Eio.Flow.close flow)
    with Eio.Io _ | Unix.Unix_error _ -> ()

  let response flow ~status body =
    let text =
      Fmt.str
        "HTTP/1.1 %d\r\n\
         Content-Type: text/html; charset=utf-8\r\n\
         Content-Length: %d\r\n\
         Connection: close\r\n\
         \r\n\
         %s"
        status (String.length body) body
    in
    try Eio.Flow.copy_string text flow with Eio.Io _ | Unix.Unix_error _ -> ()

  let request_path line =
    match String.split_on_char ' ' (String.trim line) with
    | [ "GET"; target; version ] when String.starts_with ~prefix:"HTTP/" version ->
        Some target
    | _ -> None

  let drain_headers reader =
    let rec loop bytes lines =
      if bytes > 8192 || lines > 128 then false
      else
        match Eio.Buf_read.line reader with
        | line when line = "" -> true
        | line -> loop (bytes + String.length line + 1) (lines + 1)
    in
    loop 0 0

  let anthropic ~sw ~net ~open_browser ~prompt_paste t =
    let login = Charm_fantasy.Oauth.Anthropic.begin_login ~redirect_uri () in
    let listener_result =
      try
        Ok
          (Eio.Net.listen ~reuse_addr:true ~backlog:8 ~sw net
             (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)))
      with
      | Eio.Io _ as exn -> Error (`Oauth (Fmt.str "%a" Eio.Exn.pp exn))
      | Unix.Unix_error (error, fn, arg) ->
          Error (`Oauth (Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg))
    in
    let* socket = listener_result in
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Resource.close socket))
      (fun () ->
        let parse_connection flow =
          Fun.protect
            ~finally:(fun () -> close_flow flow)
            (fun () ->
              let reader = Eio.Buf_read.of_flow ~max_size:8192 flow in
              let outcome =
                try
                  let timed =
                    Eio.Time.with_timeout t.clock 10. (fun () ->
                        Ok
                          (try
                             let line = Eio.Buf_read.line reader in
                             let headers_ok = drain_headers reader in
                             if not headers_ok then None
                             else
                               Option.bind (request_path line) (fun target ->
                                   let uri =
                                     Uri.of_string ("http://127.0.0.1" ^ target)
                                   in
                                   if Uri.path uri <> "/callback" then None
                                   else
                                     match
                                       Charm_fantasy.Oauth.Anthropic.extract_code
                                         ~url_or_code:(Uri.to_string uri)
                                         ~state:login.Charm_fantasy__Oauth.Anthropic.state
                                     with
                                     | Ok code -> Some code
                                     | Error _ -> None)
                           with
                          | End_of_file -> None
                          | Eio.Buf_read.Buffer_limit_exceeded -> None
                          | Eio.Io _ -> None
                          | Unix.Unix_error _ -> None
                          | Invalid_argument _ -> None))
                  in
                  match timed with Ok outcome -> outcome | Error `Timeout -> None
                with
                | Eio.Io _ -> None
                | Unix.Unix_error _ -> None
              in
              match outcome with
              | Some code ->
                  response flow ~status:200
                    "<html>Login complete. You can return to Crush.</html>";
                  Some code
              | None ->
                  response flow ~status:400
                    "<html>Invalid callback. Please try again.</html>";
                  None)
        in
        let callback () =
          let rec wait () =
            let flow, _ = Eio.Net.accept ~sw socket in
            match parse_connection flow with
            | Some code -> Callback code
            | None -> wait ()
          in
          try wait () with
          | Eio.Io _ as exn -> Accept_error (Fmt.str "%a" Eio.Exn.pp exn)
          | Unix.Unix_error (error, fn, arg) ->
              Accept_error (Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg)
        in
        let paste () =
          match
            try prompt_paste () with
            | End_of_file -> None
            | Eio.Io _ -> None
            | Unix.Unix_error _ -> None
          with
          | None -> Aborted
          | Some value -> Paste value
        in
        open_browser login.Charm_fantasy__Oauth.Anthropic.uri;
        let timeout () =
          Eio.Time.sleep t.clock 300.;
          Timeout
        in
        match Eio.Fiber.any [ callback; paste; timeout ] with
        | Timeout -> Error `Timeout
        | Aborted -> Error `Aborted
        | Accept_error message -> Error (`Oauth message)
        | Callback code | Paste code -> (
            match
              Charm_fantasy.Oauth.Anthropic.exchange ~sw ~clock:t.clock ~net ~redirect_uri
                ~login ~code ()
            with
            | Error oauth_error ->
                Error (`Oauth (Charm_fantasy.Error.message oauth_error))
            | Ok credential -> (
                match set t ~provider:"anthropic" (Oauth credential) with
                | Ok () -> Ok ()
                | Error error -> (Error (error :> login_error) : (_, login_error) result))
            ))
end
