open Lwt.Infix
open Lwt_result.Syntax

type credential =
  | Api_key of string
  | Oauth of Charamel_fantasy.Oauth.Credential.t
  | Disabled of { reason : string; at_ms : int }

type error = [ `Io of string * string | `Parse of string * string ]

type refresh_error =
  [ error | `Disabled of string | `No_credential | `Refresh of Charamel_fantasy.Error.t ]

type login_error = [ error | `Oauth of string | `Timeout | `Aborted ]

type t = {
  path : string;
  parent : string;
  lock_path : string;
  clock : Charamel_os.Time.clock;
  mutable entries : (string * credential) list;
  mutex : Lwt_mutex.t;
  mutable persistence_fault : error option;
}

let pp_error ppf = function
  | `Io (path, message) -> Fmt.pf ppf "auth I/O error at %s: %s" path message
  | `Parse (path, message) -> Fmt.pf ppf "auth JSON error at %s: %s" path message

let pp_refresh_error ppf = function
  | (`Io _ | `Parse _) as error -> pp_error ppf error
  | `Disabled reason -> Fmt.pf ppf "provider credential disabled: %s" reason
  | `No_credential -> Fmt.string ppf "no credential"
  | `Refresh error -> Charamel_fantasy.Error.pp ppf error

let path () = Filename.concat (Charamel_cli.Xdg.config_dir ~app:"crush") "auth.json"
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
  | Sys_error message -> message
  | Unix.Unix_error (error, fn, arg) ->
      Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg
  | End_of_file -> "unexpected end of file"
  | Invalid_argument message -> message
  | Failure message -> message
  | exn -> Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())

let with_io_error path f =
  Lwt.catch
    (fun () -> f ())
    (fun exn ->
      match exn with
      | Lwt.Canceled -> Lwt.fail exn
      | _ -> Lwt.return (Error (`Io (path, io_message exn))))

let fs_message = function
  | `Already_exists -> "already exists"
  | `Is_directory -> "is a directory"
  | `Not_found -> "not found"
  | `Permission_denied -> "permission denied"

let max_credential_bytes = 1_048_576

let lstat_opt path =
  Lwt.catch
    (fun () -> Lwt_unix.lstat path >|= fun stat -> Some stat)
    (function
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_none | exn -> Lwt.fail exn)

let read_capped path =
  Lwt_io.with_file ~mode:Lwt_io.Input path (fun ic ->
      let buffer = Buffer.create 4_096 in
      let rec loop () =
        if Buffer.length buffer > max_credential_bytes then
          Lwt.return_error "credential file exceeds 1 MiB"
        else
          Lwt_io.read ~count:4_096 ic >>= function
          | "" -> Lwt.return_ok (Buffer.contents buffer)
          | chunk ->
              Buffer.add_string buffer chunk;
              loop ()
      in
      loop ())

let load_entries path =
  with_io_error path (fun () ->
      lstat_opt path >>= function
      | None -> Lwt.return_ok []
      | Some stat when stat.Unix.st_kind <> Unix.S_REG ->
          Lwt.return_error (`Io (path, "credential path is not a regular file"))
      | Some _ -> (
          read_capped path >>= function
          | Error message -> Lwt.return_error (`Parse (path, message))
          | Ok text ->
              Lwt.return
                (match Jsonx.decode jsont text with
                | Ok entries -> Ok entries
                | Error message -> Error (`Parse (path, message)))))

let persist_entries t entries =
  with_io_error t.path (fun () ->
      let encoded = Jsonx.encode jsont entries in
      State_file.replace t.path encoded >>= function
      | Ok () -> Lwt.return_ok ()
      | Error (`Io (target, message)) -> Lwt.return_error (`Io (target, message)))

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let release_file_lock fd =
  (try Unix.lockf fd Unix.F_ULOCK 0 with Unix.Unix_error _ -> ());
  close_fd fd;
  Lwt.return_unit

let lock_file t =
  let fd =
    Unix.openfile t.lock_path [ Unix.O_CREAT; Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600
  in
  let rec attempt () =
    try
      Unix.lockf fd Unix.F_TLOCK 0;
      Lwt.return fd
    with
    | Unix.Unix_error ((Unix.EACCES | Unix.EAGAIN), _, _) ->
        Lwt_unix.sleep 0.01 >>= attempt
    | Unix.Unix_error (Unix.EINTR, _, _) -> attempt ()
    | Unix.Unix_error _ as exn ->
        close_fd fd;
        Lwt.fail exn
  in
  let pending = attempt () in
  Lwt.on_cancel pending (fun () -> close_fd fd);
  pending

let acquire_file_lock t =
  with_io_error t.lock_path (fun () ->
      Charamel_os.Fs.mkdir_p t.parent >>= function
      | Error reason -> Lwt.return_error (`Io (t.parent, fs_message reason))
      | Ok () -> lock_file t >|= fun fd -> Ok fd)

let with_loaded t ~on_fault f =
  Lwt_mutex.with_lock t.mutex (fun () ->
      match t.persistence_fault with
      | Some error -> on_fault error
      | None -> (
          acquire_file_lock t >>= function
          | Error error -> on_fault error
          | Ok fd ->
              Lwt.finalize
                (fun () ->
                  load_entries t.path >>= function
                  | Error error -> on_fault error
                  | Ok entries -> f ~previous:t.entries entries)
                (fun () -> release_file_lock fd)))

let create ~path ~clock () =
  let parent = Filename.dirname path in
  if String.equal parent path then
    Lwt.return_error (`Io (path, "credential path has no parent directory"))
  else
    let lock_path = Filename.concat parent (Filename.basename path ^ ".lock") in
    load_entries path >|= fun entries ->
    Result.map
      (fun entries ->
        {
          path;
          parent;
          lock_path;
          clock;
          entries;
          mutex = Lwt_mutex.create ();
          persistence_fault = None;
        })
      entries

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
  | Api_key key -> Some (Charamel_fantasy.Provider.Api_key key)
  | Oauth credential -> Some (Charamel_fantasy.Provider.Oauth credential)
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
    ~on_fault:(fun error -> Lwt.return_error error)
    (fun ~previous:_ entries ->
      t.entries <- entries;
      Lwt.return_ok (resolve_entries entries ~config ~env ~provider))

let validate_credential t = function
  | Api_key key when String.trim key = "" ->
      Error (`Io (t.path, "api_key credential requires a non-empty key"))
  | Oauth { access; refresh; _ } when access = "" || refresh = "" ->
      Error (`Io (t.path, "oauth tokens must be non-empty"))
  | Disabled { reason; _ } when reason = "" ->
      Error (`Io (t.path, "disabled credential requires a reason"))
  | _ -> Ok ()

let set t ~provider credential =
  let* () = Lwt.return (validate_credential t credential) in
  with_loaded t
    ~on_fault:(fun error -> Lwt.return_error error)
    (fun ~previous entries ->
      let updated = replace_entry provider credential entries in
      Lwt.no_cancel
        ( persist_entries t updated >>= function
          | Ok () ->
              t.entries <- updated;
              Lwt.return_ok ()
          | Error error ->
              t.entries <- previous;
              Lwt.return_error error ))

let remove t ~provider =
  with_loaded t
    ~on_fault:(fun error -> Lwt.return_error error)
    (fun ~previous entries ->
      let updated = remove_entry provider entries in
      Lwt.no_cancel
        ( persist_entries t updated >>= function
          | Ok () ->
              t.entries <- updated;
              Lwt.return_ok ()
          | Error error ->
              t.entries <- previous;
              Lwt.return_error error ))

let now_ms t = int_of_float (Charamel_os.Time.now t.clock *. 1000.)

let definitive_refresh_error = function
  | (`Oauth_invalid_grant reason : Charamel_fantasy.Error.t) -> Some reason
  | `Http ({ status = 401; message; _ } : Charamel_fantasy.Error.http_error) ->
      Some message
  | _ -> None

let auth_equal left right = left = right

let refresh_oauth ~force t ~provider ~entries old :
    (Charamel_fantasy.Provider.auth, refresh_error) result Lwt.t =
  (if force then Charamel_fantasy.Oauth.Anthropic.refresh ~clock:t.clock old
   else Charamel_fantasy.Oauth.Anthropic.ensure_fresh ~clock:t.clock old)
  >>= function
  | Ok fresh ->
      let updated = replace_entry provider (Oauth fresh) entries in
      if fresh = old then (
        t.entries <- entries;
        Lwt.return_ok (Charamel_fantasy.Provider.Oauth fresh))
      else (
        t.entries <- updated;
        persist_entries t updated >|= function
        | Ok () -> Ok (Charamel_fantasy.Provider.Oauth fresh)
        | Error error ->
            t.persistence_fault <- Some error;
            Error (error :> refresh_error))
  | Error oauth_error -> (
      match definitive_refresh_error oauth_error with
      | None -> Lwt.return_error (`Refresh oauth_error)
      | Some reason -> (
          let disabled =
            replace_entry provider (Disabled { reason; at_ms = now_ms t }) entries
          in
          t.entries <- disabled;
          persist_entries t disabled >|= function
          | Ok () -> Error (`Disabled reason)
          | Error error ->
              t.persistence_fault <- Some error;
              Error (error :> refresh_error)))

let ensure_fresh ~config ~env t ~provider :
    (Charamel_fantasy.Provider.auth, refresh_error) result Lwt.t =
  let callback ~previous:_ entries =
    t.entries <- entries;
    match resolve_entries entries ~config ~env ~provider with
    | None -> Lwt.return_error `No_credential
    | Some (Disabled { reason; _ }) -> Lwt.return_error (`Disabled reason)
    | Some (Api_key key) -> Lwt.return_ok (Charamel_fantasy.Provider.Api_key key)
    | Some (Oauth old) ->
        Lwt.no_cancel (refresh_oauth ~force:false t ~provider ~entries old)
  in
  with_loaded t
    ~on_fault:(fun error -> Lwt.return_error (error :> refresh_error))
    callback

let refresh ~config ~env t ~provider ~rejected :
    (Charamel_fantasy.Provider.auth, refresh_error) result Lwt.t =
  let callback ~previous:_ entries =
    t.entries <- entries;
    match resolve_entries entries ~config ~env ~provider with
    | None -> Lwt.return_error `No_credential
    | Some (Disabled { reason; _ }) -> Lwt.return_error (`Disabled reason)
    | Some (Api_key key) -> Lwt.return_ok (Charamel_fantasy.Provider.Api_key key)
    | Some (Oauth old) ->
        let force = auth_equal (Charamel_fantasy.Provider.Oauth old) rejected in
        Lwt.no_cancel (refresh_oauth ~force t ~provider ~entries old)
  in
  with_loaded t
    ~on_fault:(fun error -> Lwt.return_error (error :> refresh_error))
    callback

let disable t ~provider ~rejected ~reason ~now_ms =
  if reason = "" then
    Lwt.return_error (`Io (t.path, "disabled credential requires a reason"))
  else
    with_loaded t
      ~on_fault:(fun error -> Lwt.return_error error)
      (fun ~previous:_ entries ->
        t.entries <- entries;
        match List.assoc_opt provider entries with
        | None | Some (Disabled _) -> Lwt.return_ok ()
        | Some current -> (
            match to_fantasy current with
            | Some current_auth when auth_equal current_auth rejected ->
                let updated =
                  replace_entry provider (Disabled { reason; at_ms = now_ms }) entries
                in
                t.entries <- updated;
                Lwt.no_cancel
                  ( persist_entries t updated >>= function
                    | Ok () -> Lwt.return_ok ()
                    | Error error ->
                        t.persistence_fault <- Some error;
                        Lwt.return_error error )
            | _ -> Lwt.return_ok ()))

module Login = struct
  let port = 54545
  let redirect_uri = "http://127.0.0.1:54545/callback"
  let request_bound = 8_192
  let request_timeout = 10.
  let login_timeout = 300.
  let success_page = "<html>Login complete. You can return to Crush.</html>"
  let bad_request = "<html>Invalid callback. Please try again.</html>"

  type race =
    | Callback of string
    | Paste of string
    | Aborted
    | Timeout
    | Accept_error of string

  let close_socket fd = Lwt.catch (fun () -> Lwt_unix.close fd) (fun _ -> Lwt.return_unit)

  let open_listener () =
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    let configure () =
      Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
      Lwt_unix.bind fd (Lwt_unix.ADDR_INET (Unix.inet_addr_loopback, port)) >>= fun () ->
      Lwt_unix.listen fd 8;
      Lwt.return_ok fd
    in
    Lwt.catch configure (fun exn ->
        close_socket fd >|= fun () -> Error (`Oauth (io_message exn)))

  let request_path line =
    match String.split_on_char ' ' (String.trim line) with
    | [ "GET"; target; version ] when String.starts_with ~prefix:"HTTP/" version ->
        Some target
    | _ -> None

  let callback_code login line =
    Option.bind (request_path line) (fun target ->
        let uri = Uri.of_string ("http://127.0.0.1" ^ target) in
        if Uri.path uri <> "/callback" then None
        else
          match
            Charamel_fantasy.Oauth.Anthropic.extract_code ~url_or_code:(Uri.to_string uri)
              ~state:login.Charamel_fantasy__Oauth.Anthropic.state
          with
          | Ok code -> Some code
          | Error _ -> None)

  let read_line ic =
    Charamel_net.read_line ~bound:request_bound ~timeout:request_timeout ic

  let rec drain_headers ic bytes lines =
    if bytes > request_bound || lines > 128 then Lwt.return_false
    else
      read_line ic >>= function
      | Error _ | Ok None -> Lwt.return_false
      | Ok (Some "") -> Lwt.return_true
      | Ok (Some line) -> drain_headers ic (bytes + String.length line + 1) (lines + 1)

  let read_request ic login =
    read_line ic >>= function
    | Error _ | Ok None -> Lwt.return_none
    | Ok (Some line) -> (
        drain_headers ic (String.length line + 1) 0 >|= function
        | false -> None
        | true -> callback_code login line)

  let respond fd ~status body =
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
    let bytes = Bytes.of_string text in
    let last = Bytes.length bytes in
    let rec write offset =
      if offset >= last then Lwt.return_unit
      else
        Lwt_unix.write fd bytes offset (last - offset) >>= fun written ->
        if written = 0 then Lwt.return_unit else write (offset + written)
    in
    Lwt.catch (fun () -> write 0) (fun _ -> Lwt.return_unit)

  let close_input ic = Lwt.catch (fun () -> Lwt_io.close ic) (fun _ -> Lwt.return_unit)

  let parse_connection fd login =
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let read () =
      Lwt.catch
        (fun () ->
          Lwt_unix.with_timeout request_timeout (fun () -> read_request ic login))
        (fun exn -> match exn with Lwt.Canceled -> Lwt.fail exn | _ -> Lwt.return_none)
    in
    let reply outcome =
      match outcome with
      | None -> respond fd ~status:400 bad_request >|= fun () -> None
      | Some code -> respond fd ~status:200 success_page >|= fun () -> Some code
    in
    Lwt.finalize (fun () -> read () >>= reply) (fun () -> close_input ic)

  let rec wait_for_callback socket login () =
    Lwt_unix.accept socket >>= fun (fd, _) ->
    parse_connection fd login >>= function
    | Some code -> Lwt.return (Callback code)
    | None -> wait_for_callback socket login ()

  let accept_race socket login =
    Lwt.catch (wait_for_callback socket login) (fun exn ->
        match exn with
        | Lwt.Canceled -> Lwt.fail exn
        | _ -> Lwt.return (Accept_error (io_message exn)))

  let paste_race prompt_paste =
    Lwt.catch prompt_paste (fun exn ->
        match exn with Lwt.Canceled -> Lwt.fail exn | _ -> Lwt.return_none)
    >|= function
    | None -> Aborted
    | Some value -> Paste value

  let timeout_race () = Lwt_unix.sleep login_timeout >|= fun () -> Timeout

  let complete t login code =
    Charamel_fantasy.Oauth.Anthropic.exchange ~clock:t.clock ~redirect_uri ~login ~code ()
    >>= function
    | Error oauth_error ->
        Lwt.return_error (`Oauth (Charamel_fantasy.Error.message oauth_error))
    | Ok credential -> (
        set t ~provider:"anthropic" (Oauth credential) >|= function
        | Ok () -> Ok ()
        | Error error -> Error (error :> login_error))

  let race_login ~prompt_paste t socket login =
    Lwt.pick [ accept_race socket login; paste_race prompt_paste; timeout_race () ]
    >>= function
    | Timeout -> Lwt.return_error `Timeout
    | Aborted -> Lwt.return_error `Aborted
    | Accept_error message -> Lwt.return_error (`Oauth message)
    | Callback code | Paste code -> complete t login code

  let anthropic ~open_browser ~prompt_paste t =
    let login = Charamel_fantasy.Oauth.Anthropic.begin_login ~redirect_uri () in
    open_listener () >>= function
    | Error _ as failure -> Lwt.return failure
    | Ok socket ->
        Lwt.finalize
          (fun () ->
            open_browser login.Charamel_fantasy__Oauth.Anthropic.uri;
            race_login ~prompt_paste t socket login)
          (fun () -> close_socket socket)
end
