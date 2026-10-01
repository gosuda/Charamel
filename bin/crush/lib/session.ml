open Lwt.Infix

type model_ref = { provider : string; model : string }

type header = {
  id : string;
  title : string;
  parent : string option;
  created_ms : int;
  cwd : string;
  model : model_ref;
}

type tool_output = [ `Text of string | `Error of string | `Media of string * string ]
type decision = Allow_once | Allow_session | Deny

type event =
  | Message of { ms : int; message : Charamel_fantasy.Message.t }
  | Tool_call of { ms : int; id : string; name : string; input : Jsont.json }
  | Tool_result of {
      ms : int;
      id : string;
      name : string;
      output : tool_output;
      elapsed_ms : int;
      artifact : string option;
    }
  | Usage of {
      ms : int;
      usage : Charamel_fantasy.Usage.t;
      cost_usd : float;
      model : model_ref;
    }
  | Summary of { ms : int; text : string; through : int }
  | Permission of {
      ms : int;
      tool : string;
      action : string;
      path : string;
      decision : decision;
    }
  | Note of { ms : int; text : string }

type index_entry = {
  id : string;
  title : string;
  parent : string option;
  created_ms : int;
  updated_ms : int;
  message_count : int;
  usage : Charamel_fantasy.Usage.t;
  cost_usd : float;
}

let log_src = Logs.Src.create "crush.session"

module Log = (val Logs.src_log log_src : Logs.LOG)

let role_jsont =
  Jsont.enum
    [
      ("system", Charamel_fantasy.Message.System);
      ("user", Charamel_fantasy.Message.User);
      ("assistant", Charamel_fantasy.Message.Assistant);
      ("tool", Charamel_fantasy.Message.Tool);
    ]

let text_part_jsont =
  let open Jsont in
  Object.map (fun text -> text) |> Object.mem "text" string ~enc:Fun.id |> Object.finish

let reasoning_part_jsont =
  let open Jsont in
  Object.map (fun text signature -> (text, signature))
  |> Object.mem "text" string ~enc:fst
  |> Object.mem "signature" (option string) ~dec_absent:None ~enc:snd
  |> Object.finish

let file_part_jsont =
  let open Jsont in
  Object.map (fun mime data name -> (mime, data, name))
  |> Object.mem "mime" string ~enc:(fun (mime, _, _) -> mime)
  |> Object.mem "data" string ~enc:(fun (_, data, _) -> data)
  |> Object.mem "name" (option string) ~dec_absent:None ~enc:(fun (_, _, name) -> name)
  |> Object.finish

let tool_call_part_jsont =
  let open Jsont in
  Object.map (fun id name input -> (id, name, input))
  |> Object.mem "id" string ~enc:(fun (id, _, _) -> id)
  |> Object.mem "name" string ~enc:(fun (_, name, _) -> name)
  |> Object.mem "input" json ~enc:(fun (_, _, input) -> input)
  |> Object.finish

let text_output_jsont =
  let open Jsont in
  Object.map (fun text -> text) |> Object.mem "text" string ~enc:Fun.id |> Object.finish

let error_output_jsont = text_output_jsont

let media_output_jsont =
  let open Jsont in
  Object.map (fun mime data -> (mime, data))
  |> Object.mem "mime" string ~enc:fst
  |> Object.mem "data" string ~enc:snd
  |> Object.finish

let text_output_case =
  Jsont.Object.Case.map "text" text_output_jsont ~dec:(fun text -> `Text text)

let error_output_case =
  Jsont.Object.Case.map "error" error_output_jsont ~dec:(fun text -> `Error text)

let media_output_case =
  Jsont.Object.Case.map "media" media_output_jsont ~dec:(fun (mime, data) ->
      `Media (mime, data))

let tool_output_jsont =
  let cases =
    Jsont.Object.Case.
      [ make text_output_case; make error_output_case; make media_output_case ]
  in
  let enc_case = function
    | `Text text -> Jsont.Object.Case.value text_output_case text
    | `Error text -> Jsont.Object.Case.value error_output_case text
    | `Media (mime, data) -> Jsont.Object.Case.value media_output_case (mime, data)
  in
  Jsont.Object.map Fun.id
  |> Jsont.Object.case_mem "kind" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.finish

let tool_result_part_jsont =
  let open Jsont in
  Object.map (fun id name output -> (id, name, output))
  |> Object.mem "id" string ~enc:(fun (id, _, _) -> id)
  |> Object.mem "name" string ~enc:(fun (_, name, _) -> name)
  |> Object.mem "output" tool_output_jsont ~enc:(fun (_, _, output) -> output)
  |> Object.finish

let text_part_case =
  Jsont.Object.Case.map "text" text_part_jsont ~dec:(fun text ->
      Charamel_fantasy.Message.Text text)

let reasoning_part_case =
  Jsont.Object.Case.map "reasoning" reasoning_part_jsont ~dec:(fun (text, signature) ->
      Charamel_fantasy.Message.Reasoning { text; signature })

let file_part_case =
  Jsont.Object.Case.map "file" file_part_jsont ~dec:(fun (mime, data, name) ->
      Charamel_fantasy.Message.File { mime; data; name })

let tool_call_part_case =
  Jsont.Object.Case.map "tool_call" tool_call_part_jsont ~dec:(fun (id, name, input) ->
      Charamel_fantasy.Message.Tool_call { id; name; input })

let tool_result_part_case =
  Jsont.Object.Case.map "tool_result" tool_result_part_jsont
    ~dec:(fun (id, name, output) ->
      Charamel_fantasy.Message.Tool_result { id; name; output })

let part_jsont =
  let cases =
    Jsont.Object.Case.
      [
        make text_part_case;
        make reasoning_part_case;
        make file_part_case;
        make tool_call_part_case;
        make tool_result_part_case;
      ]
  in
  let enc_case = function
    | Charamel_fantasy.Message.Text text -> Jsont.Object.Case.value text_part_case text
    | Charamel_fantasy.Message.Reasoning { text; signature } ->
        Jsont.Object.Case.value reasoning_part_case (text, signature)
    | Charamel_fantasy.Message.File { mime; data; name } ->
        Jsont.Object.Case.value file_part_case (mime, data, name)
    | Charamel_fantasy.Message.Tool_call { id; name; input } ->
        Jsont.Object.Case.value tool_call_part_case (id, name, input)
    | Charamel_fantasy.Message.Tool_result { id; name; output } ->
        Jsont.Object.Case.value tool_result_part_case (id, name, output)
  in
  Jsont.Object.map Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.finish

let message_jsont =
  let open Jsont in
  Object.map (fun role parts -> { Charamel_fantasy.Message.role; parts })
  |> Object.mem "role" role_jsont ~enc:(fun value -> value.Charamel_fantasy.Message.role)
  |> Object.mem "parts" (list part_jsont)
       ~enc:(fun (value : Charamel_fantasy.Message.t) ->
         value.Charamel_fantasy.Message.parts)
  |> Object.finish

let tool_output_text = function
  | `Text text -> text
  | `Error text -> "error: " ^ text
  | `Media (mime, data) -> Fmt.str "media <%s> (%d bytes)" mime (String.length data)

let part_text = function
  | Charamel_fantasy.Message.Text text -> text
  | Charamel_fantasy.Message.Reasoning { text; _ } ->
      "<reasoning>" ^ text ^ "</reasoning>"
  | Charamel_fantasy.Message.File { mime; data; name } ->
      Fmt.str "<file mime=%s name=%s bytes=%d>" mime
        (Option.value ~default:"" name)
        (String.length data)
  | Charamel_fantasy.Message.Tool_call { id; name; input } ->
      Fmt.str "call %s (%s): %s" id name (Jsonx.display_string input)
  | Charamel_fantasy.Message.Tool_result { id; name; output } ->
      Fmt.str "result %s (%s): %s" id name (tool_output_text output)

let role_text = function
  | Charamel_fantasy.Message.System -> "system"
  | Charamel_fantasy.Message.User -> "user"
  | Charamel_fantasy.Message.Assistant -> "assistant"
  | Charamel_fantasy.Message.Tool -> "tool"

let message_text { Charamel_fantasy.Message.role; parts } =
  role_text role ^ ": " ^ String.concat "" (List.map part_text parts)

let model_ref_jsont =
  let open Jsont in
  Object.map (fun provider model -> { provider; model })
  |> Object.mem "provider" string ~enc:(fun (value : model_ref) -> value.provider)
  |> Object.mem "model" string ~enc:(fun (value : model_ref) -> value.model)
  |> Object.finish

let usage_jsont =
  let open Jsont in
  Object.map (fun input output cache_read cache_write reasoning ->
      { Charamel_fantasy.Usage.input; output; cache_read; cache_write; reasoning })
  |> Object.mem "input" int ~enc:(fun value -> value.Charamel_fantasy.Usage.input)
  |> Object.mem "output" int ~enc:(fun value -> value.Charamel_fantasy.Usage.output)
  |> Object.mem "cache_read" int ~enc:(fun value ->
      value.Charamel_fantasy.Usage.cache_read)
  |> Object.mem "cache_write" int ~enc:(fun value ->
      value.Charamel_fantasy.Usage.cache_write)
  |> Object.mem "reasoning" int ~enc:(fun value -> value.Charamel_fantasy.Usage.reasoning)
  |> Object.finish

let header_jsont =
  let open Jsont in
  Object.map (fun id title parent created_ms cwd model ->
      { id; title; parent; created_ms; cwd; model })
  |> Object.mem "id" string ~enc:(fun (value : header) -> value.id)
  |> Object.mem "title" string ~enc:(fun (value : header) -> value.title)
  |> Object.mem "parent" (option string) ~dec_absent:None ~enc:(fun (value : header) ->
      value.parent)
  |> Object.mem "created_ms" int ~enc:(fun (value : header) -> value.created_ms)
  |> Object.mem "cwd" string ~enc:(fun (value : header) -> value.cwd)
  |> Object.mem "model" model_ref_jsont ~enc:(fun (value : header) -> value.model)
  |> Object.finish

type message_event = { ms : int; message : Charamel_fantasy.Message.t }
type tool_call_event = { ms : int; id : string; name : string; input : Jsont.json }

type tool_result_event = {
  ms : int;
  id : string;
  name : string;
  output : tool_output;
  elapsed_ms : int;
  artifact : string option;
}

type usage_event = {
  ms : int;
  usage : Charamel_fantasy.Usage.t;
  cost_usd : float;
  model : model_ref;
}

type summary_event = { ms : int; text : string; through : int }

type permission_event = {
  ms : int;
  tool : string;
  action : string;
  path : string;
  decision : decision;
}

type note_event = { ms : int; text : string }

let message_event_jsont =
  let open Jsont in
  Object.map (fun ms message -> { ms; message })
  |> Object.mem "ms" int ~enc:(fun (value : message_event) -> value.ms)
  |> Object.mem "message" message_jsont ~enc:(fun (value : message_event) ->
      value.message)
  |> Object.finish

let tool_call_event_jsont =
  let open Jsont in
  Object.map (fun ms id name input -> { ms; id; name; input })
  |> Object.mem "ms" int ~enc:(fun (value : tool_call_event) -> value.ms)
  |> Object.mem "id" string ~enc:(fun (value : tool_call_event) -> value.id)
  |> Object.mem "name" string ~enc:(fun (value : tool_call_event) -> value.name)
  |> Object.mem "input" json ~enc:(fun (value : tool_call_event) -> value.input)
  |> Object.finish

let tool_result_event_jsont =
  let open Jsont in
  Object.map (fun ms id name output elapsed_ms artifact ->
      { ms; id; name; output; elapsed_ms; artifact })
  |> Object.mem "ms" int ~enc:(fun (value : tool_result_event) -> value.ms)
  |> Object.mem "id" string ~enc:(fun (value : tool_result_event) -> value.id)
  |> Object.mem "name" string ~enc:(fun (value : tool_result_event) -> value.name)
  |> Object.mem "output" tool_output_jsont ~enc:(fun (value : tool_result_event) ->
      value.output)
  |> Object.mem "elapsed_ms" int ~enc:(fun (value : tool_result_event) ->
      value.elapsed_ms)
  |> Object.mem "artifact" (option string) ~dec_absent:None
       ~enc:(fun (value : tool_result_event) -> value.artifact)
  |> Object.finish

let usage_event_jsont =
  let open Jsont in
  Object.map (fun ms input output cache_read cache_write reasoning cost_usd model ->
      let usage =
        { Charamel_fantasy.Usage.input; output; cache_read; cache_write; reasoning }
      in
      ({ ms; usage; cost_usd; model } : usage_event))
  |> Object.mem "ms" int ~enc:(fun (value : usage_event) -> value.ms)
  |> Object.mem "input" int ~enc:(fun (value : usage_event) ->
      value.usage.Charamel_fantasy.Usage.input)
  |> Object.mem "output" int ~enc:(fun (value : usage_event) ->
      value.usage.Charamel_fantasy.Usage.output)
  |> Object.mem "cache_read" int ~enc:(fun (value : usage_event) ->
      value.usage.Charamel_fantasy.Usage.cache_read)
  |> Object.mem "cache_write" int ~enc:(fun (value : usage_event) ->
      value.usage.Charamel_fantasy.Usage.cache_write)
  |> Object.mem "reasoning" int ~enc:(fun (value : usage_event) ->
      value.usage.Charamel_fantasy.Usage.reasoning)
  |> Object.mem "cost_usd" number ~enc:(fun (value : usage_event) -> value.cost_usd)
  |> Object.mem "model" model_ref_jsont ~enc:(fun (value : usage_event) -> value.model)
  |> Object.finish

let summary_event_jsont =
  let open Jsont in
  Object.map (fun ms text through -> { ms; text; through })
  |> Object.mem "ms" int ~enc:(fun (value : summary_event) -> value.ms)
  |> Object.mem "text" string ~enc:(fun (value : summary_event) -> value.text)
  |> Object.mem "through" int ~enc:(fun (value : summary_event) -> value.through)
  |> Object.finish

let decision_jsont =
  Jsont.enum
    [ ("allow_once", Allow_once); ("allow_session", Allow_session); ("deny", Deny) ]

let permission_event_jsont =
  let open Jsont in
  Object.map (fun ms tool action path decision -> { ms; tool; action; path; decision })
  |> Object.mem "ms" int ~enc:(fun (value : permission_event) -> value.ms)
  |> Object.mem "tool" string ~enc:(fun (value : permission_event) -> value.tool)
  |> Object.mem "action" string ~enc:(fun (value : permission_event) -> value.action)
  |> Object.mem "path" string ~enc:(fun (value : permission_event) -> value.path)
  |> Object.mem "decision" decision_jsont ~enc:(fun (value : permission_event) ->
      value.decision)
  |> Object.finish

let note_event_jsont =
  let open Jsont in
  Object.map (fun ms text -> { ms; text })
  |> Object.mem "ms" int ~enc:(fun (value : note_event) -> value.ms)
  |> Object.mem "text" string ~enc:(fun (value : note_event) -> value.text)
  |> Object.finish

let message_event_case =
  Jsont.Object.Case.map "message" message_event_jsont ~dec:(fun { ms; message } ->
      Message { ms; message })

let tool_call_event_case =
  Jsont.Object.Case.map "tool_call" tool_call_event_jsont
    ~dec:(fun { ms; id; name; input } -> Tool_call { ms; id; name; input })

let tool_result_event_case =
  Jsont.Object.Case.map "tool_result" tool_result_event_jsont
    ~dec:(fun { ms; id; name; output; elapsed_ms; artifact } ->
      Tool_result { ms; id; name; output; elapsed_ms; artifact })

let usage_event_case =
  Jsont.Object.Case.map "usage" usage_event_jsont
    ~dec:(fun { ms; usage; cost_usd; model } -> Usage { ms; usage; cost_usd; model })

let summary_event_case =
  Jsont.Object.Case.map "summary" summary_event_jsont ~dec:(fun { ms; text; through } ->
      Summary { ms; text; through })

let permission_event_case =
  Jsont.Object.Case.map "permission" permission_event_jsont
    ~dec:(fun { ms; tool; action; path; decision } ->
      Permission { ms; tool; action; path; decision })

let note_event_case =
  Jsont.Object.Case.map "note" note_event_jsont ~dec:(fun { ms; text } ->
      Note { ms; text })

let event_jsont =
  let cases =
    Jsont.Object.Case.
      [
        make message_event_case;
        make tool_call_event_case;
        make tool_result_event_case;
        make usage_event_case;
        make summary_event_case;
        make permission_event_case;
        make note_event_case;
      ]
  in
  let enc_case = function
    | Message { ms; message } ->
        Jsont.Object.Case.value message_event_case { ms; message }
    | Tool_call { ms; id; name; input } ->
        Jsont.Object.Case.value tool_call_event_case { ms; id; name; input }
    | Tool_result { ms; id; name; output; elapsed_ms; artifact } ->
        Jsont.Object.Case.value tool_result_event_case
          { ms; id; name; output; elapsed_ms; artifact }
    | Usage { ms; usage; cost_usd; model } ->
        Jsont.Object.Case.value usage_event_case { ms; usage; cost_usd; model }
    | Summary { ms; text; through } ->
        Jsont.Object.Case.value summary_event_case { ms; text; through }
    | Permission { ms; tool; action; path; decision } ->
        Jsont.Object.Case.value permission_event_case { ms; tool; action; path; decision }
    | Note { ms; text } -> Jsont.Object.Case.value note_event_case { ms; text }
  in
  Jsont.Object.map Fun.id
  |> Jsont.Object.case_mem "t" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.finish

let index_entry_jsont =
  let open Jsont in
  Object.map (fun id title parent created_ms updated_ms message_count usage cost_usd ->
      { id; title; parent; created_ms; updated_ms; message_count; usage; cost_usd })
  |> Object.mem "id" string ~enc:(fun (value : index_entry) -> value.id)
  |> Object.mem "title" string ~enc:(fun (value : index_entry) -> value.title)
  |> Object.mem "parent" (option string) ~dec_absent:None
       ~enc:(fun (value : index_entry) -> value.parent)
  |> Object.mem "created_ms" int ~enc:(fun (value : index_entry) -> value.created_ms)
  |> Object.mem "updated_ms" int ~enc:(fun (value : index_entry) -> value.updated_ms)
  |> Object.mem "message_count" int ~enc:(fun (value : index_entry) ->
      value.message_count)
  |> Object.mem "usage" usage_jsont ~enc:(fun (value : index_entry) -> value.usage)
  |> Object.mem "cost_usd" number ~enc:(fun (value : index_entry) -> value.cost_usd)
  |> Object.finish

let index_jsont = Jsont.list index_entry_jsont

type index_document = { sessions : index_entry list }

let index_document_jsont =
  let open Jsont in
  Object.map (fun sessions -> { sessions })
  |> Object.mem "sessions" (list index_entry_jsont) ~enc:(fun value -> value.sessions)
  |> Object.finish

type store = { fs_root : string; root : string; index_mutex : Lwt_mutex.t }

type t = {
  store : store;
  path : string;
  mutable header : header;
  mutable events : event array;
  mutex : Lwt_mutex.t;
}

type error =
  [ `Io of string * string
  | `Session_corrupt of string * int
  | `Not_found of string
  | `Index of string ]

let pp_error ppf = function
  | `Io (path, message) -> Fmt.pf ppf "I/O error on %s: %s" path message
  | `Session_corrupt (path, line) -> Fmt.pf ppf "corrupt session %s at line %d" path line
  | `Not_found path -> Fmt.pf ppf "session not found: %s" path
  | `Index message -> Fmt.pf ppf "session index: %s" message

let now_ms clock = int_of_float (Charamel_os.Time.now clock *. 1000.)
let fs_path fs_root name = Path.under ~root:fs_root name
let sessions_dir store = Filename.concat store.root "sessions"
let index_path store = Filename.concat store.root "sessions.json"
let session_path store id = Filename.concat (sessions_dir store) (id ^ ".jsonl")

let write_channel path flags perm contents =
  Lwt_unix.openfile path flags perm >>= fun fd ->
  let channel = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt.finalize (fun () -> Lwt_io.write channel contents) (fun () -> Lwt_io.close channel)

let write_append target fs_root contents =
  let file = fs_path fs_root target in
  Io.trap target (fun () ->
      write_channel file [ O_WRONLY; O_APPEND; O_CREAT ] 0o600 contents)

let write_truncate target fs_root contents =
  let file = fs_path fs_root target in
  Io.trap target (fun () ->
      write_channel file [ O_WRONLY; O_CREAT; O_TRUNC ] 0o600 contents)

let write_exclusive target fs_root contents =
  let file = fs_path fs_root target in
  Lwt.catch
    (fun () ->
      write_channel file [ O_WRONLY; O_CREAT; O_EXCL ] 0o600 contents >>= fun () ->
      Lwt.return_ok ())
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_error `Exists
      | (Unix.Unix_error _ | Charamel_os.Fs.E _ | Sys_error _) as exn ->
          Lwt.return_error (`Io (target, Io.message exn))
      | exn -> Lwt.fail exn)

let read_file path =
  Lwt_io.with_file ~mode:Lwt_io.Input path (fun channel -> Lwt_io.read channel)

let remove_file target fs_root =
  Charamel_os.Fs.unlink (fs_path fs_root target) >>= function
  | Ok () | Error `Already_exists -> Lwt.return_ok ()
  | Error `Not_found -> Lwt.return_error (`Not_found target)
  | Error error -> Lwt.return_error (`Io (target, Io.fs_error error))

let mkdir_private path =
  Charamel_os.Fs.mkdir_p path >>= function
  | Ok () -> Lwt_unix.chmod path 0o700
  | Error error -> Lwt.fail (Charamel_os.Fs.E (error, path))

let ensure_directories store =
  Io.trap store.root (fun () ->
      mkdir_private (fs_path store.fs_root store.root) >>= fun () ->
      mkdir_private (fs_path store.fs_root (sessions_dir store)))

let encode codec value = Jsonx.encode codec value
let decode codec text = Jsonx.decode codec text

let load_index store =
  let target = index_path store in
  Io.trap target (fun () -> read_file (fs_path store.fs_root target)) >>= function
  | Error (`Not_found _) -> Lwt.return_ok []
  | Error (`Io (path, message)) ->
      Lwt.return_error (`Index (Fmt.str "%s: %s" path message))
  | Ok text -> (
      match decode index_document_jsont text with
      | Ok { sessions } -> Lwt.return_ok sessions
      | Error message -> Lwt.return_error (`Index (Fmt.str "%s: %s" target message)))

let index_json entries = encode index_document_jsont { sessions = entries }

let replace_index store entries =
  let target = index_path store in
  State_file.replace (fs_path store.fs_root target) (index_json entries) >>= function
  | Ok () -> Lwt.return_ok ()
  | Error (`Io (path, message)) ->
      Lwt.return_error (`Index (Fmt.str "%s: %s" path message))

let update_index store f =
  Lwt_mutex.with_lock store.index_mutex (fun () ->
      load_index store >>= function
      | Error _ as failure -> Lwt.return failure
      | Ok entries -> replace_index store (f entries))

let replace_entry (id : string) (entry : index_entry) (entries : index_entry list) =
  let found = ref false in
  let entries =
    List.map
      (fun (value : index_entry) ->
        if String.equal value.id id then (
          found := true;
          entry)
        else value)
      entries
  in
  if !found then entries else entries @ [ entry ]

let usage_zero = Charamel_fantasy.Usage.zero

let entry_of_header (header : header) : index_entry =
  {
    id = header.id;
    title = header.title;
    parent = header.parent;
    created_ms = header.created_ms;
    updated_ms = header.created_ms;
    message_count = 0;
    usage = usage_zero;
    cost_usd = 0.;
  }

let update_entry (entry : index_entry) event ~updated_ms =
  match event with
  | Message _ -> { entry with updated_ms; message_count = entry.message_count + 1 }
  | Usage { usage; cost_usd; _ } ->
      {
        entry with
        updated_ms;
        usage = Charamel_fantasy.Usage.add entry.usage usage;
        cost_usd = entry.cost_usd +. cost_usd;
      }
  | Tool_call _ | Tool_result _ | Summary _ | Permission _ | Note _ ->
      { entry with updated_ms }

let valid_session_id id = Ulid.is_valid id

let store ~fs_root ~cwd =
  let data_root = Charamel_cli.Xdg.data_dir ~app:"crush" in
  let root =
    Filename.concat (Filename.concat data_root "projects") (Config.project_key ~cwd)
  in
  { fs_root; root; index_mutex = Lwt_mutex.create () }

let root store = store.root

let register store (header : header) =
  let entry = entry_of_header header in
  update_index store (fun entries -> replace_entry header.id entry entries)

let make_session store ~path header events =
  { store; path; header; events = Array.of_list events; mutex = Lwt_mutex.create () }

let rec create_id store ~clock ~random parent title ~cwd ~model remaining =
  if remaining = 0 then
    Lwt.return_error (`Io (sessions_dir store, "could not allocate a unique session id"))
  else
    let created_ms = now_ms clock in
    let id = Ulid.v ~now_ms:created_ms ~random in
    let header = { id; title; parent; created_ms; cwd; model } in
    let target = session_path store id in
    write_exclusive target store.fs_root (encode header_jsont header ^ "\n") >>= function
    | Error `Exists ->
        create_id store ~clock ~random parent title ~cwd ~model (remaining - 1)
    | Error (`Io _ as failure) -> Lwt.return (Error failure)
    | Ok () -> (
        register store header >>= function
        | Ok () -> Lwt.return_ok (make_session store ~path:target header [])
        | Error error ->
            remove_file target store.fs_root >>= fun _ -> Lwt.return_error error)

let create store ~clock ~random ?parent ?title ~cwd ~model () =
  ensure_directories store >>= function
  | Error (`Not_found path) ->
      Lwt.return_error (`Io (path, "session directory does not exist"))
  | Error (`Io _ as failure) -> Lwt.return (Error failure)
  | Ok () ->
      let title = Option.value ~default:"" title in
      create_id store ~clock ~random parent title ~cwd ~model 32

let split_lines text =
  let raw = String.split_on_char '\n' text in
  match List.rev raw with "" :: rest -> (List.rev rest, true) | _ -> (raw, false)

let joined_lines lines = String.concat "\n" lines ^ "\n"

let rewrite_prefix store target lines =
  write_truncate target store.fs_root (joined_lines lines) >>= function
  | Error (`Not_found path) ->
      Lwt.return_error (`Io (path, "session disappeared while repairing"))
  | Error (`Io _ as failure) -> Lwt.return (Error failure)
  | Ok () -> Lwt.return_ok ()

let apply_index_title store (header : header) =
  load_index store >>= function
  | Error _ -> Lwt.return header
  | Ok entries -> (
      match
        List.find_opt
          (fun (entry : index_entry) -> String.equal entry.id header.id)
          entries
      with
      | None -> Lwt.return header
      | Some entry -> Lwt.return { header with title = entry.title })

let rec parse_events store target all_lines line_number raw_events = function
  | [] -> Lwt.return_ok (List.rev raw_events, false)
  | line :: tail -> (
      match decode event_jsont line with
      | Ok event ->
          parse_events store target all_lines (line_number + 1) (event :: raw_events) tail
      | Error _ when tail = [] -> (
          let count = List.length raw_events + 1 in
          let good_lines = List.filteri (fun index _ -> index < count) all_lines in
          rewrite_prefix store target good_lines >>= function
          | Error _ as failure -> Lwt.return failure
          | Ok () ->
              Log.warn (fun m ->
                  m "discarded truncated final session line %s:%d" target line_number);
              Lwt.return_ok (List.rev raw_events, true))
      | Error _ -> Lwt.return_error (`Session_corrupt (target, line_number)))

let open_body store target id lines had_newline first rest =
  match decode header_jsont first with
  | Error _ -> Lwt.return_error (`Session_corrupt (target, 1))
  | Ok header when not (String.equal header.id id) ->
      Lwt.return_error (`Session_corrupt (target, 1))
  | Ok header -> (
      let all_lines = first :: rest in
      parse_events store target all_lines 2 [] rest >>= function
      | Error _ as failure -> Lwt.return failure
      | Ok (events, repaired) ->
          let finish () =
            apply_index_title store header >>= fun header ->
            Lwt.return_ok (make_session store ~path:target header events)
          in
          if (not had_newline) && not repaired then
            rewrite_prefix store target lines >>= function
            | Error _ as failure -> Lwt.return failure
            | Ok () -> finish ()
          else finish ())

let open_ store ~id =
  if not (valid_session_id id) then Lwt.return_error (`Not_found id)
  else
    let target = session_path store id in
    let file = fs_path store.fs_root target in
    Io.trap target (fun () -> Lwt_unix.lstat file) >>= function
    | Error (`Not_found _) -> Lwt.return_error (`Not_found target)
    | Error (`Io _ as failure) -> Lwt.return (Error failure)
    | Ok { Unix.st_kind = Unix.S_REG; _ } -> (
        Io.trap target (fun () -> read_file file) >>= function
        | Error (`Not_found _) -> Lwt.return_error (`Not_found target)
        | Error (`Io _ as failure) -> Lwt.return (Error failure)
        | Ok text -> (
            match split_lines text with
            | [], _ -> Lwt.return_error (`Session_corrupt (target, 1))
            | first :: rest, had_newline ->
                open_body store target id (first :: rest) had_newline first rest))
    | Ok _ -> Lwt.return_error (`Not_found target)

let list store =
  load_index store >>= function
  | Error _ as failure -> Lwt.return failure
  | Ok entries ->
      Lwt.return_ok
      @@ List.stable_sort
           (fun left right -> compare right.updated_ms left.updated_ms)
           entries

let last store =
  list store >>= function
  | Error _ as failure -> Lwt.return failure
  | Ok [] -> Lwt.return_error (`Not_found "")
  | Ok ((entry : index_entry) :: _) -> open_ store ~id:entry.id

let id session = session.header.id
let header session = session.header
let title session = session.header.title
let events session = Array.copy session.events
let path session = session.path

let index_entry_for session event updated_ms entries =
  let current_entry =
    List.find_opt
      (fun (entry : index_entry) -> String.equal entry.id session.header.id)
      entries
  in
  let entry =
    Option.value ~default:(entry_of_header session.header) current_entry |> fun value ->
    update_entry value event ~updated_ms
  in
  replace_entry session.header.id entry entries

let append session ~clock event =
  Lwt_mutex.with_lock session.mutex (fun () ->
      let line = encode event_jsont event ^ "\n" in
      write_append session.path session.store.fs_root line >>= function
      | Error _ as failure -> Lwt.return failure
      | Ok () -> (
          let updated_ms = now_ms clock in
          update_index session.store (index_entry_for session event updated_ms)
          >>= function
          | Error _ as failure -> Lwt.return failure
          | Ok () ->
              session.events <- Array.append session.events [| event |];
              Lwt.return_ok ()))

let set_title session ~title =
  Lwt_mutex.with_lock session.mutex (fun () ->
      let entry_update entries =
        List.map
          (fun (entry : index_entry) ->
            if String.equal entry.id session.header.id then { entry with title }
            else entry)
          entries
      in
      update_index session.store entry_update >>= function
      | Error _ as failure -> Lwt.return failure
      | Ok () ->
          session.header <- { session.header with title };
          Lwt.return_ok ())

let messages session =
  let events = session.events in
  let latest_summary =
    let found = ref None in
    Array.iter
      (fun event ->
        match event with
        | Summary { text; through; _ } -> found := Some (text, through)
        | _ -> ())
      events;
    !found
  in
  match latest_summary with
  | None ->
      Array.fold_left
        (fun messages event ->
          match event with Message { message; _ } -> message :: messages | _ -> messages)
        [] events
      |> List.rev
  | Some (summary_text, summary_through) ->
      let through = max (-1) (min summary_through (Array.length events - 1)) in
      let tail = ref [] in
      for index = through + 1 to Array.length events - 1 do
        match events.(index) with
        | Message { message; _ } -> tail := message :: !tail
        | _ -> ()
      done;
      let prefix =
        [
          Charamel_fantasy.Message.text Charamel_fantasy.Message.User
            ("Summary of the earlier conversation:\n" ^ summary_text);
          Charamel_fantasy.Message.text Charamel_fantasy.Message.Assistant "Understood.";
        ]
      in
      prefix @ List.rev !tail

let usage_total session =
  Array.fold_left
    (fun (usage, cost) event ->
      match event with
      | Usage { usage = event_usage; cost_usd; _ } ->
          (Charamel_fantasy.Usage.add usage event_usage, cost +. cost_usd)
      | _ -> (usage, cost))
    (usage_zero, 0.) session.events

let artifacts_dir store ~id =
  if not (valid_session_id id) then
    invalid_arg "Session.artifacts_dir: invalid session id"
  else Filename.concat store.root (Filename.concat "artifacts" id)

let rec remove_tree path =
  Charamel_os.Fs.read_dir path >>= function
  | Error _ -> Lwt.return_unit
  | Ok names ->
      Lwt_list.iter_s
        (fun name ->
          let child = Filename.concat path name in
          Lwt_unix.lstat child >>= fun stats ->
          if stats.Unix.st_kind = Unix.S_DIR then remove_tree child
          else Charamel_os.Fs.unlink child >|= fun _ -> ())
        names
      >>= fun () ->
      Lwt.catch
        (fun () -> Lwt_unix.rmdir path)
        (function
          | Unix.Unix_error (Unix.ENOENT, _, _) -> Lwt.return_unit | exn -> Lwt.fail exn)

let drop_index_entry store id =
  Lwt_mutex.with_lock store.index_mutex (fun () ->
      load_index store >>= function
      | Error _ as failure -> Lwt.return failure
      | Ok entries ->
          let remaining =
            List.filter
              (fun (entry : index_entry) -> not (String.equal entry.id id))
              entries
          in
          replace_index store remaining)

let delete store ~id =
  if not (valid_session_id id) then Lwt.return_error (`Not_found id)
  else
    let target = session_path store id in
    let file = fs_path store.fs_root target in
    Io.trap target (fun () -> Lwt_unix.lstat file) >>= function
    | Error (`Not_found _) -> Lwt.return_error (`Not_found target)
    | Error (`Io _ as failure) -> Lwt.return (Error failure)
    | Ok { Unix.st_kind = Unix.S_REG; _ } -> (
        drop_index_entry store id >>= function
        | Error _ as failure -> Lwt.return failure
        | Ok () -> (
            remove_file target store.fs_root >>= function
            | Error (`Not_found _) -> Lwt.return_error (`Not_found target)
            | Error (`Io _ as failure) -> Lwt.return (Error failure)
            | Ok () -> (
                let artifacts = artifacts_dir store ~id in
                Lwt.catch
                  (fun () ->
                    remove_tree (fs_path store.fs_root artifacts) >|= fun () -> Ok ())
                  (fun exn -> Lwt.return_error (`Io (artifacts, Io.message exn)))
                >>= function
                | Ok () -> Lwt.return_ok ()
                | Error _ as failure -> Lwt.return failure)))
    | Ok _ -> Lwt.return_error (`Not_found target)
