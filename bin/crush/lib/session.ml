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
  | Message of { ms : int; message : Charm_fantasy.Message.t }
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
      usage : Charm_fantasy.Usage.t;
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
  usage : Charm_fantasy.Usage.t;
  cost_usd : float;
}

let log_src = Logs.Src.create "crush.session"

module Log = (val Logs.src_log log_src : Logs.LOG)

let role_jsont =
  Jsont.enum
    [
      ("system", Charm_fantasy.Message.System);
      ("user", Charm_fantasy.Message.User);
      ("assistant", Charm_fantasy.Message.Assistant);
      ("tool", Charm_fantasy.Message.Tool);
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
      Charm_fantasy.Message.Text text)

let reasoning_part_case =
  Jsont.Object.Case.map "reasoning" reasoning_part_jsont ~dec:(fun (text, signature) ->
      Charm_fantasy.Message.Reasoning { text; signature })

let file_part_case =
  Jsont.Object.Case.map "file" file_part_jsont ~dec:(fun (mime, data, name) ->
      Charm_fantasy.Message.File { mime; data; name })

let tool_call_part_case =
  Jsont.Object.Case.map "tool_call" tool_call_part_jsont ~dec:(fun (id, name, input) ->
      Charm_fantasy.Message.Tool_call { id; name; input })

let tool_result_part_case =
  Jsont.Object.Case.map "tool_result" tool_result_part_jsont
    ~dec:(fun (id, name, output) ->
      Charm_fantasy.Message.Tool_result { id; name; output })

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
    | Charm_fantasy.Message.Text text -> Jsont.Object.Case.value text_part_case text
    | Charm_fantasy.Message.Reasoning { text; signature } ->
        Jsont.Object.Case.value reasoning_part_case (text, signature)
    | Charm_fantasy.Message.File { mime; data; name } ->
        Jsont.Object.Case.value file_part_case (mime, data, name)
    | Charm_fantasy.Message.Tool_call { id; name; input } ->
        Jsont.Object.Case.value tool_call_part_case (id, name, input)
    | Charm_fantasy.Message.Tool_result { id; name; output } ->
        Jsont.Object.Case.value tool_result_part_case (id, name, output)
  in
  Jsont.Object.map Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.finish

let message_jsont =
  let open Jsont in
  Object.map (fun role parts -> { Charm_fantasy.Message.role; parts })
  |> Object.mem "role" role_jsont ~enc:(fun value -> value.Charm_fantasy.Message.role)
  |> Object.mem "parts" (list part_jsont) ~enc:(fun (value : Charm_fantasy.Message.t) ->
      value.Charm_fantasy.Message.parts)
  |> Object.finish

let model_ref_jsont =
  let open Jsont in
  Object.map (fun provider model -> { provider; model })
  |> Object.mem "provider" string ~enc:(fun (value : model_ref) -> value.provider)
  |> Object.mem "model" string ~enc:(fun (value : model_ref) -> value.model)
  |> Object.finish

let usage_jsont =
  let open Jsont in
  Object.map (fun input output cache_read cache_write reasoning ->
      { Charm_fantasy.Usage.input; output; cache_read; cache_write; reasoning })
  |> Object.mem "input" int ~enc:(fun value -> value.Charm_fantasy.Usage.input)
  |> Object.mem "output" int ~enc:(fun value -> value.Charm_fantasy.Usage.output)
  |> Object.mem "cache_read" int ~enc:(fun value -> value.Charm_fantasy.Usage.cache_read)
  |> Object.mem "cache_write" int ~enc:(fun value ->
      value.Charm_fantasy.Usage.cache_write)
  |> Object.mem "reasoning" int ~enc:(fun value -> value.Charm_fantasy.Usage.reasoning)
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

type message_event = { ms : int; message : Charm_fantasy.Message.t }
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
  usage : Charm_fantasy.Usage.t;
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
        { Charm_fantasy.Usage.input; output; cache_read; cache_write; reasoning }
      in
      ({ ms; usage; cost_usd; model } : usage_event))
  |> Object.mem "ms" int ~enc:(fun (value : usage_event) -> value.ms)
  |> Object.mem "input" int ~enc:(fun (value : usage_event) ->
      value.usage.Charm_fantasy.Usage.input)
  |> Object.mem "output" int ~enc:(fun (value : usage_event) ->
      value.usage.Charm_fantasy.Usage.output)
  |> Object.mem "cache_read" int ~enc:(fun (value : usage_event) ->
      value.usage.Charm_fantasy.Usage.cache_read)
  |> Object.mem "cache_write" int ~enc:(fun (value : usage_event) ->
      value.usage.Charm_fantasy.Usage.cache_write)
  |> Object.mem "reasoning" int ~enc:(fun (value : usage_event) ->
      value.usage.Charm_fantasy.Usage.reasoning)
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

type store = { fs : Eio.Fs.dir_ty Eio.Path.t; root : string; index_mutex : Eio.Mutex.t }

type t = {
  store : store;
  path : string;
  mutable header : header;
  mutable events : event array;
  mutex : Eio.Mutex.t;
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

let now_ms clock = int_of_float (Eio.Time.now clock *. 1000.)
let fs_path fs name = Eio.Path.(fs / name)
let sessions_dir store = Filename.concat store.root "sessions"
let index_path store = Filename.concat store.root "sessions.json"
let session_path store id = Filename.concat (sessions_dir store) (id ^ ".jsonl")
let io_message exn = Fmt.str "%a" Eio.Exn.pp exn

let protect_io target f =
  try Ok (Eio.Cancel.protect f) with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Error (`Not_found target)
  | Eio.Io (Eio.Fs.E _, _) as exn -> Error (`Io (target, io_message exn))
  | Unix.Unix_error (error, fn, arg) ->
      Error (`Io (target, Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg))

let write_append target fs contents =
  try
    Eio.Cancel.protect (fun () ->
        Eio.Path.save ~append:true ~create:(`If_missing 0o600) (fs_path fs target)
          contents);
    Ok ()
  with
  | Eio.Io (Eio.Fs.E _, _) as exn -> Error (`Io (target, io_message exn))
  | Unix.Unix_error (error, fn, arg) ->
      Error (`Io (target, Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg))

let write_exclusive target fs contents =
  try
    Eio.Cancel.protect (fun () ->
        Eio.Path.save ~create:(`Exclusive 0o600) (fs_path fs target) contents);
    Ok ()
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Already_exists _), _) -> Error `Exists
  | Eio.Io (Eio.Fs.E _, _) as exn -> Error (`Io (target, io_message exn))
  | Unix.Unix_error (error, fn, arg) ->
      Error (`Io (target, Fmt.str "%s (%s %s)" (Unix.error_message error) fn arg))

let ensure_directories store =
  protect_io store.root (fun () ->
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (fs_path store.fs store.root);
      Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 (fs_path store.fs (sessions_dir store)))

let encode codec value = Jsonx.encode codec value
let decode codec text = Jsonx.decode codec text

let load_index store =
  match
    protect_io (index_path store) (fun () ->
        Eio.Path.load (fs_path store.fs (index_path store)))
  with
  | Ok text -> (
      match decode index_document_jsont text with
      | Ok { sessions } -> Ok sessions
      | Error message -> Error (`Index (Fmt.str "%s: %s" (index_path store) message)))
  | Error (`Not_found _) -> Ok []
  | Error (`Io (path, message)) -> Error (`Index (Fmt.str "%s: %s" path message))

let index_json entries = encode index_document_jsont { sessions = entries }

let replace_index store entries =
  match State_file.replace (fs_path store.fs (index_path store)) (index_json entries) with
  | Ok () -> Ok ()
  | Error (`Io (path, message)) -> Error (`Index (Fmt.str "%s: %s" path message))

let update_index store f =
  Eio.Mutex.use_rw ~protect:true store.index_mutex (fun () ->
      match load_index store with
      | Error error -> Error error
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

let usage_zero = Charm_fantasy.Usage.zero

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
        usage = Charm_fantasy.Usage.add entry.usage usage;
        cost_usd = entry.cost_usd +. cost_usd;
      }
  | Tool_call _ | Tool_result _ | Summary _ | Permission _ | Note _ ->
      { entry with updated_ms }

let valid_session_id id = Ulid.is_valid id

let store ~fs ~cwd =
  let data_root = Charm_cli.Xdg.data_dir ~app:"crush" in
  let root =
    Filename.concat (Filename.concat data_root "projects") (Config.project_key ~cwd)
  in
  { fs; root; index_mutex = Eio.Mutex.create () }

let root store = store.root

let create store ~clock ~random ?parent ?title ~cwd ~model () =
  match ensure_directories store with
  | Error (`Not_found path) -> Error (`Io (path, "session directory does not exist"))
  | Error (`Io (path, message)) -> Error (`Io (path, message))
  | Ok () ->
      let created_ms = now_ms clock in
      let title = Option.value ~default:"" title in
      let rec attempt remaining =
        if remaining = 0 then
          Error (`Io (sessions_dir store, "could not allocate a unique session id"))
        else
          let id = Ulid.v ~now_ms:created_ms ~random in
          let header = { id; title; parent; created_ms; cwd; model } in
          let target = session_path store id in
          match write_exclusive target store.fs (encode header_jsont header ^ "\n") with
          | Error `Exists -> attempt (remaining - 1)
          | Error (`Io (path, message)) -> Error (`Io (path, message))
          | Ok () ->
              let entry = entry_of_header header in
              begin match
                update_index store (fun entries -> replace_entry id entry entries)
              with
              | Ok () ->
                  Ok
                    {
                      store;
                      path = target;
                      header;
                      events = [||];
                      mutex = Eio.Mutex.create ();
                    }
              | Error error ->
                  ignore
                    (protect_io target (fun () ->
                         Eio.Path.unlink ~missing_ok:true (fs_path store.fs target)));
                  Error error
              end
      in
      attempt 32

let split_lines text =
  let raw = String.split_on_char '\n' text in
  match List.rev raw with "" :: rest -> (List.rev rest, true) | _ -> (raw, false)

let joined_lines lines = String.concat "\n" lines ^ "\n"

let rewrite_prefix store target lines =
  protect_io target (fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o600) (fs_path store.fs target)
        (joined_lines lines))

let apply_index_title store (header : header) =
  match load_index store with
  | Error _ -> header
  | Ok entries -> (
      match
        List.find_opt
          (fun (entry : index_entry) -> String.equal entry.id header.id)
          entries
      with
      | None -> header
      | Some entry -> { header with title = entry.title })

let open_ store ~id =
  if not (valid_session_id id) then Error (`Not_found id)
  else
    let target = session_path store id in
    match
      protect_io target (fun () -> Eio.Path.kind ~follow:false (fs_path store.fs target))
    with
    | Error (`Not_found _) -> Error (`Not_found target)
    | Error (`Io (path, message)) -> Error (`Io (path, message))
    | Ok `Regular_file ->
        begin match
          protect_io target (fun () -> Eio.Path.load (fs_path store.fs target))
        with
        | Error (`Not_found _) -> Error (`Not_found target)
        | Error (`Io (path, message)) -> Error (`Io (path, message))
        | Ok text -> (
            let lines, had_newline = split_lines text in
            match lines with
            | [] -> Error (`Session_corrupt (target, 1))
            | first :: rest -> (
                match decode header_jsont first with
                | Error _ -> Error (`Session_corrupt (target, 1))
                | Ok header when not (String.equal header.id id) ->
                    Error (`Session_corrupt (target, 1))
                | Ok header -> (
                    let rec parse line_number raw_events = function
                      | [] -> Ok (List.rev raw_events, false)
                      | line :: tail -> (
                          match decode event_jsont line with
                          | Ok event -> parse (line_number + 1) (event :: raw_events) tail
                          | Error _ when tail = [] ->
                              let all_lines = first :: rest in
                              let count = List.length raw_events + 1 in
                              let rec take n values =
                                if n = 0 then []
                                else
                                  match values with
                                  | [] -> []
                                  | value :: values -> value :: take (n - 1) values
                              in
                              let good_lines = take count all_lines in
                              begin match rewrite_prefix store target good_lines with
                              | Ok () ->
                                  Log.warn (fun m ->
                                      m "discarded truncated final session line %s:%d"
                                        target line_number);
                                  Ok (List.rev raw_events, true)
                              | Error (`Not_found path) ->
                                  Error
                                    (`Io (path, "session disappeared while repairing"))
                              | Error (`Io (path, message)) -> Error (`Io (path, message))
                              end
                          | Error _ -> Error (`Session_corrupt (target, line_number)))
                    in
                    match parse 2 [] rest with
                    | Error error -> Error error
                    | Ok (events, repaired) ->
                        if (not had_newline) && not repaired then
                          match rewrite_prefix store target lines with
                          | Error (`Not_found path) ->
                              Error (`Io (path, "session disappeared while normalizing"))
                          | Error (`Io (path, message)) -> Error (`Io (path, message))
                          | Ok () ->
                              Ok
                                {
                                  store;
                                  path = target;
                                  header = apply_index_title store header;
                                  events = Array.of_list events;
                                  mutex = Eio.Mutex.create ();
                                }
                        else
                          Ok
                            {
                              store;
                              path = target;
                              header = apply_index_title store header;
                              events = Array.of_list events;
                              mutex = Eio.Mutex.create ();
                            })))
        end
    | Ok _ -> Error (`Not_found target)

let list store =
  match load_index store with
  | Error error -> Error error
  | Ok entries ->
      Ok
        (List.stable_sort
           (fun left right -> compare right.updated_ms left.updated_ms)
           entries)

let last store =
  match list store with
  | Error error -> Error error
  | Ok [] -> Error (`Not_found "")
  | Ok (entry :: _) -> open_ store ~id:entry.id

let id session = session.header.id
let header session = session.header
let title session = session.header.title
let events session = Array.copy session.events
let path session = session.path

let append session ~clock event =
  Eio.Mutex.use_rw ~protect:true session.mutex (fun () ->
      let line = encode event_jsont event ^ "\n" in
      match write_append session.path session.store.fs line with
      | Error (`Io (path, message)) -> Error (`Io (path, message))
      | Ok () ->
          let updated_ms = now_ms clock in
          let result =
            update_index session.store (fun entries ->
                let current_entry =
                  List.find_opt
                    (fun (entry : index_entry) -> String.equal entry.id session.header.id)
                    entries
                in
                let entry =
                  Option.value ~default:(entry_of_header session.header) current_entry
                  |> fun value -> update_entry value event ~updated_ms
                in
                replace_entry session.header.id entry entries)
          in
          begin match result with
          | Error error -> Error error
          | Ok () ->
              session.events <- Array.append session.events [| event |];
              Ok ()
          end)

let set_title session ~title =
  Eio.Mutex.use_rw ~protect:true session.mutex (fun () ->
      let entry_update entries =
        List.map
          (fun (entry : index_entry) ->
            if String.equal entry.id session.header.id then { entry with title }
            else entry)
          entries
      in
      match update_index session.store entry_update with
      | Error error -> Error error
      | Ok () ->
          session.header <- { session.header with title };
          Ok ())

let messages session =
  let events = session.events in
  let latest_summary =
    let found = ref None in
    Array.iteri
      (fun _index event ->
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
          Charm_fantasy.Message.text Charm_fantasy.Message.User
            ("Summary of the earlier conversation:\n" ^ summary_text);
          Charm_fantasy.Message.text Charm_fantasy.Message.Assistant "Understood.";
        ]
      in
      prefix @ List.rev !tail

let usage_total session =
  Array.fold_left
    (fun (usage, cost) event ->
      match event with
      | Usage { usage = event_usage; cost_usd; _ } ->
          (Charm_fantasy.Usage.add usage event_usage, cost +. cost_usd)
      | _ -> (usage, cost))
    (usage_zero, 0.) session.events

let artifacts_dir store ~id =
  if not (valid_session_id id) then
    invalid_arg "Session.artifacts_dir: invalid session id"
  else Filename.concat store.root (Filename.concat "artifacts" id)

let delete store ~id =
  if not (valid_session_id id) then Error (`Not_found id)
  else
    let target = session_path store id in
    match
      protect_io target (fun () -> Eio.Path.kind ~follow:false (fs_path store.fs target))
    with
    | Error (`Not_found _) -> Error (`Not_found target)
    | Error (`Io (path, message)) -> Error (`Io (path, message))
    | Ok `Regular_file ->
        Eio.Mutex.use_rw ~protect:true store.index_mutex (fun () ->
            match load_index store with
            | Error error -> Error error
            | Ok entries ->
                let remaining =
                  List.filter
                    (fun (entry : index_entry) -> not (String.equal entry.id id))
                    entries
                in
                begin match replace_index store remaining with
                | Error error -> Error error
                | Ok () ->
                    begin match
                      protect_io target (fun () ->
                          Eio.Path.unlink ~missing_ok:false (fs_path store.fs target))
                    with
                    | Error (`Not_found _) -> Error (`Not_found target)
                    | Error (`Io (path, message)) -> Error (`Io (path, message))
                    | Ok () ->
                        let directory = artifacts_dir store ~id in
                        begin match
                          protect_io directory (fun () ->
                              Eio.Path.rmtree ~missing_ok:true
                                (fs_path store.fs directory))
                        with
                        | Ok () | Error (`Not_found _) -> Ok ()
                        | Error (`Io (path, message)) -> Error (`Io (path, message))
                        end
                    end
                end)
    | Ok _ -> Error (`Not_found target)
