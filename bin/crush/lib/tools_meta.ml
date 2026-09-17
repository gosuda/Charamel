open Result.Syntax

type todo_args = { todos : Todos.item list }
type option_arg = { label : string; description : string option }

type question_arg = {
  header : string;
  question : string;
  options : option_arg list;
  multi : bool;
  free_text : bool;
}

type question_args = { questions : question_arg list }
type logs_args = { lines : int }

let status_codec =
  Jsont.enum
    [
      ("pending", Todos.Pending);
      ("in_progress", Todos.In_progress);
      ("completed", Todos.Completed);
    ]

let todo_codec =
  let open Jsont in
  Object.map (fun content status active_form -> { Todos.content; status; active_form })
  |> Object.mem "content" string ~enc:(fun value -> value.Todos.content)
  |> Object.mem "status" status_codec ~enc:(fun value -> value.Todos.status)
  |> Object.mem "active_form" string ~enc:(fun value -> value.Todos.active_form)
  |> Object.finish

let todo_args_codec =
  let open Jsont in
  Object.map (fun todos -> { todos })
  |> Object.mem "todos" (list todo_codec) ~enc:(fun value -> value.todos)
  |> Object.finish

let option_codec =
  let open Jsont in
  Object.map (fun label description -> { label; description })
  |> Object.mem "label" string ~enc:(fun value -> value.label)
  |> Object.mem "description" (option string) ~dec_absent:None
       ~enc:(fun value -> value.description)
       ~enc_omit:Option.is_none
  |> Object.finish

let question_codec =
  let open Jsont in
  Object.map (fun header question options multi free_text ->
      { header; question; options; multi; free_text })
  |> Object.mem "header" string ~enc:(fun value -> value.header)
  |> Object.mem "question" string ~enc:(fun value -> value.question)
  |> Object.mem "options" (list option_codec) ~dec_absent:[] ~enc:(fun value ->
      value.options)
  |> Object.mem "multi" bool ~dec_absent:false ~enc:(fun value -> value.multi)
  |> Object.mem "free_text" bool ~dec_absent:false ~enc:(fun value -> value.free_text)
  |> Object.finish

let question_args_codec =
  let open Jsont in
  Object.map (fun questions -> { questions })
  |> Object.mem "questions" (list question_codec) ~enc:(fun value -> value.questions)
  |> Object.finish

let logs_codec =
  let open Jsont in
  Object.map (fun lines -> { lines })
  |> Object.mem "lines" int ~dec_absent:50 ~enc:(fun value -> value.lines)
  |> Object.finish

let todo_item_schema =
  Tool.s_object
    ~required:[ "content"; "status"; "active_form" ]
    [
      ("content", Tool.s_string ());
      ("status", Tool.s_string ~enum:[ "pending"; "in_progress"; "completed" ] ());
      ("active_form", Tool.s_string ());
    ]

let todos_schema =
  Tool.schema_object ~required:[ "todos" ] [ ("todos", Tool.s_array todo_item_schema) ]

let question_option_schema =
  Tool.s_object ~required:[ "label" ]
    [ ("label", Tool.s_string ()); ("description", Tool.s_string ()) ]

let question_item_schema =
  Tool.s_object ~required:[ "header"; "question" ]
    [
      ("header", Tool.s_string ~desc:"Short question header" ());
      ("question", Tool.s_string ~desc:"Question text" ());
      ("options", Tool.s_array question_option_schema);
      ("multi", Tool.s_bool ~default:false ());
      ("free_text", Tool.s_bool ~default:false ());
    ]

let question_schema =
  Tool.schema_object ~required:[ "questions" ]
    [ ("questions", Tool.s_array question_item_schema) ]

let info_schema = Tool.schema_object []
let info_codec = Jsont.Object.map () |> Jsont.Object.finish
let logs_schema = Tool.schema_object [ ("lines", Tool.s_int ~default:50 ()) ]

let truncate_output (ctx : Tool.ctx) text =
  let content, artifact =
    Artifact.truncate ctx.Tool.artifacts ~random:ctx.Tool.random text
  in
  Tool.ok ?artifact content

let render_todo_counts items =
  let pending, in_progress, completed =
    List.fold_left
      (fun (pending, in_progress, completed) (item : Todos.item) ->
        match item.Todos.status with
        | Todos.Pending -> (pending + 1, in_progress, completed)
        | Todos.In_progress -> (pending, in_progress + 1, completed)
        | Todos.Completed -> (pending, in_progress, completed + 1))
      (0, 0, 0) items
  in
  Fmt.str "\n%d pending, %d in progress, %d completed" pending in_progress completed

let run_todos (ctx : Tool.ctx) input =
  let* ({ todos } : todo_args) = Tool.decode todo_args_codec input in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"todos" ~action:"todos" ~path:ctx.Tool.cwd
      ~description:"Update the session todo list"
  in
  Todos.set ctx.Tool.todos todos;
  let text = Todos.render todos ^ render_todo_counts todos in
  Ok (truncate_output ctx text)

let words_count text =
  let count = ref 0 in
  let in_word = ref false in
  String.iter
    (fun character ->
      if character = ' ' || character = '\t' || character = '\r' || character = '\n' then
        in_word := false
      else if not !in_word then (
        incr count;
        in_word := true))
    text;
  !count

let validate_questions questions =
  let count = List.length questions in
  if count < 1 || count > 5 then
    Error (`Invalid_input "questions must contain between 1 and 5 items")
  else
    let rec check = function
      | [] -> Ok ()
      | question :: rest ->
          if words_count question.header > 3 then
            Error
              (`Invalid_input
                 (Fmt.str "question header %S must contain at most three words"
                    question.header))
          else if String.trim question.header = "" then
            Error (`Invalid_input "question header must not be empty")
          else if String.trim question.question = "" then
            Error
              (`Invalid_input (Fmt.str "question %S must not be empty" question.header))
          else check rest
    in
    check questions

let answer_for answers header =
  List.find_opt (fun (answer : Tool.answer) -> answer.Tool.header = header) answers

let answer_text header (answer : Tool.answer option) =
  match answer with
  | None -> Fmt.str "%s:" header
  | Some answer ->
      let selected = String.concat ", " answer.Tool.selected in
      let text = match answer.Tool.text with None -> "" | Some value -> "|" ^ value in
      Fmt.str "%s: %s%s" header selected text

let run_question (ctx : Tool.ctx) input =
  let* ({ questions } : question_args) = Tool.decode question_args_codec input in
  let* () = validate_questions questions in
  let* ask =
    match (ctx.Tool.interactive, ctx.Tool.is_subagent, ctx.Tool.ask) with
    | true, false, Some ask -> Ok ask
    | _ -> Error (`Unavailable "no interactive user")
  in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"question" ~action:"question"
      ~path:ctx.Tool.cwd ~description:"Ask the interactive user a question"
  in
  let values : Tool.question list =
    List.map
      (fun question ->
        {
          Tool.header = question.header;
          text = question.question;
          options =
            List.map
              (fun option -> (option.label, Option.value option.description ~default:""))
              question.options;
          multi = question.multi;
          free_text = question.free_text;
        })
      questions
  in
  match ask values with
  | Error `Not_interactive -> Error (`Unavailable "no interactive user")
  | Error `Aborted -> Error `Aborted
  | Ok answers ->
      let lines =
        List.map
          (fun question ->
            answer_text question.header (answer_for answers question.header))
          questions
      in
      Ok (truncate_output ctx (String.concat "\n" lines))

let selected_model_text = function
  | None -> "none"
  | Some (model : Config.selected_model) ->
      let reasoning =
        match model.Config.reasoning with
        | None -> "default"
        | Some `Off -> "off"
        | Some `Low -> "low"
        | Some `Medium -> "medium"
        | Some `High -> "high"
      in
      Fmt.str "%s/%s (%s)" model.Config.provider model.Config.model reasoning

let provider_lines (config : Config.t) =
  List.map
    (fun ((name, provider) : string * Config.provider) ->
      let credential =
        match provider.Config.api_key with
        | None -> "no configured key"
        | Some _ -> "api key configured"
      in
      let endpoint = Option.value provider.Config.base_url ~default:"default endpoint" in
      Fmt.str "  %s: %s, %s, %d configured models" name endpoint credential
        (List.length provider.Config.models))
    config.Config.providers

let lsp_lines (ctx : Tool.ctx) =
  match ctx.Tool.lsp with
  | None -> [ "  disabled" ]
  | Some lsp ->
      List.map
        (fun (name, state) ->
          let state =
            match state with
            | Lsp.Not_started -> "not started"
            | Lsp.Starting -> "starting"
            | Lsp.Ready -> "ready"
            | Lsp.Failed message -> "failed: " ^ message
            | Lsp.Disabled -> "disabled"
          in
          Fmt.str "  %s: %s" name state)
        (Lsp.servers lsp)

let mcp_lines (ctx : Tool.ctx) =
  List.map
    (fun (name, state) ->
      let state =
        match state with
        | Mcp.Connecting -> "connecting"
        | Mcp.Connected { tools; resources; prompts } ->
            Fmt.str "connected (%d tools, %d resources, %d prompts)" tools resources
              prompts
        | Mcp.Failed message -> "failed: " ^ message
        | Mcp.Disabled -> "disabled"
      in
      Fmt.str "  %s: %s" name state)
    (Mcp.states ctx.Tool.mcp)

let hook_state hooks event = if Hooks.has hooks event then "enabled" else "disabled"

let run_info (ctx : Tool.ctx) input =
  let* () = Tool.decode info_codec input in
  let* () =
    Tool.request ctx ~read_only:true ~tool:"crush_info" ~action:"info" ~path:ctx.Tool.cwd
      ~description:"Inspect crush configuration and runtime state"
  in
  let options = ctx.Tool.config.Config.options in
  let lines =
    [
      "config:";
      Fmt.str "  cwd: %s" ctx.Tool.cwd;
      "models:";
      Fmt.str "  large: %s"
        (selected_model_text ctx.Tool.config.Config.models.Config.large);
      Fmt.str "  small: %s"
        (selected_model_text ctx.Tool.config.Config.models.Config.small);
      "providers:";
    ]
    @ provider_lines ctx.Tool.config
    @ [ "lsp:" ] @ lsp_lines ctx @ [ "mcp:" ] @ mcp_lines ctx
    @ [
        "skills:";
        Skills.index_text ctx.Tool.skills;
        "permissions:";
        Fmt.str "  allowed: %s"
          (String.concat ", " ctx.Tool.config.Config.permissions.Config.allowed_tools);
        Fmt.str "  denied: %s"
          (String.concat ", " ctx.Tool.config.Config.permissions.Config.deny);
        "options:";
        Fmt.str "  data_dir: %s" options.Config.data_dir;
        Fmt.str "  debug: %b" options.Config.debug;
        Fmt.str "  auto_compaction: %b" (not options.Config.disable_auto_compaction);
        Fmt.str "  auto_lsp: %b" options.Config.auto_lsp;
        Fmt.str "  attribution: %s"
          (match options.Config.attribution.Config.trailer with
          | Config.Trailer_none -> "none"
          | Config.Co_authored -> "co_authored"
          | Config.Assisted -> "assisted");
        "hooks:";
        Fmt.str "  pre_tool: %s" (hook_state ctx.Tool.hooks Config.Pre_tool);
        Fmt.str "  post_tool: %s" (hook_state ctx.Tool.hooks Config.Post_tool);
        Fmt.str "  session_start: %s" (hook_state ctx.Tool.hooks Config.Session_start);
        Fmt.str "  stop: %s" (hook_state ctx.Tool.hooks Config.Stop);
      ]
  in
  Ok
    (truncate_output ctx
       (String.concat "\n" (List.filter (fun line -> line <> "") lines)))

let read_log (ctx : Tool.ctx) =
  try Ok (Eio.Path.load Eio.Path.(ctx.Tool.fs / ctx.Tool.log_path)) with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> Error (`Not_found ctx.Tool.log_path)
  | Eio.Io _ -> Error (`Io (ctx.Tool.log_path, "could not read log"))

let tail_lines ~count body =
  let lines = Array.of_list (String.split_on_char '\n' body) in
  let total = Array.length lines in
  let total = if total > 0 && lines.(total - 1) = "" then total - 1 else total in
  let first = max 0 (total - count) in
  let selected = List.init (total - first) (fun offset -> lines.(first + offset)) in
  String.concat "\n" selected

let run_logs (ctx : Tool.ctx) input =
  let* ({ lines } : logs_args) = Tool.decode logs_codec input in
  if lines < 1 || lines > 100 then
    Error (`Invalid_input "lines must be between 1 and 100")
  else
    let* () =
      Tool.request ctx ~read_only:true ~tool:"crush_logs" ~action:"logs"
        ~path:(Tool.absolute ctx ctx.Tool.log_path)
        ~description:"Read the crush log"
    in
    let* body = read_log ctx in
    Ok (truncate_output ctx (tail_lines ~count:lines body))

let todos =
  {
    Tool.name = "todos";
    description = "Update the current todo list";
    schema = todos_schema;
    read_only = false;
    run = run_todos;
  }

let question =
  {
    Tool.name = "question";
    description = "Ask the user one or more questions";
    schema = question_schema;
    read_only = true;
    run = run_question;
  }

let crush_info =
  {
    Tool.name = "crush_info";
    description = "Inspect crush runtime information";
    schema = info_schema;
    read_only = true;
    run = run_info;
  }

let crush_logs =
  {
    Tool.name = "crush_logs";
    description = "Read recent crush log lines";
    schema = logs_schema;
    read_only = true;
    run = run_logs;
  }

let all = [ todos; question; crush_info; crush_logs ]
