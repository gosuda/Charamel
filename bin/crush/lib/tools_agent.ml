let ( let* ) = Result.bind

type task = { prompt : string }
type request = { prompt : string option; tasks : task list option; max_active : int }

let task_codec =
  let open Jsont in
  Object.map (fun (prompt : string) -> ({ prompt } : task))
  |> Object.mem "prompt" string ~enc:(fun (value : task) -> value.prompt)
  |> Object.finish

let request_codec =
  let open Jsont in
  Object.map
    (fun (prompt : string option) (tasks : task list option) (max_active : int) ->
      ({ prompt; tasks; max_active } : request))
  |> Object.mem "prompt" (option string) ~dec_absent:None
       ~enc:(fun (value : request) -> value.prompt)
       ~enc_omit:Option.is_none
  |> Object.mem "tasks"
       (option (list task_codec))
       ~dec_absent:None
       ~enc:(fun (value : request) -> value.tasks)
       ~enc_omit:Option.is_none
  |> Object.mem "max_active" int ~dec_absent:8 ~enc:(fun (value : request) ->
      value.max_active)
  |> Object.finish

let task_schema = Tool.s_object ~required:[ "prompt" ] [ ("prompt", Tool.s_string ()) ]

let schema =
  Tool.schema_object
    [
      ("prompt", Tool.s_string ~desc:"One prompt for a child agent" ());
      ("tasks", Tool.s_array ~desc:"Prompts executed in input order" task_schema);
      ( "max_active",
        Tool.s_int ~default:8 ~desc:"Maximum active children, from one to eight" () );
    ]

let truncate_output (ctx : Tool.ctx) text =
  let content, artifact =
    Artifact.truncate ctx.Tool.artifacts ~random:ctx.Tool.random text
  in
  Tool.ok ?artifact content

let validate_request ({ prompt; tasks; max_active } : request) =
  if max_active < 1 || max_active > 8 then
    Error (`Invalid_input "max_active must be between 1 and 8")
  else
    match (prompt, tasks) with
    | Some value, None when String.trim value <> "" -> Ok [ value ]
    | None, Some values ->
        if values = [] then Error (`Invalid_input "tasks must not be empty")
        else if List.exists (fun (task : task) -> String.trim task.prompt = "") values
        then Error (`Invalid_input "task prompts must not be empty")
        else Ok (List.map (fun (task : task) -> task.prompt) values)
    | Some _, Some _ -> Error (`Invalid_input "provide either prompt or tasks, not both")
    | None, None -> Error (`Invalid_input "one of prompt or tasks is required")
    | Some _, None -> Error (`Invalid_input "prompt must not be empty")

let run_children (ctx : Tool.ctx) ~max_active prompts =
  match ctx.Tool.run_subagent with
  | None -> Error (`Unavailable "agent delegation is unavailable")
  | Some run_subagent ->
      let count = List.length prompts in
      let inputs = Array.of_list prompts in
      let results : (string, string) result option array = Array.make count None in
      let next = ref 0 in
      let mutex = Eio.Mutex.create () in
      let claim () =
        Eio.Mutex.use_rw ~protect:true mutex (fun () ->
            if !next >= count then None
            else
              let index = !next in
              incr next;
              Some index)
      in
      let worker () =
        let rec loop () =
          match claim () with
          | None -> ()
          | Some index ->
              results.(index) <- Some (run_subagent ~prompt:inputs.(index));
              loop ()
        in
        loop ()
      in
      let workers = min max_active count in
      let promises =
        Array.init workers (fun _ -> Eio.Fiber.fork_promise ~sw:ctx.Tool.sw worker)
      in
      Array.iter (fun promise -> ignore (Eio.Promise.await promise)) promises;
      let missing = Error "child worker returned no result" in
      Ok (Array.to_list (Array.map (Option.value ~default:missing) results))

let render_tasks results =
  let failures = ref false in
  let lines =
    List.mapi
      (fun index result ->
        match result with
        | Ok text -> Fmt.str "task %d: %s" (index + 1) text
        | Error message ->
            failures := true;
            Fmt.str "task %d: error: %s" (index + 1) message)
      results
  in
  (String.concat "\n" lines, !failures)

let run_single ctx = function
  | [ Ok text ] -> Ok (truncate_output ctx text)
  | [ Error message ] -> Ok (Tool.fail message)
  | _ -> Error (`Unavailable "agent returned no result")

let run_agent ctx input =
  let* ({ prompt; tasks; max_active } : request) = Tool.decode request_codec input in
  let* prompts = validate_request { prompt; tasks; max_active } in
  let* () =
    Tool.request ctx ~read_only:false ~tool:"agent" ~action:"agent" ~path:""
      ~description:"Delegate work to child agents"
  in
  let* results = run_children ctx ~max_active prompts in
  match (prompt, tasks) with
  | Some _, None -> run_single ctx results
  | None, Some _ ->
      let text, failed = render_tasks results in
      let output = truncate_output ctx text in
      Ok (if failed then { output with is_error = true } else output)
  | _ -> Error (`Invalid_input "one of prompt or tasks is required")

let agent =
  {
    Tool.name = "agent";
    description = "Delegate one prompt or a bounded task array to child agents";
    schema;
    read_only = false;
    run = run_agent;
  }
