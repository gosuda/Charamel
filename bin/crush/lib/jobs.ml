open Lwt.Infix

type status = Running | Exited of int | Killed

type job = {
  id : string;
  ordinal : int;
  timeout_s : int;
  buffer : Buffer.t;
  bytes_seen : int ref;
  overflow : bool ref;
  mutable process : Charamel_os.Process.t option;
  mutable status : status;
  mutable kill_requested : bool;
  mutable kill_scheduled : bool;
  mutable rendered : (string * string option) option;
  done_ : unit Lwt_condition.t;
}

type t = {
  sw : Lwt_switch.t;
  artifacts : Artifact.t;
  lock : Lwt_mutex.t;
  jobs : (string, job) Hashtbl.t;
  mutable next_id : int;
  random : Random.State.t;
}

let max_output_bytes = 10 * 1024 * 1024
let max_completed_jobs = 128
let wait_timeout_s = 600.

let create ~sw ~artifacts =
  {
    sw;
    artifacts;
    lock = Lwt_mutex.create ();
    jobs = Hashtbl.create 32;
    next_id = 1;
    random = Random.State.make_self_init ();
  }

let status_is_running = function Running -> true | Exited _ | Killed -> false

let append_capped buffer bytes_seen overflow text =
  let length = String.length text in
  let available = max_output_bytes - !bytes_seen in
  let accepted = min available length in
  if accepted > 0 then Buffer.add_substring buffer text 0 accepted;
  bytes_seen := !bytes_seen + length;
  if accepted < length then overflow := true

let random_bytes state length =
  Bytes.init length (fun _ -> Char.chr (Random.State.bits state land 0xFF))
  |> Bytes.to_string

let env_key value =
  match String.index_opt value '=' with
  | None -> value
  | Some index -> String.sub value 0 index

let without_override entries key = List.filter (fun entry -> env_key entry <> key) entries

let merged_environment overrides =
  let inherited = Array.to_list (Unix.environment ()) in
  List.fold_left
    (fun entries (key, value) -> without_override entries key @ [ key ^ "=" ^ value ])
    inherited overrides
  |> Array.of_list

let schedule_delayed_kill t job process =
  Lwt.async (fun () ->
      Lwt.catch
        (fun () ->
          Lwt_unix.sleep Charamel_os.Process.grace >>= fun () ->
          Lwt_mutex.with_lock t.lock (fun () ->
              if status_is_running job.status then Charamel_os.Process.kill_tree process;
              Lwt.return_unit))
        (function Unix.Unix_error _ -> Lwt.return_unit | exn -> Lwt.fail exn))

let set_process t job process =
  Lwt_mutex.with_lock t.lock (fun () ->
      job.process <- Some process;
      let schedule = job.kill_requested && not job.kill_scheduled in
      if schedule then job.kill_scheduled <- true;
      Lwt.return schedule)
  >>= function
  | false -> Lwt.return_unit
  | true ->
      Charamel_os.Process.terminate_tree process;
      Lwt.return (schedule_delayed_kill t job process)

let append_spawn_error t job message =
  Lwt_mutex.with_lock t.lock (fun () ->
      append_capped job.buffer job.bytes_seen job.overflow message;
      Lwt.return_unit)

let prune_completed t =
  let completed =
    Hashtbl.fold
      (fun id job acc ->
        if status_is_running job.status then acc else (id, job.ordinal) :: acc)
      t.jobs []
  in
  let excess = List.length completed - max_completed_jobs in
  if excess > 0 then
    List.sort (fun (_, left) (_, right) -> Int.compare left right) completed
    |> List.filteri (fun index _ -> index < excess)
    |> List.iter (fun (id, _) -> Hashtbl.remove t.jobs id)

let finish t job final_status =
  Lwt_mutex.with_lock t.lock (fun () ->
      if status_is_running job.status then begin
        job.status <- final_status;
        job.process <- None;
        Lwt_condition.broadcast job.done_ ();
        prune_completed t
      end;
      Lwt.return_unit)

let was_kill_requested t job =
  Lwt_mutex.with_lock t.lock (fun () -> Lwt.return job.kill_requested)

let read_chunk channel chunk =
  Lwt.catch
    (fun () -> Lwt_io.read_into channel chunk 0 (Bytes.length chunk))
    (function
      | End_of_file | Lwt_io.Channel_closed _ | Unix.Unix_error _ | Sys_error _ ->
          Lwt.return 0
      | exn -> Lwt.fail exn)

let capture t channel job =
  let chunk = Bytes.create 4096 in
  let rec pump () =
    read_chunk channel chunk >>= fun read ->
    if read = 0 then Lwt.return_unit
    else
      Lwt_mutex.with_lock t.lock (fun () ->
          append_capped job.buffer job.bytes_seen job.overflow
            (Bytes.sub_string chunk 0 read);
          Lwt.return_unit)
      >>= pump
  in
  pump ()

let capture_outputs t process job =
  Lwt.join
    [
      capture t (Charamel_os.Process.stdout_r process) job;
      capture t (Charamel_os.Process.stderr_r process) job;
    ]

(* [Charamel_os.Process.await] hands out the one shared promise that the reaper
   resolves. [Lwt_unix.with_timeout] and [Lwt.pick] both cancel what they wrap or
   race, and cancelling that promise would poison every later await of the same
   child. Races therefore go through [Lwt.protected], the documented cancel
   barrier, and use [Lwt.choose], which never cancels the loser. *)
let reap process = Lwt.protected (Charamel_os.Process.await process)

let exit_within process timeout_s =
  if timeout_s <= 0 then reap process >|= fun code -> `Exit code
  else
    Lwt.choose
      [
        (reap process >|= fun code -> `Exit code);
        (Lwt_unix.sleep (float_of_int timeout_s) >|= fun () -> `Deadline);
      ]

(* The deadline owns the kill: a graceful request to the group, the grace window, then
   the force. The capture pump ends when the group dies, so it is awaited after the kill,
   never before it. *)
let kill_at_deadline process =
  Charamel_os.Process.stop_after_grace process >>= fun () ->
  reap process >|= fun _ -> ()

let settle t job process captured code =
  let teardown =
    if Charamel_os.Process.group_alive process then
      Charamel_os.Process.stop_after_grace process
    else Lwt.return_unit
  in
  (* The drain and the group teardown run beside each other — a grandchild holding a
     pipe open is closed by that teardown — and the job finishes only once both are
     over, so no trailing byte lands after the status is set. *)
  Lwt.join [ captured; teardown ] >>= fun () ->
  was_kill_requested t job >>= fun requested ->
  if requested then finish t job Killed
  else
    (* An exit code of [128 + n] alone cannot tell a signal death from a deliberate
       [exit (128 + n)]; {!Charamel_os.Process.signal} keeps the two apart. *)
    Charamel_os.Process.signal process >>= function
    | Some _ -> finish t job Killed
    | None -> finish t job (Exited code)

let spawn cwd command env =
  Charamel_os.Process.spawn ~cwd ~env:(merged_environment env) ~stdin:`Inherit
    ~stdout:`Pipe ~stderr:`Pipe
    (if Sys.win32 then [ "cmd"; "/c"; command ^ " 2>&1" ]
     else [ "/bin/sh"; "-c"; "( " ^ command ^ " ) 2>&1" ])

let report_spawn t job message =
  append_spawn_error t job message >>= fun () ->
  was_kill_requested t job >>= fun requested ->
  finish t job (if requested then Killed else Exited 127)

let cancel_job t job =
  Lwt_mutex.with_lock t.lock (fun () -> Lwt.return job.process) >>= fun process ->
  Option.iter (fun process -> Charamel_os.Process.kill_tree process) process;
  finish t job Killed

let spawn_failure t job = function
  | Unix.Unix_error (error, function_name, argument) ->
      report_spawn t job
        (Fmt.str "job %s: %s" job.id
           (Io.message (Unix.Unix_error (error, function_name, argument))))
  | Invalid_argument message -> report_spawn t job (Fmt.str "job %s: %s" job.id message)
  | Lwt.Canceled -> cancel_job t job
  | exn -> Lwt.fail exn

let run_job t job ~cwd ~command ~env =
  Lwt.catch
    (fun () ->
      let process = spawn cwd command env in
      set_process t job process >>= fun () ->
      let captured = capture_outputs t process job in
      exit_within process job.timeout_s >>= function
      | `Exit code -> settle t job process captured code
      | `Deadline ->
          kill_at_deadline process >>= fun () ->
          captured >>= fun () -> finish t job Killed)
    (spawn_failure t job)

let new_job _t number =
  {
    id = Fmt.str "job-%d" number;
    ordinal = number;
    timeout_s = 0;
    buffer = Buffer.create 4096;
    bytes_seen = ref 0;
    overflow = ref false;
    process = None;
    status = Running;
    kill_requested = false;
    kill_scheduled = false;
    rendered = None;
    done_ = Lwt_condition.create ();
  }

let start t ~cwd ~command ~env ~timeout_s =
  if timeout_s <= 0 then invalid_arg "Jobs.start: timeout_s must be positive";
  let job =
    let number = t.next_id in
    t.next_id <- number + 1;
    let job = new_job t number in
    { job with timeout_s }
  in
  Hashtbl.add t.jobs job.id job;
  Lwt_switch.add_hook (Some t.sw) (fun () -> cancel_job t job);
  Lwt.async (fun () -> run_job t job ~cwd ~command ~env);
  job.id

let find t id =
  Lwt_mutex.with_lock t.lock (fun () -> Lwt.return (Hashtbl.find_opt t.jobs id))

let raw_output t job =
  Lwt_mutex.with_lock t.lock (fun () ->
      let content = Buffer.contents job.buffer in
      Lwt.return
      @@
      if !(job.overflow) then
        content ^ Fmt.str "\n[... output truncated at %d bytes ...]" max_output_bytes
      else content)

let completed_output t job raw =
  Lwt_mutex.with_lock t.lock (fun () ->
      match job.rendered with
      | Some (content, _) -> Lwt.return content
      | None ->
          Artifact.truncate t.artifacts ~random:(random_bytes t.random) raw
          >>= fun ((content, _) as value) ->
          job.rendered <- Some value;
          Lwt.return content)

let wait_until_settled t job =
  Lwt_mutex.with_lock t.lock (fun () ->
      let rec loop () =
        if not (status_is_running job.status) then Lwt.return_unit
        else Lwt_condition.wait ~mutex:t.lock job.done_ >>= loop
      in
      loop ())

let wait_for_settlement t job =
  Lwt.catch
    (fun () -> Lwt_unix.with_timeout wait_timeout_s (fun () -> wait_until_settled t job))
    (function Lwt_unix.Timeout -> Lwt.return_unit | exn -> Lwt.fail exn)

let output t ~id ~wait =
  find t id >>= function
  | None -> Lwt.return_error (`Not_found id)
  | Some job ->
      (if wait then wait_for_settlement t job else Lwt.return_unit) >>= fun () ->
      raw_output t job >>= fun raw ->
      Lwt_mutex.with_lock t.lock (fun () -> Lwt.return job.status) >>= fun status ->
      if status_is_running status then Lwt.return_ok (raw, status)
      else completed_output t job raw >|= fun value -> Ok (value, status)

let schedule_force_kill t job =
  Lwt_mutex.with_lock t.lock (fun () ->
      let should_schedule = (not job.kill_scheduled) && status_is_running job.status in
      if should_schedule then job.kill_scheduled <- true;
      Lwt.return (should_schedule, job.process))
  >>= function
  | true, Some process -> Lwt.return (schedule_delayed_kill t job process)
  | _ -> Lwt.return_unit

let kill t ~id =
  find t id >>= function
  | None -> Lwt.return_error (`Not_found id)
  | Some job ->
      ( Lwt_mutex.with_lock t.lock (fun () ->
            if status_is_running job.status then begin
              job.kill_requested <- true;
              Lwt.return job.process
            end
            else Lwt.return_none)
      >>= fun process ->
        match process with
        | None -> Lwt.return_unit
        | Some process ->
            Charamel_os.Process.terminate_tree process;
            schedule_force_kill t job )
      >>= fun () -> Lwt.return_ok ()

let running_processes t =
  Lwt_mutex.with_lock t.lock (fun () ->
      Lwt.return
      @@ Hashtbl.fold
           (fun _ job acc ->
             if status_is_running job.status then
               match job.process with
               | Some process -> (job, process) :: acc
               | None -> acc
             else acc)
           t.jobs [])

let kill_all t =
  Lwt_mutex.with_lock t.lock (fun () ->
      Lwt.return
      @@ Hashtbl.fold
           (fun id job acc ->
             if status_is_running job.status then (id, job) :: acc else acc)
           t.jobs [])
  >>= fun jobs ->
  if jobs = [] then Lwt.return_unit
  else
    Lwt_list.iter_s (fun (id, _) -> kill t ~id >|= fun _ -> ()) jobs >>= fun () ->
    Lwt_unix.sleep Charamel_os.Process.grace >>= fun () ->
    running_processes t >>= fun running ->
    Lwt_list.iter_s
      (fun (job, process) ->
        Lwt_mutex.with_lock t.lock (fun () ->
            if status_is_running job.status then Charamel_os.Process.kill_tree process;
            Lwt.return_unit))
      running

let list t =
  Lwt_mutex.with_lock t.lock (fun () ->
      Lwt.return
      @@ (Hashtbl.fold
            (fun _ job acc -> (job.ordinal, job.id, job.status) :: acc)
            t.jobs []
         |> List.sort (fun (left, _, _) (right, _, _) -> Int.compare left right)
         |> List.map (fun (_, id, status) -> (id, status))))
