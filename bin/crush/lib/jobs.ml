type status = Running | Exited of int | Killed
type process = Eio_unix.Process.ty Eio.Resource.t

type job = {
  id : string;
  ordinal : int;
  timeout_s : int;
  buffer : Buffer.t;
  bytes_seen : int ref;
  overflow : bool ref;
  mutable process : process option;
  mutable job_sw : Eio.Switch.t option;
  mutable status : status;
  mutable kill_requested : bool;
  mutable kill_scheduled : bool;
  mutable rendered : (string * string option) option;
  done_ : Eio.Condition.t;
}

type t = {
  sw : Eio.Switch.t;
  proc_mgr : Eio_unix.Process.mgr_ty Eio.Resource.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  artifacts : Artifact.t;
  lock : Eio.Mutex.t;
  jobs : (string, job) Hashtbl.t;
  mutable next_id : int;
  random : Random.State.t;
}

let max_output_bytes = 10 * 1024 * 1024
let max_completed_jobs = 128

module Capped_sink = struct
  type t = {
    buffer : Buffer.t;
    bytes_seen : int ref;
    overflow : bool ref;
    lock : Eio.Mutex.t;
  }

  let append t source offset length =
    let available = max_output_bytes - !(t.bytes_seen) in
    let accepted = min available length in
    if accepted > 0 then begin
      let bytes = Cstruct.to_bytes (Cstruct.sub source offset accepted) in
      Buffer.add_bytes t.buffer bytes
    end;
    t.bytes_seen := !(t.bytes_seen) + length;
    if accepted < length then t.overflow := true

  let single_write t buffers =
    let total = Cstruct.lenv buffers in
    Eio.Mutex.use_rw ~protect:false t.lock (fun () ->
        List.iter
          (fun buffer ->
            let length = Cstruct.length buffer in
            append t buffer 0 length)
          buffers);
    total

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let capped_sink job lock =
  Eio.Resource.T
    ( {
        Capped_sink.buffer = job.buffer;
        bytes_seen = job.bytes_seen;
        overflow = job.overflow;
        lock;
      },
      Eio.Flow.Pi.sink (module Capped_sink) )

let random_bytes state length =
  Bytes.init length (fun _ -> Char.chr (Random.State.bits state land 0xFF))
  |> Bytes.to_string

let create ~sw ~proc_mgr ~clock ~artifacts =
  {
    sw;
    proc_mgr;
    clock;
    artifacts;
    lock = Eio.Mutex.create ();
    jobs = Hashtbl.create 32;
    next_id = 1;
    random = Random.State.make_self_init ();
  }

let status_is_running = function Running -> true | Exited _ | Killed -> false

let env_key value =
  match String.index_opt value '=' with
  | None -> value
  | Some index -> String.sub value 0 index

let merged_environment overrides =
  let inherited = Eio_unix.run_in_systhread Unix.environment |> Array.to_list in
  let without_override entries key =
    List.filter (fun entry -> env_key entry <> key) entries
  in
  List.fold_left
    (fun entries (key, value) -> without_override entries key @ [ key ^ "=" ^ value ])
    inherited overrides
  |> Array.of_list

let signal_group process signal =
  let pid = Eio.Process.pid process in
  try Eio_unix.run_in_systhread (fun () -> Unix.kill (-pid) signal)
  with Unix.Unix_error ((Unix.ESRCH | Unix.EPERM), _, _) -> ()

let process_group_exists process =
  let pid = Eio.Process.pid process in
  try
    Eio_unix.run_in_systhread (fun () -> Unix.kill (-pid) 0);
    true
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> false
  | Unix.Unix_error (Unix.EPERM, _, _) -> true

let fd_of_sink sink =
  match Eio_unix.Resource.fd_opt sink with
  | Some fd -> fd
  | None -> invalid_arg "job pipe is not backed by a Unix file descriptor"

let schedule_delayed_kill t job ~sw process =
  Eio.Fiber.fork ~sw (fun () ->
      try
        Eio.Time.sleep t.clock 2.;
        let still_running =
          Eio.Mutex.use_ro t.lock (fun () -> status_is_running job.status)
        in
        if still_running then signal_group process Sys.sigkill
      with Eio.Cancel.Cancelled _ -> ())

let set_process t job ~sw process =
  let schedule =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        job.process <- Some process;
        if job.kill_requested && not job.kill_scheduled then begin
          job.kill_scheduled <- true;
          true
        end
        else false)
  in
  if schedule then begin
    signal_group process Sys.sigterm;
    schedule_delayed_kill t job ~sw process
  end

let append_spawn_error t job message =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      let length = String.length message in
      let available = max 0 (max_output_bytes - !(job.bytes_seen)) in
      let accepted = min available length in
      if accepted > 0 then Buffer.add_substring job.buffer message 0 accepted;
      job.bytes_seen := !(job.bytes_seen) + length;
      if accepted < length then job.overflow := true)

let prune_completed t =
  let completed =
    Hashtbl.fold
      (fun id job acc ->
        if status_is_running job.status then acc else (id, job.ordinal) :: acc)
      t.jobs []
  in
  let excess = List.length completed - max_completed_jobs in
  if excess > 0 then begin
    let sorted =
      List.sort (fun (_, left) (_, right) -> Int.compare left right) completed
    in
    let rec remove count = function
      | _ when count = 0 -> ()
      | (id, _) :: rest ->
          Hashtbl.remove t.jobs id;
          remove (count - 1) rest
      | [] -> ()
    in
    remove excess sorted
  end

let finish t job final_status =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      if status_is_running job.status then begin
        job.status <- final_status;
        job.process <- None;
        Eio.Condition.broadcast job.done_;
        prune_completed t
      end)

let was_kill_requested t job = Eio.Mutex.use_ro t.lock (fun () -> job.kill_requested)

let terminate_group t process =
  signal_group process Sys.sigterm;
  Eio.Time.sleep t.clock 2.;
  signal_group process Sys.sigkill

let run_job t job ~sw ~cwd ~command ~env ~timeout_s =
  let run () =
    let output_source, output_sink = Eio.Process.pipe ~sw t.proc_mgr in
    let process =
      Eio_unix.Process.spawn_unix ~sw t.proc_mgr ~pgid:0 ~env:(merged_environment env)
        ~fds:
          [
            (1, fd_of_sink output_sink, `Blocking); (2, fd_of_sink output_sink, `Blocking);
          ]
        [ "/bin/sh"; "-c"; "cd -- " ^ Filename.quote cwd ^ " && " ^ command ]
    in
    Eio.Flow.close output_sink;
    set_process t job ~sw process;
    let copy_outputs () =
      let output_sink = capped_sink job t.lock in
      try Eio.Flow.copy output_source output_sink with
      | End_of_file -> ()
      | Eio.Io _ -> ()
    in
    let (child_status, timed_out), _ =
      Eio.Fiber.pair
        (fun () ->
          let outcome =
            if timeout_s <= 0 then `Finished (Eio.Process.await process)
            else
              Eio.Fiber.first
                (fun () -> `Finished (Eio.Process.await process))
                (fun () ->
                  Eio.Time.sleep t.clock (float_of_int timeout_s);
                  `Timed_out)
          in
          match outcome with
          | `Finished status ->
              if process_group_exists process then terminate_group t process;
              (status, false)
          | `Timed_out ->
              signal_group process Sys.sigterm;
              Eio.Time.sleep t.clock 2.;
              signal_group process Sys.sigkill;
              (Eio.Process.await process, true))
        copy_outputs
    in
    if timed_out then finish t job Killed
    else
      match child_status with
      | `Exited code ->
          finish t job (if was_kill_requested t job then Killed else Exited code)
      | `Signaled _ -> finish t job Killed
  in
  try run () with
  | Eio.Cancel.Cancelled _ ->
      let process = Eio.Mutex.use_ro t.lock (fun () -> job.process) in
      Option.iter (fun process -> signal_group process Sys.sigkill) process;
      finish t job Killed
  | Eio.Io _ as exception_ ->
      append_spawn_error t job (Fmt.str "job %s: %a" job.id Eio.Exn.pp exception_);
      finish t job (if was_kill_requested t job then Killed else Exited 127)
  | Unix.Unix_error (error, function_name, argument) ->
      append_spawn_error t job
        (Fmt.str "job %s: %s (%s %s)" job.id (Unix.error_message error) function_name
           argument);
      finish t job (if was_kill_requested t job then Killed else Exited 127)
  | Invalid_argument message ->
      append_spawn_error t job (Fmt.str "job %s: %s" job.id message);
      finish t job (if was_kill_requested t job then Killed else Exited 127)

let start t ~cwd ~command ~env ~timeout_s =
  if timeout_s <= 0 then invalid_arg "Jobs.start: timeout_s must be positive";
  let id, job =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        let number = t.next_id in
        t.next_id <- number + 1;
        let id = Fmt.str "job-%d" number in
        let job =
          {
            id;
            ordinal = number;
            timeout_s;
            buffer = Buffer.create 4096;
            bytes_seen = ref 0;
            overflow = ref false;
            process = None;
            job_sw = None;
            status = Running;
            kill_requested = false;
            kill_scheduled = false;
            rendered = None;
            done_ = Eio.Condition.create ();
          }
        in
        Hashtbl.add t.jobs id job;
        (id, job))
  in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
      try
        Eio.Switch.run (fun job_sw ->
            Eio.Mutex.use_rw ~protect:true t.lock (fun () -> job.job_sw <- Some job_sw);
            run_job t job ~sw:job_sw ~cwd ~command ~env ~timeout_s:job.timeout_s)
      with Eio.Cancel.Cancelled _ -> finish t job Killed);
  id

let find t id = Eio.Mutex.use_ro t.lock (fun () -> Hashtbl.find_opt t.jobs id)

let raw_output t job =
  Eio.Mutex.use_ro t.lock (fun () ->
      let content = Buffer.contents job.buffer in
      if !(job.overflow) then
        content ^ Fmt.str "\n[... output truncated at %d bytes ...]" max_output_bytes
      else content)

let completed_output t job raw =
  Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
      match job.rendered with
      | Some (content, _) -> content
      | None ->
          let value = Artifact.truncate t.artifacts ~random:(random_bytes t.random) raw in
          job.rendered <- Some value;
          fst value)

let output t ~id ~wait =
  match find t id with
  | None -> Error (`Not_found id)
  | Some job ->
      if wait then begin
        let _ =
          Eio.Time.with_timeout t.clock 600. (fun () ->
              Eio.Mutex.use_ro t.lock (fun () ->
                  while status_is_running job.status do
                    Eio.Condition.await job.done_ t.lock
                  done;
                  Ok ()))
        in
        ()
      end;
      let raw = raw_output t job in
      let status = Eio.Mutex.use_ro t.lock (fun () -> job.status) in
      let value = if status_is_running status then raw else completed_output t job raw in
      Ok (value, status)

let schedule_force_kill t job process =
  let should_schedule =
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        if job.kill_scheduled || not (status_is_running job.status) then false
        else begin
          job.kill_scheduled <- true;
          true
        end)
  in
  if should_schedule then
    match Eio.Mutex.use_ro t.lock (fun () -> job.job_sw) with
    | Some sw -> schedule_delayed_kill t job ~sw process
    | None -> signal_group process Sys.sigkill

let kill t ~id =
  match find t id with
  | None -> Error (`Not_found id)
  | Some job ->
      let process =
        Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
            if status_is_running job.status then begin
              job.kill_requested <- true;
              job.process
            end
            else None)
      in
      (match process with
      | None -> ()
      | Some process ->
          signal_group process Sys.sigterm;
          schedule_force_kill t job process);
      Ok ()

let running_processes t =
  Eio.Mutex.use_ro t.lock (fun () ->
      Hashtbl.fold
        (fun _ job acc ->
          if status_is_running job.status then
            match job.process with Some process -> (job, process) :: acc | None -> acc
          else acc)
        t.jobs [])

let kill_all t =
  let jobs =
    Eio.Mutex.use_ro t.lock (fun () ->
        Hashtbl.fold
          (fun id job acc ->
            if status_is_running job.status then (id, job) :: acc else acc)
          t.jobs [])
  in
  if jobs <> [] then begin
    List.iter (fun (id, _) -> ignore (kill t ~id)) jobs;
    Eio.Time.sleep t.clock 2.;
    List.iter
      (fun (job, process) ->
        let running = Eio.Mutex.use_ro t.lock (fun () -> status_is_running job.status) in
        if running then signal_group process Sys.sigkill)
      (running_processes t)
  end

let list t =
  Eio.Mutex.use_ro t.lock (fun () ->
      Hashtbl.fold (fun _ job acc -> (job.ordinal, job.id, job.status) :: acc) t.jobs []
      |> List.sort (fun (left, _, _) (right, _, _) -> Int.compare left right)
      |> List.map (fun (_, id, status) -> (id, status)))
