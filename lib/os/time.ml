type clock = Real | Simulated of virtual_state
and virtual_state = { mutable now : float; mutable sleepers : sleeper list }
and sleeper = { at : float; task : unit Lwt.t; wake : unit Lwt.u }

type virtual_clock = virtual_state

let lwt = Real
let monotonic () = Int64.to_float (Mtime.to_uint64_ns (Mtime_clock.now ())) /. 1e9
let now = function Real -> monotonic () | Simulated state -> state.now
let wall = function Real -> Unix.gettimeofday () | Simulated state -> state.now

let insert sleeper =
  let rec place already = function
    | [] -> List.rev (sleeper :: already)
    | first :: rest when first.at <= sleeper.at -> place (first :: already) rest
    | rest -> List.rev_append (sleeper :: already) rest
  in
  place []

let sleep clock seconds =
  if seconds <= 0. then Lwt.pause ()
  else
    match clock with
    | Real -> Lwt_unix.sleep seconds
    | Simulated state ->
        (* [Lwt.task], not [Lwt.wait]: a promise from [wait] cannot be cancelled, and a
           sleeper that outlives its cancellation would wake later on its own deadline. *)
        let task, wake = Lwt.task () in
        let sleeper = { at = state.now +. seconds; task; wake } in
        state.sleepers <- insert sleeper state.sleepers;
        Lwt.on_cancel task (fun () ->
            state.sleepers <- List.filter (fun other -> other != sleeper) state.sleepers);
        task

let wake_all state deadline =
  let due, waiting =
    List.partition (fun sleeper -> sleeper.at <= deadline) state.sleepers
  in
  state.sleepers <- waiting;
  List.iter
    (fun { task; wake; _ } -> if Lwt.state task = Lwt.Sleep then Lwt.wakeup wake ())
    (List.sort (fun a b -> compare a.at b.at) due)

let create_virtual () =
  let state = { now = 0.; sleepers = [] } in
  let advance seconds =
    if seconds < 0. then invalid_arg "Charamel_os.Time: cannot advance a clock backwards";
    state.now <- state.now +. seconds;
    wake_all state state.now
  in
  (state, advance)

let next_deadline { sleepers; _ } =
  match sleepers with [] -> None | first :: _ -> Some first.at

let of_virtual state = Simulated state
