let is_tty_stdin = Os_platform.Tty.is_stdin
let is_tty_stdout = Os_platform.Tty.is_stdout
let size_stdout = Os_platform.Tty.size_stdout
let size_of_output = Os_platform.Tty.size_of_output
let supports_suspend = Os_platform.Tty.supports_suspend
let open_controlling_in = Os_platform.Tty.controlling_input

let enter_raw () =
  let saved = Os_platform.Tty.enter_raw () in
  fun () -> Os_platform.Tty.restore saved

let echo_off () =
  let saved = Os_platform.Tty.echo_off () in
  fun () -> Os_platform.Tty.restore saved

(* One watcher per process, one entry per subscriber: a subscriber that has gone away is
   disarmed by its identifier rather than by comparing function values, and the last
   unsubscribe leaves the watcher installed, which is harmless because it only reads. *)
let counter = ref 0

type subscription = { id : int; mutable callback : (unit -> unit) option }

let subscribers : subscription list ref = ref []
let watching = ref false

let notify () =
  List.iter
    (fun { callback; _ } ->
      match callback with Some callback -> callback () | None -> ())
    !subscribers

let on_resize callback =
  incr counter;
  let subscription = { id = !counter; callback = Some callback } in
  subscribers := subscription :: !subscribers;
  if not !watching then (
    watching := true;
    Os_platform.Tty.watch_resizes notify);
  let unsubscribe () =
    subscription.callback <- None;
    subscribers := List.filter (fun other -> other.id <> subscription.id) !subscribers
  in
  Lwt.return unsubscribe
