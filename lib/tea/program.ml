open Lwt.Syntax

type error = [ `Interrupted | `Exn of exn * Printexc.raw_backtrace ]

type 'msg script_event =
  [ `Key of Key.t
  | `Text of string
  | `Resize of int * int
  | `Msg of 'msg
  | `Wait of float ]

type stop_reason = [ `Normal | `Interrupted ]

exception Daemon_failed of exn * Printexc.raw_backtrace

type 'msg action =
  | Effect_quit
  | Effect_interrupt
  | Effect_suspend
  | Effect_resume
  | Effect_exec of string list * (int -> 'msg)
  | Effect_print of string
  | Effect_clipboard of { selection : [ `System | `Primary ]; content : string }
  | Effect_read_clipboard of [ `System | `Primary ]
  | Effect_raw of string
  | Effect_query of
      [ `Background
      | `Foreground
      | `Cursor_color
      | `Terminal_version
      | `Kitty_flags
      | `Cursor_position
      | `Capability of string ]
  | Effect_window_size

let map_effect : type a b. (a -> b) -> a action -> b action =
 fun mapping action ->
  match action with
  | Effect_quit -> Effect_quit
  | Effect_interrupt -> Effect_interrupt
  | Effect_suspend -> Effect_suspend
  | Effect_resume -> Effect_resume
  | Effect_exec (argv, on_exit) -> Effect_exec (argv, fun code -> mapping (on_exit code))
  | Effect_print text -> Effect_print text
  | Effect_clipboard clipboard -> Effect_clipboard clipboard
  | Effect_read_clipboard selection -> Effect_read_clipboard selection
  | Effect_raw bytes -> Effect_raw bytes
  | Effect_query query -> Effect_query query
  | Effect_window_size -> Effect_window_size

type 'msg queued =
  | Queued_event of Event.t
  | Queued_initial of Event.t
  | Queued_message of 'msg * unit Lwt.u option
  | Queued_effect of 'msg action * unit Lwt.u
  | Queued_stop of stop_reason
  | Queued_script_done

type 'msg stream_source = { identity : Obj.t; pull : unit -> 'msg option Lwt.t }
type 'msg stream_reader = { source : 'msg stream_source; cancel : unit Lwt.t option ref }

type 'msg handlers = {
  key : (Key.t -> 'msg) list;
  key_release : (Key.t -> 'msg) list;
  mouse : (Mouse.t -> 'msg) list;
  paste : (string -> 'msg) list;
  focus : ([ `Focused | `Blurred ] -> 'msg) list;
  resize : (rows:int -> cols:int -> 'msg) list;
  terminal : (Event.t -> 'msg) list;
  every : (float * (Mtime.t -> 'msg)) list;
  streams : 'msg stream_source list;
  resume : (unit -> 'msg) list;
}

type 'msg timer = {
  interval : float;
  mutable callbacks : (Mtime.t -> 'msg) list;
  cancel : unit Lwt.t option ref;
}

type anchor = Fresh_line | Column_start | No_anchor

type output_effect =
  | Output_bytes of string * unit Lwt.u
  | Output_print of string * unit Lwt.u

type ('model, 'msg) runtime_state = {
  queue : 'msg queued Lwt_stream.t;
  push : 'msg queued -> unit Lwt.t;
  push_mutex : Lwt_mutex.t;
  mutable queued_count : int;
  effects : output_effect Queue.t;
  mutex : Lwt_mutex.t;
  condition : unit Lwt_condition.t;
  mutable model : 'model;
  mutable view : View.t;
  mutable handlers : 'msg handlers;
  mutable desired_size : int * int;
  mutable stop_requested : bool;
  mutable stop_reason : stop_reason;
  mutable script_finished : bool;
  mutable active_commands : int;
  command_cancels : (int * unit Lwt.t) list ref;
  mutable next_command_id : int;
  mutable initial_events_left : int;
  mutable dirty : bool;
  mutable paused : bool;
  mutable anchor : anchor;
  mutable last_frame : string;
  mutable timers : 'msg timer list;
  mutable streams : 'msg stream_reader list;
  mutable renderer_stop : bool;
  backlog : string Queue.t;
}

let empty_handlers =
  {
    key = [];
    key_release = [];
    mouse = [];
    paste = [];
    focus = [];
    resize = [];
    terminal = [];
    every = [];
    streams = [];
    resume = [];
  }

let append_handlers (a : 'msg handlers) (b : 'msg handlers) =
  {
    key = a.key @ b.key;
    key_release = a.key_release @ b.key_release;
    mouse = a.mouse @ b.mouse;
    paste = a.paste @ b.paste;
    focus = a.focus @ b.focus;
    resize = a.resize @ b.resize;
    terminal = a.terminal @ b.terminal;
    every = a.every @ b.every;
    streams = a.streams @ b.streams;
    resume = a.resume @ b.resume;
  }

let map_stream_source f source =
  let pull () =
    let* next = source.pull () in
    Lwt.return (Option.map f next)
  in
  { identity = source.identity; pull }

let same_stream_source a b = a.identity == b.identity

let dedupe_streams sources =
  let rec loop seen = function
    | [] -> List.rev seen
    | source :: rest ->
        if List.exists (fun other -> same_stream_source source other) seen then
          loop seen rest
        else loop (source :: seen) rest
  in
  loop [] sources

let rec collect_sub : type a. a Sub.t -> a handlers = function
  | Sub.None_ -> empty_handlers
  | Sub.Batch subs ->
      List.fold_left
        (fun acc sub -> append_handlers acc (collect_sub sub))
        empty_handlers subs
  | Sub.Map (f, sub) ->
      let handlers = collect_sub sub in
      {
        key = List.map (fun handler key -> f (handler key)) handlers.key;
        key_release = List.map (fun handler key -> f (handler key)) handlers.key_release;
        mouse = List.map (fun handler mouse -> f (handler mouse)) handlers.mouse;
        paste = List.map (fun handler text -> f (handler text)) handlers.paste;
        focus = List.map (fun handler focus -> f (handler focus)) handlers.focus;
        resize =
          List.map (fun handler ~rows ~cols -> f (handler ~rows ~cols)) handlers.resize;
        terminal = List.map (fun handler event -> f (handler event)) handlers.terminal;
        every =
          List.map
            (fun (interval, handler) -> (interval, fun time -> f (handler time)))
            handlers.every;
        streams = List.map (map_stream_source f) handlers.streams;
        resume = List.map (fun handler () -> f (handler ())) handlers.resume;
      }
  | Sub.Key handler -> { empty_handlers with key = [ handler ] }
  | Sub.Key_release handler -> { empty_handlers with key_release = [ handler ] }
  | Sub.Mouse handler -> { empty_handlers with mouse = [ handler ] }
  | Sub.Paste handler -> { empty_handlers with paste = [ handler ] }
  | Sub.Focus handler -> { empty_handlers with focus = [ handler ] }
  | Sub.Resize handler -> { empty_handlers with resize = [ handler ] }
  | Sub.Every (interval, handler) ->
      { empty_handlers with every = [ (interval, handler) ] }
  | Sub.Terminal handler -> { empty_handlers with terminal = [ handler ] }
  | Sub.Stream stream ->
      {
        empty_handlers with
        streams =
          [ { identity = Obj.repr stream; pull = (fun () -> Lwt_stream.get stream) } ];
      }
  | Sub.Resume handler -> { empty_handlers with resume = [ handler ] }

let group_every entries =
  List.fold_left
    (fun groups (interval, callback) ->
      let rec insert = function
        | [] -> [ (interval, [ callback ]) ]
        | (old_interval, callbacks) :: rest when old_interval = interval ->
            (old_interval, callbacks @ [ callback ]) :: rest
        | group :: rest -> group :: insert rest
      in
      insert groups)
    [] entries

let valid_delay name value =
  if value < 0. || Float.is_nan value then invalid_arg (name ^ " must be non-negative");
  value

let mutate_state state mutate =
  Lwt_mutex.with_lock state.mutex (fun () ->
      mutate ();
      Lwt_condition.broadcast state.condition ();
      Lwt.return_unit)

let mutate_state_result state mutate =
  Lwt_mutex.with_lock state.mutex (fun () ->
      let value = mutate () in
      Lwt_condition.broadcast state.condition ();
      Lwt.return value)

let peek_state state read =
  Lwt_mutex.with_lock state.mutex (fun () -> Lwt.return (read ()))

let queue_external state item =
  state.queued_count <- state.queued_count + 1;
  Lwt_mutex.with_lock state.push_mutex (fun () ->
      let* () = state.push item in
      mutate_state state (fun () -> ()))

let wait_for_item state =
  let rec wait () =
    if state.queued_count > 0 then (
      let* item = Lwt_stream.get state.queue in
      state.queued_count <- state.queued_count - 1;
      Lwt.return item)
    else if (state.script_finished || state.stop_requested) && state.active_commands = 0
    then Lwt.return_none
    else
      let* () = Lwt_condition.wait ~mutex:state.mutex state.condition in
      wait ()
  in
  Lwt_mutex.with_lock state.mutex wait

let queue_effect state action =
  let promise, resolver = Lwt.wait () in
  let* () = queue_external state (Queued_effect (action, resolver)) in
  promise

let normalize_print text =
  if String.length text = 0 then "\n"
  else if String.get text (String.length text - 1) = '\n' then text
  else text ^ "\n"

let clipboard_prefix = function `System -> "c" | `Primary -> "p"

let clipboard_set_bytes selection content =
  "\x1b]52;" ^ clipboard_prefix selection ^ ";" ^ Base64.encode_string content ^ "\x07"

let clipboard_read_bytes selection = "\x1b]52;" ^ clipboard_prefix selection ^ ";?\x07"

let hex_digit value =
  Char.chr (if value < 10 then Char.code '0' + value else Char.code 'a' + value - 10)

let hex_encode s =
  let out = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      let value = Char.code c in
      Buffer.add_char out (hex_digit (value lsr 4));
      Buffer.add_char out (hex_digit (value land 15)))
    s;
  Buffer.contents out

let capability_query_bytes name = "\x1bP+q" ^ hex_encode name ^ "\x1b\\"
let set_dirty state = mutate_state state (fun () -> state.dirty <- true)
let update_anchor state anchor = mutate_state state (fun () -> state.anchor <- anchor)
let set_paused state paused = mutate_state state (fun () -> state.paused <- paused)

let drain_queue queue =
  let rec loop acc =
    if Queue.is_empty queue then List.rev acc else loop (Queue.take queue :: acc)
  in
  loop []

let take_render_batch ~paint state =
  let rec wait () =
    if
      (not state.renderer_stop)
      && (state.paused || ((not state.dirty) && Queue.is_empty state.effects))
    then
      let* () = Lwt_condition.wait ~mutex:state.mutex state.condition in
      wait ()
    else Lwt.return_unit
  in
  Lwt_mutex.with_lock state.mutex (fun () ->
      let* () = wait () in
      let effects =
        if state.paused && not state.renderer_stop then [] else drain_queue state.effects
      in
      let should_render =
        state.dirty && paint && ((not state.paused) || state.renderer_stop)
      in
      if should_render || not paint then state.dirty <- false;
      Lwt.return (effects, should_render, state.renderer_stop))

let queue_size = 256

let run_core ~(terminal : Terminal.t) ~fps ~filter ~clock ~now ~exec ~suspend ~signals
    ?(paint = true) ?profile ?script (app : ('model, 'msg) App.t) =
  if fps <= 0 then invalid_arg "fps must be positive";
  let fps = min 120 fps in
  let rows, cols = terminal.Terminal.size () in
  if rows <= 0 || cols <= 0 then invalid_arg "terminal size must be positive";
  let screen = Screen.create ~rows ~cols in
  let output_mutex = Lwt_mutex.create () in
  let can_suspend =
    Charamel_os.Tty.supports_suspend && terminal.Terminal.local
    && terminal.Terminal.is_tty
  in
  let profile =
    match profile with
    | Some profile -> profile
    | None ->
        Charamel_colorprofile.detect ~is_tty:terminal.Terminal.is_tty
          ~env:terminal.Terminal.env
  in
  let writer = Charamel_colorprofile.Writer.create ~profile terminal.Terminal.output in
  let entered = ref false in
  let old_handlers : (Sys.signal * Sys.signal_behavior) list ref = ref [] in
  let signal_watchers : Lwt_unix.signal_handler_id list ref = ref [] in
  let state_ref : ('model, 'msg) runtime_state option ref = ref None in
  let write_output_locked text =
    if text = "" then Lwt.return_unit else Charamel_colorprofile.Writer.write writer text
  in
  let with_output f = Lwt_mutex.with_lock output_mutex f in
  let restore_handlers () =
    List.iter Lwt_unix.disable_signal_handler !signal_watchers;
    signal_watchers := [];
    List.iter (fun (signal, behavior) -> Sys.set_signal signal behavior) !old_handlers;
    old_handlers := []
  in
  let failure, fail_waker = Lwt.task () in
  let fail_run exn backtrace =
    try Lwt.wakeup_exn fail_waker (Daemon_failed (exn, backtrace))
    with Invalid_argument _ -> ()
  in
  let cleanup () =
    let restore_output () =
      match !state_ref with
      | None -> Lwt.return_unit
      | Some state ->
          with_output (fun () ->
              let* () =
                if paint then write_output_locked (Screen.restore screen)
                else Lwt.return_unit
              in
              Lwt_list.iter_s write_output_locked (drain_queue state.backlog))
    in
    let leave_terminal () =
      if !entered then terminal.Terminal.leave ();
      Lwt.return_unit
    in
    Lwt.finalize
      (fun () -> Lwt.finalize restore_output leave_terminal)
      (fun () ->
        restore_handlers ();
        Lwt.return_unit)
  in
  Lwt.catch
    (fun () ->
      Lwt.finalize
        (fun () ->
          if paint then begin
            entered := true;
            terminal.Terminal.enter ()
          end;
          Lwt_switch.with_switch (fun sw ->
              let daemons : unit Lwt.t list ref = ref [] in
              Lwt_switch.add_hook (Some sw) (fun () ->
                  let handles = !daemons in
                  daemons := [];
                  List.iter Lwt.cancel handles;
                  Lwt.return_unit);
              let spawn_daemon_with_stop stop body =
                let task =
                  Lwt.catch
                    (fun () -> Lwt.pick [ body stop (); stop ])
                    (function
                      | Lwt.Canceled -> Lwt.return_unit
                      | exn ->
                          let backtrace = Printexc.get_raw_backtrace () in
                          fail_run exn backtrace;
                          Lwt.return_unit)
                in
                daemons := task :: !daemons;
                Lwt.async (fun () ->
                    Lwt.finalize
                      (fun () -> task)
                      (fun () ->
                        daemons := List.filter (fun other -> other != task) !daemons;
                        Lwt.return_unit))
              in
              let new_daemon_stop () = fst (Lwt.task ()) in
              let spawn_daemon body =
                let stop = new_daemon_stop () in
                spawn_daemon_with_stop stop body;
                stop
              in
              let fork_daemon body = ignore (spawn_daemon body) in
              let initial_model, initial_cmd = app.App.init () in
              let initial_view = app.App.view initial_model in
              let queue, source = Lwt_stream.create_bounded queue_size in
              let push = source#push in
              let state =
                {
                  queue;
                  push;
                  push_mutex = Lwt_mutex.create ();
                  queued_count = 0;
                  effects = Queue.create ();
                  mutex = Lwt_mutex.create ();
                  condition = Lwt_condition.create ();
                  model = initial_model;
                  view = initial_view;
                  handlers = empty_handlers;
                  desired_size = (rows, cols);
                  stop_requested = false;
                  stop_reason = `Normal;
                  script_finished = false;
                  active_commands = 0;
                  command_cancels = ref [];
                  next_command_id = 0;
                  initial_events_left = 2;
                  dirty = false;
                  paused = false;
                  anchor = Fresh_line;
                  last_frame = Charamel_ansi.Text.strip initial_view.View.content;
                  timers = [];
                  streams = [];
                  renderer_stop = false;
                  backlog = Queue.create ();
                }
              in
              state_ref := Some state;
              let renderer_promise, renderer_resolver = Lwt.wait () in
              let screen_size = ref (rows, cols) in
              let last_render_time = ref None in
              let last_rendered_alt = ref false in
              let reader_cancel : unit Lwt.t option ref = ref None in
              let reader_generation = ref 0 in
              let escape_generation = ref 0 in
              let stop_timers () =
                List.iter
                  (fun timer ->
                    match !(timer.cancel) with
                    | None -> ()
                    | Some cancel -> Lwt.cancel cancel)
                  state.timers;
                state.timers <- []
              in
              let stop_reader () =
                incr reader_generation;
                incr escape_generation;
                match !reader_cancel with
                | None -> ()
                | Some cancel ->
                    reader_cancel := None;
                    Lwt.cancel cancel
              in
              let render_locked () =
                let* view, desired_size, anchor =
                  peek_state state (fun () ->
                      (state.view, state.desired_size, state.anchor))
                in
                let desired_rows, desired_cols = desired_size in
                if desired_rows <= 0 || desired_cols <= 0 then
                  invalid_arg "terminal size must be positive";
                let* () =
                  if desired_size <> !screen_size then begin
                    Screen.resize screen ~rows:desired_rows ~cols:desired_cols;
                    screen_size := desired_size;
                    if (not view.View.alt_screen) && anchor = No_anchor then
                      update_anchor state Column_start
                    else Lwt.return_unit
                  end
                  else Lwt.return_unit
                in
                let* () =
                  if
                    (not !last_rendered_alt) && (not view.View.alt_screen)
                    && anchor = No_anchor
                  then update_anchor state Column_start
                  else Lwt.return_unit
                in
                let* current_anchor = peek_state state (fun () -> state.anchor) in
                let* () =
                  if not view.View.alt_screen then begin
                    let* () =
                      match current_anchor with
                      | Fresh_line -> write_output_locked "\r\n"
                      | Column_start -> write_output_locked "\r"
                      | No_anchor -> Lwt.return_unit
                    in
                    if current_anchor <> No_anchor then
                      mutate_state state (fun () -> state.anchor <- No_anchor)
                    else Lwt.return_unit
                  end
                  else Lwt.return_unit
                in
                let bytes = Screen.render screen view in
                let* () = write_output_locked bytes in
                last_rendered_alt := view.View.alt_screen;
                mutate_state state (fun () ->
                    state.last_frame <- Charamel_ansi.Text.strip view.View.content)
              in
              let render_with_rate ~immediate =
                let* () =
                  match (immediate, !last_render_time) with
                  | false, Some before ->
                      let remaining =
                        (1. /. float fps) -. (Charamel_os.Time.now clock -. before)
                      in
                      if remaining > 0. then Charamel_os.Time.sleep clock remaining
                      else Lwt.return_unit
                  | true, Some _ | _, None -> Lwt.return_unit
                in
                let* () = render_locked () in
                last_render_time := Some (Charamel_os.Time.now clock);
                Lwt.return_unit
              in
              let enqueue_output action =
                mutate_state state (fun () -> Queue.add action state.effects)
              in
              let print_above_view text =
                let* view = peek_state state (fun () -> state.view) in
                if view.View.alt_screen then Lwt.return_unit
                else
                  let* anchor = peek_state state (fun () -> state.anchor) in
                  let* () =
                    match anchor with
                    | Fresh_line -> write_output_locked "\r\n"
                    | Column_start -> write_output_locked "\r"
                    | No_anchor -> Lwt.return_unit
                  in
                  let* () =
                    if anchor <> No_anchor then
                      mutate_state state (fun () -> state.anchor <- No_anchor)
                    else Lwt.return_unit
                  in
                  let* () = write_output_locked (Screen.clear screen) in
                  let* () = write_output_locked text in
                  render_locked ()
              in
              let process_output_effect = function
                | Output_bytes (bytes, resolver) ->
                    let* () =
                      if paint then write_output_locked bytes else Lwt.return_unit
                    in
                    Lwt.wakeup_later resolver () |> Lwt.return
                | Output_print (text, resolver) ->
                    let* () =
                      if paint then print_above_view text else write_output_locked text
                    in
                    Lwt.wakeup_later resolver () |> Lwt.return
              in
              let rec renderer_loop () =
                let* effects, should_render, finishing = take_render_batch ~paint state in
                if finishing && effects = [] && not should_render then Lwt.return_unit
                else
                  let* () =
                    with_output (fun () ->
                        let* () = Lwt_list.iter_s process_output_effect effects in
                        if should_render then render_with_rate ~immediate:finishing
                        else Lwt.return_unit)
                  in
                  renderer_loop ()
              in
              fork_daemon (fun _stop ->
                  fun () ->
                   Lwt.finalize renderer_loop (fun () ->
                       if Lwt.is_sleeping renderer_promise then
                         Lwt.wakeup_later renderer_resolver ();
                       Lwt.return_unit));
              let input_awaits_flush decoder =
                Input.pending_escape decoder || Input.pending_cluster decoder
              in
              let feed_bytes decoder bytes =
                let events = Input.feed decoder bytes in
                incr escape_generation;
                let queue_event event = queue_external state (Queued_event event) in
                let* () = Lwt_list.iter_s queue_event events in
                if not (input_awaits_flush decoder) then Lwt.return_unit
                else
                  let generation = !escape_generation in
                  let flush_pending () =
                    let* () = Charamel_os.Time.sleep clock 0.05 in
                    if generation = !escape_generation && input_awaits_flush decoder then
                      Lwt_list.iter_s queue_event (Input.flush decoder)
                    else Lwt.return_unit
                  in
                  fork_daemon (fun _stop -> flush_pending);
                  Lwt.return_unit
              in
              let rec read_input ~generation ~decoder ~finish_input =
                if generation <> !reader_generation || state.stop_requested then
                  Lwt.return_unit
                else
                  let* chunk = Charamel_os.Console_input.read terminal.Terminal.input in
                  if chunk = "" then finish_input ()
                  else
                    let* () = feed_bytes decoder chunk in
                    read_input ~generation ~decoder ~finish_input
              in
              let start_reader () =
                incr reader_generation;
                let generation = !reader_generation in
                let stop = new_daemon_stop () in
                if generation = !reader_generation then reader_cancel := Some stop;
                spawn_daemon_with_stop stop (fun _stop () ->
                    let decoder = Input.create () in
                    let finish_input () =
                      let queue_event event = queue_external state (Queued_event event) in
                      let* () = Lwt_list.iter_s queue_event (Input.flush decoder) in
                      queue_external state (Queued_stop `Normal)
                    in
                    Lwt.finalize
                      (fun () -> read_input ~generation ~decoder ~finish_input)
                      (fun () ->
                        if generation = !reader_generation then reader_cancel := None;
                        Lwt.return_unit))
              in
              let cancel_timer timer =
                match !(timer.cancel) with None -> () | Some cancel -> Lwt.cancel cancel
              in
              let start_timer timer =
                let stop = new_daemon_stop () in
                timer.cancel := Some stop;
                spawn_daemon_with_stop stop (fun _stop () ->
                    let rec loop () =
                      if state.stop_requested || state.script_finished then
                        Lwt.return_unit
                      else
                        let* () = Charamel_os.Time.sleep clock timer.interval in
                        if state.stop_requested || state.script_finished then
                          Lwt.return_unit
                        else begin
                          let callbacks = timer.callbacks in
                          let tick callback =
                            let message = callback (now ()) in
                            queue_external state (Queued_message (message, None))
                          in
                          let* () = Lwt_list.iter_s tick callbacks in
                          loop ()
                        end
                    in
                    loop ())
              in
              let start_stream_reader source =
                let stop = new_daemon_stop () in
                let reader = { source; cancel = ref (Some stop) } in
                spawn_daemon_with_stop stop (fun _stop () ->
                    let rec loop () =
                      if state.stop_requested || state.script_finished then
                        Lwt.return_unit
                      else
                        let* next = source.pull () in
                        match next with
                        | None -> Lwt.return_unit
                        | Some message ->
                            let* () =
                              queue_external state (Queued_message (message, None))
                            in
                            loop ()
                    in
                    loop ());
                reader
              in
              let cancel_stream_reader (reader : 'msg stream_reader) =
                match !(reader.cancel) with
                | None -> ()
                | Some cancel -> Lwt.cancel cancel
              in
              let sync_subscriptions () =
                let handlers = collect_sub (app.App.subscriptions state.model) in
                List.iter
                  (fun (interval, _) ->
                    ignore (valid_delay "subscription interval" interval))
                  handlers.every;
                state.handlers <- handlers;
                let groups = group_every handlers.every in
                let old_timers = state.timers in
                let new_timers =
                  List.map
                    (fun (interval, callbacks) ->
                      match
                        List.find_opt (fun timer -> timer.interval = interval) old_timers
                      with
                      | Some timer ->
                          timer.callbacks <- callbacks;
                          timer
                      | None ->
                          let timer = { interval; callbacks; cancel = ref None } in
                          if (not state.stop_requested) && not state.script_finished then
                            start_timer timer;
                          timer)
                    groups
                in
                List.iter
                  (fun timer ->
                    if not (List.exists (fun candidate -> candidate == timer) new_timers)
                    then cancel_timer timer)
                  old_timers;
                state.timers <- new_timers;
                let sources = dedupe_streams handlers.streams in
                let kept =
                  List.filter
                    (fun reader -> List.exists (same_stream_source reader.source) sources)
                    state.streams
                in
                List.iter
                  (fun reader ->
                    if not (List.exists (fun other -> other == reader) kept) then
                      cancel_stream_reader reader)
                  state.streams;
                let already_reading source reader =
                  same_stream_source source reader.source
                in
                let started =
                  List.filter_map
                    (fun source ->
                      if List.exists (already_reading source) kept then None
                      else Some (start_stream_reader source))
                    sources
                in
                state.streams <- kept @ started;
                Lwt.return_unit
              in
              let mark_initial_event () =
                if state.initial_events_left > 0 then begin
                  state.initial_events_left <- state.initial_events_left - 1;
                  if state.initial_events_left = 0 then set_dirty state
                  else Lwt.return_unit
                end
                else Lwt.return_unit
              in
              let set_view model =
                let view = app.App.view model in
                mutate_state state (fun () ->
                    state.view <- view;
                    state.last_frame <- Charamel_ansi.Text.strip view.View.content;
                    if state.initial_events_left = 0 then state.dirty <- true)
              in
              let start_command (command : 'msg Cmd.t) : unit Lwt.t =
                let* command_id =
                  mutate_state_result state (fun () ->
                      let id = state.next_command_id in
                      state.next_command_id <- state.next_command_id + 1;
                      state.active_commands <- state.active_commands + 1;
                      id)
                in
                let remove_command () =
                  mutate_state state (fun () ->
                      state.command_cancels :=
                        List.filter
                          (fun (id, _) -> id <> command_id)
                          !(state.command_cancels))
                in
                let rec run_cmd : type a.
                    a Cmd.t ->
                    emit:(a -> unit Lwt.t) ->
                    emit_effect:(a action -> unit Lwt.t) ->
                    unit Lwt.t =
                 fun command ~emit ~emit_effect ->
                  match command with
                  | Cmd.None_ -> Lwt.return_unit
                  | Cmd.Batch commands ->
                      Lwt_list.iter_p
                        (fun child -> run_cmd child ~emit ~emit_effect)
                        commands
                  | Cmd.Seq commands ->
                      Lwt_list.iter_s
                        (fun child -> run_cmd child ~emit ~emit_effect)
                        commands
                  | Cmd.Map (mapping, child) ->
                      run_cmd child
                        ~emit:(fun value -> emit (mapping value))
                        ~emit_effect:(fun action ->
                          emit_effect (map_effect mapping action))
                  | Cmd.Msg message -> emit message
                  | Cmd.Perform thunk -> emit (thunk ())
                  | Cmd.Await promise ->
                      let* message = promise in
                      emit message
                  | Cmd.After (delay, thunk) ->
                      let* () =
                        Charamel_os.Time.sleep clock (valid_delay "command delay" delay)
                      in
                      emit (thunk ())
                  | Cmd.Quit -> emit_effect Effect_quit
                  | Cmd.Interrupt -> emit_effect Effect_interrupt
                  | Cmd.Suspend -> emit_effect Effect_suspend
                  | Cmd.Exec { argv; on_exit } ->
                      if argv = [] then invalid_arg "exec requires a command";
                      emit_effect (Effect_exec (argv, on_exit))
                  | Cmd.Print text -> emit_effect (Effect_print (normalize_print text))
                  | Cmd.Set_clipboard { selection; content } ->
                      emit_effect (Effect_clipboard { selection; content })
                  | Cmd.Read_clipboard selection ->
                      emit_effect (Effect_read_clipboard selection)
                  | Cmd.Raw bytes -> emit_effect (Effect_raw bytes)
                  | Cmd.Query query -> emit_effect (Effect_query query)
                  | Cmd.Window_size -> emit_effect Effect_window_size
                in
                ignore
                  (spawn_daemon (fun stop () ->
                       Lwt.finalize
                         (fun () ->
                           let* () =
                             mutate_state state (fun () ->
                                 state.command_cancels :=
                                   (command_id, stop) :: !(state.command_cancels))
                           in
                           let* stopping =
                             peek_state state (fun () -> state.stop_requested)
                           in
                           if stopping then Lwt.return_unit
                           else
                             Lwt.catch
                               (fun () ->
                                 let emit message =
                                   let promise, resolver = Lwt.wait () in
                                   let* () =
                                     queue_external state
                                       (Queued_message (message, Some resolver))
                                   in
                                   promise
                                 in
                                 let emit_effect action = queue_effect state action in
                                 run_cmd command ~emit ~emit_effect)
                               (function
                                 | Lwt.Canceled -> Lwt.return_unit
                                 | exn ->
                                     let backtrace = Printexc.get_raw_backtrace () in
                                     fail_run exn backtrace;
                                     Lwt.return_unit))
                         (fun () ->
                           let* () = remove_command () in
                           mutate_state state (fun () ->
                               state.active_commands <- state.active_commands - 1))));
                Lwt.return_unit
              in
              let apply_message message =
                match filter state.model message with
                | None -> Lwt.return_unit
                | Some message ->
                    let model, command = app.App.update message state.model in
                    state.model <- model;
                    let* () = set_view model in
                    let* () = sync_subscriptions () in
                    start_command command
              in
              let dispatch_event event =
                let handlers = state.handlers in
                let* messages =
                  match event with
                  | Event.Key key -> (
                      match key.Key.event with
                      | Key.Release ->
                          Lwt.return
                            (List.map (fun handler -> handler key) handlers.key_release)
                      | Key.Press | Key.Repeat ->
                          Lwt.return (List.map (fun handler -> handler key) handlers.key))
                  | Event.Mouse mouse ->
                      let enabled =
                        match state.view.View.mouse with
                        | View.Mouse_off -> false
                        | _ -> true
                      in
                      Lwt.return
                        (if enabled then
                           List.map (fun handler -> handler mouse) handlers.mouse
                         else [])
                  | Event.Paste text ->
                      let enabled = state.view.View.bracketed_paste in
                      Lwt.return
                        (if enabled then
                           List.map (fun handler -> handler text) handlers.paste
                         else [])
                  | Event.Focus ->
                      Lwt.return
                        (if state.view.View.report_focus then
                           List.map (fun handler -> handler `Focused) handlers.focus
                         else [])
                  | Event.Blur ->
                      Lwt.return
                        (if state.view.View.report_focus then
                           List.map (fun handler -> handler `Blurred) handlers.focus
                         else [])
                  | Event.Resize { rows; cols } ->
                      if rows <= 0 || cols <= 0 then
                        invalid_arg "terminal size must be positive";
                      let* () =
                        mutate_state state (fun () -> state.desired_size <- (rows, cols))
                      in
                      let* () = set_dirty state in
                      Lwt.return
                        (List.map (fun handler -> handler ~rows ~cols) handlers.resize)
                  | Event.Profile _ | Event.Cursor_position _ | Event.Background_color _
                  | Event.Foreground_color _ | Event.Cursor_color _
                  | Event.Terminal_version _ | Event.Kitty_flags _ | Event.Mode_report _
                  | Event.Resume | Event.Clipboard _ | Event.Capability _
                  | Event.Unknown _ ->
                      Lwt.return
                        (List.map (fun handler -> handler event) handlers.terminal)
                in
                Lwt_list.iter_s apply_message messages
              in
              let query_bytes = function
                | `Background -> Charamel_ansi.Seq.bg_query
                | `Foreground -> Charamel_ansi.Seq.fg_query
                | `Cursor_color -> Charamel_ansi.Seq.cursor_color_query
                | `Terminal_version -> Charamel_ansi.Seq.xtversion
                | `Kitty_flags -> "\x1b[?u"
                | `Cursor_position -> "\x1b[6n"
                | `Capability name -> capability_query_bytes name
              in
              let request_stop reason =
                if state.stop_requested then ()
                else begin
                  state.stop_requested <- true;
                  state.stop_reason <- reason;
                  stop_reader ();
                  stop_timers ();
                  ignore
                    (mutate_state state (fun () ->
                         List.iter Lwt.cancel (List.map snd !(state.command_cancels))))
                end
              in
              let deliver_resume () =
                if Charamel_os.Tty.supports_suspend && terminal.Terminal.local then begin
                  let* () = queue_external state (Queued_event Event.Resume) in
                  Lwt_list.iter_s
                    (fun make -> queue_external state (Queued_message (make (), None)))
                    state.handlers.resume
                end
                else Lwt.return_unit
              in
              let resume_after_foreign () =
                with_output (fun () ->
                    if paint then terminal.Terminal.enter ();
                    Screen.reset screen;
                    let* () = update_anchor state Fresh_line in
                    let* () = set_paused state false in
                    if script = None && not state.stop_requested then start_reader ();
                    let* () = deliver_resume () in
                    set_dirty state)
              in
              let with_foreign fn =
                let* () = set_paused state true in
                stop_reader ();
                Lwt.finalize
                  (fun () ->
                    with_output (fun () ->
                        let* () =
                          if paint then write_output_locked (Screen.restore screen)
                          else Lwt.return_unit
                        in
                        terminal.Terminal.leave ();
                        fn ()))
                  resume_after_foreign
              in
              let handle_effect action resolver =
                let resolve () = Lwt.wakeup_later resolver () in
                match action with
                | Effect_quit ->
                    request_stop `Normal;
                    Lwt.return (resolve ())
                | Effect_interrupt ->
                    request_stop `Interrupted;
                    Lwt.return (resolve ())
                | Effect_suspend ->
                    if state.stop_requested || not can_suspend then
                      Lwt.return (resolve ())
                    else
                      let* () =
                        with_foreign (fun () ->
                            suspend ();
                            Lwt.return_unit)
                      in
                      Lwt.return (resolve ())
                | Effect_resume ->
                    let* () =
                      if state.paused then resume_after_foreign () else Lwt.return_unit
                    in
                    Lwt.return (resolve ())
                | Effect_exec (argv, callback) ->
                    let* code = with_foreign (fun () -> exec argv) in
                    let* () = apply_message (callback code) in
                    Lwt.return (resolve ())
                | Effect_print text ->
                    let* alt_screen =
                      peek_state state (fun () -> state.view.View.alt_screen)
                    in
                    if alt_screen then begin
                      Queue.add text state.backlog;
                      Lwt.return (resolve ())
                    end
                    else enqueue_output (Output_print (text, resolver))
                | Effect_clipboard { selection; content } ->
                    enqueue_output
                      (Output_bytes (clipboard_set_bytes selection content, resolver))
                | Effect_read_clipboard selection ->
                    enqueue_output
                      (Output_bytes (clipboard_read_bytes selection, resolver))
                | Effect_raw bytes -> enqueue_output (Output_bytes (bytes, resolver))
                | Effect_query query ->
                    enqueue_output (Output_bytes (query_bytes query, resolver))
                | Effect_window_size ->
                    let rows, cols = terminal.Terminal.size () in
                    let* () = dispatch_event (Event.Resize { rows; cols }) in
                    Lwt.return (resolve ())
              in
              (* Delivery runs the action itself. A flag polled by a waiting fiber loses
                 the wakeup between the failed check and the park — the race an
                 atomic condition loop used to close. On POSIX the action runs from
                 [Lwt_unix.on_signal], which hands the signal to the event loop; on
                 Windows the CRT handler installed here runs it directly, and resizes
                 keep arriving through the console record queue. *)
              let install_signal signal action =
                if Charamel_os.Signal.supported (Sys.signal_to_int signal) then begin
                  let number = Sys.signal_to_int signal in
                  if Sys.win32 then begin
                    (* The CRT handler is the delivery path there: Lwt's signal
                       dispatcher has no Windows console route, and the job-control and
                       window signals are already filtered out above. *)
                    let previous =
                      Sys.signal signal (Sys.Signal_handle (fun _ -> Lwt.async action))
                    in
                    old_handlers := (signal, previous) :: !old_handlers
                  end
                  else begin
                    (* Register with the event loop first, then capture the pre-program
                       disposition with a placeholder that forwards to
                       [Lwt_unix.handle_signal]. That call is thread-safe and notifies
                       every Lwt subscriber for the signal, so capturing never silences
                       another watcher (a resize subscription, say), the action is never
                       run inside the raw signal handler, and a signal arriving in the
                       installation window is delivered rather than dropped. *)
                    signal_watchers :=
                      Lwt_unix.on_signal number (fun _ -> Lwt.async action)
                      :: !signal_watchers;
                    old_handlers :=
                      ( signal,
                        Sys.signal signal
                          (Sys.Signal_handle (fun _ -> Lwt_unix.handle_signal number)) )
                      :: !old_handlers
                  end
                end
              in
              let setup_signals () =
                if not signals then Lwt.return_unit
                else begin
                  let resize_action () =
                    let rows, cols = terminal.Terminal.size () in
                    queue_external state (Queued_event (Event.Resize { rows; cols }))
                  in
                  let stop_action reason () = queue_external state (Queued_stop reason) in
                  install_signal Sys.sigwinch resize_action;
                  install_signal Sys.sigint (stop_action `Interrupted);
                  install_signal Sys.sigterm (stop_action `Normal);
                  install_signal Sys.sigtstp (fun () -> queue_effect state Effect_suspend);
                  install_signal Sys.sigcont (fun () -> queue_effect state Effect_resume);
                  Lwt.return_unit
                end
              in
              let setup_resize_stream () =
                match terminal.Terminal.on_resize with
                | None -> Lwt.return_unit
                | Some stream ->
                    fork_daemon (fun _stop ->
                        fun () ->
                         let rec loop () =
                           let* item = Lwt_stream.get stream in
                           match item with
                           | None -> Lwt.return_unit
                           | Some notify ->
                               notify ();
                               if state.stop_requested then Lwt.return_unit
                               else begin
                                 let rows, cols = terminal.Terminal.size () in
                                 let* () =
                                   queue_external state
                                     (Queued_event (Event.Resize { rows; cols }))
                                 in
                                 loop ()
                               end
                         in
                         loop ());
                    Lwt.return_unit
              in
              let setup_script events =
                fork_daemon (fun _stop ->
                    fun () ->
                     let decoder = Input.create () in
                     let deliver event =
                       match event with
                       | `Key key -> queue_external state (Queued_event (Event.Key key))
                       | `Text text -> feed_bytes decoder text
                       | `Resize (rows, cols) ->
                           queue_external state
                             (Queued_event (Event.Resize { rows; cols }))
                       | `Msg message ->
                           queue_external state (Queued_message (message, None))
                       | `Wait seconds ->
                           Charamel_os.Time.sleep clock
                             (valid_delay "script delay" seconds)
                     in
                     let* () = Lwt_list.iter_s deliver events in
                     let* () =
                       Lwt_list.iter_s
                         (fun event -> queue_external state (Queued_event event))
                         (Input.flush decoder)
                     in
                     queue_external state Queued_script_done);
                Lwt.return_unit
              in
              let* () = sync_subscriptions () in
              let* () = queue_external state (Queued_initial (Event.Profile profile)) in
              let* () =
                queue_external state (Queued_initial (Event.Resize { rows; cols }))
              in
              let* () = setup_signals () in
              let* () = setup_resize_stream () in
              let* () =
                match script with
                | None ->
                    start_reader ();
                    Lwt.return_unit
                | Some events -> setup_script events
              in
              let* () = start_command initial_cmd in
              let finish_if_ready () =
                let pending = state.queued_count > 0 in
                if
                  state.script_finished && (not state.stop_requested) && (not pending)
                  && state.active_commands = 0
                then request_stop `Normal;
                if state.stop_requested && (not pending) && state.active_commands = 0 then begin
                  let* () = mutate_state state (fun () -> state.renderer_stop <- true) in
                  let* () = renderer_promise in
                  let* model, frame =
                    peek_state state (fun () -> (state.model, state.last_frame))
                  in
                  let result =
                    match state.stop_reason with
                    | `Normal -> Ok (model, frame)
                    | `Interrupted -> Error `Interrupted
                  in
                  Lwt.return (Some result)
                end
                else Lwt.return_none
              in
              let rec loop () =
                let* finished = finish_if_ready () in
                match finished with
                | Some result -> Lwt.return result
                | None -> (
                    let* next = wait_for_item state in
                    match next with
                    | None -> loop ()
                    | Some item ->
                        let* () =
                          match item with
                          | Queued_event event -> dispatch_event event
                          | Queued_initial event ->
                              let* () = dispatch_event event in
                              mark_initial_event ()
                          | Queued_message (message, resolver) ->
                              let* () = apply_message message in
                              Lwt.return
                                (match resolver with
                                | None -> ()
                                | Some resolver -> Lwt.wakeup_later resolver ())
                          | Queued_effect (action, resolver) ->
                              handle_effect action resolver
                          | Queued_stop reason ->
                              request_stop reason;
                              Lwt.return_unit
                          | Queued_script_done ->
                              state.script_finished <- true;
                              stop_timers ();
                              Lwt.return_unit
                        in
                        loop ())
              in
              Lwt.pick [ loop (); failure ]))
        cleanup)
    (function
      | Lwt.Canceled as exn -> Lwt.fail exn
      | Daemon_failed (exn, backtrace) -> Lwt.return (Error (`Exn (exn, backtrace)))
      | exn -> Lwt.return (Error (`Exn (exn, Printexc.get_raw_backtrace ()))))

let spawn_foreground argv =
  match argv with
  | [] -> invalid_arg "exec requires a command"
  | program :: _ ->
      let arguments = Array.of_list argv in
      let executable = Option.value (Charamel_os.Exe.find program) ~default:program in
      Unix.create_process executable arguments Unix.stdin Unix.stdout Unix.stderr

let run ?terminal ?fps ?filter ?renderer ?color_profile ~clock app =
  let paint =
    match Option.value renderer ~default:`Terminal with
    | `None -> false
    | `Terminal -> true
  in
  let terminal = Option.value terminal ~default:(Terminal.local ()) in
  let fps = Option.value fps ~default:60 in
  let filter = Option.value filter ~default:(fun _ message -> Some message) in
  let default_exec argv =
    let pid = spawn_foreground argv in
    let* _, status = Lwt_unix.waitpid [] pid in
    Lwt.return
      (match status with
      | Unix.WEXITED code -> code
      | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal)
  in
  let exec = Option.value ~default:default_exec terminal.Terminal.exec in
  let suspend () = Unix.kill (Unix.getpid ()) Sys.sigstop in
  let* result =
    run_core ~terminal ~fps ~filter ~clock ~paint ?profile:color_profile
      ~now:(fun () -> Mtime_clock.now ())
      ~exec ~suspend ~signals:true app
  in
  Lwt.return (match result with Ok (model, _) -> Ok model | Error error -> Error error)
