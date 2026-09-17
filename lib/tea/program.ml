open Result.Syntax

type error = [ `Interrupted | `Killed | `Exn of exn * Printexc.raw_backtrace ]

type 'msg script_event =
  [ `Key of Key.t
  | `Text of string
  | `Resize of int * int
  | `Msg of 'msg
  | `Wait of float ]

type stop_reason = [ `Normal | `Interrupted | `Killed ]

exception Program_stopped

type 'msg action =
  | Effect_quit
  | Effect_interrupt
  | Effect_suspend
  | Effect_resume
  | Effect_exec of string list * (int -> 'msg)
  | Effect_print of string
  | Effect_clipboard of string
  | Effect_query of
      [ `Background
      | `Foreground
      | `Cursor_color
      | `Terminal_version
      | `Kitty_flags
      | `Cursor_position ]
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
  | Effect_clipboard text -> Effect_clipboard text
  | Effect_query query -> Effect_query query
  | Effect_window_size -> Effect_window_size

type 'msg queued =
  | Queued_event of Event.t
  | Queued_initial of Event.t
  | Queued_message of 'msg * unit Eio.Promise.u option
  | Queued_effect of 'msg action * unit Eio.Promise.u
  | Queued_stop of stop_reason
  | Queued_script_done

type 'msg handlers = {
  key : (Key.t -> 'msg) list;
  key_release : (Key.t -> 'msg) list;
  mouse : (Mouse.t -> 'msg) list;
  paste : (string -> 'msg) list;
  focus : ([ `Focused | `Blurred ] -> 'msg) list;
  resize : (rows:int -> cols:int -> 'msg) list;
  terminal : (Event.t -> 'msg) list;
  every : (float * (Mtime.t -> 'msg)) list;
}

type 'msg timer = {
  interval : float;
  mutable callbacks : (Mtime.t -> 'msg) list;
  cancel : Eio.Cancel.t option ref;
}

type anchor = Fresh_line | Column_start | No_anchor

type output_effect =
  | Output_bytes of string * unit Eio.Promise.u
  | Output_print of string * unit Eio.Promise.u

type ('model, 'msg) runtime_state = {
  queue : 'msg queued Eio.Stream.t;
  effects : output_effect Queue.t;
  mutex : Eio.Mutex.t;
  condition : Eio.Condition.t;
  mutable model : 'model;
  mutable view : View.t;
  mutable handlers : 'msg handlers;
  mutable desired_size : int * int;
  mutable stop_requested : bool;
  mutable stop_reason : stop_reason;
  mutable script_finished : bool;
  mutable active_commands : int;
  command_cancels : (int * Eio.Cancel.t) list ref;
  mutable next_command_id : int;
  mutable initial_events_left : int;
  mutable dirty : bool;
  mutable paused : bool;
  mutable anchor : anchor;
  mutable last_frame : string;
  mutable timers : 'msg timer list;
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
  }

let append_handlers a b =
  {
    key = a.key @ b.key;
    key_release = a.key_release @ b.key_release;
    mouse = a.mouse @ b.mouse;
    paste = a.paste @ b.paste;
    focus = a.focus @ b.focus;
    resize = a.resize @ b.resize;
    terminal = a.terminal @ b.terminal;
    every = a.every @ b.every;
  }

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

let queue_external state item =
  Eio.Stream.add state.queue item;
  Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
      Eio.Condition.broadcast state.condition)

let wait_for_item state =
  let has_item =
    Eio.Mutex.use_ro state.mutex (fun () ->
        while
          Eio.Stream.is_empty state.queue
          && (not (state.script_finished && state.active_commands = 0))
          && not (state.stop_requested && state.active_commands = 0)
        do
          Eio.Condition.await state.condition state.mutex
        done;
        not (Eio.Stream.is_empty state.queue))
  in
  if has_item then Some (Eio.Stream.take state.queue) else None

let queue_effect state action =
  let promise, resolver = Eio.Promise.create () in
  queue_external state (Queued_effect (action, resolver));
  Eio.Promise.await promise

let normalize_print text =
  if String.length text = 0 then "\n"
  else if String.get text (String.length text - 1) = '\n' then text
  else text ^ "\n"

let set_dirty state =
  Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
      state.dirty <- true;
      Eio.Condition.broadcast state.condition)

let update_anchor state anchor =
  Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
      state.anchor <- anchor;
      Eio.Condition.broadcast state.condition)

let set_paused state paused =
  Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
      state.paused <- paused;
      Eio.Condition.broadcast state.condition)

let drain_queue queue =
  let rec loop acc =
    if Queue.is_empty queue then List.rev acc else loop (Queue.take queue :: acc)
  in
  loop []

let take_render_batch state =
  Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
      while
        (not state.renderer_stop)
        && (state.paused || ((not state.dirty) && Queue.is_empty state.effects))
      do
        Eio.Condition.await state.condition state.mutex
      done;
      let effects =
        if state.paused && not state.renderer_stop then [] else drain_queue state.effects
      in
      let should_render = state.dirty && ((not state.paused) || state.renderer_stop) in
      if should_render then state.dirty <- false;
      (effects, should_render, state.renderer_stop))

let run_core ~(terminal : Terminal.t) ~fps ~filter ~clock ~now ~exec ~suspend ~signals
    ?script (app : ('model, 'msg) App.t) =
  if fps <= 0 then invalid_arg "fps must be positive";
  let fps = min 120 fps in
  let rows, cols = terminal.Terminal.size () in
  if rows <= 0 || cols <= 0 then invalid_arg "terminal size must be positive";
  let screen = Screen.create ~rows ~cols in
  let output_mutex = Eio.Mutex.create () in
  let profile =
    Charamel_colorprofile.detect ~is_tty:terminal.Terminal.is_tty
      ~env:terminal.Terminal.env
  in
  let writer = Charamel_colorprofile.Writer.create ~profile terminal.Terminal.output in
  let entered = ref false in
  let old_handlers : (Sys.signal * Sys.signal_behavior) list ref = ref [] in
  let state_ref : ('model, 'msg) runtime_state option ref = ref None in
  let write_output_locked text =
    if text <> "" then Charamel_colorprofile.Writer.write writer text
  in
  let with_output f =
    Eio.Mutex.lock output_mutex;
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect (fun () -> Eio.Mutex.unlock output_mutex))
      f
  in
  let with_output_protected f = Eio.Cancel.protect (fun () -> with_output f) in
  let restore_handlers () =
    List.iter (fun (signal, behavior) -> Sys.set_signal signal behavior) !old_handlers;
    old_handlers := []
  in
  let cleanup () =
    Fun.protect
      ~finally:(fun () -> Eio.Cancel.protect restore_handlers)
      (fun () ->
        Eio.Cancel.protect (fun () ->
            Fun.protect
              ~finally:(fun () -> if !entered then terminal.Terminal.leave ())
              (fun () ->
                match !state_ref with
                | None -> ()
                | Some state ->
                    with_output (fun () ->
                        let bytes = Screen.restore screen in
                        write_output_locked bytes;
                        Queue.iter (fun text -> write_output_locked text) state.backlog;
                        Queue.clear state.backlog))))
  in
  try
    Fun.protect ~finally:cleanup (fun () ->
        entered := true;
        terminal.Terminal.enter ();
        Eio.Switch.run (fun sw ->
            let fork_daemon f =
              Eio.Fiber.fork_daemon ~sw (fun () ->
                  f ();
                  `Stop_daemon)
            in
            let initial_model, initial_cmd = app.App.init () in
            let initial_view = app.App.view initial_model in
            let state =
              {
                queue = Eio.Stream.create 256;
                effects = Queue.create ();
                mutex = Eio.Mutex.create ();
                condition = Eio.Condition.create ();
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
                renderer_stop = false;
                backlog = Queue.create ();
              }
            in
            state_ref := Some state;
            let renderer_promise, renderer_resolver = Eio.Promise.create () in
            let screen_size = ref (rows, cols) in
            let last_render_time = ref None in
            let last_rendered_alt = ref false in
            let reader_cancel : Eio.Cancel.t option ref = ref None in
            let reader_generation = ref 0 in
            let escape_generation = ref 0 in
            let stop_timers () =
              List.iter
                (fun timer ->
                  match !(timer.cancel) with
                  | None -> ()
                  | Some cancel ->
                      Eio.Cancel.cancel cancel (Failure "subscription stopped"))
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
                  Eio.Cancel.cancel cancel (Failure "terminal input paused")
            in
            let render_locked () =
              let view, desired_size, anchor =
                Eio.Mutex.use_ro state.mutex (fun () ->
                    (state.view, state.desired_size, state.anchor))
              in
              let desired_rows, desired_cols = desired_size in
              if desired_rows <= 0 || desired_cols <= 0 then
                invalid_arg "terminal size must be positive";
              if desired_size <> !screen_size then begin
                Screen.resize screen ~rows:desired_rows ~cols:desired_cols;
                screen_size := desired_size;
                if (not view.View.alt_screen) && anchor = No_anchor then
                  update_anchor state Column_start
              end;
              if
                (not !last_rendered_alt) && (not view.View.alt_screen)
                && anchor = No_anchor
              then update_anchor state Column_start;
              let current_anchor =
                Eio.Mutex.use_ro state.mutex (fun () -> state.anchor)
              in
              if not view.View.alt_screen then begin
                (match current_anchor with
                | Fresh_line -> write_output_locked "\r\n"
                | Column_start -> write_output_locked "\r"
                | No_anchor -> ());
                if current_anchor <> No_anchor then
                  Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                      state.anchor <- No_anchor)
              end;
              let bytes = Screen.render screen view in
              write_output_locked bytes;
              last_rendered_alt := view.View.alt_screen;
              Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                  state.last_frame <- Charamel_ansi.Text.strip view.View.content)
            in
            let render_with_rate ~immediate =
              (match (immediate, !last_render_time) with
              | false, Some before ->
                  let remaining = (1. /. float fps) -. (Eio.Time.now clock -. before) in
                  if remaining > 0. then Eio.Time.sleep clock remaining
              | _ -> ());
              render_locked ();
              last_render_time := Some (Eio.Time.now clock)
            in
            let enqueue_output action =
              Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                  Queue.add action state.effects;
                  Eio.Condition.broadcast state.condition)
            in
            let process_output_effect = function
              | Output_bytes (bytes, resolver) ->
                  write_output_locked bytes;
                  Eio.Promise.resolve resolver ()
              | Output_print (text, resolver) ->
                  let view = Eio.Mutex.use_ro state.mutex (fun () -> state.view) in
                  if not view.View.alt_screen then begin
                    let anchor = Eio.Mutex.use_ro state.mutex (fun () -> state.anchor) in
                    (match anchor with
                    | Fresh_line -> write_output_locked "\r\n"
                    | Column_start -> write_output_locked "\r"
                    | No_anchor -> ());
                    if anchor <> No_anchor then
                      Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                          state.anchor <- No_anchor);
                    let clear = Screen.clear screen in
                    write_output_locked clear;
                    write_output_locked text;
                    render_locked ()
                  end;
                  Eio.Promise.resolve resolver ()
            in
            let renderer_loop () =
              let rec loop () =
                let effects, should_render, finishing = take_render_batch state in
                if finishing && effects = [] && not should_render then ()
                else begin
                  with_output (fun () ->
                      List.iter process_output_effect effects;
                      if should_render then render_with_rate ~immediate:finishing);
                  loop ()
                end
              in
              Fun.protect
                ~finally:(fun () -> ignore (Eio.Promise.try_resolve renderer_resolver ()))
                loop
            in
            fork_daemon (fun () ->
                try renderer_loop () with
                | Eio.Cancel.Cancelled _ -> ()
                | ex ->
                    let bt = Printexc.get_raw_backtrace () in
                    Eio.Switch.fail ~bt sw ex);
            let feed_bytes decoder bytes =
              let events = Input.feed decoder bytes in
              incr escape_generation;
              List.iter (fun event -> queue_external state (Queued_event event)) events;
              if Input.pending_escape decoder then begin
                let generation = !escape_generation in
                fork_daemon (fun () ->
                    try
                      Eio.Time.sleep clock 0.05;
                      if generation = !escape_generation && Input.pending_escape decoder
                      then
                        List.iter
                          (fun event -> queue_external state (Queued_event event))
                          (Input.flush decoder)
                    with Eio.Cancel.Cancelled _ -> ())
              end
            in
            let rec read_input ~generation ~decoder ~buffer ~finish_input =
              if generation <> !reader_generation || state.stop_requested then ()
              else begin
                let count = Eio.Flow.single_read terminal.Terminal.input buffer in
                if count <= 0 then finish_input ()
                else begin
                  feed_bytes decoder (Cstruct.to_string (Cstruct.sub buffer 0 count));
                  read_input ~generation ~decoder ~buffer ~finish_input
                end
              end
            in
            let start_reader () =
              incr reader_generation;
              let generation = !reader_generation in
              fork_daemon (fun () ->
                  try
                    Eio.Cancel.sub (fun cancel ->
                        if generation = !reader_generation then
                          reader_cancel := Some cancel;
                        let decoder = Input.create () in
                        let buffer = Cstruct.create 4096 in
                        let finish_input () =
                          List.iter
                            (fun event -> queue_external state (Queued_event event))
                            (Input.flush decoder);
                          queue_external state (Queued_stop `Normal)
                        in
                        Fun.protect
                          ~finally:(fun () ->
                            if generation = !reader_generation then reader_cancel := None)
                          (fun () ->
                            try read_input ~generation ~decoder ~buffer ~finish_input
                            with End_of_file -> finish_input ()))
                  with
                  | Eio.Cancel.Cancelled _ -> ()
                  | ex ->
                      let bt = Printexc.get_raw_backtrace () in
                      Eio.Switch.fail ~bt sw ex)
            in
            let cancel_timer timer =
              match !(timer.cancel) with
              | None -> ()
              | Some cancel -> Eio.Cancel.cancel cancel (Failure "subscription removed")
            in
            let start_timer timer =
              fork_daemon (fun () ->
                  try
                    Eio.Cancel.sub (fun cancel ->
                        timer.cancel := Some cancel;
                        let rec loop () =
                          if state.stop_requested || state.script_finished then ()
                          else begin
                            Eio.Time.sleep clock timer.interval;
                            if (not state.stop_requested) && not state.script_finished
                            then begin
                              let callbacks = timer.callbacks in
                              List.iter
                                (fun callback ->
                                  queue_external state
                                    (Queued_message (callback (now ()), None)))
                                callbacks;
                              loop ()
                            end
                          end
                        in
                        loop ())
                  with Eio.Cancel.Cancelled _ -> ())
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
              state.timers <- new_timers
            in
            let mark_initial_event () =
              if state.initial_events_left > 0 then begin
                state.initial_events_left <- state.initial_events_left - 1;
                if state.initial_events_left = 0 then set_dirty state
              end
            in
            let set_view model =
              let view = app.App.view model in
              Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                  state.view <- view;
                  state.last_frame <- Charamel_ansi.Text.strip view.View.content;
                  if state.initial_events_left = 0 then state.dirty <- true;
                  Eio.Condition.broadcast state.condition)
            in
            let start_command : 'msg Cmd.t -> unit =
             fun command ->
              let command_id =
                Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                    let id = state.next_command_id in
                    state.next_command_id <- state.next_command_id + 1;
                    state.active_commands <- state.active_commands + 1;
                    Eio.Condition.broadcast state.condition;
                    id)
              in
              let remove_command () =
                Eio.Cancel.protect (fun () ->
                    Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                        state.command_cancels :=
                          List.filter
                            (fun (id, _) -> id <> command_id)
                            !(state.command_cancels)))
              in
              let rec run_cmd : type a.
                  a Cmd.t -> emit:(a -> unit) -> emit_effect:(a action -> unit) -> unit =
               fun command ~emit ~emit_effect ->
                match command with
                | Cmd.None_ -> ()
                | Cmd.Batch commands ->
                    Eio.Fiber.List.iter
                      (fun child -> run_cmd child ~emit ~emit_effect)
                      commands
                | Cmd.Seq commands ->
                    List.iter (fun child -> run_cmd child ~emit ~emit_effect) commands
                | Cmd.Map (mapping, child) ->
                    run_cmd child
                      ~emit:(fun value -> emit (mapping value))
                      ~emit_effect:(fun action -> emit_effect (map_effect mapping action))
                | Cmd.Msg message -> emit message
                | Cmd.Perform thunk -> emit (thunk ())
                | Cmd.After (delay, thunk) ->
                    Eio.Time.sleep clock (valid_delay "command delay" delay);
                    emit (thunk ())
                | Cmd.Quit -> emit_effect Effect_quit
                | Cmd.Interrupt -> emit_effect Effect_interrupt
                | Cmd.Suspend -> emit_effect Effect_suspend
                | Cmd.Exec { argv; on_exit } ->
                    if argv = [] then invalid_arg "exec requires a command";
                    emit_effect (Effect_exec (argv, on_exit))
                | Cmd.Print text -> emit_effect (Effect_print (normalize_print text))
                | Cmd.Set_clipboard text -> emit_effect (Effect_clipboard text)
                | Cmd.Query query -> emit_effect (Effect_query query)
                | Cmd.Window_size -> emit_effect Effect_window_size
              in
              fork_daemon (fun () ->
                  Fun.protect
                    ~finally:(fun () ->
                      remove_command ();
                      Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                          state.active_commands <- state.active_commands - 1;
                          Eio.Condition.broadcast state.condition))
                    (fun () ->
                      try
                        Eio.Cancel.sub (fun cancel ->
                            Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                                state.command_cancels :=
                                  (command_id, cancel) :: !(state.command_cancels));
                            Fun.protect ~finally:remove_command (fun () ->
                                let stopping =
                                  Eio.Mutex.use_ro state.mutex (fun () ->
                                      state.stop_requested)
                                in
                                if not stopping then begin
                                  let emit message =
                                    let promise, resolver = Eio.Promise.create () in
                                    queue_external state
                                      (Queued_message (message, Some resolver));
                                    Eio.Promise.await promise
                                  in
                                  let emit_effect action = queue_effect state action in
                                  run_cmd command ~emit ~emit_effect
                                end))
                      with
                      | Eio.Cancel.Cancelled Program_stopped -> ()
                      | Eio.Cancel.Cancelled _ as ex -> raise ex
                      | ex ->
                          let bt = Printexc.get_raw_backtrace () in
                          Eio.Switch.fail ~bt sw ex))
            in
            let apply_message message =
              match filter state.model message with
              | None -> ()
              | Some message ->
                  let model, command = app.App.update message state.model in
                  state.model <- model;
                  set_view model;
                  sync_subscriptions ();
                  start_command command
            in
            let dispatch_event event =
              let handlers = state.handlers in
              let messages =
                match event with
                | Event.Key key -> (
                    match key.Key.event with
                    | Key.Release ->
                        List.map (fun handler -> handler key) handlers.key_release
                    | Key.Press | Key.Repeat ->
                        List.map (fun handler -> handler key) handlers.key)
                | Event.Mouse mouse ->
                    let enabled =
                      Eio.Mutex.use_ro state.mutex (fun () ->
                          match state.view.View.mouse with
                          | View.Mouse_off -> false
                          | _ -> true)
                    in
                    if enabled then List.map (fun handler -> handler mouse) handlers.mouse
                    else []
                | Event.Paste text ->
                    let enabled =
                      Eio.Mutex.use_ro state.mutex (fun () ->
                          state.view.View.bracketed_paste)
                    in
                    if enabled then List.map (fun handler -> handler text) handlers.paste
                    else []
                | Event.Focus ->
                    let enabled =
                      Eio.Mutex.use_ro state.mutex (fun () ->
                          state.view.View.report_focus)
                    in
                    if enabled then
                      List.map (fun handler -> handler `Focused) handlers.focus
                    else []
                | Event.Blur ->
                    let enabled =
                      Eio.Mutex.use_ro state.mutex (fun () ->
                          state.view.View.report_focus)
                    in
                    if enabled then
                      List.map (fun handler -> handler `Blurred) handlers.focus
                    else []
                | Event.Resize { rows; cols } ->
                    if rows <= 0 || cols <= 0 then
                      invalid_arg "terminal size must be positive";
                    Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                        state.desired_size <- (rows, cols));
                    set_dirty state;
                    List.map (fun handler -> handler ~rows ~cols) handlers.resize
                | Event.Profile _ | Event.Cursor_position _ | Event.Background_color _
                | Event.Foreground_color _ | Event.Cursor_color _
                | Event.Terminal_version _ | Event.Kitty_flags _ | Event.Mode_report _
                | Event.Unknown _ ->
                    List.map (fun handler -> handler event) handlers.terminal
              in
              List.iter apply_message messages
            in
            let query_bytes = function
              | `Background -> Charamel_ansi.Seq.bg_query
              | `Foreground -> Charamel_ansi.Seq.fg_query
              | `Cursor_color -> Charamel_ansi.Seq.cursor_color_query
              | `Terminal_version -> Charamel_ansi.Seq.xtversion
              | `Kitty_flags -> "\x1b[?u"
              | `Cursor_position -> "\x1b[6n"
            in
            let request_stop reason =
              if not state.stop_requested then begin
                state.stop_requested <- true;
                state.stop_reason <- reason;
                stop_reader ();
                stop_timers ();
                let cancels =
                  Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                      let cancels = List.map snd !(state.command_cancels) in
                      Eio.Condition.broadcast state.condition;
                      cancels)
                in
                List.iter (fun cancel -> Eio.Cancel.cancel cancel Program_stopped) cancels
              end
            in
            let resume_after_foreign () =
              terminal.Terminal.enter ();
              Screen.reset screen;
              update_anchor state Fresh_line;
              set_paused state false;
              if script = None && not state.stop_requested then start_reader ();
              set_dirty state
            in
            let with_foreign fn =
              set_paused state true;
              stop_reader ();
              Fun.protect
                ~finally:(fun () ->
                  with_output_protected (fun () -> resume_after_foreign ()))
                (fun () ->
                  with_output (fun () ->
                      let bytes = Screen.restore screen in
                      write_output_locked bytes;
                      terminal.Terminal.leave ();
                      fn ()))
            in
            let handle_effect action resolver =
              match action with
              | Effect_quit ->
                  request_stop `Normal;
                  Eio.Promise.resolve resolver ()
              | Effect_interrupt ->
                  request_stop `Interrupted;
                  Eio.Promise.resolve resolver ()
              | Effect_suspend ->
                  if not state.stop_requested then begin
                    with_foreign (fun () -> suspend ());
                    Eio.Promise.resolve resolver ()
                  end
                  else Eio.Promise.resolve resolver ()
              | Effect_resume ->
                  if state.paused then
                    with_output_protected (fun () -> resume_after_foreign ());
                  Eio.Promise.resolve resolver ()
              | Effect_exec (argv, callback) ->
                  let code = with_foreign (fun () -> exec argv) in
                  apply_message (callback code);
                  Eio.Promise.resolve resolver ()
              | Effect_print text ->
                  let alt_screen =
                    Eio.Mutex.use_ro state.mutex (fun () -> state.view.View.alt_screen)
                  in
                  if alt_screen then begin
                    Queue.add text state.backlog;
                    Eio.Promise.resolve resolver ()
                  end
                  else begin
                    enqueue_output (Output_print (text, resolver))
                  end
              | Effect_clipboard text ->
                  enqueue_output
                    (Output_bytes (Charamel_ansi.Seq.clipboard_osc52 text, resolver))
              | Effect_query query ->
                  enqueue_output (Output_bytes (query_bytes query, resolver))
              | Effect_window_size ->
                  let rows, cols = terminal.Terminal.size () in
                  dispatch_event (Event.Resize { rows; cols });
                  Eio.Promise.resolve resolver ()
            in
            let setup_signals () =
              if signals then begin
                let winch = Eio.Condition.create () in
                let interrupt = Eio.Condition.create () in
                let terminate = Eio.Condition.create () in
                let suspend_signal = Eio.Condition.create () in
                let continue_signal = Eio.Condition.create () in
                let winch_pending = Atomic.make false in
                let interrupt_pending = Atomic.make false in
                let terminate_pending = Atomic.make false in
                let suspend_pending = Atomic.make false in
                let continue_pending = Atomic.make false in
                let install signal condition pending =
                  let previous =
                    Sys.signal signal
                      (Sys.Signal_handle
                         (fun _ ->
                           Atomic.set pending true;
                           Eio.Condition.broadcast condition))
                  in
                  old_handlers := (signal, previous) :: !old_handlers
                in
                install Sys.sigwinch winch winch_pending;
                install Sys.sigint interrupt interrupt_pending;
                install Sys.sigterm terminate terminate_pending;
                install Sys.sigtstp suspend_signal suspend_pending;
                install Sys.sigcont continue_signal continue_pending;
                let watch condition pending action =
                  fork_daemon (fun () ->
                      try
                        while not state.stop_requested do
                          let pending =
                            Eio.Condition.loop_no_mutex condition (fun () ->
                                if state.stop_requested then Some false
                                else if Atomic.exchange pending false then Some true
                                else None)
                          in
                          if pending then action ()
                        done
                      with Eio.Cancel.Cancelled _ -> ())
                in
                watch winch winch_pending (fun () ->
                    let rows, cols = terminal.Terminal.size () in
                    queue_external state (Queued_event (Event.Resize { rows; cols })));
                watch interrupt interrupt_pending (fun () ->
                    queue_external state (Queued_stop `Interrupted));
                watch terminate terminate_pending (fun () ->
                    queue_external state (Queued_stop `Normal));
                watch suspend_signal suspend_pending (fun () ->
                    queue_effect state Effect_suspend);
                watch continue_signal continue_pending (fun () ->
                    queue_effect state Effect_resume)
              end
            in
            let setup_resize_stream () =
              match terminal.Terminal.on_resize with
              | None -> ()
              | Some stream ->
                  fork_daemon (fun () ->
                      try
                        while not state.stop_requested do
                          let notify = Eio.Stream.take stream in
                          notify ();
                          if not state.stop_requested then begin
                            let rows, cols = terminal.Terminal.size () in
                            queue_external state
                              (Queued_event (Event.Resize { rows; cols }))
                          end
                        done
                      with Eio.Cancel.Cancelled _ -> ())
            in
            let setup_script () =
              match script with
              | None -> start_reader ()
              | Some events ->
                  fork_daemon (fun () ->
                      try
                        let decoder = Input.create () in
                        List.iter
                          (function
                            | `Key key ->
                                queue_external state (Queued_event (Event.Key key))
                            | `Text text -> feed_bytes decoder text
                            | `Resize (rows, cols) ->
                                queue_external state
                                  (Queued_event (Event.Resize { rows; cols }))
                            | `Msg message ->
                                queue_external state (Queued_message (message, None))
                            | `Wait seconds ->
                                Eio.Time.sleep clock (valid_delay "script delay" seconds))
                          events;
                        List.iter
                          (fun event -> queue_external state (Queued_event event))
                          (Input.flush decoder);
                        queue_external state Queued_script_done
                      with Eio.Cancel.Cancelled _ -> ())
            in
            sync_subscriptions ();
            queue_external state (Queued_initial (Event.Profile profile));
            queue_external state (Queued_initial (Event.Resize { rows; cols }));
            setup_signals ();
            setup_resize_stream ();
            setup_script ();
            start_command initial_cmd;
            let finish_if_ready () =
              let pending = not (Eio.Stream.is_empty state.queue) in
              if
                state.script_finished && (not state.stop_requested) && (not pending)
                && state.active_commands = 0
              then request_stop `Normal;
              if state.stop_requested && (not pending) && state.active_commands = 0 then begin
                state.renderer_stop <- true;
                Eio.Mutex.use_rw ~protect:false state.mutex (fun () ->
                    Eio.Condition.broadcast state.condition);
                Eio.Promise.await renderer_promise;
                let model, frame =
                  Eio.Mutex.use_ro state.mutex (fun () -> (state.model, state.last_frame))
                in
                match state.stop_reason with
                | `Normal -> Some (Ok (model, frame))
                | `Interrupted -> Some (Error `Interrupted)
                | `Killed -> Some (Error `Killed)
              end
              else None
            in
            let rec loop () =
              match finish_if_ready () with
              | Some result -> result
              | None -> (
                  match wait_for_item state with
                  | None -> loop ()
                  | Some item ->
                      (match item with
                      | Queued_event event -> dispatch_event event
                      | Queued_initial event ->
                          dispatch_event event;
                          mark_initial_event ()
                      | Queued_message (message, resolver) -> (
                          apply_message message;
                          match resolver with
                          | None -> ()
                          | Some resolver -> Eio.Promise.resolve resolver ())
                      | Queued_effect (action, resolver) -> handle_effect action resolver
                      | Queued_stop reason -> request_stop reason
                      | Queued_script_done ->
                          state.script_finished <- true;
                          stop_timers ());
                      loop ())
            in
            loop ()))
  with
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | ex ->
      let backtrace = Printexc.get_raw_backtrace () in
      Error (`Exn (ex, backtrace))

let run ?terminal ?fps ?filter ~clock app env =
  let terminal : Terminal.t = Option.value terminal ~default:(Terminal.local env) in
  let fps = Option.value fps ~default:60 in
  let filter = Option.value filter ~default:(fun _ message -> Some message) in
  let exec argv =
    match argv with
    | [] -> invalid_arg "exec requires a command"
    | _ -> (
        try
          Eio.Process.run (Eio.Stdenv.process_mgr env) ~stdin:terminal.Terminal.input
            ~stdout:terminal.Terminal.output ~stderr:terminal.Terminal.output argv;
          0
        with
        | Eio.Io (Eio.Process.E (Eio.Process.Child_error (`Exited code)), _) -> code
        | Eio.Io (Eio.Process.E (Eio.Process.Child_error (`Signaled signal)), _) ->
            128 + signal)
  in
  let suspend () =
    if terminal.Terminal.is_tty then Unix.kill (Unix.getpid ()) Sys.sigstop
    else invalid_arg "suspend requires a local terminal"
  in
  let* model, _ =
    run_core ~terminal ~fps ~filter ~clock
      ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
      ~exec ~suspend ~signals:true app
  in
  Ok model
