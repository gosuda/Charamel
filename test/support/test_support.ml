let contains ~needle ~haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search index =
    index + needle_length <= haystack_length
    && (String.sub haystack index needle_length = needle || search (index + 1))
  in
  needle_length = 0 || search 0

let spawn ~exe ~argv ~env ~cwd ~stdin_read ~stdout_write ~stderr_write =
  let previous_cwd = Sys.getcwd () in
  let restore_cwd () = if Option.is_some cwd then Unix.chdir previous_cwd in
  Option.iter Unix.chdir cwd;
  Fun.protect ~finally:restore_cwd (fun () ->
      match env with
      | Some env ->
          Unix.create_process_env exe argv env stdin_read stdout_write stderr_write
      | None -> Unix.create_process exe argv stdin_read stdout_write stderr_write)

let read_chunk ~target ~fd =
  let chunk = Bytes.create 4096 in
  let count = Unix.read fd chunk 0 (Bytes.length chunk) in
  if count = 0 then false
  else begin
    Buffer.add_subbytes target chunk 0 count;
    true
  end

let write_once ~pending ~fd =
  match !pending with
  | None -> true
  | Some content ->
      let count = Unix.write_substring fd content 0 (String.length content) in
      if count >= String.length content then begin
        pending := None;
        true
      end
      else begin
        pending := Some (String.sub content count (String.length content - count));
        false
      end

let write_available ~pending ~fd =
  match write_once ~pending ~fd with
  | done_writing -> done_writing
  | exception Unix.Unix_error ((EPIPE | EBADF), _, _) ->
      pending := None;
      true

let kill_child pid =
  match Unix.kill pid Sys.sigkill with
  | () -> ()
  | exception Unix.Unix_error (ESRCH, _, _) -> ()

let run_cli ~exe ?env ?cwd ?(timeout = 10.) ?(stdin = "") args =
  ignore (Sys.set_signal Sys.sigpipe Sys.Signal_ignore);
  let argv = Array.of_list (exe :: args) in
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write = Unix.pipe ~cloexec:true () in
  let stderr_read, stderr_write = Unix.pipe ~cloexec:true () in
  let pid = spawn ~exe ~argv ~env ~cwd ~stdin_read ~stdout_write ~stderr_write in
  Unix.close stdin_read;
  Unix.close stdout_write;
  Unix.close stderr_write;
  let stdout_buffer = Buffer.create 256 in
  let stderr_buffer = Buffer.create 128 in
  let open_reads = ref [ stdout_read; stderr_read ] in
  let pending_stdin = ref (if String.length stdin = 0 then None else Some stdin) in
  let stdin_open = ref true in
  let close_stdin () =
    if !stdin_open then begin
      stdin_open := false;
      Unix.close stdin_write
    end
  in
  let handle_read fd =
    let target = if fd = stdout_read then stdout_buffer else stderr_buffer in
    if read_chunk ~target ~fd then ()
    else begin
      Unix.close fd;
      open_reads := List.filter (fun open_fd -> open_fd <> fd) !open_reads
    end
  in
  if !pending_stdin = None then close_stdin ();
  let deadline = Unix.gettimeofday () +. timeout in
  let rec pump () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0. then `Timeout
    else if !open_reads = [] && !pending_stdin = None then `Streams_done
    else begin
      let writable = if Option.is_some !pending_stdin then [ stdin_write ] else [] in
      let ready_read, ready_write, _ =
        Unix.select !open_reads writable [] (min remaining 0.1)
      in
      List.iter handle_read ready_read;
      List.iter
        (fun _ ->
          if write_available ~pending:pending_stdin ~fd:stdin_write then close_stdin ())
        ready_write;
      pump ()
    end
  in
  let timed_out = match pump () with `Timeout -> true | `Streams_done -> false in
  close_stdin ();
  let rec reap () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if timed_out then begin
          kill_child pid;
          snd (Unix.waitpid [] pid)
        end
        else begin
          ignore (Unix.select [] [] [] 0.05);
          reap ()
        end
    | _, status -> status
  in
  let status =
    match reap () with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal | Unix.WSTOPPED signal -> 128 + signal
  in
  (status, Buffer.contents stdout_buffer, Buffer.contents stderr_buffer)

let with_temp_dir f =
  Eio_main.run (fun env ->
      let path = Filename.temp_file "charamel-test-" ".dir" in
      Sys.remove path;
      let dir = Eio.Path.(env#fs / path) in
      Eio.Path.mkdir ~perm:0o700 dir;
      Fun.protect
        ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir)
        (fun () -> f dir))

let corpus =
  [
    ("\u{D55C}", 2, 1);
    ("\u{1112}\u{1161}\u{11AB}", 2, 1);
    ("\u{304B}\u{306A}", 4, 2);
    ("\u{FF76}\u{FF77}", 2, 2);
    ("\u{FF76}\u{FF9E}", 1, 1);
    ("\u{4E16}", 2, 1);
    ("\u{FF21}", 2, 1);
    ("\u{3001}\u{3002}", 4, 2);
    ("\u{26A0}\u{FE0F}", 2, 1);
    ("\u{2639}\u{FE0E}", 1, 1);
    ("\u{1F468}\u{200D}\u{1F469}\u{200D}\u{1F467}\u{200D}\u{1F466}", 2, 1);
    ("\u{1F1FA}\u{1F1F8}", 2, 1);
    ("\u{4E16}\u{0301}", 2, 1);
    ("\u{00A7}", 1, 1);
    ("\u{0410}", 1, 1);
    ("\u{D55C}\u{AD6D}\u{C5B4}\u{D14D}\u{C2A4}\u{D2B8}\u{D3B8}\u{C9D1}\u{AE30}", 18, 9);
    ("\u{4F60}\u{597D}\u{FF0C}\u{4E16}\u{754C}\u{FF01}", 12, 6);
  ]

let cjk_gen =
  let open QCheck2.Gen in
  let piece =
    oneof_weighted
      [
        (2, string_size ~gen:(map Char.chr (int_range 32 126)) (int_range 1 6));
        (2, map (fun (text, _, _) -> text) (oneof_list corpus));
      ]
  in
  map (String.concat "") (list_size (int_range 0 6) piece)
