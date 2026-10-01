open Lwt.Infix

(* Test helpers: environment injection, scratch paths, bounded Lwt runs. Every test that
   blocks goes through [run], so a broken spawn fails with a timeout instead of hanging the
   suite. *)

let escaped =
  Alcotest.testable
    (fun ppf bytes -> Format.pp_print_string ppf (String.escaped (Bytes.to_string bytes)))
    Bytes.equal

let show_failure = function
  | `Not_found -> "Not_found"
  | `Already_exists -> "Already_exists"
  | `Permission_denied -> "Permission_denied"
  | `Is_directory -> "Is_directory"
  | `Unsupported -> "Unsupported"
  | `Error text -> text
  | `No_home -> "No_home"

type failure =
  [ `Already_exists
  | `Error of string
  | `Is_directory
  | `No_home
  | `Not_found
  | `Permission_denied
  | `Unsupported ]

let unit_result : 'a. ('a, [< failure ]) result -> string = function
  | Ok _ -> "Ok ()"
  | Error failure -> "Error " ^ show_failure failure

let size_result : (int * int, [< failure ]) result -> string = function
  | Ok (rows, cols) -> "Ok " ^ string_of_int rows ^ "x" ^ string_of_int cols
  | Error failure -> "Error " ^ show_failure failure

let text_result : (string, [< failure ]) result -> string = function
  | Ok text -> "Ok " ^ text
  | Error failure -> "Error " ^ show_failure failure

(* [Unix.unsetenv] only exists from OCaml 5.5, so the removal is bound like the
   library's own foreign calls: POSIX [unsetenv] deletes the variable, and on Windows a
   bare [_putenv "NAME"] does the same. *)
let unsetenv =
  if Sys.win32 then
    let putenv = Foreign.foreign "_putenv" Ctypes.(string @-> returning int) in
    fun name -> ignore (putenv name : int)
  else
    let unsetenv = Foreign.foreign "unsetenv" Ctypes.(string @-> returning int) in
    fun name -> ignore (unsetenv name : int)

let with_environment : 'a. (string * string) list -> (unit -> 'a) -> 'a =
 fun bindings f ->
  let previous = List.map (fun (name, _) -> (name, Sys.getenv_opt name)) bindings in
  let undo (name, value) =
    match value with Some text -> Unix.putenv name text | None -> unsetenv name
  in
  List.iter (fun (name, value) -> Unix.putenv name value) bindings;
  Fun.protect ~finally:(fun () -> List.iter undo previous) f

let without_environment : 'a. string list -> (unit -> 'a) -> 'a =
 fun names f ->
  let previous = List.map (fun name -> (name, Sys.getenv_opt name)) names in
  let undo (name, value) =
    match value with Some text -> Unix.putenv name text | None -> unsetenv name
  in
  List.iter unsetenv names;
  Fun.protect ~finally:(fun () -> List.iter undo previous) f

let counter = ref 0

let scratch stem =
  incr counter;
  Filename.concat
    (Filename.get_temp_dir_name ())
    ("charamel-os-test-"
    ^ string_of_int (Unix.getpid ())
    ^ "-" ^ string_of_int !counter ^ "-" ^ stem)

let run f = Lwt_main.run (Lwt_unix.with_timeout 20.0 f)

(* A test's scratch files and named pipes are removed whatever the assertion outcome, so the
   suite leaves the temporary directory as it found it. *)
let with_scratch : 'a. string list -> (unit -> 'a) -> 'a =
 fun paths f ->
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun path -> try Unix.unlink path with Unix.Unix_error _ -> ()) paths)
    f

let posix_only () = if Sys.win32 then Alcotest.skip ()
let windows_only () = if not Sys.win32 then Alcotest.skip ()

let show_error = function
  | `Not_found -> "Not_found"
  | `Already_exists -> "Already_exists"
  | `Permission_denied -> "Permission_denied"
  | `Is_directory -> "Is_directory"

let reason = function
  | `Error text -> text
  | `Unsupported -> "unsupported"
  | _ -> "failed"

let is_alive pid =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error _ -> false

let rec gone_within attempts pid =
  if not (is_alive pid) then Lwt.return_true
  else if attempts <= 0 then Lwt.return_false
  else Lwt_unix.sleep 0.05 >>= fun () -> gone_within (attempts - 1) pid

(* {1 Time} *)

let wall_is_the_calendar () =
  Alcotest.(check bool)
    "the real clock's wall reading is calendar time" true
    (Charamel_os.Time.wall Charamel_os.Time.lwt > 1_600_000_000.);
  let clock, advance = Charamel_os.Time.create_virtual () in
  let t = Charamel_os.Time.of_virtual clock in
  advance 42.5;
  Alcotest.(check (float 1e-9))
    "a virtual clock's wall reading is simulated seconds from the epoch" 42.5
    (Charamel_os.Time.wall t)

let virtual_clock_drives_sleeps () =
  let clock, advance = Charamel_os.Time.create_virtual () in
  let t = Charamel_os.Time.of_virtual clock in
  let early = Charamel_os.Time.sleep t 1.0 in
  let late = Charamel_os.Time.sleep t 5.0 in
  Alcotest.(check bool) "a sleeper waits for an advance" true (Lwt.state early = Lwt.Sleep);
  advance 2.0;
  Alcotest.(check bool)
    "one advance wakes the earlier deadline" true
    (Lwt.state early = Lwt.Return ());
  Alcotest.(check bool) "and leaves the later pending" true (Lwt.state late = Lwt.Sleep);
  advance 3.5;
  Alcotest.(check bool) "a second advance wakes it" true (Lwt.state late = Lwt.Return ());
  Alcotest.(check (float 0.001))
    "the clock reads the sum of the advances" 5.5 (Charamel_os.Time.now t)

let sleepers_wake_in_deadline_order () =
  let clock, advance = Charamel_os.Time.create_virtual () in
  let t = Charamel_os.Time.of_virtual clock in
  let order = ref [] in
  let record name promise = Lwt.map (fun () -> order := name :: !order) promise in
  let later = record "later" (Charamel_os.Time.sleep t 2.0) in
  let sooner = record "sooner" (Charamel_os.Time.sleep t 1.0) in
  let also_later = record "also-later" (Charamel_os.Time.sleep t 2.0) in
  advance 2.0;
  run (fun () -> Lwt.join [ later; sooner; also_later ]);
  Alcotest.(check (list string))
    "earliest first, ties in arrival order"
    [ "also-later"; "later"; "sooner" ]
    !order

let durations_that_are_already_over () =
  let clock, _ = Charamel_os.Time.create_virtual () in
  let t = Charamel_os.Time.of_virtual clock in
  let resolves_without_advance seconds =
    run (fun () -> Lwt.map (fun () -> true) (Charamel_os.Time.sleep t seconds))
  in
  Alcotest.(check bool)
    "a zero sleep resolves without advancing the clock" true
    (resolves_without_advance 0.0);
  Alcotest.(check bool) "so does a negative one" true (resolves_without_advance (-2.0));
  Alcotest.(check bool)
    "neither registers a deadline" true
    (Charamel_os.Time.next_deadline clock = None)

let cancelled_sleepers_are_not_woken () =
  let clock, advance = Charamel_os.Time.create_virtual () in
  let t = Charamel_os.Time.of_virtual clock in
  let waiting = Charamel_os.Time.sleep t 1.0 in
  let cancelled = Charamel_os.Time.sleep t 3.0 in
  Lwt.cancel cancelled;
  advance 4.0;
  Alcotest.(check bool) "the live sleeper woke" true (Lwt.state waiting = Lwt.Return ());
  Alcotest.(check bool)
    "the cancelled one stayed cancelled" true
    (Lwt.state cancelled = Lwt.Fail Lwt.Canceled)

let advancing_backwards_is_rejected () =
  let _, advance = Charamel_os.Time.create_virtual () in
  Alcotest.check_raises "a negative advance is a programming error"
    (Invalid_argument "Charamel_os.Time: cannot advance a clock backwards") (fun () ->
      advance (-1.0))

let real_clock_sleeps_and_stamps () =
  run (fun () ->
      let before = Charamel_os.Time.now Charamel_os.Time.lwt in
      Charamel_os.Time.sleep Charamel_os.Time.lwt 0.01 >>= fun () ->
      let after = Charamel_os.Time.now Charamel_os.Time.lwt in
      Alcotest.(check bool) "monotonic stamps move forward" true (after > before);
      Lwt.return_unit)

(* {1 Console_input.encode} *)

open Charamel_os.Console_input

let key ?(down = true) ?(repeat = 1) ?(virtual_key = 0) ?(wide_char = 0)
    ?(control_key_state = 0) () =
  Key_event { down; repeat; virtual_key; wide_char; control_key_state }

let char c = key ~wide_char:(Char.code c) ()

let encode_ignores_releases () =
  Alcotest.check escaped "a key-up contributes nothing" (Bytes.of_string "")
    (encode [ key ~down:false ~wide_char:(Char.code 'a') () ]);
  Alcotest.check escaped "a released special key contributes nothing" (Bytes.of_string "")
    (encode [ key ~down:false ~virtual_key:0x26 () ])

let encode_text_and_repeats () =
  Alcotest.check escaped "one key-down is one character" (Bytes.of_string "a")
    (encode [ char 'a' ]);
  Alcotest.check escaped "the repeat count is honored" (Bytes.of_string "aaaa")
    (encode [ key ~repeat:4 ~wide_char:(Char.code 'a') () ]);
  Alcotest.check escaped "records arrive in order" (Bytes.of_string "abc")
    (encode [ char 'a'; char 'b'; char 'c' ]);
  Alcotest.check escaped "a zero repeat still emits once" (Bytes.of_string "z")
    (encode [ key ~repeat:0 ~wide_char:(Char.code 'z') () ])

let encode_surrogate_pairs () =
  Alcotest.check escaped "a pair split across records joins"
    (Bytes.of_string "\xf0\x9f\x98\x80")
    (encode [ key ~wide_char:0xd83d (); key ~wide_char:0xde00 () ]);
  Alcotest.check escaped "the completing record's repeat applies"
    (Bytes.of_string "\xf0\x9f\x98\x80\xf0\x9f\x98\x80")
    (encode [ key ~wide_char:0xd83d (); key ~repeat:2 ~wide_char:0xde00 () ]);
  Alcotest.check escaped "a lone high surrogate is dropped" (Bytes.of_string "")
    (encode [ key ~wide_char:0xd83d () ]);
  Alcotest.check escaped "a low surrogate with no high is dropped" (Bytes.of_string "")
    (encode [ key ~wide_char:0xde00 () ]);
  Alcotest.check escaped "a dropped high surrogate does not spoil the next character"
    (Bytes.of_string "b")
    (encode [ key ~wide_char:0xd83d (); char 'b' ]);
  Alcotest.check escaped "basic multilingual text is three bytes"
    (Bytes.of_string "\xe3\x81\x82")
    (encode [ key ~wide_char:0x3042 () ]);
  Alcotest.check escaped "astral text is four"
    (Bytes.of_string "\xf0\x90\x81\x82")
    (encode [ key ~wide_char:0xd800 (); key ~wide_char:0xdc42 () ])

let check_key name expected virtual_key =
  Alcotest.check escaped name (Bytes.of_string expected) (encode [ key ~virtual_key () ])

let encode_virtual_keys () =
  check_key "up arrow" "\x1b[A" 0x26;
  check_key "down arrow" "\x1b[B" 0x28;
  check_key "right arrow" "\x1b[C" 0x27;
  check_key "left arrow" "\x1b[D" 0x25;
  check_key "home" "\x1b[H" 0x24;
  check_key "end" "\x1b[F" 0x23;
  check_key "page up" "\x1b[5~" 0x21;
  check_key "page down" "\x1b[6~" 0x22;
  check_key "insert" "\x1b[2~" 0x2d;
  check_key "delete" "\x1b[3~" 0x2e;
  check_key "backspace is DEL" "\x7f" 0x08;
  check_key "tab" "\t" 0x09;
  check_key "enter" "\r" 0x0d;
  check_key "escape" "\x1b" 0x1b;
  check_key "space" " " 0x20;
  check_key "F1" "\x1bOP" 0x70;
  check_key "F2" "\x1bOQ" 0x71;
  check_key "F5" "\x1b[15~" 0x74;
  check_key "F12" "\x1b[24~" 0x7b;
  Alcotest.check escaped "an unknown code contributes nothing" (Bytes.of_string "")
    (encode [ key ~virtual_key:0xf7 () ]);
  Alcotest.check escaped "a repeated arrow repeats its bytes"
    (Bytes.of_string "\x1b[B\x1b[B")
    (encode [ key ~repeat:2 ~virtual_key:0x28 () ])

let encode_modifiers () =
  Alcotest.check escaped "ctrl+right takes the parameter form"
    (Bytes.of_string "\x1b[1;5C")
    (encode [ key ~virtual_key:0x27 ~control_key_state:0x0008 () ]);
  Alcotest.check escaped "shift+delete inserts the parameter"
    (Bytes.of_string "\x1b[3;2~")
    (encode [ key ~virtual_key:0x2e ~control_key_state:0x0010 () ]);
  Alcotest.check escaped "alt+page up adds two" (Bytes.of_string "\x1b[5;3~")
    (encode [ key ~virtual_key:0x21 ~control_key_state:0x0002 () ]);
  Alcotest.check escaped "ctrl+alt+F1 keeps the letter form" (Bytes.of_string "\x1b[1;7P")
    (encode [ key ~virtual_key:0x70 ~control_key_state:0x000a () ]);
  Alcotest.check escaped "a control character arrives unchanged" (Bytes.of_string "\x03")
    (encode [ key ~wide_char:0x03 () ])

let encode_structural_records () =
  Alcotest.check escaped "a buffer resize reports its geometry"
    (Bytes.of_string "\x1b[8;24;80t")
    (encode [ Buffer_size { rows = 24; cols = 80 } ]);
  Alcotest.check escaped "focus gained" (Bytes.of_string "\x1b[I") (encode [ Focus true ]);
  Alcotest.check escaped "focus lost" (Bytes.of_string "\x1b[O") (encode [ Focus false ]);
  Alcotest.check escaped "ignored records contribute nothing" (Bytes.of_string "")
    (encode [ Ignored ]);
  Alcotest.check escaped "structure and keys interleave"
    (Bytes.of_string "a\x1b[A\x1b[I")
    (encode [ char 'a'; key ~virtual_key:0x26 (); Focus true ]);
  Alcotest.check escaped "an empty batch is empty" (Bytes.of_string "") (encode [])

let next_deadline_tracks_the_earliest_sleeper () =
  let clock, advance = Charamel_os.Time.create_virtual () in
  let t = Charamel_os.Time.of_virtual clock in
  Alcotest.(check (option (float 0.001)))
    "an idle clock has no deadline" None
    (Charamel_os.Time.next_deadline clock);
  let later = Charamel_os.Time.sleep t 4.0 in
  let sooner = Charamel_os.Time.sleep t 1.5 in
  Alcotest.(check (option (float 0.001)))
    "the earliest sleeper answers" (Some 1.5)
    (Charamel_os.Time.next_deadline clock);
  advance 1.5;
  Alcotest.(check (option (float 0.001)))
    "after waking it reports the next one" (Some 4.0)
    (Charamel_os.Time.next_deadline clock);
  advance 3.0;
  Alcotest.(check bool)
    "both sleepers finished" true
    (Lwt.state sooner = Lwt.Return () && Lwt.state later = Lwt.Return ());
  Alcotest.(check (option (float 0.001)))
    "and the clock is idle again" None
    (Charamel_os.Time.next_deadline clock)

let cancelled_sleepers_leave_the_deadline_list () =
  let clock, advance = Charamel_os.Time.create_virtual () in
  let t = Charamel_os.Time.of_virtual clock in
  let doomed = Charamel_os.Time.sleep t 2.0 in
  Lwt.cancel doomed;
  Alcotest.(check (option (float 0.001)))
    "a cancelled sleeper is not a deadline" None
    (Charamel_os.Time.next_deadline clock);
  advance 5.0;
  Alcotest.(check bool)
    "and it stays cancelled" true
    (Lwt.state doomed = Lwt.Fail Lwt.Canceled)

let queued_source_delivers_its_script () =
  let source = Charamel_os.Console_input.of_queue [ "ab"; "c" ] in
  let read = Charamel_os.Console_input.read in
  Lwt_main.run
    ( read source >>= fun first ->
      read source >>= fun second ->
      read source >>= fun third ->
      read source >|= fun fourth ->
      Alcotest.(check (list string))
        "chunks arrive in order, then end of input" [ "ab"; "c"; ""; "" ]
        [ first; second; third; fourth ] )

let reader_source_reports_none_as_end_of_input () =
  let remaining = ref [ Some "x"; Some "y" ] in
  let source =
    Charamel_os.Console_input.of_reader (fun () ->
        match !remaining with
        | first :: rest ->
            remaining := rest;
            Lwt.return first
        | [] -> Lwt.return_none)
  in
  let read = Charamel_os.Console_input.read in
  run (fun () ->
      read source >>= fun first ->
      read source >>= fun second ->
      read source >|= fun third ->
      Alcotest.(check (list string))
        "a finished reader answers the empty string" [ "x"; "y"; "" ]
        [ first; second; third ])

let channel_source_reads_and_ends () =
  let path = scratch "console-channel" in
  with_scratch [ path ] (fun () ->
      run (fun () ->
          Lwt_io.with_file ~mode:Lwt_io.output path (fun channel ->
              Lwt_io.write channel "hello")
          >>= fun () ->
          Lwt_io.with_file ~mode:Lwt_io.input path (fun channel ->
              let source = Charamel_os.Console_input.of_channel channel in
              Charamel_os.Console_input.read source >>= fun text ->
              Charamel_os.Console_input.read source >|= fun rest ->
              Alcotest.(check (pair string string))
                "a channel yields its content, then nothing" ("hello", "") (text, rest))))

let blocked_source_never_answers () =
  run (fun () ->
      let pending =
        Charamel_os.Console_input.read (Charamel_os.Console_input.blocked ())
      in
      Charamel_os.Console_input.read (Charamel_os.Console_input.of_queue [ "x" ])
      >>= fun _ ->
      Alcotest.(check bool)
        "a blocked source stays pending while others answer" true
        (Lwt.state pending = Lwt.Sleep);
      Lwt.return_unit)

let records_source_ends_on_posix () =
  posix_only ();
  run (fun () ->
      Charamel_os.Console_input.read (Charamel_os.Console_input.of_console_records ())
      >|= fun text -> Alcotest.(check string) "POSIX has no record queue to read" "" text)

let posix_console_queues_nothing () =
  posix_only ();
  run (fun () ->
      Charamel_os.Console_input.records () >|= fun records ->
      Alcotest.(check int) "a POSIX console has no input records" 0 (List.length records))

(* {1 Dirs} *)

let home_and_tilde () =
  with_environment
    [ ("HOME", "/tmp/fake-home"); ("USERPROFILE", "") ]
    (fun () ->
      Alcotest.(check string)
        "HOME answers" "Ok /tmp/fake-home"
        (text_result (Charamel_os.Dirs.home ()));
      Alcotest.(check string)
        "a bare tilde is home" "Ok /tmp/fake-home"
        (text_result (Charamel_os.Dirs.expand_tilde "~"));
      Alcotest.(check string)
        "tilde-slash joins"
        ("Ok " ^ Filename.concat "/tmp/fake-home" "notes.md")
        (text_result (Charamel_os.Dirs.expand_tilde "~/notes.md"));
      Alcotest.(check string)
        "an absolute path is untouched" "Ok /etc/hosts"
        (text_result (Charamel_os.Dirs.expand_tilde "/etc/hosts"));
      Alcotest.(check string)
        "a user-home form is not guessed at" "Ok ~rcfile"
        (text_result (Charamel_os.Dirs.expand_tilde "~rcfile")));
  posix_only ();
  without_environment [ "HOME" ] (fun () ->
      Alcotest.(check string)
        "no HOME is a typed failure, not an exception" "Error No_home"
        (text_result (Charamel_os.Dirs.home ()));
      Alcotest.(check string)
        "expansion reports the same failure" "Error No_home"
        (text_result (Charamel_os.Dirs.expand_tilde "~/x")))

let xdg_precedence () =
  with_environment
    [
      ("HOME", "/tmp/fake-home");
      ("XDG_CONFIG_HOME", "/tmp/fake-config");
      ("XDG_DATA_HOME", "relative-is-ignored");
      ("XDG_STATE_HOME", "");
    ]
    (fun () ->
      posix_only ();
      Alcotest.(check string)
        "an absolute variable wins" "/tmp/fake-config/charm"
        (Charamel_os.Dirs.config_dir ~app:"charm");
      Alcotest.(check string)
        "a relative one is ignored" "/tmp/fake-home/.local/share/charm"
        (Charamel_os.Dirs.data_dir ~app:"charm");
      Alcotest.(check string)
        "an empty one too" "/tmp/fake-home/.local/state/charm"
        (Charamel_os.Dirs.state_dir ~app:"charm");
      Alcotest.(check string)
        "the fallback joins the application" "/tmp/fake-home/.cache/charm"
        (Charamel_os.Dirs.cache_dir ~app:"charm"))

let windows_layout () =
  windows_only ();
  with_environment
    [ ("LOCALAPPDATA", "C:\\fake\\LocalAppData"); ("USERPROFILE", "C:\\fake\\Profile") ]
    (fun () ->
      Alcotest.(check string)
        "data goes to LocalAppData"
        (Filename.concat "C:\\fake\\LocalAppData" "charm")
        (Charamel_os.Dirs.data_dir ~app:"charm");
      Alcotest.(check string)
        "config goes under the profile"
        (Filename.concat (Filename.concat "C:\\fake\\Profile" ".config") "charm")
        (Charamel_os.Dirs.config_dir ~app:"charm"))

let temp_directory_follows_the_environment () =
  posix_only ();
  with_environment
    [ ("TMPDIR", "/tmp/fake-temp") ]
    (fun () ->
      Alcotest.(check string)
        "TMPDIR is honored at call time" "/tmp/fake-temp"
        (Charamel_os.Dirs.temp_dir ()));
  without_environment [ "TMPDIR"; "TEMP"; "TMP" ] (fun () ->
      Alcotest.(check string)
        "POSIX falls back to /tmp" "/tmp"
        (Charamel_os.Dirs.temp_dir ()))

let app_dir_without_any_base () =
  posix_only ();
  without_environment [ "HOME"; "XDG_CONFIG_HOME" ] (fun () ->
      Alcotest.check_raises "a config directory needs a base"
        (Invalid_argument
           "Charamel_os.Dirs: $XDG_CONFIG_HOME is not set to an absolute directory and \
            no home directory is usable") (fun () ->
          ignore (Charamel_os.Dirs.config_dir ~app:"charm")))

(* {1 Exe} *)

let searches_path_by_spawn () =
  posix_only ();
  run (fun () ->
      let child = Charamel_os.Process.spawn ~stdout:`Pipe [ "sh"; "-c"; "echo found" ] in
      Lwt_io.read_line (Charamel_os.Process.stdout_r child) >>= fun line ->
      Alcotest.(check string) "a bare program name is searched in PATH" "found" line;
      Charamel_os.Process.await child >|= fun code ->
      Alcotest.(check int) "and the program exited cleanly" 0 code)

let searches_path () =
  posix_only ();
  with_environment
    [ ("PATH", "/bin:/nonexistent-dir") ]
    (fun () ->
      Alcotest.(check (option string))
        "a program on PATH resolves" (Some "/bin/sh") (Charamel_os.Exe.find "sh");
      Alcotest.(check (option string))
        "nothing answers for an unknown program" None
        (Charamel_os.Exe.find "charamel-os-not-here");
      Alcotest.(check (option string))
        "nor for an empty name" None (Charamel_os.Exe.find ""))

let checks_explicit_paths () =
  with_environment
    [ ("PATH", "") ]
    (fun () ->
      if not Sys.win32 then
        Alcotest.(check (option string))
          "an absolute path is checked as given" (Some "/bin/sh")
          (Charamel_os.Exe.find "/bin/sh");
      Alcotest.(check (option string))
        "a directory is never an executable" None (Charamel_os.Exe.find "/bin"))

(* {1 Shell} *)

let command_uses_the_platform_shell () =
  posix_only ();
  Alcotest.(check (list string))
    "POSIX runs through sh"
    [ "/bin/sh"; "-c"; "echo hi" ]
    (Charamel_os.Shell.command "echo hi");
  Alcotest.(check (list string))
    "with the directory embedded"
    [ "/bin/sh"; "-c"; "cd '/tmp/my dir' && echo hi" ]
    (Charamel_os.Shell.command ~cwd:"/tmp/my dir" "echo hi");
  Alcotest.(check (list string))
    "a command with no directory has no cd" [ "/bin/sh"; "-c"; "true" ]
    (Charamel_os.Shell.command "true")

let quoting_protects_word_boundaries () =
  posix_only ();
  Alcotest.(check string) "single quotes wrap" "'plain'" (Charamel_os.Shell.quote "plain");
  Alcotest.(check string) "nothing at all still quotes" "''" (Charamel_os.Shell.quote "");
  Alcotest.(check string)
    "an embedded apostrophe closes and reopens" "'it'\\''s'"
    (Charamel_os.Shell.quote "it's");
  Alcotest.(check string)
    "metacharacters become inert" "'a; b | c && d $(e)'"
    (Charamel_os.Shell.quote "a; b | c && d $(e)");
  Alcotest.(check string) "a newline survives" "'a\nb'" (Charamel_os.Shell.quote "a\nb")

let splitting_words () =
  let check name expected text =
    Alcotest.(check (list string)) name expected (Charamel_os.Shell.split_words text)
  in
  check "spaces divide words" [ "less"; "-R" ] "less -R";
  check "tabs divide too" [ "a"; "b" ] "a\t\tb";
  check "single quotes hold a space" [ "my editor"; "-w" ] "'my editor' -w";
  check "double quotes hold a space" [ "code --wait" ] "\"code --wait\"";
  check "an apostrophe inside double quotes" [ "it's fine" ] "\"it's fine\"";
  check "adjacent quoted fragments join" [ "foobar" ] "\"foo\"'bar'";
  check "padding is dropped" [ "a" ] "   a   ";
  check "an unterminated quote ends with the input" [ "a" ] "\"a";
  check "empty input is no words" [] "";
  check "a newline is not a separator" [ "a\nb" ] "a\nb"

(* {1 Editor} *)

let defaults_come_from_the_platform () =
  without_environment [ "EDITOR"; "PAGER"; "BROWSER" ] (fun () ->
      if Sys.win32 then
        Alcotest.(check (list string))
          "Windows edits in notepad" [ "notepad" ]
          (Charamel_os.Editor.editor ())
      else (
        Alcotest.(check (list string))
          "POSIX edits in vi" [ "vi" ]
          (Charamel_os.Editor.editor ());
        Alcotest.(check (list string))
          "POSIX pages with less -r" [ "less"; "-r" ] (Charamel_os.Editor.pager ()));
      if Sys.win32 then
        Alcotest.(check (list string))
          "Windows starts a URL through cmd" [ "cmd"; "/c"; "start" ]
          (Charamel_os.Editor.browser ())
      else
        let mac = Sys.file_exists "/System/Library/CoreServices/SystemVersion.plist" in
        Alcotest.(check (list string))
          "the browser default follows the system"
          (if mac then [ "open" ] else [ "xdg-open" ])
          (Charamel_os.Editor.browser ()))

let environment_overrides_are_word_split () =
  with_environment
    [ ("EDITOR", "code -w") ]
    (fun () ->
      Alcotest.(check (list string))
        "EDITOR" [ "code"; "-w" ]
        (Charamel_os.Editor.editor ()));
  with_environment
    [ ("PAGER", "bat --paging never") ]
    (fun () ->
      Alcotest.(check (list string))
        "PAGER"
        [ "bat"; "--paging"; "never" ]
        (Charamel_os.Editor.pager ()));
  with_environment
    [ ("BROWSER", "firefox --private-window") ]
    (fun () ->
      Alcotest.(check (list string))
        "BROWSER"
        [ "firefox"; "--private-window" ]
        (Charamel_os.Editor.browser ()));
  with_environment
    [ ("EDITOR", "   ") ]
    (fun () ->
      let default = if Sys.win32 then [ "notepad" ] else [ "vi" ] in
      Alcotest.(check (list string))
        "a blank variable counts as unset" default
        (Charamel_os.Editor.editor ()))

(* {1 Signal} *)

let signals_available_here () =
  let numbers =
    List.map Sys.signal_to_int
      [ Sys.sigwinch; Sys.sigtstp; Sys.sigcont; Sys.sigint; Sys.sigterm ]
  in
  if not Sys.win32 then
    List.iter
      (fun number ->
        Alcotest.(check bool)
          "every signal is available on POSIX" true
          (Charamel_os.Signal.supported number))
      numbers
  else
    match numbers with
    | [ winch; suspend; resume; interrupt; _ ] ->
        Alcotest.(check bool)
          "resize cannot be caught on Windows" false
          (Charamel_os.Signal.supported winch);
        Alcotest.(check bool)
          "suspend cannot" false
          (Charamel_os.Signal.supported suspend);
        Alcotest.(check bool) "resume cannot" false (Charamel_os.Signal.supported resume);
        Alcotest.(check bool)
          "an interrupt can" true
          (Charamel_os.Signal.supported interrupt)
    | _ -> Alcotest.fail "the platform defines the signals this test lists"

(* {1 Tty} *)

let detection_matches_the_runtime () =
  Alcotest.(check bool)
    "stdin is a terminal exactly when the runtime says" true
    (Charamel_os.Tty.is_tty_stdin = Unix.isatty Unix.stdin);
  Alcotest.(check bool)
    "stdout likewise" true
    (Charamel_os.Tty.is_tty_stdout = Unix.isatty Unix.stdout);
  match Charamel_os.Tty.size_stdout () with
  | Some (rows, cols) ->
      Alcotest.(check bool) "a reported size is positive" true (rows > 0 && cols > 0)
  | None -> ()

let suspend_support_matches_platform () =
  Alcotest.(check bool)
    "suspension means what the platform says" true
    (Charamel_os.Tty.supports_suspend = not Sys.win32)

let resize_subscription_installs_and_removes () =
  run (fun () ->
      let hits = ref 0 in
      Charamel_os.Tty.on_resize (fun () -> incr hits) >>= fun unsubscribe ->
      unsubscribe () |> Lwt.return >>= fun () ->
      Alcotest.(check int) "installing delivers nothing until a resize" 0 !hits;
      Lwt.return_unit)

let raw_mode_requires_a_terminal () =
  if Charamel_os.Tty.is_tty_stdin then (
    let restore = Charamel_os.Tty.enter_raw () in
    restore ();
    let quiet = Charamel_os.Tty.echo_off () in
    quiet ();
    ())
  else
    let call = if Sys.win32 then ("GetConsoleMode", "CON") else ("tcgetattr", "") in
    Alcotest.check_raises "enter_raw on a non-terminal fails with ENOTTY"
      (Unix.Unix_error (Unix.ENOTTY, fst call, snd call))
      (fun () ->
        let restore = Charamel_os.Tty.enter_raw () in
        restore ())

(* {1 Fs} *)

let write_to path text =
  Charamel_os.Fs.with_open_out ~perm:0o600 path (fun channel -> Lwt_io.write channel text)

let writes_renames_and_stats () =
  let path = scratch "fs-file" in
  let other = scratch "fs-other" in
  with_scratch [ path; other ] (fun () ->
      run (fun () ->
          write_to path "content" >>= fun () ->
          Charamel_os.Fs.stat path >>= function
          | Error error -> Alcotest.failf "stat failed: %s" (show_error error)
          | Ok info ->
              Alcotest.(check bool) "the bytes landed" true (info.Unix.st_size = 7);

              Charamel_os.Fs.rename_replace ~src:path ~dst:other >>= fun () ->
              Charamel_os.Fs.stat path >|= fun moved ->
              Alcotest.(check string)
                "the source is gone" "Error Not_found" (unit_result moved)))

let replacing_an_existing_file () =
  let path = scratch "fs-replace" in
  let target = scratch "fs-replace-target" in
  with_scratch [ path; target ] (fun () ->
      run (fun () ->
          write_to path "new" >>= fun () ->
          write_to target "old-and-longer" >>= fun () ->
          Charamel_os.Fs.rename_replace ~src:path ~dst:target >>= fun () ->
          Charamel_os.Fs.stat target >|= function
          | Ok info ->
              Alcotest.(check bool)
                "the destination was replaced" true (info.Unix.st_size = 3)
          | Error error -> Alcotest.failf "stat failed: %s" (show_failure error)))

let missing_paths_report_typed_errors () =
  let path = scratch "fs-absent" in
  with_scratch [ path ] (fun () ->
      run (fun () ->
          Charamel_os.Fs.stat path >>= fun stat ->
          Alcotest.(check string)
            "stat says Not_found" "Error Not_found" (unit_result stat);
          Charamel_os.Fs.read_link path >>= fun link ->
          Alcotest.(check string)
            "readlink says Not_found" "Error Not_found" (unit_result link);
          Charamel_os.Fs.unlink path >|= fun gone ->
          Alcotest.(check string)
            "unlink says Not_found" "Error Not_found" (unit_result gone)))

let non_links_report_not_found () =
  posix_only ();
  let path = scratch "fs-plain" in
  with_scratch [ path ] (fun () ->
      run (fun () ->
          write_to path "x" >>= fun () ->
          Charamel_os.Fs.read_link path >|= fun link ->
          Alcotest.(check string)
            "a regular file is no link" "Error Not_found" (unit_result link)))

let directories_are_created_and_listed () =
  let root = scratch "fs-tree" in
  let nested = Filename.concat (Filename.concat root "a") "b" in
  with_scratch
    [ nested; Filename.concat root "a"; root ]
    (fun () ->
      run (fun () ->
          Charamel_os.Fs.mkdir_p nested >>= fun made ->
          Alcotest.(check string)
            "mkdir_p builds every component" "Ok ()" (unit_result made);
          Charamel_os.Fs.mkdir_p nested >>= fun again ->
          Alcotest.(check string)
            "and tolerates an existing directory" "Ok ()" (unit_result again);
          Charamel_os.Fs.read_dir root >>= fun names ->
          Alcotest.(check (list string))
            "listing reports the child" [ "a" ]
            (Result.get_ok
               (Result.map (List.filter (fun name -> name <> "." && name <> "..")) names));
          Charamel_os.Fs.unlink root >|= fun removed ->
          Alcotest.(check string)
            "unlinking a directory reports Is_directory" "Error Is_directory"
            (unit_result removed)))

let a_file_in_the_way_is_a_collision () =
  let root = scratch "fs-clash" in
  let plain = Filename.concat root "plain" in
  with_scratch [ plain; root ] (fun () ->
      run (fun () ->
          Charamel_os.Fs.mkdir_p root >>= fun _ ->
          write_to plain "x" >>= fun () ->
          Charamel_os.Fs.mkdir_p (Filename.concat plain "sub") >|= fun clash ->
          Alcotest.(check string)
            "mkdir_p will not overwrite a file" "Error Already_exists" (unit_result clash)))

(* {1 Process} *)

let exit_codes () =
  posix_only ();
  run (fun () ->
      let living = Charamel_os.Process.spawn [ "/bin/sh"; "-c"; "sleep 30" ] in
      Alcotest.(check bool)
        "a fresh child is alive" true
        (Charamel_os.Process.alive living);
      Charamel_os.Process.terminate living;
      Charamel_os.Process.await living >>= fun _terminated ->
      let child = Charamel_os.Process.spawn [ "/bin/sh"; "-c"; "exit 3" ] in
      Charamel_os.Process.await child >|= fun code ->
      Alcotest.(check int) "the status passes through" 3 code)

let pipes_round_trip () =
  posix_only ();
  run (fun () ->
      let child = Charamel_os.Process.spawn ~stdin:`Pipe ~stdout:`Pipe [ "/bin/cat" ] in
      let input = Charamel_os.Process.stdin_w child in
      let output = Charamel_os.Process.stdout_r child in
      Lwt_io.write input "hello\n" >>= fun () ->
      Lwt_io.flush input >>= fun () ->
      Lwt_io.read_line output >>= fun line ->
      Alcotest.(check string) "cat echoes the line" "hello" line;
      Charamel_os.Process.terminate child;
      Charamel_os.Process.await child >|= fun code ->
      Alcotest.(check int)
        "SIGTERM to cat becomes 143"
        (128 + Sys.signal_to_int Sys.sigterm)
        code)

let closing_stdin_ends_the_stream () =
  posix_only ();
  run (fun () ->
      let child = Charamel_os.Process.spawn ~stdin:`Pipe ~stdout:`Pipe [ "/bin/cat" ] in
      let input = Charamel_os.Process.stdin_w child in
      Lwt_io.write input "one\ntwo\n" >>= fun () ->
      Lwt_io.flush input >>= fun () ->
      Lwt_io.close input >>= fun () ->
      Charamel_os.Process.stdout_r child |> Lwt_io.read >>= fun echoed ->
      Charamel_os.Process.await child >|= fun code ->
      Alcotest.(check string) "stdin end-of-file reaches the child" "one\ntwo\n" echoed;
      Alcotest.(check int) "and the child exits cleanly" 0 code)

let stderr_is_separate () =
  posix_only ();
  run (fun () ->
      let child =
        Charamel_os.Process.spawn ~stdout:`Pipe ~stderr:`Pipe
          [ "/bin/sh"; "-c"; "echo out; echo err 1>&2" ]
      in
      Lwt_io.read_line (Charamel_os.Process.stdout_r child) >>= fun out ->
      Lwt_io.read_line (Charamel_os.Process.stderr_r child) >>= fun err ->
      Alcotest.(check string) "stdout" "out" out;
      Alcotest.(check string) "stderr" "err" err;
      Charamel_os.Process.await child >|= fun code ->
      Alcotest.(check int) "and the program succeeded" 0 code)

let environment_replaces_and_directory_applies () =
  posix_only ();
  run (fun () ->
      let child =
        Charamel_os.Process.spawn ~cwd:"/tmp" ~env:[| "PATH=/bin:/usr/bin" |]
          ~stdout:`Pipe
          [ "/bin/sh"; "-c"; "pwd; printf '%s' \"$PATH\"" ]
      in
      Lwt_io.read_line (Charamel_os.Process.stdout_r child) >>= fun directory ->
      Alcotest.(check bool)
        "cwd is applied" true
        (directory = "/tmp" || directory = "/private/tmp");
      Lwt_io.read_line (Charamel_os.Process.stdout_r child) >>= fun path ->
      Alcotest.(check string) "env replaces rather than adds" "/bin:/usr/bin" path;
      Charamel_os.Process.await child >|= fun code ->
      Alcotest.(check int) "the program exited cleanly" 0 code)

let null_and_inherit_are_not_pipes () =
  posix_only ();
  run (fun () ->
      let child = Charamel_os.Process.spawn ~stdout:`Null [ "/bin/true" ] in
      Alcotest.check_raises "asking for a non-pipe stream is a programming error"
        (Invalid_argument "Charamel_os.Process.stdout_r: that stream was not a pipe")
        (fun () -> ignore (Charamel_os.Process.stdout_r child));
      Charamel_os.Process.await child >>= fun code ->
      Alcotest.(check int) "the program still ran" 0 code;
      Alcotest.(check bool) "and is reported dead" false (Charamel_os.Process.alive child);
      Lwt.return_unit)

let missing_program_raises_typed_failure () =
  posix_only ();
  Alcotest.check_raises "a PATH miss is an ENOENT"
    (Unix.Unix_error (Unix.ENOENT, "posix_spawnp", "charamel-os-no-such-program"))
    (fun () -> ignore (Charamel_os.Process.spawn [ "charamel-os-no-such-program" ]))

let kill_tree_reaches_the_whole_group () =
  posix_only ();
  run (fun () ->
      let child =
        Charamel_os.Process.spawn ~stdout:`Pipe
          [ "/bin/sh"; "-c"; "sleep 30 & echo $!; sleep 30" ]
      in
      Lwt_io.read_line (Charamel_os.Process.stdout_r child) >>= fun text ->
      let grandchild = int_of_string text in
      is_alive grandchild |> Lwt.return >>= fun started ->
      Alcotest.(check bool) "the grandchild started" true started;
      Charamel_os.Process.kill_tree child;
      Charamel_os.Process.await child >>= fun code ->
      Alcotest.(check int) "SIGKILL to the group reports 137" 137 code;
      gone_within 40 grandchild >|= fun vanished ->
      Alcotest.(check bool) "the whole group is gone" true vanished)

(* [await] hands out the one shared promise the reaper resolves. A race that abandons
   the wait must leave that promise usable: [Lwt.protected] stops cancellation at the
   barrier and [Lwt.choose] never cancels the loser — the pattern [Jobs] and the MCP
   transport depend on. A poisoned promise would answer [Lwt.Canceled] here instead of
   the child's signal status. *)
let abandoned_race_keeps_the_shared_await_usable () =
  posix_only ();
  run (fun () ->
      let child = Charamel_os.Process.spawn [ "/bin/sh"; "-c"; "sleep 30" ] in
      Lwt.choose
        [
          (Lwt.protected (Charamel_os.Process.await child) >|= fun code -> Some code);
          (Lwt_unix.sleep 0.1 >|= fun () -> None);
        ]
      >>= fun raced ->
      Alcotest.(check bool) "the deadline won the race" true (raced = None);
      Charamel_os.Process.kill_tree child;
      Charamel_os.Process.await child >|= fun code ->
      Alcotest.(check int) "the later await still answers" 137 code)

(* {1 Pty} *)

let pty_is_unsupported_on_windows () =
  windows_only ();
  run (fun () ->
      Charamel_os.Pty.create () >|= fun created ->
      Alcotest.(check bool)
        "Windows refuses pseudo-terminals" true
        (match created with
        | Error `Unsupported -> true
        | Ok _ | Error (`Error _) -> false))

let rec pty_write_all pty text offset =
  Charamel_os.Pty.write pty text offset (String.length text - offset) >>= function
  | Ok count when offset + count < String.length text ->
      pty_write_all pty text (offset + count)
  | Ok _ -> Lwt.return_unit
  | Error error -> Alcotest.failf "write failed: %s" (reason error)

let pty_geometry_and_echo () =
  posix_only ();
  run (fun () ->
      Charamel_os.Pty.create ~rows:30 ~cols:100 () >>= fun created ->
      match created with
      | Error error -> Alcotest.failf "create failed: %s" (reason error)
      | Ok pty -> begin
          Alcotest.(check string)
            "the requested geometry is in effect" "Ok 30x100"
            (size_result (Charamel_os.Pty.size pty));
          Alcotest.(check bool)
            "the slave has a path" true
            (String.length (Charamel_os.Pty.slave_path pty) > 0);
          Charamel_os.Pty.exec pty [ "/bin/cat" ] >>= fun started ->
          match started with
          | Error error -> Alcotest.failf "exec failed: %s" (reason error)
          | Ok child ->
              Alcotest.(check bool) "a child pid came back" true (child > 0);
              pty_write_all pty "ping\n" 0 >>= fun () ->
              Charamel_os.Pty.read pty 64 >>= fun echoed ->
              (match echoed with
              | Ok received ->
                  Alcotest.(check bool)
                    "the pty echoes its input" true
                    (String.length received >= 4 && String.sub received 0 4 = "ping")
              | Error error -> Alcotest.failf "read failed: %s" (reason error));
              Alcotest.(check string)
                "resizing is accepted" "Ok ()"
                (unit_result (Charamel_os.Pty.resize pty ~rows:12 ~cols:40));
              Alcotest.(check string)
                "and visible" "Ok 12x40"
                (size_result (Charamel_os.Pty.size pty));
              Charamel_os.Pty.terminate pty;
              Charamel_os.Pty.close pty;
              Charamel_os.Pty.close pty;
              Lwt.return_unit
        end)

let pty_requires_a_geometry () =
  posix_only ();
  Alcotest.check_raises "a zero-row terminal is a programming error"
    (Invalid_argument
       "Charamel_os.Pty.create: a terminal has at least one row and one column")
    (fun () -> ignore (Charamel_os.Pty.create ~rows:0 ()));
  Alcotest.check_raises "so is a zero-column one"
    (Invalid_argument
       "Charamel_os.Pty.create: a terminal has at least one row and one column")
    (fun () -> ignore (Charamel_os.Pty.create ~cols:0 ()))

(* A master read returns whatever is queued, which may be a fragment of a line, so a test that
   needs a whole line accumulates until the newline arrives or the attempts run out. *)
let rec read_until_line pty attempts text =
  Charamel_os.Pty.read pty 128 >>= fun chunk ->
  match chunk with
  | Error failure -> Alcotest.failf "read failed: %s" (reason failure)
  | Ok received ->
      let accumulated = text ^ received in
      if
        String.length accumulated
        > String.length (String.concat "" (String.split_on_char '\n' accumulated))
        || attempts <= 1
      then Lwt.return accumulated
      else read_until_line pty (attempts - 1) accumulated

let pty_child_owns_the_controlling_terminal () =
  posix_only ();
  run (fun () ->
      Charamel_os.Pty.create ~rows:24 ~cols:80 () >>= fun created ->
      match created with
      | Error failure -> Alcotest.failf "create failed: %s" (reason failure)
      | Ok pty -> (
          let slave = Charamel_os.Pty.slave_path pty in
          Charamel_os.Pty.exec pty [ "/bin/sh"; "-c"; "tty" ] >>= fun started ->
          match started with
          | Error failure -> Alcotest.failf "exec failed: %s" (reason failure)
          | Ok child ->
              Alcotest.(check bool) "a child pid came back" true (child > 0);
              read_until_line pty 8 "" >>= fun text ->
              Charamel_os.Pty.terminate pty;
              Charamel_os.Pty.close pty;
              Alcotest.(check bool)
                "the child's controlling terminal is the slave we gave it" true
                (Test_support.contains ~needle:slave ~haystack:text);
              Lwt.return_unit))

let pty_rejects_a_bad_range () =
  posix_only ();
  run (fun () ->
      Charamel_os.Pty.create () >|= fun created ->
      match created with
      | Error error -> Alcotest.failf "create failed: %s" (reason error)
      | Ok pty ->
          Alcotest.check_raises "a range outside the string is a programming error"
            (Invalid_argument "Charamel_os.Pty.write: range outside the string")
            (fun () -> ignore (Charamel_os.Pty.write pty "abc" 2 5));
          Charamel_os.Pty.close pty)

let suites =
  [
    ( "time",
      [
        Alcotest.test_case "virtual clock drives sleeps" `Quick
          virtual_clock_drives_sleeps;
        Alcotest.test_case "wake order" `Quick sleepers_wake_in_deadline_order;
        Alcotest.test_case "durations already over" `Quick durations_that_are_already_over;
        Alcotest.test_case "cancelled sleepers" `Quick cancelled_sleepers_are_not_woken;
        Alcotest.test_case "backwards advance" `Quick advancing_backwards_is_rejected;
        Alcotest.test_case "next deadline" `Quick
          next_deadline_tracks_the_earliest_sleeper;
        Alcotest.test_case "deadline list shrinks" `Quick
          cancelled_sleepers_leave_the_deadline_list;
        Alcotest.test_case "real clock" `Quick real_clock_sleeps_and_stamps;
        Alcotest.test_case "wall reads the calendar" `Quick wall_is_the_calendar;
      ] );
    ( "console_input",
      [
        Alcotest.test_case "releases ignored" `Quick encode_ignores_releases;
        Alcotest.test_case "text and repeats" `Quick encode_text_and_repeats;
        Alcotest.test_case "surrogate pairs" `Quick encode_surrogate_pairs;
        Alcotest.test_case "virtual keys" `Quick encode_virtual_keys;
        Alcotest.test_case "modifiers" `Quick encode_modifiers;
        Alcotest.test_case "structural records" `Quick encode_structural_records;
        Alcotest.test_case "posix console" `Quick posix_console_queues_nothing;
        Alcotest.test_case "queued source" `Quick queued_source_delivers_its_script;
        Alcotest.test_case "reader source" `Quick
          reader_source_reports_none_as_end_of_input;
        Alcotest.test_case "channel source" `Quick channel_source_reads_and_ends;
        Alcotest.test_case "blocked source" `Quick blocked_source_never_answers;
        Alcotest.test_case "records source" `Quick records_source_ends_on_posix;
      ] );
    ( "dirs",
      [
        Alcotest.test_case "home and tilde" `Quick home_and_tilde;
        Alcotest.test_case "XDG precedence" `Quick xdg_precedence;
        Alcotest.test_case "Windows layout" `Quick windows_layout;
        Alcotest.test_case "temp directory" `Quick temp_directory_follows_the_environment;
        Alcotest.test_case "no base directory" `Quick app_dir_without_any_base;
      ] );
    ( "exe",
      [
        Alcotest.test_case "PATH search" `Quick searches_path;
        Alcotest.test_case "explicit paths" `Quick checks_explicit_paths;
      ] );
    ( "shell",
      [
        Alcotest.test_case "command" `Quick command_uses_the_platform_shell;
        Alcotest.test_case "quoting" `Quick quoting_protects_word_boundaries;
        Alcotest.test_case "splitting" `Quick splitting_words;
      ] );
    ( "editor",
      [
        Alcotest.test_case "platform defaults" `Quick defaults_come_from_the_platform;
        Alcotest.test_case "environment overrides" `Quick
          environment_overrides_are_word_split;
      ] );
    ("signal", [ Alcotest.test_case "available signals" `Quick signals_available_here ]);
    ( "tty",
      [
        Alcotest.test_case "detection" `Quick detection_matches_the_runtime;
        Alcotest.test_case "suspend support" `Quick suspend_support_matches_platform;
        Alcotest.test_case "resize subscription" `Quick
          resize_subscription_installs_and_removes;
        Alcotest.test_case "raw mode" `Quick raw_mode_requires_a_terminal;
      ] );
    ( "fs",
      [
        Alcotest.test_case "write rename stat" `Quick writes_renames_and_stats;
        Alcotest.test_case "replace existing" `Quick replacing_an_existing_file;
        Alcotest.test_case "missing paths" `Quick missing_paths_report_typed_errors;
        Alcotest.test_case "non-links" `Quick non_links_report_not_found;
        Alcotest.test_case "directories" `Quick directories_are_created_and_listed;
        Alcotest.test_case "collisions" `Quick a_file_in_the_way_is_a_collision;
      ] );
    ( "process",
      [
        Alcotest.test_case "exit codes" `Quick exit_codes;
        Alcotest.test_case "PATH search by spawn" `Quick searches_path_by_spawn;
        Alcotest.test_case "pipes round trip" `Quick pipes_round_trip;
        Alcotest.test_case "closing stdin" `Quick closing_stdin_ends_the_stream;
        Alcotest.test_case "stderr is separate" `Quick stderr_is_separate;
        Alcotest.test_case "environment and cwd" `Quick
          environment_replaces_and_directory_applies;
        Alcotest.test_case "null and inherit" `Quick null_and_inherit_are_not_pipes;
        Alcotest.test_case "missing program" `Quick missing_program_raises_typed_failure;
        Alcotest.test_case "kill tree" `Quick kill_tree_reaches_the_whole_group;
        Alcotest.test_case "abandoned race keeps await usable" `Quick
          abandoned_race_keeps_the_shared_await_usable;
      ] );
    ( "pty",
      [
        Alcotest.test_case "unsupported on windows" `Quick pty_is_unsupported_on_windows;
        Alcotest.test_case "geometry and echo" `Quick pty_geometry_and_echo;
        Alcotest.test_case "bad range" `Quick pty_rejects_a_bad_range;
        Alcotest.test_case "geometry" `Quick pty_requires_a_geometry;
        Alcotest.test_case "controlling terminal" `Quick
          pty_child_owns_the_controlling_terminal;
      ] );
  ]

let () = Alcotest.run "charamel.os" suites
