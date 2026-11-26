type error = [ `Cannot_hide_input ]

let read_all input =
  let buffer = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buffer input 1024
     done
   with End_of_file -> ());
  Buffer.contents buffer

let read_lines input =
  let lines = ref [] in
  (try
     while true do
       lines := input_line input :: !lines
     done
   with End_of_file -> ());
  List.rev !lines

let read_stdin_phrase ?(is_tty = Charamel_os.Tty.is_tty_stdin)
    ?(echo_off = Charamel_os.Tty.echo_off) ?(input = stdin) ?(prompt = prerr_endline) () =
  if not is_tty then Lwt.return (Ok (read_all input))
  else
    match try Some (echo_off ()) with Unix.Unix_error _ -> None with
    | None -> Lwt.return (Error `Cannot_hide_input)
    | Some restore ->
        let tell message = try prompt message with Sys_error _ -> () in
        Fun.protect
          ~finally:(fun () -> restore ())
          (fun () ->
            tell
              (if Sys.win32 then "Enter recovery phrase; finish with Ctrl-Z, then Enter:"
               else "Enter recovery phrase; finish with Ctrl-D on an empty line:");
            let text = String.concat "\n" (read_lines input) in
            tell "";
            Lwt.return (Ok text))
