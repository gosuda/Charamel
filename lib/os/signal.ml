(* Converted per call rather than at initialization: on a platform with no job control the
   question is never asked at startup, and a module that touches the signal table when
   loaded is a module that can fail when merely linked. *)
let unsupported_on_win32 = [ Sys.sigwinch; Sys.sigtstp; Sys.sigcont ]

let supported number =
  if not Sys.win32 then true
  else
    let translated = List.map Sys.signal_to_int unsupported_on_win32 in
    not (List.mem number translated)
