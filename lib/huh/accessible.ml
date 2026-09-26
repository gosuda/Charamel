open Lwt.Syntax

type reader = {
  read_line : unit -> string option Lwt.t;
  read_password : unit -> string option Lwt.t;
}

let reader_of_channel ~stdin ~echo_off =
  let read_line () =
    Lwt.catch
      (fun () ->
        let* line = Lwt_io.read_line stdin in
        Lwt.return (Some line))
      (function End_of_file -> Lwt.return_none | exn -> Lwt.fail exn)
  in
  let read_password () =
    match echo_off with
    | None -> read_line ()
    | Some restore ->
        Lwt.finalize
          (fun () -> read_line ())
          (fun () ->
            restore ();
            Lwt.return_unit)
  in
  { read_line; read_password }

let prompt_string ~out reader ~prompt ~default ~validate =
  let rec loop () =
    let* () = out prompt in
    let* input = reader.read_line () in
    match input with
    | None ->
        let* () = out "\n" in
        Lwt.return default
    | Some input -> (
        let input = String.trim input in
        let candidate = if input = "" then default else input in
        match validate candidate with
        | Ok () -> Lwt.return candidate
        | Error error ->
            let* () = out (error ^ "\n") in
            loop ())
  in
  loop ()

let prompt_int out reader ~prompt ~low ~high ~default =
  let range_error () =
    if low = high then Fmt.str "Invalid: must be %d" low
    else Fmt.str "Invalid: must be a number between %d and %d" low high
  in
  let rec loop () =
    let* () = out prompt in
    let* input = reader.read_line () in
    match input with
    | None ->
        let* () = out "\n" in
        Lwt.return (match default with Some value -> value | None -> low)
    | Some input -> (
        let input = String.trim input in
        if input = "" then
          match default with
          | Some value -> Lwt.return value
          | None ->
              let* () = out (range_error () ^ "\n") in
              loop ()
        else
          match int_of_string_opt input with
          | Some value when value >= low && value <= high -> Lwt.return value
          | _ ->
              let* () = out (range_error () ^ "\n") in
              loop ())
  in
  loop ()

let prompt_bool out reader ~prompt ~default =
  let rec loop () =
    let* () = out prompt in
    let* input = reader.read_line () in
    match input with
    | None ->
        let* () = out "\n" in
        Lwt.return default
    | Some input -> (
        match String.lowercase_ascii (String.trim input) with
        | "" -> Lwt.return default
        | "y" | "yes" -> Lwt.return true
        | "n" | "no" -> Lwt.return false
        | _ ->
            let* () = out "invalid input. please try again\n" in
            loop ())
  in
  loop ()

let prompt_password ~out reader ~prompt ~validate =
  let rec loop () =
    let* () = out prompt in
    let* input = reader.read_password () in
    match input with
    | None ->
        let* () = out "\n" in
        Lwt.return ""
    | Some input -> (
        let input = String.trim input in
        match validate input with
        | Ok () -> Lwt.return input
        | Error error ->
            let* () = out (error ^ "\n") in
            loop ())
  in
  loop ()
