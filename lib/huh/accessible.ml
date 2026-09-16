type reader = { read_line : unit -> string option; read_password : unit -> string option }

let reader_of_flow ~stdin ~echo_off =
  let buffered = Eio.Buf_read.of_flow ~max_size:65536 stdin in
  let read_line () = try Some (Eio.Buf_read.line buffered) with End_of_file -> None in
  let read_password () =
    match echo_off with
    | None -> read_line ()
    | Some disable ->
        let restore = disable () in
        Fun.protect
          ~finally:(fun () -> Eio.Cancel.protect restore)
          (fun () -> read_line ())
  in
  { read_line; read_password }

let prompt_string ~out reader ~prompt ~default ~validate =
  let rec loop () =
    out prompt;
    match reader.read_line () with
    | None ->
        out "\n";
        default
    | Some input -> (
        let input = String.trim input in
        let candidate = if input = "" then default else input in
        match validate candidate with
        | Ok () -> candidate
        | Error error ->
            out (error ^ "\n");
            loop ())
  in
  loop ()

let prompt_int out reader ~prompt ~low ~high ~default =
  let range_error () =
    if low = high then Fmt.str "Invalid: must be %d" low
    else Fmt.str "Invalid: must be a number between %d and %d" low high
  in
  let rec loop () =
    out prompt;
    match reader.read_line () with
    | None -> (
        out "\n";
        match default with Some value -> value | None -> low)
    | Some input -> (
        let input = String.trim input in
        if input = "" then (
          match default with
          | Some value -> value
          | None ->
              out (range_error () ^ "\n");
              loop ())
        else
          match int_of_string_opt input with
          | Some value when value >= low && value <= high -> value
          | _ ->
              out (range_error () ^ "\n");
              loop ())
  in
  loop ()

let prompt_bool out reader ~prompt ~default =
  let rec loop () =
    out prompt;
    match reader.read_line () with
    | None ->
        out "\n";
        default
    | Some input -> (
        match String.lowercase_ascii (String.trim input) with
        | "" -> default
        | "y" | "yes" -> true
        | "n" | "no" -> false
        | _ ->
            out "invalid input. please try again\n";
            loop ())
  in
  loop ()

let prompt_password out reader ~prompt ~validate =
  let rec loop () =
    out prompt;
    match reader.read_password () with
    | None ->
        out "\n";
        ""
    | Some input -> (
        let input = String.trim input in
        match validate input with
        | Ok () -> input
        | Error error ->
            out (error ^ "\n");
            loop ())
  in
  loop ()
