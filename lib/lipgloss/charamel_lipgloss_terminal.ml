open Lwt.Infix

let hex_digit c =
  match c with
  | '0' .. '9' -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

let scale_component s =
  let len = String.length s in
  if len = 0 || len > 4 then None
  else begin
    let value = ref 0 in
    let valid = ref true in
    String.iter
      (fun c ->
        match hex_digit c with
        | Some digit -> value := (!value lsl 4) lor digit
        | None -> valid := false)
      s;
    if not !valid then None
    else if len = 1 then Some (!value * 0x11)
    else Some (!value lsr ((4 * len) - 8))
  end

let parse_spec spec =
  let prefix = "rgb:" in
  if String.length spec >= 4 && String.sub spec 0 4 = prefix then
    match String.split_on_char '/' (String.sub spec 4 (String.length spec - 4)) with
    | [ r; g; b ] -> (
        match (scale_component r, scale_component g, scale_component b) with
        | Some r, Some g, Some b -> Charamel_ansi.Color.rgb r g b
        | _ -> None)
    | _ -> None
  else Charamel_ansi.Color.of_hex spec

let reply actions =
  actions
  |> List.filter_map (function
    | Charamel_ansi.Parser.Osc ("11" :: spec :: _) -> Some spec
    | _ -> None)
  |> List.find_map parse_spec

let rec scan parser channel =
  Lwt_io.read ~count:64 channel >>= fun chunk ->
  if String.equal chunk "" then Lwt.return_none
  else
    match reply (Charamel_ansi.Parser.feed parser chunk) with
    | Some color -> Lwt.return (Some color)
    | None -> scan parser channel

let exchange ~timeout input output =
  let parser = Charamel_ansi.Parser.create () in
  Lwt.catch
    (fun () ->
      Lwt_unix.with_timeout timeout (fun () ->
          Lwt_io.write output (Charamel_ansi.Seq.bg_query ^ Charamel_ansi.Seq.da1)
          >>= fun () ->
          Lwt_io.flush output >>= fun () -> scan parser input))
    (function
      | Lwt_unix.Timeout -> Lwt.return_none
      | Lwt_io.Channel_closed _ | Unix.Unix_error _ | End_of_file | Sys_error _ ->
          Lwt.return_none
      | exn -> Lwt.fail exn)

let in_raw_mode run =
  if not Charamel_os.Tty.is_tty_stdin then run ()
  else
    Lwt.catch
      (fun () ->
        let restore = Charamel_os.Tty.enter_raw () in
        Lwt.finalize run (fun () ->
            restore ();
            Lwt.return_unit))
      (function Unix.Unix_error _ -> run () | exn -> Lwt.fail exn)

let background_color ?(timeout = 2.0) ?input ?output () =
  let output = match output with Some channel -> channel | None -> Lwt_io.stdout in
  match input with
  | Some channel -> exchange ~timeout channel output
  | None -> (
      try
        let channel = Charamel_os.Tty.open_controlling_in () in
        Lwt.finalize
          (fun () -> in_raw_mode (fun () -> exchange ~timeout channel output))
          (fun () ->
            Lwt.catch (fun () -> Lwt_io.close channel) (fun _ -> Lwt.return_unit))
      with Unix.Unix_error _ -> Lwt.return_none)

let has_dark_background ?timeout ?input ?output () =
  background_color ?timeout ?input ?output () >>= function
  | None -> Lwt.return_true
  | Some color -> Lwt.return (Charamel_lipgloss.Color_util.is_dark color)

type triple = Charamel_lipgloss.Color_util.triple

let complete = Charamel_lipgloss.Color_util.complete
let complete_adaptive = Charamel_lipgloss.Color_util.complete_adaptive
