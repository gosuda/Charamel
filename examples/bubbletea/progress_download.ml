open Lwt.Syntax
module Color = Charamel_ansi.Color
module Cmd = Charamel_tea.Cmd
module Progress = Charamel_bubbles.Progress
module Style = Charamel_lipgloss.Style
module Sub = Charamel_tea.Sub

let padding = 2
let max_width = 80
let help_style = Style.(empty |> foreground (Color.of_hex_or "#626262"))

type model = { progress : Progress.t; err : string option }

type msg =
  | Key of Charamel_tea.Key.t
  | Win of int
  | Frame of Progress.msg
  | Ratio of float
  | Failed of string
  | Settled

let final_pause = Cmd.after 0.75 (fun () -> Settled)

let on_win cols model =
  let wanted = cols - (padding * 2) - 4 in
  let width = if wanted > max_width then max_width else wanted in
  ({ model with progress = Progress.set_width width model.progress }, Cmd.none)

let on_ratio ratio model =
  let model = { model with progress = Progress.set_percent ratio model.progress } in
  if ratio >= 1.0 then (model, Cmd.seq [ final_pause; Cmd.quit ]) else (model, Cmd.none)

let update msg model =
  match msg with
  | Key _ -> (model, Cmd.quit)
  | Win cols -> on_win cols model
  | Ratio ratio -> on_ratio ratio model
  | Failed message -> ({ model with err = Some message }, Cmd.quit)
  | Settled -> (model, Cmd.none)
  | Frame frame ->
      let progress, cmd = Progress.update frame model.progress in
      ({ model with progress }, Cmd.map (fun frame -> Frame frame) cmd)

let view model =
  match model.err with
  | Some message -> Charamel_tea.View.v ("Error downloading: " ^ message ^ "\n")
  | None ->
      let pad = String.make padding ' ' in
      Charamel_tea.View.v
        ("\n" ^ pad ^ Progress.view model.progress ^ "\n\n" ^ pad
        ^ Style.render help_style "Press any key to quit")

let app values : (model, msg) Charamel_tea.app =
  {
    init = (fun () -> ({ progress = Progress.v (); err = None }, Cmd.none));
    update;
    view;
    subscriptions =
      (fun model ->
        Sub.batch
          [
            Sub.key (fun key -> Key key);
            Sub.resize (fun ~rows:_ ~cols -> Win cols);
            Sub.map (fun frame -> Frame frame) (Progress.subscriptions model.progress);
            Sub.stream values;
          ]);
  }

let usage () =
  prerr_endline "Usage of progress-download:";
  prerr_endline "  -url string";
  prerr_endline "    \turl for the file to download"

let is_url_flag arg = String.equal arg "-url" || String.equal arg "--url"

let split_flag arg =
  match String.index_opt arg '=' with
  | Some i -> Some (String.sub arg 0 i, String.sub arg (i + 1) (String.length arg - i - 1))
  | None -> None

let url_arg argv =
  let rec go = function
    | [] | [ _ ] -> None
    | arg :: value :: rest -> (
        if is_url_flag arg then Some value
        else
          match split_flag arg with
          | Some (flag, value) when is_url_flag flag -> Some value
          | _ -> go (value :: rest))
  in
  go (Stdlib.List.tl (Array.to_list argv))

let content_length response =
  match Cohttp.Header.get (Cohttp.Response.headers response) "content-length" with
  | Some header -> int_of_string_opt (String.trim header)
  | None -> None

let abort stream message =
  (if stream = `Error then prerr_endline else print_endline) message;
  exit 1

let pump ~push ~sink ~total chunks =
  let downloaded = ref 0 in
  let write chunk =
    downloaded := !downloaded + String.length chunk;
    let* () = sink chunk in
    if total > 0 then push (Some (Ratio (float_of_int !downloaded /. float_of_int total)));
    Lwt.return_unit
  in
  Lwt.try_bind
    (fun () -> Lwt_stream.iter_s write chunks)
    (fun () -> Lwt.return_unit)
    (fun exn ->
      push (Some (Failed (Printexc.to_string exn)));
      Lwt.return_unit)

let open_file filename =
  let* opened =
    Lwt.try_bind
      (fun () -> Lwt_io.open_file ~perm:0o644 ~mode:Lwt_io.Output filename)
      (fun file -> Lwt.return_ok file)
      (fun exn -> Lwt.return_error (Printexc.to_string exn))
  in
  match opened with
  | Ok file -> Lwt.return file
  | Error message -> abort `Out ("could not create file: " ^ message)

let start ~push url =
  let* response = Charamel_net.call ~meth:`GET ~body:None (Uri.of_string url) in
  match response with
  | Error error ->
      Lwt.return
        (abort `Error
           (Fmt.str "could not get response %s" (Charamel_net.error_message error)))
  | Ok (response, chunks) -> (
      let status = Cohttp.Code.code_of_status (Cohttp.Response.status response) in
      if status <> 200 then
        Lwt.return
          (abort `Out
             (Fmt.str "could not get response receiving status of %d for url: %s" status
                url))
      else
        match content_length response with
        | Some total when total > 0 ->
            let* file = open_file (Filename.basename url) in
            Lwt.async (fun () -> pump ~push ~sink:(Lwt_io.write file) ~total chunks);
            Lwt.return_unit
        | _ -> Lwt.return (abort `Out "can't parse content length, aborting download"))

let main () =
  match url_arg Sys.argv with
  | None ->
      usage ();
      exit 1
  | Some url ->
      let values, push = Lwt_stream.create () in
      let* () = start ~push url in
      Smoke.run_ (app values)

let smoke () =
  let app = app (Lwt_stream.of_list []) in
  Smoke.expect app [] [ "0%"; "Press any key to quit" ]
  @ Smoke.expect app [ `Msg (Ratio 0.5); `Wait 3.0; Smoke.key "q" ] [ "50%" ]
  @ Smoke.expect app [ `Msg (Ratio 1.0); `Wait 3.0 ] [ "100%"; "Press any key to quit" ]
  @ Smoke.expect app
      [ `Msg (Failed "connection reset by peer") ]
      [ "Error downloading: connection reset by peer" ]
