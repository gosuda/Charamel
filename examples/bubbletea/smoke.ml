let key name =
  match Charamel_tea.Key.of_string name with
  | Ok key -> `Key key
  | Error (`Msg message) -> invalid_arg message

type 'msg event =
  [ `Key of Charamel_tea.Key.t
  | `Text of string
  | `Resize of int * int
  | `Msg of 'msg
  | `Wait of float ]

let frame ?(size = (24, 80)) app events =
  snd (Charamel_tea.Test.run app ~events:(events :> 'msg event list) ~size)

let expect ?size app events needles =
  let shown = frame ?size app events in
  List.map (fun needle -> (needle, shown)) needles

let run ?renderer ?color_profile ?fps ?filter app =
  let open Lwt.Syntax in
  let* result =
    Charamel_tea.run ?renderer ?color_profile ?fps ?filter ~clock:Charamel_os.Time.lwt app
  in
  match result with
  | Ok model -> Lwt.return (Some model)
  | Error `Interrupted -> Lwt.return None
  | Error (`Exn (exn, backtrace)) -> Printexc.raise_with_backtrace exn backtrace

let run_ app = Lwt.map ignore (run app)
