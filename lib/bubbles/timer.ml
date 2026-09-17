module Cmd = Charamel_tea.Cmd
module Sub = Charamel_tea.Sub

let epsilon = 0.000001

type msg = Tick | Start | Stop | Toggle
type t = { interval : float; timeout : float; running : bool }

let v ?(interval = 1.0) ~timeout () =
  { interval = max epsilon interval; timeout; running = true }

let timed_out t = t.timeout <= 0.0
let running t = t.running && not (timed_out t)
let timeout t = t.timeout

let start t =
  if timed_out t then { t with running = false } else { t with running = true }

let stop t = { t with running = false }
let toggle t = if running t then stop t else start t

let update message t =
  let t =
    match message with
    | Tick -> if running t then { t with timeout = t.timeout -. t.interval } else t
    | Start -> start t
    | Stop -> stop t
    | Toggle -> toggle t
  in
  (t, Cmd.none)

let view t = Duration.to_string t.timeout
let key _ _ = None
let subscriptions t = if running t then Sub.every t.interval (fun _ -> Tick) else Sub.none
