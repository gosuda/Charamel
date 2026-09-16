module Cmd = Charm_tea.Cmd
module Sub = Charm_tea.Sub

let epsilon = 0.000001

type msg = Tick | Start | Stop | Reset
type t = { interval : float; elapsed : float; running : bool }

let v ?(interval = 1.0) () =
  { interval = max epsilon interval; elapsed = 0.0; running = false }

let elapsed t = t.elapsed
let running t = t.running
let start t = { t with running = true }
let stop t = { t with running = false }
let toggle t = if t.running then stop t else start t
let reset t = { t with elapsed = 0.0 }

let update message t =
  let t =
    match message with
    | Tick -> if t.running then { t with elapsed = t.elapsed +. t.interval } else t
    | Start -> start t
    | Stop -> stop t
    | Reset -> reset t
  in
  (t, Cmd.none)

let view t = Duration.to_string t.elapsed
let key _ _ = None
let subscriptions t = if t.running then Sub.every t.interval (fun _ -> Tick) else Sub.none
