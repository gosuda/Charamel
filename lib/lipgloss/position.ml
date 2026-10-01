type t = float

let v x = if Float.is_nan x then 0.0 else max 0.0 (min 1.0 x)
let left = 0.0
let top = 0.0
let center = 0.5
let right = 1.0
let bottom = 1.0
let to_float x = x

let split p n =
  if n <= 0 then (0, n)
  else if p = 0.0 then (0, n)
  else if p = 1.0 then (n, 0)
  else
    let share = int_of_float (Float.round (p *. float_of_int n)) in
    (share, n - share)
